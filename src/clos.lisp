(in-package #:spec-protocol)

;;; CLOS projection: one DEFSPEC expands to a class (vars = slots, consts = initargs),
;;; generic functions + methods (actions, vals, defs), DEFTYPEs (sum types),
;;; CHECK-INVARIANTS, and registers the AST for the spec backends.
;;;
;;; Action semantics: guards and preconditions run first; every (setf var …) in the body is
;;; staged and all right-hand sides see the pre-state; the stage is committed at once;
;;; postconditions and invariants run on the post-state.

(defclass spec-state () ()
  (:documentation "Superclass of every DEFSPEC-generated state class."))

(defvar *check-invariants* t
  "When true, generated actions run CHECK-INVARIANTS on the post-state.")

(defgeneric spec-of (state) (:documentation "The SPEC-MODULE this state was generated from."))
(defgeneric spec-state-alist (state) (:documentation "Vars as an alist (var-symbol . value)."))
(defgeneric spec-const-alist (state) (:documentation "Consts as an alist (const-symbol . value)."))
(defgeneric spec-copy-state (state) (:documentation "Shallow copy (pre-state snapshot for :ensures)."))
(defgeneric check-invariants (state &key invariants)
  (:documentation "Signal SPEC-INVARIANT-VIOLATION (restart CONTINUE) for each failing invariant.
INVARIANTS is :ALL or a list of invariant names. Returns T."))
(defmethod check-invariants ((state spec-state) &key (invariants :all))
  (declare (ignore invariants))
  t)

(defmethod print-object ((state spec-state) stream)
  (print-unreadable-object (state stream :type t :identity nil)
    (format stream "~{~a=~s~^ ~}"
            (loop for (k . v) in (spec-state-alist state) collect k collect v))))

(defun %var-slot (state var)
  (let ((decl (find var (module-vars (spec-of state)) :key #'decl-lisp-name :test #'name=)))
    (unless decl (error 'spec-error :message (format nil "~a has no var ~a" (type-of state) var)))
    (decl-lisp-name decl)))

(defun spec-state-get (state var) (slot-value state (%var-slot state var)))
(defun spec-state-set (state var value) (setf (slot-value state (%var-slot state var)) value))

(defun action-generic (module name)
  "Generic-function name for action NAME (compared by NAME=)."
  (let ((decl (find name (module-actions module) :key #'decl-lisp-name :test #'name=)))
    (unless decl (error 'spec-error :message (format nil "~a has no action ~a" (module-name module) name)))
    (decl-lisp-name decl)))

(defun apply-action (state name &rest args)
  "Call action NAME on STATE. Returns T when applied, NIL when disabled and continued."
  (apply (fdefinition (action-generic (spec-of state) name)) state args))

;;; Runtime support called from generated code

(defun %disabled (state action form thunk)
  (restart-case (error 'spec-action-disabled :action action :guard form :state state)
    (continue ()
      :report "Skip the disabled action (no state change)."
      (funcall thunk))))

(defun %precondition-failed (state operator form)
  (restart-case (error 'spec-precondition-error :operator operator :form form :state state)
    (continue () :report "Proceed despite the violated precondition." nil)))

(defun %postcondition-failed (state operator form)
  (restart-case (error 'spec-postcondition-error :operator operator :form form :state state)
    (continue () :report "Accept the state despite the violated postcondition." nil)))

(defun %invariant-failed (state name)
  (restart-case (error 'spec-invariant-violation :invariant name :state state)
    (continue () :report "Ignore the violated invariant." nil)))

(defun %commit (state staged)
  (loop for (slot . value) in staged do (setf (slot-value state slot) value)))

;;; Types -> CL type specifiers

(defun cl-type (type module)
  (etypecase type
    (null t)
    (type-ref (case (type-ref-name type)
                (:int 'integer) (:str 'string) (:bool 'boolean)
                (t (let ((decl (module-find-decl module (type-ref-name type))))
                     (if decl (decl-lisp-name decl) t)))))
    (set-type 'spec-set)
    (list-type 'spec-seq)
    (map-type 'spec-map)
    (tuple-type 'list)
    (record-type 'list)
    (sum-type `(or ,@(loop for (label . payload) in (sum-variants type)
                           collect (if payload `(cons (member ,label) t) `(member ,label)))))))

;;; Expression compiler

(defstruct cctx module self old action-name (mode :expr))

(defun %ctx (ctx &rest changes)
  (let ((c (copy-cctx ctx)))
    (loop for (k v) on changes by #'cddr
          do (ecase k
               (:mode (setf (cctx-mode c) v))
               (:old (setf (cctx-old c) v))))
    c))

(defun %need-self (ctx what)
  (unless (cctx-self ctx)
    (error 'spec-emit-error :message (format nil "~a needs a state object (not allowed in instance bindings)" what))))

(defun %user-op (module name)
  (find name (module-ops module) :key #'decl-lisp-name :test #'name=))

(defun compile-expr (e ctx)
  "AST -> CL form (expression mode)."
  (let ((module (cctx-module ctx)) (self (cctx-self ctx)))
    (etypecase e
      (literal (case (literal-value e) (:true t) (:false nil) (t (literal-value e))))
      (name-ref
       (ecase (ref-kind e)
         (:param (ref-name e))
         ((:var :const) (%need-self ctx (ref-name e)) (ref-name e))
         (:pure-op `(,(ref-name e)))
         (:op (%need-self ctx (ref-name e)) `(,(ref-name e) ,self))))
      (variant-expr
       (if (variant-expr-arg e)
           `(cons ,(variant-expr-label e) ,(compile-expr (variant-expr-arg e) ctx))
           (variant-expr-label e)))
      (app (%compile-app e ctx))
      (lambda-expr `(lambda ,(lambda-params e) ,(compile-expr (lambda-body e) ctx)))
      (let-expr `(let* ,(loop for (v . x) in (let-bindings e) collect `(,v ,(compile-expr x ctx)))
                   ,(compile-expr (let-body e) ctx)))
      (if-expr `(if ,(compile-expr (if-test e) ctx)
                    ,(compile-expr (if-then e) ctx)
                    ,(compile-expr (if-else e) ctx)))
      (match-expr (%compile-match e ctx #'compile-expr))
      (record-expr `(rec ,@(loop for (f . x) in (record-fields e)
                                  collect (%label-keyword f) collect (compile-expr x ctx))))
      (field-access `(rec-field ,(compile-expr (access-record e) ctx) ,(%label-keyword (access-field e))))
      (lisp-expr (lisp-form e))
      (old-expr
       (unless (cctx-old ctx) (error 'spec-emit-error :message "(old …) outside :ensures"))
       `(let ((,self ,(cctx-old ctx))) ,(compile-expr (old-inner e) ctx)))
      (result-expr '%result)
      (all-expr `(and ,@(mapcar (lambda (f) (compile-expr f ctx)) (block-forms e))))
      ((or assign-expr any-expr nondet-expr raw-expr)
       (error 'spec-emit-error
              :message (format nil "~a has no CL meaning here: ~s" (type-of e) (expr-source e)))))))

(defun %compile-app (e ctx)
  (let* ((op (app-op e))
         (args (mapcar (lambda (a) (compile-expr a ctx)) (app-args e)))
         (b (builtin op)))
    (cond
      (b
       (when (builtin-spec-only-p b)
         (error 'spec-emit-error :message (format nil "~a is spec-only: ~s" (builtin-quint b) (expr-source e))))
       (case (builtin-style b)
         (:map `(make-map ,@(loop for tup in (app-args e)
                                  collect `(cons ,(compile-expr (first (app-args tup)) ctx)
                                                 ,(compile-expr (second (app-args tup)) ctx)))))
         (:tuple `(list ,@args))
         (:list `(make-seq ,@args))
         (:ctor `(make-set ,@args))
         (:with (destructuring-bind (r field v) (app-args e)
                  `(rec-with ,(compile-expr r ctx) ,(%label-keyword (literal-value field)) ,(compile-expr v ctx))))
         (t `(,(builtin-cl b) ,@args))))
      (t
       (let ((decl (%user-op (cctx-module ctx) op)))
         (unless decl (error 'spec-emit-error :message (format nil "unknown operator ~a" op)))
         (ecase (op-mode decl)
           ((:pure-val :pure-def) `(,(decl-lisp-name decl) ,@args))
           ((:val :def) (%need-self ctx op) `(,(decl-lisp-name decl) ,(cctx-self ctx) ,@args))
           (:action (error 'spec-emit-error
                           :message (format nil "action ~a called from CL code; compose actions in :spec-only steps" op)))
           ((:temporal :run) (error 'spec-emit-error :message (format nil "~a is spec-only" op)))))))))

(defun %compile-match (e ctx body-compiler)
  (let ((v (gensym "V")))
    `(let ((,v ,(compile-expr (match-scrutinee e) ctx)))
       (case (variant-label ,v)
         ,@(loop for (label var body) in (match-clauses e)
                 collect (if (eq label :default)
                             `(t ,(funcall body-compiler body ctx))
                             `((,label) ,(if var
                                             `(let ((,var (variant-value ,v))) ,(funcall body-compiler body ctx))
                                             (funcall body-compiler body ctx)))))
         ,@(unless (find :default (match-clauses e) :key #'first)
             `((t (error 'spec-error :message (format nil "no match clause for ~s" ,v)))))))))

;;; Action-mode compiler: setf stages, bare booleans guard, control flow recurses.

(defun compile-action-form (e ctx)
  (etypecase e
    (assign-expr `(push (cons ',(assign-var e) ,(compile-expr (assign-value e) ctx)) %staged))
    (all-expr `(progn ,@(mapcar (lambda (f) (compile-action-form f ctx)) (block-forms e)) nil))
    (if-expr `(if ,(compile-expr (if-test e) ctx)
                  ,(compile-action-form (if-then e) ctx)
                  ,(compile-action-form (if-else e) ctx)))
    (let-expr `(let* ,(loop for (v . x) in (let-bindings e) collect `(,v ,(compile-expr x ctx)))
                 ,(compile-action-form (let-body e) ctx)))
    (match-expr (%compile-match e ctx #'compile-action-form))
    (lisp-expr (lisp-form e))
    (literal (if (eq (literal-value e) :true) nil (%guard-form e ctx)))
    (expr (%guard-form e ctx))))

(defun %guard-form (e ctx)
  `(unless ,(compile-expr e ctx)
     (%disabled ,(cctx-self ctx) ',(cctx-action-name ctx) ',(expr-source e)
                (lambda () (return-from %action nil)))))

;;; Expansion

(defun %self-symbol (module)
  "The state parameter is named SELF in the spec's own package so (:lisp …) forms can use it."
  (intern "SELF" (symbol-package (module-lisp-name module))))

(defun %option (module key default)
  (getf (module-options module) key default))

(defun %class-name (module) (%option module :class (module-lisp-name module)))

(defun %accessor (module slot)
  (let ((prefix (%option module :accessor-prefix (format nil "~a-" (module-lisp-name module)))))
    (intern (string-upcase (format nil "~a~a" prefix slot)) (symbol-package (module-lisp-name module)))))

(defun %slot-macros (module self)
  (loop for d in (append (module-consts module) (module-vars module))
        collect `(,(decl-lisp-name d) (slot-value ,self ',(decl-lisp-name d)))))

(defun %declarations (params module)
  (let ((types (loop for (p . type) in params
                     when type collect `(type ,(cl-type type module) ,p))))
    (when types `((declare ,@types)))))

(defun expand-type-decl (d module)
  `(deftype ,(decl-lisp-name d) () ',(cl-type (type-decl-type d) module)))

(defun expand-class (module)
  (let ((class (%class-name module)))
    `(defclass ,class (,@(%option module :superclasses '()) spec-state)
       (,@(loop for c in (module-consts module)
                collect `(,(decl-lisp-name c) :initarg ,(%label-keyword (decl-lisp-name c))
                                              :reader ,(%accessor module (decl-lisp-name c))
                                              :type ,(cl-type (decl-type c) module)))
        ,@(loop for v in (module-vars module)
                collect `(,(decl-lisp-name v) :accessor ,(%accessor module (decl-lisp-name v))
                                              :type ,(cl-type (decl-type v) module)))))))

(defun expand-state-methods (module)
  (let ((class (%class-name module)) (self (%self-symbol module)))
    `((defmethod spec-of ((,self ,class)) (find-spec ',(module-lisp-name module)))
      (defmethod spec-state-alist ((,self ,class))
        (list ,@(loop for v in (module-vars module)
                      collect `(cons ',(decl-lisp-name v)
                                     (and (slot-boundp ,self ',(decl-lisp-name v))
                                          (slot-value ,self ',(decl-lisp-name v)))))))
      (defmethod spec-const-alist ((,self ,class))
        (list ,@(loop for c in (module-consts module)
                      collect `(cons ',(decl-lisp-name c) (slot-value ,self ',(decl-lisp-name c))))))
      (defmethod spec-copy-state ((,self ,class))
        (let ((copy (allocate-instance (class-of ,self))))
          ,@(loop for d in (append (module-consts module) (module-vars module))
                  collect `(when (slot-boundp ,self ',(decl-lisp-name d))
                             (setf (slot-value copy ',(decl-lisp-name d)) (slot-value ,self ',(decl-lisp-name d)))))
          copy)))))

(defun %contract-checks (forms ctx failer operator)
  (loop for f in forms
        collect `(unless ,(compile-expr f ctx)
                   (,failer ,(cctx-self ctx) ',operator ',(expr-source f)))))

(defun expand-op (op module)
  (let* ((self (%self-symbol module))
         (name (decl-lisp-name op))
         (params (mapcar #'car (op-params op)))
         (ctx (make-cctx :module module :self self :action-name name)))
    (ecase (op-mode op)
      ((:pure-val :pure-def)
       (let ((pure-ctx (make-cctx :module module :self nil)))
         `((defun ,name ,params
             ,@(%declarations (op-params op) module)
             ,@(%contract-checks (op-requires op) pure-ctx '%precondition-failed name)
             (let ((%result ,(compile-expr (op-body op) pure-ctx)))
               ,@(%contract-checks (op-ensures op) pure-ctx '%postcondition-failed name)
               %result)))))
      ((:val :def)
       `((defgeneric ,name (,self ,@params))
         (defmethod ,name ((,self ,(%class-name module)) ,@params)
           ,@(%declarations (op-params op) module)
           (symbol-macrolet ,(%slot-macros module self)
             ,@(%contract-checks (op-requires op) ctx '%precondition-failed name)
             (let ((%result ,(compile-expr (op-body op) ctx)))
               ,@(%contract-checks (op-ensures op) ctx '%postcondition-failed name)
               %result)))))
      (:action
       (let ((old (gensym "OLD")))
         `((defgeneric ,name (,self ,@params))
           (defmethod ,name ((,self ,(%class-name module)) ,@params)
             ,@(%declarations (op-params op) module)
             (symbol-macrolet ,(%slot-macros module self)
               (block %action
                 (let ((%staged '())
                       ,@(when (op-ensures op) `((,old (spec-copy-state ,self)))))
                   ,@(%contract-checks (op-requires op) ctx '%precondition-failed name)
                   ,@(loop for g in (op-guards op) collect (%guard-form g ctx))
                   ,(compile-action-form (op-body op) (%ctx ctx :mode :action))
                   (%commit ,self %staged)
                   ,@(%contract-checks (op-ensures op) (%ctx ctx :old old) '%postcondition-failed name)
                   (when *check-invariants* (check-invariants ,self))
                   t))))))))))

(defun expand-invariants (module)
  (let* ((self (%self-symbol module))
         (ctx (make-cctx :module module :self self))
         (invariants (module-invariants module)))
    `((defmethod check-invariants ((,self ,(%class-name module)) &key (invariants :all))
        (symbol-macrolet ,(%slot-macros module self)
          ,@(loop for inv in invariants
                  collect `(when (or (eq invariants :all) (member ',(decl-lisp-name inv) invariants :test #'name=))
                             (unless ,(compile-expr (op-body inv) ctx)
                               (%invariant-failed ,self ',(decl-lisp-name inv)))))
          t)))))

(defun expand-clos (module)
  "All CLOS forms for MODULE (excluding spec-only declarations)."
  (append
   (loop for d in (module-types module) collect (expand-type-decl d module))
   (list (expand-class module))
   (expand-state-methods module)
   (loop for op in (module-ops module)
         unless (decl-spec-only-p op)
           append (expand-op op module))
   (expand-invariants module)))

(defmacro defspec (name options &body decls)
  "Define a protocol as a formal spec and a CLOS protocol at once. See spec-protocol README."
  (let ((module (parse-spec-form name options decls)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (register-spec (parse-spec-form ',name ',options ',decls)))
       ,@(when (getf options :clos t) (expand-clos module))
       ',name)))

(defun %extend-spec (spec-name decl-form)
  "Parse DECL-FORM against SPEC-NAME's module and add it. Returns (values module decl)."
  (let* ((module (find-spec spec-name))
         (spec-only (and (consp decl-form) (eq (car decl-form) :spec-only)))
         (form (if spec-only (second decl-form) decl-form))
         (decl (parse-decl form (module-env module) spec-only)))
    (module-add-decl module decl)
    (values module decl)))

(defmacro defspec-decl (spec-name decl-form)
  "Add one declaration to an existing spec, expanding its CLOS side. Toplevel-redefinable."
  (multiple-value-bind (module decl) (%extend-spec spec-name decl-form)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (%extend-spec ',spec-name ',decl-form))
       ,@(cond ((decl-spec-only-p decl) '())
               ((typep decl 'op-def)
                (append (expand-op decl module)
                        (when (op-invariant-p decl) (expand-invariants module))))
               (t (error 'spec-emit-error
                         :message "defspec-decl supports operator declarations; vars/consts/types belong in defspec")))
       ',(decl-lisp-name decl))))

(defmacro defaction (spec-name name &rest rest)
  `(defspec-decl ,spec-name (action ,name ,@rest)))

(defmacro definvariant (spec-name name body)
  `(defspec-decl ,spec-name (invariant ,name ,body)))

(defmacro defrun (spec-name name body)
  `(defspec-decl ,spec-name (run ,name ,body)))
