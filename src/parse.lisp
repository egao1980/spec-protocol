(in-package #:spec-protocol)

;;; sexp DSL -> AST. All identifiers are compared by SYMBOL-NAME (see NAME=), so the DSL
;;; reads the same from any package — the same rule LOOP keywords follow.

(defstruct (penv (:constructor %make-penv))
  (consts '()) (vars '()) (types '()) (labels '())
  (ops '())                 ; alist (symbol . mode)
  (scope '())               ; lexical symbols: params, let, lambda, nondet
  (action-p nil)            ; inside an action body (setf allowed)
  (spec-only-p nil)         ; inside a spec-only decl (any / nondet / one-of / :raw allowed)
  (old-p nil)               ; inside an :ensures clause ((old …) allowed)
  (result-p nil))           ; inside a function :ensures clause ((result) allowed)

(defun %fail (form fmt &rest args)
  (restart-case (error 'spec-parse-error :form form :message (apply #'format nil fmt args))
    (use-value (value)
      :report "Use a replacement AST node."
      value)))

(defun %head-is (form name)
  (and (consp form) (symbolp (car form)) (string= (symbol-name (car form)) name)))

(defun %sym-in (sym list) (member sym list :test #'name=))
(defun %assoc-name (sym alist) (assoc sym alist :test #'name=))

;;; Types

(defparameter *primitive-types* '("INT" "STR" "BOOL"))

(defun parse-type (form &optional env)
  (cond
    ((and (symbolp form) (member (symbol-name form) *primitive-types* :test #'string=))
     (make-instance 'type-ref :name (intern (symbol-name form) :keyword)))
    ((symbolp form)
     (when (and env (not (%sym-in form (penv-types env))))
       (%fail form "unknown type ~a" form))
     (make-instance 'type-ref :name form))
    ((%head-is form "SET") (make-instance 'set-type :elem (parse-type (second form) env)))
    ((%head-is form "LIST") (make-instance 'list-type :elem (parse-type (second form) env)))
    ((%head-is form "MAP")
     (make-instance 'map-type :key (parse-type (second form) env) :value (parse-type (third form) env)))
    ((%head-is form "TUPLE")
     (make-instance 'tuple-type :elems (mapcar (lambda (f) (parse-type f env)) (cdr form))))
    ((%head-is form "REC")
     (make-instance 'record-type
                    :fields (mapcar (lambda (f) (cons (first f) (parse-type (second f) env))) (cdr form))))
    ((%head-is form "OR")
     (make-instance 'sum-type :variants (mapcar (lambda (v) (%parse-variant v env)) (cdr form))))
    (t (%fail form "malformed type"))))

(defun %label-keyword (designator)
  (intern (string-upcase (symbol-name designator)) :keyword))

(defun %parse-variant (v env)
  (cond ((symbolp v) (cons (%label-keyword v) nil))
        ((and (consp v) (symbolp (first v)))
         (cons (%label-keyword (first v)) (and (second v) (parse-type (second v) env))))
        (t (%fail v "malformed variant"))))

;;; Expressions

(defun %params (list)
  "((who str) n) -> alist (symbol . type-or-nil)"
  (mapcar (lambda (p) (if (consp p) (cons (first p) (parse-type (second p))) (cons p nil))) list))

(defun %with-scope (env symbols)
  (let ((e (copy-penv env)))
    (setf (penv-scope e) (append symbols (penv-scope e)))
    e))

(defun %label-p (sym env)
  (or (keywordp sym)
      (and (symbolp sym) (member (%label-keyword sym) (penv-labels env)))))

(defun %spec-only-only (form env what)
  (unless (penv-spec-only-p env)
    (%fail form "~a is spec-only: wrap the declaration in (:spec-only …)" what)))

(defun parse-expr (form env)
  (let ((node (%parse-expr form env)))
    (when (typep node 'expr) (setf (expr-source node) form))
    node))

(defun %parse-expr (form env)
  (cond
    ((integerp form) (make-instance 'literal :value form))
    ((stringp form) (make-instance 'literal :value form))
    ((eq form t) (make-instance 'literal :value :true))
    ((null form) (make-instance 'literal :value :false))
    ((keywordp form) (make-instance 'variant-expr :label form))
    ((symbolp form) (%parse-name form env))
    ((consp form) (%parse-compound form env))
    (t (%fail form "unsupported literal"))))

(defun %parse-name (sym env)
  (let ((kind (cond ((%sym-in sym (penv-scope env)) :param)
                    ((%sym-in sym (penv-vars env)) :var)
                    ((%sym-in sym (penv-consts env)) :const)
                    ((%assoc-name sym (penv-ops env))
                     (let ((mode (cdr (%assoc-name sym (penv-ops env)))))
                       (if (member mode '(:pure-val :pure-def)) :pure-op :op)))
                    ((%label-p sym env) :label)
                    (t nil))))
    (case kind
      ((nil) (%fail sym "unknown name ~a" sym))
      (:label (make-instance 'variant-expr :label (%label-keyword sym)))
      (t (make-instance 'name-ref :name sym :kind kind)))))

(defun %assign (form var value env)
  (unless (penv-action-p env) (%fail form "assignment outside an action"))
  (unless (%sym-in var (penv-vars env)) (%fail form "assignment to non-var ~a" var))
  (make-instance 'assign-expr :var var :value value))

(defun %parse-compound (form env)
  (let* ((head (car form))
         (hname (and (symbolp head) (symbol-name head)))
         (args (cdr form)))
    (flet ((sub (f &optional (e env)) (parse-expr f e))
           (subs (fs &optional (e env)) (mapcar (lambda (f) (parse-expr f e)) fs)))
      (cond
        ((eq head :raw)
         (%spec-only-only form env ":raw")
         (make-instance 'raw-expr :text (first args)))
        ((eq head :lisp) (make-instance 'lisp-expr :form (first args)))
        ((null hname) (%fail form "operator must be a symbol"))
        ((string= hname "SETF")
         (let ((assigns (loop for (var value) on args by #'cddr
                              collect (%assign form var (sub value) env))))
           (if (= (length assigns) 1)
               (first assigns)
               (make-instance 'all-expr :forms assigns))))
        ((or (string= hname "INCF") (string= hname "DECF"))
         (let ((var (first args))
               (delta (if (rest args) (sub (second args)) (make-instance 'literal :value 1))))
           (%assign form var
                    (make-instance 'app :op (if (string= hname "INCF") '+ '-)
                                        :args (list (sub var) delta))
                    env)))
        ((string= hname "ALL") (make-instance 'all-expr :forms (subs args)))
        ((string= hname "ANY")
         (%spec-only-only form env "any")
         (make-instance 'any-expr :forms (subs args)))
        ((string= hname "NONDET")
         (%spec-only-only form env "nondet")
         (destructuring-bind ((var domain) body) args
           (make-instance 'nondet-expr :var var :domain (sub domain)
                                       :body (sub body (%with-scope env (list var))))))
        ((string= hname "OLD")
         (unless (penv-old-p env) (%fail form "(old …) is only valid inside :ensures"))
         (make-instance 'old-expr :expr (sub (first args))))
        ((string= hname "RESULT")
         (unless (penv-result-p env) (%fail form "(result) is only valid inside a function :ensures"))
         (make-instance 'result-expr))
        ((string= hname "IF")
         (destructuring-bind (test then &optional (else nil else-p)) args
           ;; A missing else is a no-op in an action (frame condition added by the emitter)
           ;; and false in an expression.
           (make-instance 'if-expr :test (sub test) :then (sub then)
                                   :else (cond (else-p (sub else))
                                               ((penv-action-p env) (make-instance 'all-expr :forms '()))
                                               (t (make-instance 'literal :value :false))))))
        ((string= hname "LET")
         (destructuring-bind (bindings body) args
           (let ((e env) (parsed '()))
             (dolist (b bindings)
               (push (cons (first b) (sub (second b) e)) parsed)
               (setf e (%with-scope e (list (first b)))))
             (make-instance 'let-expr :bindings (nreverse parsed) :body (sub body e)))))
        ((string= hname "LAMBDA")
         (destructuring-bind (params body) args
           (make-instance 'lambda-expr :params params
                                       :body (sub body (%with-scope env params)))))
        ((string= hname "MATCH")
         (make-instance 'match-expr :scrutinee (sub (first args))
                                    :clauses (mapcar (lambda (c) (%parse-clause c env)) (rest args))))
        ((string= hname "REC")
         (make-instance 'record-expr :fields (mapcar (lambda (f) (cons (first f) (sub (second f)))) args)))
        ((string= hname "FIELD")
         (make-instance 'field-access :record (sub (first args)) :field (second args)))
        ((string= hname "MAP-OF")
         ;; (map-of ((k S)) v) => S.mapBy(k => v)
         (destructuring-bind (((var domain)) body) args
           (make-instance 'app :op 'map-by
                               :args (list (sub domain)
                                           (make-instance 'lambda-expr :params (list var)
                                                                       :body (sub body (%with-scope env (list var))))))))
        ((string= hname "MAP-LIT")
         (make-instance 'app :op 'map-lit
                             :args (mapcar (lambda (kv) (make-instance 'app :op 'tuple :args (subs kv))) args)))
        ((string= hname "WITH")
         (destructuring-bind (record field value) args
           (make-instance 'app :op 'with :args (list (sub record) (make-instance 'literal :value field) (sub value)))))
        ((string= hname "ITEM")
         (make-instance 'app :op 'item :args (list (sub (first args)) (make-instance 'literal :value (second args)))))
        ((builtin head)
         (let ((b (builtin head)))
           (when (builtin-spec-only-p b) (%spec-only-only form env (builtin-quint b)))
           (make-instance 'app :op (builtin-name b) :args (subs args))))
        ((%label-p head env)
         (make-instance 'variant-expr :label (%label-keyword head) :arg (and args (sub (first args)))))
        ((%assoc-name head (penv-ops env))
         (when (and (eq (cdr (%assoc-name head (penv-ops env))) :action)
                    (not (penv-spec-only-p env)))
           (%fail form "calling action ~a from an action is spec-only (simultaneous update)" head))
         (make-instance 'app :op head :args (subs args)))
        (t (%fail form "unknown operator ~a" head))))))

(defun %parse-clause (clause env)
  (destructuring-bind (pattern body) clause
    (cond ((and (symbolp pattern) (string= (symbol-name pattern) "_"))
           (list :default nil (parse-expr body env)))
          ((keywordp pattern) (list pattern nil (parse-expr body env)))
          ((and (consp pattern) (null (cdr pattern)))
           (list (%label-keyword (first pattern)) nil (parse-expr body env)))
          ((consp pattern)
           (list (%label-keyword (first pattern)) (second pattern)
                 (parse-expr body (%with-scope env (list (second pattern))))))
          (t (%fail clause "malformed match clause")))))

;;; Declarations

(defparameter *op-heads*
  '(("PURE-VAL" . :pure-val) ("PURE-DEF" . :pure-def) ("VAL" . :val) ("DEF" . :def)
    ("ACTION" . :action) ("TEMPORAL" . :temporal) ("RUN" . :run)
    ("INVARIANT" . :val) ("WITNESS" . :val)))

(defun %decl-head (form)
  (and (consp form) (symbolp (car form)) (symbol-name (car form))))

(defun %flatten-decls (forms)
  "Expand (:spec-only d1 d2) into (d1 . spec-only) entries."
  (loop for f in forms
        if (and (consp f) (eq (car f) :spec-only))
          append (mapcar (lambda (d) (cons d t)) (cdr f))
        else collect (cons f nil)))

(defparameter *special-heads*
  '("SETF" "INCF" "DECF" "ALL" "ANY" "IF" "LET" "LAMBDA" "MATCH" "NONDET" "REC" "FIELD"
    "MAP-OF" "MAP-LIT" "WITH" "ITEM" "OLD" "RESULT"))

(defun %operator-name-p (sym env)
  (and (symbolp sym)
       (or (member (symbol-name sym) *special-heads* :test #'string=)
           (builtin sym)
           (%assoc-name sym (penv-ops env))
           (%label-p sym env))))

(defun %param-list-p (x env)
  "A parameter list is a list of plain symbols or (symbol type) pairs whose first element
is not an operator — so (incf x) as the first body form is not mistaken for parameters."
  (and (listp x)
       (every (lambda (p) (or (and (symbolp p) p (not (keywordp p)))
                              (and (consp p) (symbolp (first p)) (= (length p) 2))))
              x)
       (or (null x)
           (let ((head (if (consp (first x)) (first (first x)) (first x))))
             (not (%operator-name-p head env))))))

(defun %clause-p (x) (and (consp x) (member (car x) '(:requires :guard :ensures :returns))))

(defun %split-op-form (mode form env)
  "-> (values name params clauses body doc). Params are optional; clauses are
(:requires …) (:guard …) (:ensures …) (:returns T) lists between params and body."
  (let* ((rest (cddr form))
         (name (second form))
         (params nil)
         (doc nil)
         (clauses '()))
    (when (and (member mode '(:pure-def :def :action))
               (> (length rest) 1)
               (%param-list-p (first rest) env)
               (not (%clause-p (first rest))))
      (setf params (first rest) rest (cdr rest)))
    (when (and (> (length rest) 1) (stringp (first rest)))
      (setf doc (first rest) rest (cdr rest)))
    (loop while (and (> (length rest) 1) (%clause-p (first rest)))
          do (push (first rest) clauses) (setf rest (cdr rest)))
    (cond ((null rest) (%fail form "operator ~a has no body" name))
          ((and (cdr rest) (eq mode :action))
           ;; several body forms in an action = implicit (all …)
           (setf rest (list (cons (intern "ALL" :spec-protocol) rest))))
          ((cdr rest) (%fail form "operator ~a must have exactly one body form" name)))
    (values name params (nreverse clauses) (first rest) doc)))

(defun %collect-env (decls)
  (let ((env (%make-penv)))
    (loop for (form . nil) in decls
          for head = (%decl-head form)
          do (cond ((null head) (%fail form "malformed declaration"))
                   ((string= head "CONST") (push (second form) (penv-consts env)))
                   ((string= head "VAR") (push (second form) (penv-vars env)))
                   ((string= head "TYPE")
                    (push (second form) (penv-types env))
                    (when (%head-is (third form) "OR")
                      (dolist (v (cdr (third form)))
                        (push (%label-keyword (if (consp v) (first v) v)) (penv-labels env)))))
                   ((assoc head *op-heads* :test #'string=)
                    (push (cons (second form) (cdr (assoc head *op-heads* :test #'string=)))
                          (penv-ops env)))
                   ((member head '("IMPORT" "ASSUME") :test #'string=))
                   (t (%fail form "unknown declaration ~a" head))))
    env))

(defun module-env (module)
  "Parse environment for extending an existing module (DEFACTION etc.)."
  (let ((env (%make-penv)))
    (setf (penv-consts env) (mapcar #'decl-lisp-name (module-consts module))
          (penv-vars env) (mapcar #'decl-lisp-name (module-vars module))
          (penv-types env) (mapcar #'decl-lisp-name (module-types module))
          (penv-labels env) (module-labels module)
          (penv-ops env) (mapcar (lambda (op) (cons (decl-lisp-name op) (op-mode op))) (module-ops module)))
    env))

(defun %keyword-args (form start)
  (let ((tail (nthcdr start form)))
    (loop for (k v) on tail by #'cddr
          when (keywordp k) collect k and collect v)))

(defun %parse-clauses (clauses env mode)
  "-> (values requires guards ensures return-type)."
  (let ((requires '()) (guards '()) (ensures '()) (returns nil))
    (dolist (c clauses)
      (let ((e (copy-penv env)))
        (setf (penv-action-p e) nil)
        (ecase (car c)
          (:returns (setf returns (parse-type (second c) env)))
          (:requires (dolist (f (cdr c)) (push (parse-expr f e) requires)))
          (:guard
           (unless (eq mode :action) (%fail c ":guard is only valid on actions"))
           (dolist (f (cdr c)) (push (parse-expr f e) guards)))
          (:ensures
           (setf (penv-old-p e) (eq mode :action)
                 (penv-result-p e) (not (eq mode :action)))
           (dolist (f (cdr c)) (push (parse-expr f e) ensures))))))
    (values (nreverse requires) (nreverse guards) (nreverse ensures) returns)))

(defun parse-decl (form env spec-only-p)
  (let ((head (%decl-head form)))
    (flet ((mk (class &rest initargs)
             (apply #'make-instance class :name (second form) :spec-only-p spec-only-p initargs)))
      (cond
        ((string= head "CONST")
         (mk 'const-decl :type (parse-type (third form) env) :as (getf (%keyword-args form 3) :as)))
        ((string= head "VAR")
         (mk 'var-decl :type (parse-type (third form) env) :as (getf (%keyword-args form 3) :as)))
        ((string= head "TYPE")
         (mk 'type-decl :type (parse-type (third form) env) :as (getf (%keyword-args form 3) :as)))
        ((string= head "ASSUME")
         (mk 'assume-decl :body (parse-expr (third form) env) :spec-only-p t))
        ((string= head "IMPORT")
         (let ((opts (%keyword-args form 2)))
           (mk 'import-decl :module (second form) :alias (getf opts :as)
                            :wildcard-p (getf opts :wildcard t)
                            :overrides (mapcar (lambda (b) (cons (first b) (parse-expr (second b) env)))
                                               (getf opts :bindings)))))
        ((assoc head *op-heads* :test #'string=)
         (let* ((mode (cdr (assoc head *op-heads* :test #'string=)))
                (spec-only (or spec-only-p (member mode '(:temporal :run)))))
           (multiple-value-bind (name params clauses body doc) (%split-op-form mode form env)
             (let* ((plist (%params params))
                    (e (%with-scope env (mapcar #'car plist))))
               (setf (penv-action-p e) (eq mode :action)
                     (penv-spec-only-p e) (and spec-only t))
               (multiple-value-bind (requires guards ensures returns) (%parse-clauses clauses e mode)
                 (make-instance 'op-def :name name :mode mode :params plist :doc doc
                                        :return-type returns
                                        :invariant-p (string= head "INVARIANT")
                                        :witness-p (string= head "WITNESS")
                                        :requires requires :guards guards :ensures ensures
                                        :spec-only-p (and spec-only t)
                                        :body (parse-expr body e)))))))
        (t (%fail form "unknown declaration ~a" head))))))

(defun %parse-instance (spec env)
  (when spec
    (destructuring-bind (name &rest bindings) spec
      (make-instance 'instance-decl
                     :name name
                     :bindings (mapcar (lambda (b)
                                         (unless (%sym-in (first b) (penv-consts env))
                                           (%fail b "instance binds unknown const ~a" (first b)))
                                         (cons (first b) (parse-expr (second b) env)))
                                       bindings)))))

(defun parse-spec-form (name options decl-forms)
  "Parse (defspec NAME OPTIONS . DECL-FORMS) into a SPEC-MODULE."
  (let* ((flat (%flatten-decls decl-forms))
         (env (%collect-env flat))
         (decls (loop for (form . spec-only) in flat
                      collect (parse-decl form env spec-only))))
    (make-instance 'spec-module
                   :lisp-name name
                   :name (or (getf options :module) (mangle-type-name name))
                   :doc (getf options :doc)
                   :options options
                   :instance (%parse-instance (getf options :instance) env)
                   :decls decls)))

(defun module-add-decl (module decl)
  "Append DECL to MODULE, replacing a same-named declaration."
  (setf (module-decls module)
        (append (remove (decl-lisp-name decl) (module-decls module)
                        :key #'decl-lisp-name :test #'name=)
                (list decl)))
  decl)

;;; Registry

(defvar *specs* (make-hash-table :test #'equal)
  "Registered SPEC-MODULEs keyed by SYMBOL-NAME of the Lisp name.")

(defun register-spec (module)
  (setf (gethash (symbol-name (module-lisp-name module)) *specs*) module))

(defun find-spec (name &optional (errorp t))
  (or (gethash (symbol-name name) *specs*)
      (and errorp (error 'spec-error :message (format nil "no spec named ~a" name)))))

(defun list-specs ()
  (loop for m being the hash-values of *specs* collect m))
