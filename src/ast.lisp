(in-package #:spec-protocol)

;;; Backend-neutral spec AST. Both projections (CLOS expansion, Quint emission) walk this.

;;; Types

(defclass spec-type () ())
(defclass type-ref (spec-type)
  ((name :initarg :name :reader type-ref-name))
  (:documentation "int / str / bool or a declared type name (Lisp symbol)."))
(defclass set-type (spec-type) ((elem :initarg :elem :reader type-elem)))
(defclass list-type (spec-type) ((elem :initarg :elem :reader type-elem)))
(defclass map-type (spec-type)
  ((key :initarg :key :reader type-key) (value :initarg :value :reader type-value)))
(defclass tuple-type (spec-type) ((elems :initarg :elems :reader type-elems)))
(defclass record-type (spec-type)
  ((fields :initarg :fields :reader type-fields :documentation "alist (symbol . spec-type)")))
(defclass sum-type (spec-type)
  ((variants :initarg :variants :reader sum-variants
             :documentation "alist (label-keyword . payload-type-or-nil)")))

;;; Expressions

(defclass expr ()
  ((source :initarg :source :initform nil :accessor expr-source)))   ; original sexp, for messages
(defclass literal (expr) ((value :initarg :value :reader literal-value)))   ; integer | string | :true | :false
(defclass name-ref (expr)
  ((name :initarg :name :reader ref-name)
   (kind :initarg :kind :accessor ref-kind :initform nil)))   ; :var :const :param :op :pure-op, resolved by parser
(defclass app (expr)
  ((op :initarg :op :reader app-op)          ; builtin symbol or user op symbol
   (args :initarg :args :reader app-args)))
(defclass lambda-expr (expr)
  ((params :initarg :params :reader lambda-params) (body :initarg :body :reader lambda-body)))
(defclass let-expr (expr)
  ((bindings :initarg :bindings :reader let-bindings)   ; alist (symbol . expr)
   (body :initarg :body :reader let-body)))
(defclass if-expr (expr)
  ((test :initarg :test :reader if-test) (then :initarg :then :reader if-then) (else :initarg :else :reader if-else)))
(defclass match-expr (expr)
  ((scrutinee :initarg :scrutinee :reader match-scrutinee)
   (clauses :initarg :clauses :reader match-clauses
            :documentation "list of (label-or-:default var-or-nil body-expr)")))
(defclass record-expr (expr) ((fields :initarg :fields :reader record-fields)))   ; alist (symbol . expr)
(defclass field-access (expr)
  ((record :initarg :record :reader access-record) (field :initarg :field :reader access-field)))
(defclass assign-expr (expr)
  ((var :initarg :var :reader assign-var) (value :initarg :value :reader assign-value)))
(defclass nondet-expr (expr)
  ((var :initarg :var :reader nondet-var) (domain :initarg :domain :reader nondet-domain)
   (body :initarg :body :reader nondet-body)))
(defclass block-expr (expr) ((forms :initarg :forms :reader block-forms)))
(defclass all-expr (block-expr) ())
(defclass any-expr (block-expr) ())
(defclass variant-expr (expr)
  ((label :initarg :label :reader variant-expr-label)
   (arg :initarg :arg :reader variant-expr-arg :initform nil)))
(defclass raw-expr (expr) ((text :initarg :text :reader raw-text)))          ; spec-only verbatim text
(defclass lisp-expr (expr) ((form :initarg :form :reader lisp-form)))        ; CL-only verbatim form
(defclass old-expr (expr) ((expr :initarg :expr :reader old-inner)))         ; pre-state value in :ensures
(defclass result-expr (expr) ())                                             ; return value in function :ensures

;;; Declarations

(defclass decl ()
  ((name :initarg :name :reader decl-lisp-name)
   (as :initarg :as :initform nil :reader decl-as)
   (doc :initarg :doc :initform nil :reader decl-doc)
   (spec-only-p :initarg :spec-only-p :initform nil :accessor decl-spec-only-p)))

(defgeneric decl-name (decl)
  (:documentation "Spec identifier (mangled or :AS override)."))
(defmethod decl-name ((d decl))
  (or (decl-as d) (mangle-name (decl-lisp-name d))))

(defclass const-decl (decl) ((type :initarg :type :reader decl-type)))
(defclass var-decl (decl) ((type :initarg :type :reader decl-type)))
(defclass type-decl (decl) ((type :initarg :type :reader type-decl-type)))
(defmethod decl-name ((d type-decl))
  (or (decl-as d) (mangle-type-name (decl-lisp-name d))))
(defclass assume-decl (decl) ((body :initarg :body :reader op-body)))
(defclass import-decl (decl)
  ((module :initarg :module :reader import-module)
   (alias :initarg :alias :initform nil :reader import-alias)
   (overrides :initarg :overrides :initform nil :reader import-overrides)   ; alist (symbol . expr)
   (wildcard-p :initarg :wildcard-p :initform t :reader import-wildcard-p)))
(defclass op-def (decl)
  ((mode :initarg :mode :reader op-mode)              ; :pure-val :pure-def :val :def :action :temporal :run
   (params :initarg :params :initform nil :reader op-params)   ; alist (symbol . spec-type-or-nil)
   (return-type :initarg :return-type :initform nil :reader op-return-type)
   (body :initarg :body :reader op-body)
   (invariant-p :initarg :invariant-p :initform nil :reader op-invariant-p)
   (witness-p :initarg :witness-p :initform nil :reader op-witness-p)
   (requires :initarg :requires :initform nil :reader op-requires)    ; preconditions (caller obligation)
   (guards :initarg :guards :initform nil :reader op-guards)          ; enabling conditions (actions)
   (ensures :initarg :ensures :initform nil :reader op-ensures)))     ; postconditions

(defclass instance-decl ()
  ((name :initarg :name :reader instance-name)          ; string, e.g. "BankTest"
   (bindings :initarg :bindings :reader instance-bindings)))   ; alist (const-symbol . expr)

(defclass spec-module ()
  ((lisp-name :initarg :lisp-name :reader module-lisp-name)
   (name :initarg :name :reader module-name)             ; string
   (decls :initarg :decls :accessor module-decls)
   (instance :initarg :instance :initform nil :reader module-instance)
   (doc :initarg :doc :initform nil :reader module-doc)
   (options :initarg :options :initform nil :reader module-options)))

(defun %decls-of-type (module type)
  (remove-if-not (lambda (d) (typep d type)) (module-decls module)))

(defun module-consts (m) (%decls-of-type m 'const-decl))
(defun module-vars (m) (%decls-of-type m 'var-decl))
(defun module-types (m) (%decls-of-type m 'type-decl))
(defun module-ops (m) (%decls-of-type m 'op-def))
(defun module-actions (m) (remove-if-not (lambda (d) (eq (op-mode d) :action)) (module-ops m)))
(defun module-invariants (m) (remove-if-not #'op-invariant-p (module-ops m)))
(defun module-witnesses (m) (remove-if-not #'op-witness-p (module-ops m)))
(defun module-runs (m) (remove-if-not (lambda (d) (eq (op-mode d) :run)) (module-ops m)))
(defun module-find-decl (m name &optional (type 'decl))
  "Declaration named NAME (by NAME=), restricted to TYPE, e.g. 'type-decl or '(not type-decl)."
  (find-if (lambda (d) (and (typep d type) (name= (decl-lisp-name d) name))) (module-decls m)))
(defun module-labels (m)
  "All sum-type variant labels declared in M (keywords)."
  (loop for td in (module-types m)
        when (typep (type-decl-type td) 'sum-type)
          append (mapcar #'car (sum-variants (type-decl-type td)))))
(defun module-main-name (m)
  "Module name to hand to the tool: the instance module when present."
  (if (module-instance m) (instance-name (module-instance m)) (module-name m)))

;;; Builtin operator table. One row = one Lisp operator that both projections understand.
;;;   quint   textual name
;;;   style   :infix | :prefix | :method | :ctor | :list | :tuple | :item | :with
;;;   cl      function symbol for the CLOS projection, NIL when spec-only

(defstruct (builtin (:predicate builtin-struct-p)) name quint style cl spec-only-p)

;;; Keyed by SYMBOL-NAME: DSL forms are read in the user's package, so `union` or `put`
;;; there is not EQ to ours. Every name comparison in the parser goes through NAME=.
(defparameter *builtins* (make-hash-table :test #'equal))

(defun name= (a b)
  (and (symbolp a) (symbolp b) (string= (symbol-name a) (symbol-name b))))

(defmacro define-builtins (&body rows)
  `(progn
     ,@(loop for (name quint style cl) in rows
             collect `(setf (gethash ,(symbol-name name) *builtins*)
                            (make-builtin :name ',name :quint ,quint :style ,style
                                          :cl ',cl :spec-only-p ,(null cl))))))

(defun %not-equal (a b) (not (spec-equal a b)))
(defun %implies (a b) (or (not a) (and b t)))
(defun %iff (a b) (eq (and a t) (and b t)))
(defun %contains (set x) (set-in-p x set))

(define-builtins
  (+ "+" :infix +) (- "-" :infix -) (* "*" :infix *) (/ "/" :infix int-div) (% "%" :infix rem)
  (= "==" :infix spec-equal) (/= "!=" :infix %not-equal)
  (< "<" :infix <) (<= "<=" :infix <=) (> ">" :infix >) (>= ">=" :infix >=)
  (and "and" :infix and) (or "or" :infix or)
  (not "not" :prefix not)
  (implies "implies" :method %implies) (iff "iff" :method %iff)
  (in "in" :method set-in-p) (contains "contains" :method %contains)
  (union "union" :method set-union) (intersect "intersect" :method set-intersect)
  (exclude "exclude" :method set-exclude) (subseteq "subseteq" :method set-subset-p)
  (size "size" :method set-size) (fold "fold" :method set-fold)
  (map "map" :method set-map) (filter "filter" :method set-filter)
  (exists "exists" :method set-exists) (forall "forall" :method set-forall)
  (to "to" :method int-range) (one-of "oneOf" :method nil)
  (get "get" :method map-get) (put "set" :method map-put) (set-by "setBy" :method map-set-by)
  (keys "keys" :method map-keys) (map-by "mapBy" :method map-by)
  (append "append" :method seq-append) (concat "concat" :method seq-concat)
  (head "head" :method seq-head) (tail "tail" :method seq-tail)
  (length "length" :method seq-length) (nth "nth" :method seq-nth)
  (foldl "foldl" :method seq-foldl) (select "select" :method seq-select)
  (set "Set" :ctor make-set) (list "List" :list vector) (tuple "" :tuple list) (map-lit "Map" :map make-map)
  (item "" :item tuple-item) (with "with" :with rec-with)
  (then "then" :method nil) (reps "reps" :method nil) (expect "expect" :method nil) (fail "fail" :method nil)
  (always "always" :prefix nil) (eventually "eventually" :prefix nil) (next "next" :prefix nil))

(defun builtin (name) (and (symbolp name) (not (keywordp name)) (gethash (symbol-name name) *builtins*)))
(defun builtin-p (name) (and (builtin name) t))
