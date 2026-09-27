(in-package #:spec-protocol.mbt)

;;; Model-based conformance: replay an ITF trace (quint run --mbt) against a system under test.

(defclass mbt-driver () ())

(defgeneric mbt-init (driver state)
  (:documentation "Build the SUT from the initial ITF-STATE. Returns the SUT."))
(defgeneric mbt-apply (driver sut action picks state)
  (:documentation "Apply ACTION (string from mbt::actionTaken) with PICKS (alist keyword . value) to SUT."))
(defgeneric mbt-observe (driver sut)
  (:documentation "Alist (variable-name-string . lisp-value) of the SUT's observable state."))
(defgeneric mbt-compare (driver variable expected actual)
  (:documentation "True when the SUT value ACTUAL conforms to the trace value EXPECTED."))

(defmethod mbt-compare ((driver mbt-driver) variable expected actual)
  (declare (ignore variable))
  (spec-protocol.itf:itf-equal expected actual))

(defun %check-state (driver sut trace state variables action)
  (let ((observed (mbt-observe driver sut)))
    (dolist (var (if (eq variables :all) (spec-protocol.itf:state-variables state) variables))
      (let* ((key (spec-protocol:name-last-segment (string var)))
             (cell (assoc key observed :test #'string=)))
        (when cell
          (let ((expected (spec-protocol.itf:state-value state key)))
            (unless (mbt-compare driver key expected (cdr cell))
              (restart-case
                  (error 'spec-protocol:spec-conformance-divergence
                         :trace trace :step-index (spec-protocol.itf:state-index state)
                         :action action :variable key :expected expected :actual (cdr cell))
                (continue () :report "Ignore this variable and keep replaying." nil)))))))))

(defun replay-trace (driver trace &key (variables :all) (check-initial t))
  "Replay TRACE on a SUT built by MBT-INIT. Signals SPEC-CONFORMANCE-DIVERGENCE (restarts CONTINUE,
ABORT-TRACE) when the SUT disagrees with a trace state. Returns (values SUT status) where
status is :OK or :ABORTED."
  (let* ((states (spec-protocol.itf:trace-states trace))
         (initial (first states))
         (sut (mbt-init driver initial)))
    (unless (spec-protocol.itf:state-action-taken initial)
      (error 'spec-protocol:spec-error
             :message "trace has no mbt::actionTaken — produce it with quint run --mbt"))
    (values sut
            (restart-case
                (progn
                  (when check-initial (%check-state driver sut trace initial variables "init"))
                  (loop for state in (rest states)
                        for action = (spec-protocol.itf:state-action-taken state)
                        do (mbt-apply driver sut action (spec-protocol.itf:state-nondet-picks state) state)
                           (%check-state driver sut trace state variables action))
                  :ok)
              (spec-protocol:abort-trace ()
                :report "Stop replaying this trace."
                :aborted)))))

(defun replay-traces (driver traces &rest keys)
  (mapcar (lambda (tr) (apply #'replay-trace driver tr keys)) traces))

;;; Automatic driver for DEFSPEC-generated classes: slots are the vars, generics are the actions.

(defclass clos-driver (mbt-driver)
  ((spec :initarg :spec :reader clos-driver-spec)          ; spec-module
   (consts :initarg :consts :initform nil :reader clos-driver-consts)))   ; alist (const-symbol . value)

(defun make-clos-driver (spec &key consts)
  "SPEC is a spec name or module. CONSTS override the instance bindings (alist symbol . value)."
  (let ((module (if (typep spec 'spec-protocol:spec-module) spec (spec-protocol:find-spec spec))))
    (make-instance 'clos-driver :spec module :consts consts)))

(defun %instance-consts (module)
  (let ((instance (spec-protocol:module-instance module)))
    (when instance
      (loop for (sym . expr) in (spec-protocol:instance-bindings instance)
            collect (cons sym (eval (spec-protocol::compile-expr
                                     expr (spec-protocol::make-cctx :module module :self nil))))))))

(defmethod mbt-init ((driver clos-driver) state)
  (let* ((module (clos-driver-spec driver))
         (class (spec-protocol::%class-name module))
         (consts (or (clos-driver-consts driver) (%instance-consts module)))
         (sut (apply #'make-instance class
                     (loop for (sym . value) in consts
                           collect (intern (symbol-name sym) :keyword) collect value))))
    (dolist (var (spec-protocol:module-vars module) sut)
      (let ((name (spec-protocol:decl-name var)))
        (when (nth-value 1 (gethash name (spec-protocol.itf:state-values state)))
          (spec-protocol:spec-state-set sut (spec-protocol:decl-lisp-name var)
                                        (spec-protocol.itf:state-value state name)))))))

(defmethod mbt-apply ((driver clos-driver) sut action picks state)
  (declare (ignore state))
  (let* ((module (clos-driver-spec driver))
         (decl (find action (spec-protocol:module-actions module)
                     :key #'spec-protocol:decl-name :test #'string=)))
    (unless decl
      (error 'spec-protocol:spec-error :message (format nil "trace action ~a is not an action of ~a"
                                                        action (spec-protocol:module-name module))))
    (let ((args (loop for (param . nil) in (spec-protocol:op-params decl)
                      for key = (spec-protocol:demangle-name (spec-protocol:mangle-name param))
                      for cell = (assoc key picks)
                      do (unless cell
                           (error 'spec-protocol:spec-error
                                  :message (format nil "trace has no nondet pick for parameter ~a of ~a" param action)))
                      collect (cdr cell))))
      (handler-bind ((spec-protocol:spec-action-disabled #'continue))
        (apply #'spec-protocol:apply-action sut (spec-protocol:decl-lisp-name decl) args)))))

(defmethod mbt-observe ((driver clos-driver) sut)
  (loop for (sym . value) in (spec-protocol:spec-state-alist sut)
        collect (cons (spec-protocol:mangle-name sym) value)))
