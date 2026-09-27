(in-package #:spec-protocol)

(define-condition spec-error (error)
  ((message :initarg :message :reader spec-error-message :initform nil))
  (:report (lambda (c s)
             (format s "spec error~@[: ~a~]" (spec-error-message c)))))

(define-condition spec-parse-error (spec-error)
  ((form :initarg :form :reader spec-parse-error-form :initform nil))
  (:report (lambda (c s)
             (format s "spec parse error~@[: ~a~]~@[ in ~s~]"
                     (spec-error-message c) (spec-parse-error-form c)))))

(define-condition spec-emit-error (spec-error) ())

(define-condition spec-tool-missing-error (spec-error)
  ((program :initarg :program :reader spec-tool-missing-error-program))
  (:report (lambda (c s)
             (format s "spec tool not found: ~a~@[ (~a)~]"
                     (spec-tool-missing-error-program c) (spec-error-message c)))))

(define-condition spec-tool-error (spec-error)
  ((exit-code :initarg :exit-code :reader spec-tool-error-exit-code :initform nil)
   (stdout :initarg :stdout :reader spec-tool-error-stdout :initform "")
   (stderr :initarg :stderr :reader spec-tool-error-stderr :initform ""))
  (:report (lambda (c s)
             (format s "spec tool failed~@[ (exit ~a)~]~@[: ~a~]~%~a"
                     (spec-tool-error-exit-code c) (spec-error-message c)
                     (spec-tool-error-stderr c)))))

(define-condition spec-typecheck-error (spec-error)
  ((errors :initarg :errors :reader spec-typecheck-error-errors :initform nil))
  (:report (lambda (c s)
             (format s "spec typecheck failed:~{~%  ~a~}"
                     (spec-typecheck-error-errors c)))))

(define-condition spec-invariant-violation (spec-error)
  ((invariant :initarg :invariant :reader spec-invariant-violation-invariant)
   (state :initarg :state :reader spec-invariant-violation-state :initform nil)
   (result :initarg :result :reader spec-invariant-violation-result :initform nil))
  (:report (lambda (c s)
             (format s "invariant ~a violated~@[ on ~a~]"
                     (spec-invariant-violation-invariant c)
                     (spec-invariant-violation-state c)))))

(define-condition spec-action-disabled (spec-error)
  ((action :initarg :action :reader spec-action-disabled-action)
   (guard :initarg :guard :reader spec-action-disabled-guard :initform nil)
   (state :initarg :state :reader spec-action-disabled-state :initform nil))
  (:report (lambda (c s)
             (format s "action ~a is disabled~@[: guard ~s is false~]~@[ on ~a~]"
                     (spec-action-disabled-action c)
                     (spec-action-disabled-guard c)
                     (spec-action-disabled-state c)))))

(define-condition spec-contract-error (spec-error)
  ((operator :initarg :operator :reader spec-contract-error-operator)
   (form :initarg :form :reader spec-contract-error-form :initform nil)
   (state :initarg :state :reader spec-contract-error-state :initform nil)))

(define-condition spec-precondition-error (spec-contract-error) ()
  (:report (lambda (c s)
             (format s "precondition of ~a violated: ~s~@[ on ~a~]"
                     (spec-contract-error-operator c) (spec-contract-error-form c)
                     (spec-contract-error-state c)))))

(define-condition spec-postcondition-error (spec-contract-error) ()
  (:report (lambda (c s)
             (format s "postcondition of ~a violated: ~s~@[ on ~a~]"
                     (spec-contract-error-operator c) (spec-contract-error-form c)
                     (spec-contract-error-state c)))))

(define-condition spec-conformance-divergence (spec-error)
  ((trace :initarg :trace :reader divergence-trace :initform nil)
   (step-index :initarg :step-index :reader divergence-step-index)
   (action :initarg :action :reader divergence-action :initform nil)
   (variable :initarg :variable :reader divergence-variable)
   (expected :initarg :expected :reader divergence-expected)
   (actual :initarg :actual :reader divergence-actual))
  (:report (lambda (c s)
             (format s "trace diverges at state ~a~@[ after ~a~]: ~a expected ~s, got ~s"
                     (divergence-step-index c) (divergence-action c)
                     (divergence-variable c) (divergence-expected c) (divergence-actual c)))))

(defun abort-trace (&optional condition)
  "Invoke the ABORT-TRACE restart established by REPLAY-TRACE."
  (let ((r (find-restart 'abort-trace condition)))
    (when r (invoke-restart r))))
