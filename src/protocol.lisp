(in-package #:spec-protocol)

;;; Backend protocol: emitters / checkers (spec-backend-quint is the hero backend).

(defclass spec-backend () ())

(defvar *spec-backend* nil)

(defun %ensure-backend (&optional (backend *spec-backend*))
  (or backend
      (error 'spec-error :message "*spec-backend* is nil — load spec-backend-quint")))

(defun %module (designator)
  (if (typep designator 'spec-module) designator (find-spec designator)))

(defclass spec-result ()
  ((status :initarg :status :reader result-status)        ; :ok :violation :error :skipped
   (tool :initarg :tool :reader result-tool :initform nil)
   (seed :initarg :seed :reader result-seed :initform nil)
   (stdout :initarg :stdout :reader result-stdout :initform "")
   (stderr :initarg :stderr :reader result-stderr :initform "")
   (errors :initarg :errors :reader result-errors :initform '())     ; list of strings (ADR-002 explanations)
   (traces :initarg :traces :reader result-traces :initform '())     ; list of itf-trace
   (passed :initarg :passed :reader result-passed :initform '())     ; run-tests: names
   (failed :initarg :failed :reader result-failed :initform '())
   (output :initarg :output :reader result-output :initform nil)     ; emit/compile text
   (duration :initarg :duration :reader result-duration :initform nil)))

(defmethod print-object ((r spec-result) stream)
  (print-unreadable-object (r stream :type t)
    (format stream "~a~@[ ~a~]~@[ seed=~a~]~@[ ~d trace~:p~]"
            (result-status r) (result-tool r) (result-seed r)
            (and (result-traces r) (length (result-traces r))))))

(defgeneric backend-emit (backend module &key stream)
  (:documentation "Write MODULE as spec text. Returns the text when STREAM is nil."))
(defgeneric backend-typecheck (backend module &key directory)
  (:documentation "-> SPEC-RESULT (:ok or :error with ERRORS)."))
(defgeneric backend-simulate (backend module &key init step invariants witnesses max-steps max-samples
                                                  seed n-traces mbt directory)
  (:documentation "Random simulation -> SPEC-RESULT with ITF TRACES (MBT metadata when MBT)."))
(defgeneric backend-verify (backend module &key init step invariants temporal max-steps checker directory)
  (:documentation "Model checking. CHECKER is :apalache (bounded, symbolic) or :tlc (explicit, finite)."))
(defgeneric backend-run-tests (backend module &key match seed directory)
  (:documentation "Execute the module's RUN definitions -> SPEC-RESULT with PASSED / FAILED."))
(defgeneric backend-compile (backend module &key target directory)
  (:documentation "Translate to TARGET (:tlaplus | :json) -> SPEC-RESULT with OUTPUT text."))

(defun emit (module &key stream (backend *spec-backend*))
  (backend-emit (%ensure-backend backend) (%module module) :stream stream))

(defun typecheck (module &rest keys &key (backend *spec-backend*) &allow-other-keys)
  (let ((keys (copy-list keys))) (remf keys :backend)
    (apply #'backend-typecheck (%ensure-backend backend) (%module module) keys)))

(defun simulate (module &rest keys &key (backend *spec-backend*) &allow-other-keys)
  (let ((keys (copy-list keys))) (remf keys :backend)
    (apply #'backend-simulate (%ensure-backend backend) (%module module) keys)))

(defun verify (module &rest keys &key (backend *spec-backend*) &allow-other-keys)
  (let ((keys (copy-list keys))) (remf keys :backend)
    (apply #'backend-verify (%ensure-backend backend) (%module module) keys)))

(defun run-tests (module &rest keys &key (backend *spec-backend*) &allow-other-keys)
  (let ((keys (copy-list keys))) (remf keys :backend)
    (apply #'backend-run-tests (%ensure-backend backend) (%module module) keys)))

(defun compile-spec (module &rest keys &key (backend *spec-backend*) &allow-other-keys)
  (let ((keys (copy-list keys))) (remf keys :backend)
    (apply #'backend-compile (%ensure-backend backend) (%module module) keys)))

(defun check (module &rest keys &key (backend *spec-backend*) invariants &allow-other-keys)
  "SIMULATE and signal SPEC-INVARIANT-VIOLATION (restart CONTINUE) on a violation. Returns the result."
  (let* ((keys (copy-list keys))
         (m (%module module)))
    (remf keys :backend)
    (let ((result (apply #'backend-simulate (%ensure-backend backend) m
                         :invariants (or invariants (mapcar #'decl-name (module-invariants m)))
                         keys)))
      (when (eq (result-status result) :violation)
        (restart-case (error 'spec-invariant-violation
                             :invariant (or invariants :all) :result result)
          (continue () :report "Return the violating result." nil)))
      result)))
