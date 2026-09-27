(in-package #:spec-protocol/tests)

(deftest bank-module-shape
  (let ((m (find-spec 'bank)))
    (ok (string= "Bank" (module-name m)))
    (ok (string= "BankTest" (module-main-name m)))
    (ok (equal '("init" "deposit" "withdraw" "step") (mapcar #'decl-name (module-actions m))))
    (ok (equal '("noNegatives" "totalNonNegative") (mapcar #'decl-name (module-invariants m))))
    (ok (equal '("happyTest") (mapcar #'decl-name (module-runs m))))
    (ok (equal '(:idle :busy) (module-labels m)))
    (ok (decl-spec-only-p (module-find-decl m 'step)))
    (ng (decl-spec-only-p (module-find-decl m 'deposit)))
    (let ((deposit (module-find-decl m 'deposit)))
      (ok (= 2 (length (op-requires deposit))))
      (ok (= 1 (length (op-ensures deposit))))
      (ok (typep (op-body deposit) 'all-expr)))
    (ok (op-guards (module-find-decl m 'withdraw)))
    (ok (typep (type-decl-type (module-find-decl m 'status 'type-decl)) 'sum-type))))

(defun parse1 (&rest decls)
  (parse-spec-form 'p '(:module "P") decls))

(deftest parse-errors
  (ok (signals (parse1 '(var x int) '(val v (setf x 1))) 'spec-parse-error) "setf outside action")
  (ok (signals (parse1 '(var x int) '(action a (setf y 1))) 'spec-parse-error) "setf to unknown var")
  (ok (signals (parse1 '(var x int) '(action a (nondet (n (one-of (set 1))) (setf x n)))) 'spec-parse-error)
      "nondet outside :spec-only")
  (ok (signals (parse1 '(var x int) '(action a (any (setf x 1) (setf x 2)))) 'spec-parse-error)
      "any outside :spec-only")
  (ok (signals (parse1 '(var x int) '(action a (setf x 1)) '(action b (a))) 'spec-parse-error)
      "action call from action")
  (ok (signals (parse1 '(var x int) '(val v nonsense)) 'spec-parse-error) "unknown name")
  (ok (signals (parse1 '(var x int) '(val v (:raw "x"))) 'spec-parse-error) ":raw outside :spec-only")
  (ok (signals (parse1 '(var x int) '(val v (old x))) 'spec-parse-error) "old outside :ensures"))

(deftest parse-error-use-value-restart
  (let ((m (handler-bind ((spec-parse-error
                            (lambda (c) (declare (ignore c))
                              (use-value (make-instance 'literal :value 0)))))
             (parse1 '(var x int) '(val v nonsense)))))
    (ok (typep (op-body (module-find-decl m 'v)) 'literal))))

(deftest spec-only-allows-nondeterminism
  (let ((m (parse1 '(var x int)
                   '(action a (setf x 1))
                   '(:spec-only (action step (nondet (n (one-of (set 1 2))) (any (a) (setf x n))))))))
    (ok (decl-spec-only-p (module-find-decl m 'step)))
    (ok (typep (op-body (module-find-decl m 'step)) 'nondet-expr))))

(deftest missing-else-in-action-is-noop
  (let* ((m (parse1 '(var x int) '(action a (if (> x 0) (setf x 0)))))
         (body (op-body (module-find-decl m 'a))))
    (ok (typep body 'if-expr))
    (ok (and (typep (if-else body) 'all-expr) (null (block-forms (if-else body)))))))

(deftest incf-decf-and-multi-setf
  (let* ((m (parse1 '(var x int) '(var y int) '(action a (incf x) (decf y 2) (setf x 1 y 2))))
         (body (op-body (module-find-decl m 'a))))
    (ok (typep body 'all-expr))
    (ok (= 3 (length (block-forms body))))
    (ok (typep (first (block-forms body)) 'assign-expr))
    (ok (typep (assign-value (first (block-forms body))) 'app))))
