(in-package #:spec-protocol/tests)

(deftest bank-actions-and-vals
  (let ((b (fresh-bank)))
    (ok (eq :idle (bank-status b)))
    (ok (= 0 (total b)))
    (ok (deposit b "a" 3))
    (ok (equal '(:busy . 3) (bank-status b)))
    (ok (= 3 (map-get (bank-balances b) "a")))
    (ok (withdraw b "a" 1))
    (ok (equal '(:busy . 4) (bank-status b)))
    (ok (= 2 (total b)))
    (ok (equal '((spec-protocol.examples::balances . nil) (spec-protocol.examples::status . nil))
               (mapcar (lambda (c) (cons (car c) nil)) (spec-state-alist b))))
    (ok (typep b 'spec-state))
    (ok (eq (find-spec 'bank) (spec-of b)))))

(deftest guard-disables-action
  (let ((b (fresh-bank)))
    (ok (signals (withdraw b "a" 1) 'spec-action-disabled))
    (ng (handler-bind ((spec-action-disabled #'continue)) (withdraw b "a" 1)) "continue => NIL, no change")
    (ok (= 0 (map-get (bank-balances b) "a")))
    (ok (= 0 (handler-bind ((spec-action-disabled #'continue))
               (apply-action b 'withdraw "a" 1)
               (total b))))))

(deftest precondition-is-a-caller-error
  (let ((b (fresh-bank)))
    (ok (signals (deposit b "zz" 1) 'spec-precondition-error))
    (ok (signals (deposit b "a" 0) 'spec-precondition-error))
    (ok (handler-bind ((spec-precondition-error #'continue)) (deposit b "a" 0)) "continue proceeds")))

(deftest invariants
  (let ((b (fresh-bank)))
    (ok (check-invariants b))
    (setf (bank-balances b) (map-put (map-put (bank-balances b) "a" -1) "b" 5))
    (ok (signals (check-invariants b) 'spec-invariant-violation))
    (ok (check-invariants b :invariants '(total-non-negative)) "subset selection")
    (ok (handler-bind ((spec-invariant-violation #'continue)) (check-invariants b)))
    (let ((*check-invariants* nil))
      (ok (deposit b "b" 1) "actions skip the check when disabled"))
    (ok (signals (deposit b "b" 1) 'spec-invariant-violation) "post-state is checked after commit")))

(defspec two-vars (:module "TwoVars")
  (var x int)
  (var y int)
  (action init (setf x 1 y 2))
  (action swap (setf x y y x))
  (action bump (if (> x 0) (incf x)))
  (action bad-ensures (:ensures (= x (+ (old x) 2))) (incf x))
  (invariant positive (and (> x 0) (> y 0))))

(deftest simultaneous-assignment
  (let ((s (make-instance 'two-vars)))
    (init s)
    (swap s)
    (ok (= 2 (two-vars-x s)))
    (ok (= 1 (two-vars-y s)))))

(deftest missing-else-is-noop-and-incf-works
  (let ((s (make-instance 'two-vars)))
    (init s)
    (bump s)
    (ok (= 2 (two-vars-x s)))
    (setf (two-vars-x s) -1)
    (let ((*check-invariants* nil))
      (ok (bump s))
      (ok (= -1 (two-vars-x s))))))

(defspec lookup (:module "Lookup")
  (var m (map int int))
  (var k int)
  (action init (setf m (map-lit (1 10)) k 0))
  (invariant known-keys-positive (implies (in k (keys m)) (> (get m k) 0))))

(deftest implies-short-circuits
  (let ((s (make-instance 'lookup)))
    (ok (init s) "consequent's (get m 0) is not evaluated when 0 is not a key")
    (ok (check-invariants s))))

(deftest postcondition-with-old
  (let ((s (make-instance 'two-vars)))
    (init s)
    (ok (signals (bad-ensures s) 'spec-postcondition-error))
    (ok (= 2 (two-vars-x s)) "state committed before the postcondition ran")))

(deftest defaction-extends-registered-spec
  (defaction two-vars reset () (:guard (/= x 0)) (setf x 0 y 0))
  (ok (member "reset" (mapcar #'decl-name (module-actions (find-spec 'two-vars))) :test #'string=))
  (let ((s (make-instance 'two-vars)) (*check-invariants* nil))
    (init s)
    (ok (funcall 'reset s))
    (ok (= 0 (two-vars-x s)))
    (ok (signals (funcall 'reset s) 'spec-action-disabled))))

(deftest definvariant-extends-check
  (definvariant two-vars y-small (< y 100))
  (let ((s (make-instance 'two-vars)))
    (init s)
    (setf (two-vars-y s) 200)
    (ok (signals (check-invariants s) 'spec-invariant-violation))))

(deftest types-and-declarations
  (ok (subtypep 'spec-protocol.examples::status '(or (member :idle) (cons (member :busy) t))))
  (ok (signals (deposit (fresh-bank) 1 1) 'type-error) "declared param type"))
