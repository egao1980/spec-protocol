(in-package #:spec-protocol.examples)

;;; The canonical example: one form, a CLOS protocol and a Quint module.

(defspec bank (:module "Bank" :instance ("BankTest" (accounts (set "a" "b"))))
  (const accounts (set str))
  (type status (or :idle (busy int)))
  (var balances (map str int))
  (var status status)

  (pure-def can-withdraw ((b int) (n int))
    (:returns bool)
    (>= (- b n) 0))

  (val total
    (fold (keys balances) 0 (lambda (acc k) (+ acc (get balances k)))))

  (action init
    (setf balances (map-of ((k accounts)) 0)
          status :idle))

  (action deposit ((who str) (n int))
    (:requires (in who accounts) (> n 0))
    (:ensures (= total (+ (old total) n)))
    (setf balances (put balances who (+ (get balances who) n))
          status (busy n)))

  (action withdraw ((who str) (n int))
    (:requires (in who accounts))
    (:guard (can-withdraw (get balances who) n))
    (:ensures (= total (- (old total) n)))
    (setf balances (put balances who (- (get balances who) n)))
    (setf status (match status ((busy k) (busy (+ k n))) (_ :idle))))

  (invariant no-negatives (forall accounts (lambda (a) (>= (get balances a) 0))))
  (invariant total-non-negative (>= total 0))

  (:spec-only
   (action step
     (nondet (who (one-of accounts))
       (nondet (n (one-of (to 1 5)))
         (any (deposit who n) (withdraw who n)))))
   (temporal eventually-busy (eventually (/= status :idle)))
   (run happy-test
     (expect (then (then init (deposit "a" 3)) (withdraw "a" 1))
             (= (get balances "a") 2)))))
