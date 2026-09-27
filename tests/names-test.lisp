(in-package #:spec-protocol/tests)

(deftest-parametrize mangle
    ((input style expected)
     ('step-completed :camel "stepCompleted")
     ('step-completed :pascal "StepCompleted")
     ('x :camel "x")
     ('half-closed-local :pascal "HalfClosedLocal")
     ('windows-bounded-6-9-1 :camel "windowsBounded_6_9_1")
     ('end-stream-p :camel "endStreamP"))
  (ok (string= (mangle-name input :style style) expected)))

(deftest mangle-string-passthrough
  (ok (string= (mangle-name "AsIs") "AsIs")))

(deftest-parametrize demangle
    ((input expected)
     ("stepCompleted" :step-completed)
     ("HalfClosedLocal" :half-closed-local)
     ("windowsBounded_6_9_1" :windows-bounded-6-9-1)
     ("BankTest::Bank::balances" :balances)
     ("mbt::actionTaken" :action-taken))
  (ok (eq (demangle-name input) expected)))

(deftest name-round-trip
  (dolist (sym '(balances pending-timers half-closed-remote))
    (ok (eq (demangle-name (mangle-name sym)) (intern (symbol-name sym) :keyword)))))
