(in-package #:spec-protocol/tests)

(deftest mangle
  (loop for (input style expected) in '((step-completed :camel "stepCompleted")
                                        (step-completed :pascal "StepCompleted")
                                        (x :camel "x")
                                        (half-closed-local :pascal "HalfClosedLocal")
                                        (windows-bounded-6-9-1 :camel "windowsBounded_6_9_1")
                                        (end-stream-p :camel "endStreamP"))
        do (ok (string= (mangle-name input :style style) expected) expected))
  (ok (string= (mangle-name "AsIs") "AsIs") "strings pass through"))

(deftest demangle
  (loop for (input expected) in '(("stepCompleted" :step-completed)
                                  ("HalfClosedLocal" :half-closed-local)
                                  ("windowsBounded_6_9_1" :windows-bounded-6-9-1)
                                  ("BankTest::Bank::balances" :balances)
                                  ("mbt::actionTaken" :action-taken))
        do (ok (eq (demangle-name input) expected) input)))

(deftest name-round-trip
  (dolist (sym '(balances pending-timers half-closed-remote))
    (ok (eq (demangle-name (mangle-name sym)) (intern (symbol-name sym) :keyword)))))
