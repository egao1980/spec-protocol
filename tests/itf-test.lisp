(in-package #:spec-protocol/tests)

(setup (json-backend-jzon:use-jzon-backend))

(deftest itf-value-decoding
  (flet ((decode (json) (spec-protocol.itf:itf-value->lisp (json-protocol:decode json))))
    (ok (= 42 (decode "{\"#bigint\": \"42\"}")))
    (ok (string= "s" (decode "\"s\"")))
    (ok (eq t (decode "true")))
    (ok (null (decode "false")))
    (ok (spec-equal (make-set 1 2) (decode "{\"#set\": [{\"#bigint\": \"2\"}, {\"#bigint\": \"1\"}]}")))
    (ok (spec-equal (make-map (cons "a" 1)) (decode "{\"#map\": [[\"a\", {\"#bigint\": \"1\"}]]}")))
    (ok (equal '(1 "x") (decode "{\"#tup\": [{\"#bigint\": \"1\"}, \"x\"]}")))
    (ok (spec-equal (make-seq 1 2) (decode "[{\"#bigint\": \"1\"}, {\"#bigint\": \"2\"}]")))
    (ok (eq :idle (decode "{\"tag\": \"Idle\", \"value\": {\"#tup\": []}}")))
    (ok (equal '(:busy . 3) (decode "{\"tag\": \"Busy\", \"value\": {\"#bigint\": \"3\"}}")))
    (ok (equal '(:half-closed-local . 1) (decode "{\"tag\": \"HalfClosedLocal\", \"value\": {\"#bigint\": \"1\"}}")))
    (ok (equal '(:some . nil) (decode "{\"tag\": \"Some\", \"value\": false}")) "a false payload is not a bare label")
    (ok (equal (rec :a 1 :b-c "x") (decode "{\"bC\": \"x\", \"a\": {\"#bigint\": \"1\"}}")))))

(deftest itf-value-round-trip
  (dolist (v (list 7 "s" t nil :idle '(:busy . 3) '(:some . nil) (rec :a 1 :b "x") (make-set 1 2) (make-map (cons "k" (make-seq 1)))))
    (ok (spec-equal v (spec-protocol.itf:itf-value->lisp (spec-protocol.itf:lisp->itf-value v))) (format nil "~s" v))))

(deftest read-fixture-trace
  (let* ((tr (spec-protocol.itf:read-itf (fixture "bank-ok-0.itf.json")))
         (states (spec-protocol.itf:trace-states tr))
         (s1 (second states)))
    (ok (= 9 (length states)))
    (ok (= 0 (spec-protocol.itf:state-index (first states))))
    (ok (string= "init" (spec-protocol.itf:state-action-taken (first states))))
    (ok (string= "deposit" (spec-protocol.itf:state-action-taken s1)))
    (ok (equal '("balances" "status") (sort (spec-protocol.itf:state-variables s1) #'string<)))
    (ok (assoc :who (spec-protocol.itf:state-nondet-picks s1)))
    (ok (assoc :n (spec-protocol.itf:state-nondet-picks s1)))
    (ok (typep (spec-protocol.itf:state-value s1 "balances") 'spec-map))
    (ok (typep (spec-protocol.itf:state-value s1 'balances) 'spec-map))
    (ok (signals (spec-protocol.itf:state-value s1 "nope") 'spec-error))))

(deftest namespaced-variables-are-stripped
  (let ((tr (spec-protocol.itf:parse-itf
             "{\"vars\": [\"BankTest::Bank::x\", \"mbt::actionTaken\"],
               \"states\": [{\"#meta\": {\"index\": 0}, \"BankTest::Bank::x\": {\"#bigint\": \"1\"}, \"mbt::actionTaken\": \"init\"}]}")))
    (ok (= 1 (spec-protocol.itf:state-value (first (spec-protocol.itf:trace-states tr)) "x")))
    (ok (equal '("x") (spec-protocol.itf:state-variables (first (spec-protocol.itf:trace-states tr)))))))

(deftest false-nondet-picks-are-kept
  (let* ((tr (spec-protocol.itf:parse-itf
              "{\"vars\": [\"x\"], \"states\": [{\"#meta\": {\"index\": 0}, \"x\": true,
                \"mbt::actionTaken\": \"flip\",
                \"mbt::nondetPicks\": {\"b\": {\"tag\": \"Some\", \"value\": false},
                                       \"c\": {\"tag\": \"None\", \"value\": {\"#tup\": []}}}}]}"))
         (picks (spec-protocol.itf:state-nondet-picks (first (spec-protocol.itf:trace-states tr)))))
    (ok (equal '((:b . nil)) picks))))

(deftest write-itf-round-trip
  (let* ((tr (spec-protocol.itf:read-itf (fixture "bank-ok-1.itf.json")))
         (path (merge-pathnames "spec-protocol-itf-test.json" (uiop:temporary-directory))))
    (spec-protocol.itf:write-itf tr path)
    (let ((tr2 (spec-protocol.itf:read-itf path)))
      (ok (= (length (spec-protocol.itf:trace-states tr)) (length (spec-protocol.itf:trace-states tr2))))
      (loop for a in (spec-protocol.itf:trace-states tr)
            for b in (spec-protocol.itf:trace-states tr2)
            do (ok (spec-equal (spec-protocol.itf:state-value a "balances") (spec-protocol.itf:state-value b "balances")))))
    (ignore-errors (delete-file path))))
