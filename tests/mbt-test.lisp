(in-package #:spec-protocol/tests)

(setup (json-backend-jzon:use-jzon-backend))

(deftest clos-driver-replays-frozen-traces
  (dolist (name '("bank-ok-0.itf.json" "bank-ok-1.itf.json"))
    (let ((sut (spec-protocol.mbt:replay-trace (spec-protocol.mbt:make-clos-driver 'bank)
                                               (spec-protocol.itf:read-itf (fixture name)))))
      (ok (typep sut 'bank) name)
      (ok (check-invariants sut)))))

(deftest clos-driver-detects-divergence
  ;; Trace from a Quint bank whose withdraw has no guard: the model goes negative, the CLOS
  ;; protocol refuses (guard => disabled), so balances diverge at the first bad withdraw.
  (let ((tr (spec-protocol.itf:read-itf (fixture "bank-buggy-withdraw.itf.json"))))
    (ok (signals (spec-protocol.mbt:replay-trace (spec-protocol.mbt:make-clos-driver 'bank) tr)
                 'spec-conformance-divergence))
    (handler-case (spec-protocol.mbt:replay-trace (spec-protocol.mbt:make-clos-driver 'bank) tr)
      (spec-conformance-divergence (c)
        (ok (= 2 (divergence-step-index c)))
        (ok (string= "withdraw" (divergence-action c)))
        (ok (string= "balances" (divergence-variable c)))))
    (ok (typep (handler-bind ((spec-conformance-divergence #'continue))
                 (spec-protocol.mbt:replay-trace (spec-protocol.mbt:make-clos-driver 'bank) tr))
               'bank)
        "continue skips the variable")
    (ok (eq :aborted (nth-value 1 (handler-bind ((spec-conformance-divergence (lambda (c) (abort-trace c))))
                                    (spec-protocol.mbt:replay-trace (spec-protocol.mbt:make-clos-driver 'bank) tr))))
        "abort-trace stops")))

;;; A hand-written driver over a plain hash table, the shape used for SUTs not built on defspec.

(defclass table-bank-driver (spec-protocol.mbt:mbt-driver) ())

(defmethod spec-protocol.mbt:mbt-init ((d table-bank-driver) state)
  (let ((table (make-hash-table :test #'equal)))
    (fset2:do-map (k v (spec-protocol.itf:state-value state "balances"))
      (setf (gethash k table) v))
    table))

(defmethod spec-protocol.mbt:mbt-apply ((d table-bank-driver) table action picks state)
  (declare (ignore state))
  (let ((who (cdr (assoc :who picks))) (n (cdr (assoc :n picks))))
    (cond ((string= action "deposit") (incf (gethash who table) n))
          ((string= action "withdraw") (when (>= (gethash who table) n) (decf (gethash who table) n)))
          (t (error "unmapped action ~a" action)))))

(defmethod spec-protocol.mbt:mbt-observe ((d table-bank-driver) table)
  (list (cons "balances" (apply #'make-map (loop for k being the hash-keys of table using (hash-value v)
                                                collect (cons k v))))))

(deftest custom-driver-with-variable-subset
  (let ((tr (spec-protocol.itf:read-itf (fixture "bank-ok-0.itf.json"))))
    (ok (hash-table-p (spec-protocol.mbt:replay-trace (make-instance 'table-bank-driver) tr
                                                      :variables '("balances"))))
    ;; status is not observed by this driver => ignored even with :all
    (ok (hash-table-p (spec-protocol.mbt:replay-trace (make-instance 'table-bank-driver) tr)))))

(deftest replay-requires-mbt-metadata
  (let ((tr (spec-protocol.itf:parse-itf
             "{\"vars\": [\"x\"], \"states\": [{\"#meta\": {\"index\": 0}, \"x\": {\"#bigint\": \"1\"}}]}")))
    (ok (signals (spec-protocol.mbt:replay-trace (spec-protocol.mbt:make-clos-driver 'bank) tr) 'spec-error))))
