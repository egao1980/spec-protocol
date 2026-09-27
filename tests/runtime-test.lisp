(in-package #:spec-protocol/tests)

(deftest sets-are-unordered-and-deduplicated
  (ok (spec-equal (make-set 1 2 2 3) (make-set 3 2 1)))
  (ok (= 3 (set-size (make-set 1 2 2 3))))
  (ok (set-in-p 2 (make-set 1 2)))
  (ng (set-in-p 9 (make-set 1 2)))
  (ok (spec-equal (set-union (make-set 1) (make-set 2)) (make-set 2 1)))
  (ok (spec-equal (set-exclude (make-set 1 2 3) (make-set 2)) (make-set 1 3)))
  (ok (spec-equal (int-range 1 3) (make-set 1 2 3)))
  (ok (= 6 (set-fold (make-set 1 2 3) 0 #'+)))
  (ok (set-forall (make-set 2 4) #'evenp))
  (ok (set-exists (make-set 1 2) #'evenp)))

(deftest maps-are-persistent
  (let* ((m (make-map (cons "a" 1)))
         (m2 (map-put m "b" 2)))
    (ok (= 1 (map-get m "a")))
    (ok (= 2 (map-get m2 "b")))
    (ng (set-in-p "b" (map-keys m)))
    (ok (spec-equal (map-keys m2) (make-set "a" "b")))
    (ok (= 5 (map-get (map-set-by m "a" (lambda (v) (+ v 4))) "a")))
    (ok (spec-equal (map-by (make-set "x" "y") (constantly 0)) (make-map (cons "x" 0) (cons "y" 0))))
    (ok (signals (map-get m "zz") 'spec-error))))

(deftest seqs
  (let ((s (make-seq 1 2)))
    (ok (= 2 (seq-length s)))
    (ok (= 1 (seq-head s)))
    (ok (spec-equal (seq-append s 3) (make-seq 1 2 3)))
    (ok (spec-equal (seq-tail s) (make-seq 2)))
    (ok (= 3 (seq-foldl s 0 #'+)))))

(deftest variants-records-tuples
  (ok (eq :idle (variant :idle)))
  (ok (equal '(:busy . 3) (variant :busy 3)))
  (ok (eq :busy (variant-label '(:busy . 3))))
  (ok (= 3 (variant-value '(:busy . 3))))
  (ok (equal (rec :b 2 :a 1) '(:a 1 :b 2)) "records are canonical plists")
  (ok (equal (rec-with (rec :a 1) :b 2) '(:a 1 :b 2)))
  (ok (= 2 (tuple-item '(1 2 3) 2)))
  (ok (spec-equal (make-set '(:busy . 1) :idle) (make-set :idle '(:busy . 1)))))
