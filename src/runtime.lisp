(in-package #:spec-protocol)

;;; Runtime value model shared by the CLOS projection and ITF decoding.
;;;
;;;   int / str / bool      integer / string / T,NIL
;;;   Set[T]                fset2:set   (CHAMP, unordered, structural equality)
;;;   K -> V                fset2:map   (CHAMP)
;;;   List[T]               fset2:seq
;;;   (a, b)                list
;;;   { f: v }              keyword plist
;;;   Label / Label(v)      keyword / (keyword . v)
;;;
;;; FSet collections are persistent: every operation returns a fresh value, which is what
;;; simultaneous action updates need (all right-hand sides see the pre-state).

(deftype spec-set () 'fset2:set)
(deftype spec-map () 'fset2:map)
(deftype spec-seq () 'fset2:seq)

(defun spec-equal (a b)
  "Structural equality over the runtime value model."
  (cond ((and (consp a) (consp b))
         (and (spec-equal (car a) (car b)) (spec-equal (cdr a) (cdr b))))
        ((or (typep a 'fset2:collection) (typep b 'fset2:collection))
         (fset2:equal? a b))
        (t (equal a b))))

;;; Sets

(defun make-set (&rest elements) (fset2:convert 'fset2:set elements))
(defun set-elements (set) (fset2:convert 'list set))
(defun set-in-p (element set) (fset2:contains? set element))
(defun set-union (a b) (fset2:union a b))
(defun set-intersect (a b) (fset2:intersection a b))
(defun set-exclude (a b) (fset2:set-difference a b))
(defun set-subset-p (a b) (fset2:subset? a b))
(defun set-size (set) (fset2:size set))
(defun set-fold (set init fn)
  (let ((acc init))
    (fset2:do-set (x set acc) (setf acc (funcall fn acc x)))))
(defun set-map (set fn) (fset2:image fn set))
(defun set-filter (set fn) (fset2:filter fn set))
(defun set-exists (set fn) (and (fset2:find-if fn set) t))
(defun set-forall (set fn) (not (fset2:find-if-not fn set)))
(defun int-range (from to) (fset2:convert 'fset2:set (loop for i from from to to collect i)))

;;; Maps

(defun make-map (&rest pairs)
  "PAIRS are (key . value) conses."
  (let ((m (fset2:empty-map)))
    (dolist (p pairs m) (setf m (fset2:with m (car p) (cdr p))))))
(defun map-pairs (map)
  (let ((acc '()))
    (fset2:do-map (k v map) (push (cons k v) acc))
    (nreverse acc)))
(defun map-get (map key)
  (multiple-value-bind (v found) (fset2:lookup map key)
    (unless found (error 'spec-error :message (format nil "key ~s not in map" key)))
    v))
(defun map-put (map key value) (fset2:with map key value))
(defun map-set-by (map key fn) (fset2:with map key (funcall fn (map-get map key))))
(defun map-keys (map) (fset2:domain map))
(defun map-by (set fn)
  (let ((m (fset2:empty-map)))
    (fset2:do-set (k set m) (setf m (fset2:with m k (funcall fn k))))))

;;; Lists

(defun make-seq (&rest elements) (fset2:convert 'fset2:seq elements))
(defun seq-append (seq x) (fset2:with-last seq x))
(defun seq-concat (a b) (fset2:concat a b))
(defun seq-head (seq) (fset2:first seq))
(defun seq-tail (seq) (fset2:less-first seq))
(defun seq-length (seq) (fset2:size seq))
(defun seq-nth (seq i) (fset2:lookup seq i))
(defun seq-foldl (seq init fn)
  (let ((acc init))
    (fset2:do-seq (x seq) (setf acc (funcall fn acc x)))
    acc))
(defun seq-select (seq fn) (fset2:filter fn seq))
(defun seq-map (seq fn) (fset2:image fn seq))
(defun seq-elements (seq) (fset2:convert 'list seq))

;;; Variants, records, tuples

(defun variant (label &optional (value nil value-p))
  (if value-p (cons label value) label))
(defun variant-p (x) (or (keywordp x) (and (consp x) (keywordp (car x)))))
(defun variant-label (x) (if (consp x) (car x) x))
(defun variant-value (x) (if (consp x) (cdr x) nil))

(defun rec (&rest plist)
  "Record = keyword plist in canonical (sorted) key order, so EQUAL works across origins."
  (let ((pairs (loop for (k v) on plist by #'cddr collect (cons k v))))
    (loop for (k . v) in (sort pairs #'string< :key (lambda (p) (symbol-name (car p))))
          collect k collect v)))
(defun rec-field (rec field) (getf rec field))
(defun rec-with (rec field value)
  (let ((copy (copy-list rec)))
    (setf (getf copy field) value)
    (apply #'rec copy)))

(defun tuple-item (tuple index) (nth (1- index) tuple))

(defun int-div (a b) (values (truncate a b)))
