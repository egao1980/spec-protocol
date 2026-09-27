(in-package #:spec-protocol.itf)

;;; Informal Trace Format (ADR-015) — emitted by Quint and Apalache.
;;; Values decode into the spec-protocol runtime model:
;;;   {"#bigint": "3"} -> 3          "s" -> "s"          true/false -> T/NIL
;;;   [a, b]  -> fset2:seq           {"#set": …} -> fset2:set     {"#map": [[k v] …]} -> fset2:map
;;;   {"#tup": …} -> list (tuples are lists on the CL side too)
;;;   {"tag": "L", "value": v} -> :L or (:L . v)      plain object -> canonical keyword plist (REC)
;;; Variable names are namespaced by Quint ("Main::Mod::x"); use the last segment.

(defstruct (itf-unserializable (:constructor make-itf-unserializable (text))) text)

(defclass itf-trace ()
  ((meta :initarg :meta :reader trace-meta :initform nil)
   (vars :initarg :vars :reader trace-vars :initform '())
   (params :initarg :params :reader trace-params :initform '())
   (states :initarg :states :reader trace-states :initform '())
   (loop :initarg :loop :reader trace-loop :initform nil)))

(defclass itf-state ()
  ((index :initarg :index :reader state-index)
   (meta :initarg :meta :reader state-meta :initform nil)
   (values :initarg :values :reader state-values)))   ; hash-table: short var name -> lisp value

(defmethod print-object ((s itf-state) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~a~@[ ~a~]" (state-index s) (state-action-taken s))))

(defmethod print-object ((tr itf-trace) stream)
  (print-unreadable-object (tr stream :type t)
    (format stream "~d state~:p" (length (trace-states tr)))))

(defun %key (obj key) (gethash key obj))
(defun %json-object-p (x) (hash-table-p x))

(defun itf-value->lisp (v)
  (cond
    ((eq v :null) nil)
    ((stringp v) v)
    ((integerp v) v)
    ((eq v t) t)
    ((null v) nil)
    ((vectorp v) (fset2:convert 'fset2:seq (map 'list #'itf-value->lisp v)))
    ((%json-object-p v)
     (multiple-value-bind (big found) (gethash "#bigint" v)
       (when found (return-from itf-value->lisp (parse-integer big))))
     (multiple-value-bind (set found) (gethash "#set" v)
       (when found
         (return-from itf-value->lisp (fset2:convert 'fset2:set (map 'list #'itf-value->lisp set)))))
     (multiple-value-bind (pairs found) (gethash "#map" v)
       (when found
         (return-from itf-value->lisp
           (let ((m (fset2:empty-map)))
             (loop for pair across pairs
                   do (setf m (fset2:with m (itf-value->lisp (aref pair 0)) (itf-value->lisp (aref pair 1)))))
             m))))
     (multiple-value-bind (tup found) (gethash "#tup" v)
       (when found (return-from itf-value->lisp (map 'list #'itf-value->lisp tup))))
     (multiple-value-bind (u found) (gethash "#unserializable" v)
       (when found (return-from itf-value->lisp (make-itf-unserializable u))))
     (multiple-value-bind (tag found) (gethash "tag" v)
       (when (and found (= (hash-table-count v) 2) (nth-value 1 (gethash "value" v)))
         (let ((label (spec-protocol:demangle-name tag))
               (payload (itf-value->lisp (gethash "value" v))))
           (return-from itf-value->lisp
             (if (null payload) label (cons label payload))))))
     (let ((plist '()))
       (maphash (lambda (k val)
                  (unless (and (plusp (length k)) (char= (char k 0) #\#))
                    (push (itf-value->lisp val) plist)
                    (push (spec-protocol:demangle-name k) plist)))
                v)
       (apply #'spec-protocol:rec plist)))
    (t (error 'spec-protocol:spec-error :message (format nil "unsupported ITF value ~s" v)))))

(defun %plist-p (v)
  "Even-length list whose even positions are keywords: a record."
  (and (consp v) (evenp (length v))
       (loop for (k nil) on v by #'cddr always (keywordp k))))

(defun lisp->itf-value (v)
  "Inverse of ITF-VALUE->LISP, producing json-protocol data (hash tables / vectors)."
  (flet ((obj (&rest kv) (let ((h (make-hash-table :test #'equal)))
                           (loop for (k val) on kv by #'cddr do (setf (gethash k h) val))
                           h)))
    (cond
      ((eq v t) t)
      ((null v) nil)
      ((integerp v) (obj "#bigint" (princ-to-string v)))
      ((stringp v) v)
      ((keywordp v) (obj "tag" (spec-protocol:mangle-type-name v) "value" (obj "#tup" (vector))))
      ((%plist-p v)
       (let ((h (make-hash-table :test #'equal)))
         (loop for (k val) on v by #'cddr do (setf (gethash (spec-protocol:mangle-name k) h) (lisp->itf-value val)))
         h))
      ((and (consp v) (keywordp (car v)))
       (obj "tag" (spec-protocol:mangle-type-name (car v))
            "value" (if (listp (cdr v))
                        (obj "#tup" (map 'vector #'lisp->itf-value (cdr v)))
                        (lisp->itf-value (cdr v)))))
      ((listp v) (obj "#tup" (map 'vector #'lisp->itf-value v)))
      ((typep v 'fset2:seq) (map 'vector #'lisp->itf-value (fset2:convert 'list v)))
      ((typep v 'fset2:set) (obj "#set" (map 'vector #'lisp->itf-value (fset2:convert 'list v))))
      ((typep v 'fset2:map)
       (obj "#map" (map 'vector (lambda (p) (vector (lisp->itf-value (car p)) (lisp->itf-value (cdr p))))
                        (spec-protocol:map-pairs v))))
      (t (error 'spec-protocol:spec-error :message (format nil "cannot encode ~s as ITF" v))))))

(defun itf-equal (a b) (spec-protocol:spec-equal a b))

(defun %parse-state (obj index)
  (let ((values (make-hash-table :test #'equal)) (meta nil))
    (maphash (lambda (k v)
               (if (string= k "#meta")
                   (setf meta v)
                   (setf (gethash (spec-protocol:name-last-segment k) values) (itf-value->lisp v))))
             obj)
    (make-instance 'itf-state :index index :meta meta :values values)))

(defun parse-itf (source)
  "SOURCE is a JSON string or an already-decoded json-protocol object."
  (let ((obj (if (stringp source) (json-protocol:decode source) source)))
    (make-instance 'itf-trace
                   :meta (%key obj "#meta")
                   :vars (map 'list #'identity (or (%key obj "vars") #()))
                   :params (map 'list #'identity (or (%key obj "params") #()))
                   :loop (let ((l (%key obj "loop"))) (and (integerp l) l))
                   :states (loop for s across (or (%key obj "states") #())
                                 for i from 0
                                 collect (%parse-state s i)))))

(defun read-itf (pathname)
  (with-open-file (in pathname :external-format :utf-8)
    (let ((text (make-string (file-length in))))
      (let ((n (read-sequence text in)))
        (parse-itf (subseq text 0 n))))))

(defun write-itf (trace pathname)
  "Freeze TRACE as an ITF JSON fixture."
  (let ((root (make-hash-table :test #'equal)))
    (setf (gethash "#meta" root) (or (trace-meta trace) (make-hash-table :test #'equal))
          (gethash "vars" root) (coerce (trace-vars trace) 'vector)
          (gethash "states" root)
          (coerce (loop for s in (trace-states trace)
                        collect (let ((h (make-hash-table :test #'equal)))
                                  (setf (gethash "#meta" h)
                                        (or (state-meta s)
                                            (let ((m (make-hash-table :test #'equal)))
                                              (setf (gethash "index" m) (state-index s)) m)))
                                  (maphash (lambda (k v) (setf (gethash k h) (lisp->itf-value v))) (state-values s))
                                  h))
                  'vector))
    (with-open-file (out pathname :direction :output :if-exists :supersede :external-format :utf-8)
      (json-protocol:encode root :stream out))
    pathname))

(defun state-value (state name)
  "Value of variable NAME (string, symbol or keyword; namespaces ignored)."
  (let ((key (if (stringp name) (spec-protocol:name-last-segment name) (spec-protocol:mangle-name name))))
    (multiple-value-bind (v found) (gethash key (state-values state))
      (unless found
        (error 'spec-protocol:spec-error :message (format nil "state ~a has no variable ~a" (state-index state) key)))
      v)))

(defun state-variables (state)
  "Spec variables in STATE (mbt:: bookkeeping excluded)."
  (loop for k being the hash-keys of (state-values state)
        unless (member k '("actionTaken" "nondetPicks") :test #'string=)
          collect k))

(defun state-action-taken (state)
  (values (gethash "actionTaken" (state-values state))))

(defun state-nondet-picks (state)
  "Alist (pick-name-keyword . value); None picks are dropped."
  (let ((picks (gethash "nondetPicks" (state-values state))))
    (loop for (k v) on picks by #'cddr
          unless (eq v :none)
            collect (cons k (if (and (consp v) (eq (car v) :some)) (cdr v) v)))))
