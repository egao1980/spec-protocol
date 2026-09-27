(in-package #:spec-protocol)

;;; Deterministic Lisp <-> spec identifier mapping.
;;; kebab-case symbols become camelCase (vars, consts, ops, params, fields) or PascalCase
;;; (types, variant labels). A segment starting with a digit is joined with "_" so
;;; windows-bounded-6-9-1 -> windowsBounded_6_9_1 round-trips.

(defun %segments (designator)
  (let ((name (string-downcase (string designator))))
    (loop with start = 0
          for pos = (position #\- name :start start)
          collect (subseq name start pos)
          while pos
          do (setf start (1+ pos)))))

(defun %capitalize (segment)
  (if (zerop (length segment))
      segment
      (concatenate 'string (string (char-upcase (char segment 0))) (subseq segment 1))))

(defun %join-segments (segments capitalize-first)
  (with-output-to-string (out)
    (loop for seg in segments
          for first = t then nil
          do (cond ((zerop (length seg)))
                   ((digit-char-p (char seg 0))
                    (unless first (write-char #\_ out))
                    (write-string seg out))
                   ((and first (not capitalize-first))
                    (write-string seg out))
                   (t (write-string (%capitalize seg) out))))))

(defun mangle-name (designator &key (style :camel))
  "Map a Lisp symbol or string to a spec identifier. STYLE is :CAMEL or :PASCAL.
Strings are returned unchanged (explicit :AS overrides)."
  (check-type style (member :camel :pascal))
  (if (stringp designator)
      designator
      (%join-segments (%segments designator) (eq style :pascal))))

(defun mangle-type-name (designator)
  (mangle-name designator :style :pascal))

(defun name-last-segment (string)
  "Strip Quint module namespaces: \"BankTest::Bank::balances\" -> \"balances\"."
  (let ((pos (search "::" string :from-end t)))
    (if pos (subseq string (+ pos 2)) string)))

(defun demangle-name (string &key (package :keyword))
  "Inverse of MANGLE-NAME: \"halfClosedLocal\" / \"HalfClosedLocal\" -> :HALF-CLOSED-LOCAL.
Namespaces are stripped first."
  (let ((s (name-last-segment string)))
    (intern
     (with-output-to-string (out)
       (loop for ch across s
             for i from 0
             do (cond ((char= ch #\_) (write-char #\- out))
                      ((upper-case-p ch)
                       (unless (zerop i) (write-char #\- out))
                       (write-char ch out))
                      (t (write-char (char-upcase ch) out)))))
     package)))
