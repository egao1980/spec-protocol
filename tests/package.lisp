(defpackage #:spec-protocol/tests
  (:use #:cl #:rove #:spec-protocol)
  (:shadowing-import-from #:rove #:run-tests)
  (:import-from #:spec-protocol.examples #:bank #:bank-balances #:bank-status #:init #:deposit #:withdraw #:total))
(in-package #:spec-protocol/tests)

(defun fixture (name)
  (asdf:system-relative-pathname "spec-protocol" (format nil "tests/fixtures/~a" name)))

(defun fresh-bank (&rest accounts)
  (let ((b (make-instance 'bank :accounts (apply #'make-set (or accounts '("a" "b"))))))
    (init b)
    b))
