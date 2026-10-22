(defsystem "spec-protocol"
  :version "0.1.1"
  :description "One defspec = CLOS protocol + formal spec (Quint/TLA+) for cl-stack; ITF traces; MBT replay"
  :author "egao1980"
  :license "MIT"
  :depends-on ("fset" "json-protocol")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "names")
               (:file "runtime")
               (:file "ast")
               (:file "parse")
               (:file "clos")
               (:file "protocol")
               (:file "itf")
               (:file "mbt")
               (:module "examples"
                :components ((:file "bank"))))
  :in-order-to ((test-op (test-op "spec-protocol/tests"))))

(defsystem "spec-protocol/tests"
  :depends-on ("spec-protocol" "json-backend-jzon" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "names-test")
               (:file "runtime-test")
               (:file "parse-test")
               (:file "clos-test")
               (:file "itf-test")
               (:file "mbt-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
