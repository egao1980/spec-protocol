(defpackage #:spec-protocol
  (:use #:cl)
  (:nicknames #:stack-spec)
  (:export
   ;; conditions
   #:spec-error #:spec-error-message
   #:spec-parse-error #:spec-parse-error-form
   #:spec-emit-error
   #:spec-tool-missing-error #:spec-tool-missing-error-program
   #:spec-tool-error #:spec-tool-error-exit-code #:spec-tool-error-stdout #:spec-tool-error-stderr
   #:spec-typecheck-error #:spec-typecheck-error-errors
   #:spec-invariant-violation #:spec-invariant-violation-invariant #:spec-invariant-violation-state
   #:spec-invariant-violation-result
   #:spec-action-disabled #:spec-action-disabled-action #:spec-action-disabled-guard #:spec-action-disabled-state
   #:spec-contract-error #:spec-contract-error-operator #:spec-contract-error-form #:spec-contract-error-state
   #:spec-precondition-error #:spec-postcondition-error
   #:spec-conformance-divergence #:divergence-trace #:divergence-step-index #:divergence-action
   #:divergence-variable #:divergence-expected #:divergence-actual
   #:abort-trace
   ;; names
   #:name= #:mangle-name #:mangle-type-name #:demangle-name #:name-last-segment
   ;; runtime values
   #:spec-set #:spec-map #:spec-seq #:spec-equal
   #:make-set #:set-elements #:set-in-p #:set-union #:set-intersect #:set-exclude
   #:set-subset-p #:set-size #:set-fold #:set-map #:set-filter #:set-exists #:set-forall #:int-range
   #:make-map #:map-pairs #:map-get #:map-put #:map-set-by #:map-keys #:map-by
   #:make-seq #:seq-elements #:seq-append #:seq-concat #:seq-head #:seq-tail #:seq-length #:seq-nth
   #:seq-foldl #:seq-select #:seq-map
   #:variant #:variant-p #:variant-label #:variant-value
   #:rec #:rec-field #:rec-with #:tuple-item #:int-div
   ;; ast
   #:spec-module #:module-name #:module-decls #:module-instance #:module-doc #:module-lisp-name #:module-options
   #:module-consts #:module-vars #:module-types #:module-ops #:module-actions #:module-invariants
   #:module-witnesses #:module-runs #:module-find-decl #:module-labels #:module-main-name #:module-add-decl
   #:const-decl #:var-decl #:type-decl #:import-decl #:op-def #:assume-decl #:instance-decl
   #:decl-name #:decl-lisp-name #:decl-type #:decl-doc #:decl-spec-only-p
   #:type-decl-type #:op-mode #:op-params #:op-return-type #:op-body #:op-invariant-p #:op-witness-p
   #:op-requires #:op-guards #:op-ensures
   #:import-module #:import-alias #:import-overrides #:import-wildcard-p
   #:instance-name #:instance-bindings
   #:spec-type #:type-ref #:type-ref-name #:set-type #:list-type #:map-type #:tuple-type #:record-type #:sum-type
   #:type-elem #:type-key #:type-value #:type-elems #:type-fields #:sum-variants
   #:expr #:expr-source
   #:literal #:literal-value #:name-ref #:ref-name #:ref-kind
   #:app #:app-op #:app-args
   #:lambda-expr #:lambda-params #:lambda-body
   #:let-expr #:let-bindings #:let-body
   #:if-expr #:if-test #:if-then #:if-else
   #:match-expr #:match-scrutinee #:match-clauses
   #:record-expr #:record-fields #:field-access #:access-record #:access-field
   #:assign-expr #:assign-var #:assign-value
   #:nondet-expr #:nondet-var #:nondet-domain #:nondet-body
   #:block-expr #:all-expr #:any-expr #:block-forms
   #:variant-expr #:variant-expr-label #:variant-expr-arg
   #:raw-expr #:raw-text #:lisp-expr #:lisp-form #:old-expr #:old-inner #:result-expr
   #:*builtins* #:builtin #:builtin-p #:builtin-name #:builtin-quint #:builtin-style #:builtin-cl
   #:builtin-spec-only-p
   ;; parse + registry
   #:parse-spec-form #:parse-type #:parse-expr #:parse-decl #:module-env
   #:defspec #:defspec-decl #:defaction #:definvariant #:defrun
   #:find-spec #:list-specs #:register-spec #:*specs*
   ;; clos projection
   #:spec-state #:spec-of #:spec-state-alist #:spec-const-alist #:spec-copy-state
   #:check-invariants #:*check-invariants*
   #:spec-state-set #:spec-state-get #:apply-action #:action-generic #:cl-type
   ;; driver protocol
   #:spec-backend #:*spec-backend*
   #:backend-emit #:backend-typecheck #:backend-simulate #:backend-verify #:backend-run-tests #:backend-compile
   #:emit #:typecheck #:simulate #:verify #:run-tests #:compile-spec #:check
   #:spec-result #:result-status #:result-tool #:result-seed #:result-stdout #:result-stderr
   #:result-errors #:result-traces #:result-duration #:result-passed #:result-failed #:result-output))

(defpackage #:spec-protocol.itf
  (:use #:cl)
  (:export
   #:itf-trace #:trace-meta #:trace-vars #:trace-params #:trace-states #:trace-loop
   #:itf-state #:state-index #:state-meta #:state-values #:state-value #:state-variables
   #:state-action-taken #:state-nondet-picks
   #:parse-itf #:read-itf #:write-itf #:itf-value->lisp #:lisp->itf-value #:itf-equal
   #:itf-unserializable #:itf-unserializable-p #:itf-unserializable-text))

(defpackage #:spec-protocol.mbt
  (:use #:cl)
  (:export
   #:mbt-driver #:mbt-init #:mbt-apply #:mbt-observe #:mbt-compare
   #:replay-trace #:replay-traces
   #:clos-driver #:make-clos-driver #:clos-driver-spec #:clos-driver-consts))

(defpackage #:spec-protocol.examples
  (:use #:cl #:spec-protocol)
  (:export #:bank #:bank-balances #:bank-status #:bank-accounts
           #:init #:deposit #:withdraw #:total #:can-withdraw))
