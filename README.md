# spec-protocol

One `defspec` = a **CLOS protocol** and a **formal spec** ([Quint](https://quint.sh) → Apalache / TLC / TLA+) from the same form. Part of [cl-stack](https://github.com/egao1980/cl-stack). Hero backend: [`spec-backend-quint`](https://github.com/egao1980/spec-backend-quint).

| System | Provides |
|--------|----------|
| `spec-protocol` (nickname `stack-spec`) | `defspec` / `defaction` / `definvariant` / `defrun`, CLOS projection, backend protocol, FSet runtime values |
| `spec-protocol.itf` (package) | ITF traces ([ADR-015](https://apalache-mc.org/docs/adr/015adr-trace.html)) ⇄ Lisp |
| `spec-protocol.mbt` (package) | trace replay: `mbt-driver`, `replay-trace`, automatic `clos-driver` |
| `spec-protocol.examples` | `bank` |

```lisp
(asdf:load-system "spec-backend-quint")   ; + process-backend-uiop, json-backend-jzon

(defspec bank (:module "Bank" :instance ("BankTest" (accounts (set "a" "b"))))
  (const accounts (set str))
  (type status (or :idle (busy int)))
  (var balances (map str int))
  (var status status)
  (pure-def can-withdraw ((b int) (n int)) (:returns bool) (>= (- b n) 0))
  (val total (fold (keys balances) 0 (lambda (acc k) (+ acc (get balances k)))))
  (action init (setf balances (map-of ((k accounts)) 0) status :idle))
  (action deposit ((who str) (n int))
    (:requires (in who accounts) (> n 0))
    (:ensures (= total (+ (old total) n)))
    (setf balances (put balances who (+ (get balances who) n)) status (busy n)))
  (action withdraw ((who str) (n int))
    (:guard (can-withdraw (get balances who) n))
    (setf balances (put balances who (- (get balances who) n))))
  (invariant no-negatives (forall accounts (lambda (a) (>= (get balances a) 0))))
  (:spec-only
   (action step (nondet (who (one-of accounts)) (nondet (n (one-of (to 1 5))) (any (deposit who n) (withdraw who n)))))
   (run happy-test (expect (then (then init (deposit "a" 3)) (withdraw "a" 1)) (= (get balances "a") 2)))))

;; CLOS side
(let ((b (make-instance 'bank :accounts (make-set "a" "b"))))
  (init b) (deposit b "a" 3) (withdraw b "a" 1)      ; generics on the state object
  (total b)                                          ; => 2
  (withdraw b "a" 99))                               ; => SPEC-ACTION-DISABLED (restart CONTINUE)

;; Spec side
(emit 'bank)                                         ; Quint text
(typecheck 'bank) (run-tests 'bank)                  ; #<SPEC-RESULT OK>
(simulate 'bank :seed 1 :n-traces 3)                 ; ITF traces with --mbt metadata
(verify 'bank :max-steps 5 :checker :apalache)       ; or :tlc
(compile-spec 'bank :target :tlaplus)

;; Conformance: replay model traces against the generated class, no driver code
(spec-protocol.mbt:replay-traces (spec-protocol.mbt:make-clos-driver 'bank)
                                 (result-traces (simulate 'bank :n-traces 3)))
```

## Projections

| Declaration | CLOS | Quint |
|-------------|------|-------|
| `const` | initarg + reader | `const` (bound by `:instance`) |
| `var` | slot + accessor | `var` |
| `type … (or …)` | `deftype` (keyword / `(label . payload)`) | sum type |
| `pure-val` / `pure-def` | `defun` | `pure val` / `pure def` |
| `val` / `def` | generic on the state | `val` / `def` |
| `action` | generic + method: `:requires` → `spec-precondition-error`, `:guard` → `spec-action-disabled`, `setf`s staged and committed at once, `:ensures` with `(old …)` → `spec-postcondition-error`, then `check-invariants` | `action`; frame `v' = v` added for untouched vars |
| `invariant` / `witness` | `check-invariants` | `val` → `--invariants` / `--witnesses` |
| `(:spec-only …)` `nondet` `any` `temporal` `run` `assume` `(:raw …)` | not compiled | emitted |
| `(:lisp …)` | compiled | dropped |

Values: `t`/`nil`, integers, strings, `fset2:set` / `fset2:map` / `fset2:seq`, tuples = lists, records = canonical keyword plists, variants = keyword or `(label . value)`. Names: kebab-case → camelCase (types / labels PascalCase); `:as "Name"` overrides.

`defaction spec name (params) …`, `definvariant spec name expr`, `defrun spec name expr` extend a registered spec at top level.

## License

MIT
