# Number Library — Release Summary

## Version 1.8.1 — Ready to Release

### New Features

- **`Euler` type** — canonical lazy complex polar form `r·e^(iθ)` with full
  de Moivre arithmetic (multiplication, division, power, conjugate sum/difference)
- **Complex log** — `log(-1) = iπ`, `log(i) = iπ/2` via `IsEuler` extractor
- **Complex trig/hyperbolic structural rules** — `sin(ix) = i·sinh(x)`,
  `cos(ix) = cosh(x)`, `sinh(ix) = i·sin(x)`, `cosh(ix) = cos(x)`
- **Trig special angle identities** — `sin(π/2) = 1`, `cos(π/2) = 0`,
  `sin(π) = 0`, `cos(π) = -1`, `sin(-π/2) = -1`, `cos(-π/2) = 0`
- **Parser support** — `i` as imaginary unit, `sinh`/`cosh`, Cartesian `a+bi`
  and polar `r·e^(iθ)` complex forms in `LaTeXParser`

### Infrastructure

- **`Error` propagation** — `simplify` now correctly propagates `em.Error`
  as `ExpressionException` rather than silently ignoring it
- **`debug` method** on `Expression` — unambiguous structural display using
  `productIterator` for test failure messages
- **`IsEuler` extractor** — recognises `MinusOne`, `I`, and `-i` as Euler
  forms for use in simplification rules
- **`IsMinusOne` fix** — now correctly matches the `MinusOne` case object
  as well as `ValueExpression(WholeNumber(-1))` and `UniFunction(One, Negate)`

### Test Suite

- 3,772+ tests passing, 0 failures
- 15 pending tests (12 in `ComplexFunctionSpec`, all deferred to 1.9.0)

---

## Version 1.9.0 — Planned

### Acceptance Criterion

Un-pending all 12 remaining pending tests in `ComplexFunctionSpec`.

### Planned Work

- **Work Item 10: Complex approximation** (Tier 3) — approximate evaluation
  of complex-valued expressions via a new `approximationComplex` path:
    - Complex trig/hyperbolic operations in the algebra layer (`ComplexCartesian`)
    - `applyComplex` on `ExpressionMonoFunction` and `ExpressionBiFunction`
    - `approximationComplex` on `UniFunction`, `BiFunction`, `ValueExpression`, `Euler`
    - Updated `materialize` to try the complex approximation path as fallback

- **Issue C: Pythagorean identities** — symbolic verification that
  `sin²(z) + cos²(z) = 1` and `cosh²(z) - sinh²(z) = 1`

### Pending Tests Carried Forward

| Test | Issue | Work Item |
|---|---|---|
| `sin(i) = i·sinh(1)` approximately | #189/#192 | Work Item 10 |
| `cos(i) = cosh(1)` approximately | #189/#192 | Work Item 10 |
| `sin(1+i)` approximately | #189/#192 | Work Item 10 |
| `cos(1+i)` approximately | #189/#192 | Work Item 10 |
| `sinh(1+i)` approximately | #189/#192 | Work Item 10 |
| `cosh(1+i)` approximately | #189/#192 | Work Item 10 |
| `exp(i)` approximately | #189/#192 | Work Item 10 |
| `sinh(ix) = i·sin(x)` symbolically | #189/#192 | Work Item 10 |
| `cosh(ix) = cos(x)` symbolically | #189/#192 | Work Item 10 |
| `sin²(z) + cos²(z) = 1` | #193 | Issue C |
| `cosh²(z) - sinh²(z) = 1` | #193 | Issue C |
| `sin(ix) = i·sinh(x)` symbolically | #189/#192 | Work Item 10 |