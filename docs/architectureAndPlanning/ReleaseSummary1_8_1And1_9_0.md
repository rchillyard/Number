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

- 3,772 tests passing, 0 failures
- 15 pending tests (12 in `ComplexFunctionSpec`, all deferred to 1.9.0)

---

## Version 1.9.1 — Released

### New Features

- **Complex approximation** — `materialize` now correctly evaluates
  complex-valued expressions such as `sin(1+i)`, `cos(i)`, `exp(i)` via
  a new `approximationComplex` path, strictly additive to the existing
  real-valued `approximation` path
- **`n * i` materialisation** — `BiFunction.identitiesMatcher` now
  correctly reduces scalar multiples of `i` to exact `Complex` values
  (e.g. `2 * i` → `Complex(ComplexCartesian(0, 2))`) via the new
  `IsImaginaryExpression` extractor

### Infrastructure

- **`approximationComplex` on `Approximate` trait** — new method with
  default `None`, overridden in `ValueExpression`, `I`, `UniFunction`,
  and `BiFunction`
- **`applyComplex` on `ExpressionMonoFunction`** — dispatches to the
  algebra layer's complex implementations of `sin`, `cos`, `sinh`, `cosh`,
  `exp`, `negate`, `reciprocal`, and `ln` via `z.complex.*`
- **`IsImaginaryExpression` extractor** — lifts `IsImaginary` from `Eager`
  to `Expression` level via `evaluateAsIs`
- **Updated `materialize`** — falls back to `approximationComplex` after
  `approximation` in the evaluation chain:
  ```scala
  asIs.map(normalizeIfAppropriate)
    .orElse(approximation)
    .orElse(approximationComplex)
  ```
- **`sinh`/`cosh`/`tanh` in algebra layer** — `MonadicOperationSinh/Cosh/Tanh`
  in `Operations`, `Number`, `Real`, `Field`, `BaseComplex` (with correct
  fuzz propagation via derivative)
- **`InversePowerTimesNumberCommutative` guard** — prevents imaginary
  `InversePower` values from being incorrectly multiplied as real
- **`IsEuler.unapply`** — uses `IsMinusOne(_)` for generality
- **`IsMinusOne.unapply`** — fixed to match `MinusOne` case object
- **`Real.negate`** — returns `Real` directly, avoiding cast
- **`InversePower` extractors** — new extractor objects for positive and
  imaginary square roots
- **`render` fix** — `CompositeExpression.render` uses `simplify` rather
  than repeated `matchSimpler` calls
- **`ValueExpression.equals`** — short-circuits with `eq` before `===`
- **`operandsMatcher` guard** — restored `&& u.x.contains(I)` in
  `UniFunction.operandsMatcher`

### Test Suite

- 3,793 tests passing, 0 failures
- 4 pending / ignored tests:
  - 3 for Issue C (Pythagorean identities) — deferred to future release
  - 1 unrelated parsing test — deferred to future release

### Acceptance Criterion

Un-pending all remaining pending tests in `ComplexFunctionSpec` — ✅ complete.

---

## Remaining Work

### Issue C — Pythagorean Identities (#193)

Symbolic simplification of:
- `sin²(z) + cos²(z) = 1` (complex `z`)
- `cos²(z) - sin²(z) = cos(2z)` (optional extension)
- `cosh²(z) - sinh²(z) = 1` (complex `z`)

These are fundamentally Euler's equation applied symbolically. The
simplifier needs new rules to recognise and reduce these forms. Currently
verified numerically but not symbolically.

### Parsing — unrelated pending test

One pending test in `LaTeXParserSpec`, unrelated to complex arithmetic.

### Future Milestones

- Version 2.0.0 — major release
- Potential Typelevel affiliate submission of `Matchers` library