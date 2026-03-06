# Work Item 10 — Complex Approximation (Tier 3)

*Number Library — Architecture & Planning — March 2026*
*Milestone: Version 1.9*

---

## Overview

Un-pending the approximate evaluation tests in `ComplexFunctionSpec`:

```scala
it should "evaluate sin(1 + i) approximately"   // ≈ 1.2985 + 0.6350i
it should "evaluate cos(1 + i) approximately"   // ≈ 0.8337 - 0.9889i
it should "evaluate sinh(1 + i) approximately"  // ≈ 0.6350 + 1.2985i
it should "evaluate cosh(1 + i) approximately"  // ≈ 0.8337 + 0.9889i
it should "evaluate sin(i) approximately"       // ≈ 1.1752i
it should "evaluate cos(i) approximately"       // ≈ 1.5431
it should "evaluate exp(i) approximately"       // ≈ 0.5403 + 0.8415i
```

These require materialisation of complex-valued expressions, which currently
fails because `approximation` returns `Option[Real]` and has no complex path.

---

## Root Cause

`Expression.materialize` currently tries:

```scala
lazy val materialize: Eager = {
  val asIs = simplify.evaluateAsIs
  val maybeValuable = asIs.map(normalizeIfAppropriate) orElse approximation
  recover(maybeValuable)(ExpressionException(s"materialize: logic error on $this"))
}
```

For `sin(1+i)`:
1. `evaluateAsIs` → `None` (not exactly evaluable)
2. `approximation` → `None` (argument `1+i` is complex, not real)
3. `recover` → throws `ExpressionException`

The fix requires a parallel complex approximation path.

---

## Design

### Principle

Preserve the existing `approximation: Option[Real]` contract for all
real-valued cases. Add a parallel `approximationComplex: Option[Complex]`
path for complex results. This is strictly additive — nothing in the
existing code changes behaviour.

### Four Layers of Work

#### Layer 1 — Complex trig/hyperbolic in the algebra layer

Add complex-valued implementations to `Complex` (or a companion object)
using the standard decomposition identities:

```
sin(a+bi)  =  sin(a)·cosh(b)  +  i·cos(a)·sinh(b)
cos(a+bi)  =  cos(a)·cosh(b)  -  i·sin(a)·sinh(b)
sinh(a+bi) =  sinh(a)·cos(b)  +  i·cosh(a)·sin(b)
cosh(a+bi) =  cosh(a)·cos(b)  +  i·sinh(a)·sin(b)
exp(a+bi)  =  e^a·cos(b)      +  i·e^a·sin(b)
```

All right-hand side operations are real `sin`/`cos`/`sinh`/`cosh`/`exp`
on the real and imaginary parts separately — all already available.

These can be implemented as methods on `ComplexCartesian` or as functions
in a `ComplexMath` utility object.

Note: `NaturalExponential.negate` is not supported, so complex expressions
must be converted to fuzzy `Real` values before negation. This is handled
naturally by the `approximationComplex` path since it operates on
approximate values throughout.

#### Layer 2 — `applyComplex` on `ExpressionMonoFunction`

Add a method to `ExpressionMonoFunction`:

```scala
def applyComplex(z: Complex): Option[Complex]
```

Implementations for each function:

```scala
case Sine    => Some(complexSin(z))
case Cosine  => Some(complexCos(z))
case Sinh    => Some(complexSinh(z))
case Cosh    => Some(complexCosh(z))
case Exp     => Some(complexExp(z))
case Negate  => Some(z.negate)
case Reciprocal => Some(z.reciprocal)
case Ln      => Some(complexLn(z))   // principal value: ln|z| + i·arg(z)
case _       => None
```

#### Layer 3 — `approximationComplex` on `Expression` subtypes

Add a new method to the `Expression` trait:

```scala
def approximationComplex(force: Boolean): Option[Complex] = None
```

Override in the relevant subtypes:

**`UniFunction`:**
```scala
def approximationComplex(force: Boolean): Option[Complex] =
  x.approximationComplex(force) flatMap (z => f.applyComplex(z))
```

**`BiFunction`:**
```scala
def approximationComplex(force: Boolean): Option[Complex] =
  for {
    za <- a.approximationComplex(force)
    zb <- b.approximationComplex(force)
    result <- f.applyComplex(za, zb)
  } yield result
```

Note: `ExpressionBiFunction` will also need `applyComplex(za, zb)` for
`Sum`, `Product`, `Power` etc.

**`ValueExpression` (for atomic complex values like `I`):**
```scala
def approximationComplex(force: Boolean): Option[Complex] =
  evaluate(AnyContext) collect { case c: Complex => c }
```

**`Euler`:**
```scala
def approximationComplex(force: Boolean): Option[Complex] =
  evaluate(AnyContext) collect { case c: Complex => c }
```

#### Layer 4 — Updated `materialize`

```scala
lazy val materialize: Eager = {
  val asIs = simplify.evaluateAsIs
  val maybeValuable = asIs.map(normalizeIfAppropriate)
    .orElse(approximation)
    .orElse(approximationComplex(force = true))
  recover(maybeValuable)(ExpressionException(s"materialize: logic error on $this"))
}
```

The `recover` method accepts `Option[Eager]` and `approximationComplex`
returns `Option[Complex]` — since `Complex` extends `Eager`, this composes
cleanly.

---

## Test Strategy

The existing pending tests use `isSame` for fuzzy complex equality:

```scala
materialized.asInstanceOf[Complex].complex.isSame(ComplexCartesian(1.2985, 0.6350))
```

These can be un-pending once the four layers are in place. The tolerance
in `isSame` should be sufficient for double-precision arithmetic.

---

## Not in Scope

- Symbolic complex trig simplification (Issue C — Pythagorean identities)
- `fuzzy` on complex expressions (separate work item)
- Complex `Log` for non-unit-modulus arguments (extension of Work Item 8)

---

## Dependencies

- Work Item 8 (complex log) — ✅ complete
- Work Item 9 (complex trig structural rules) — ✅ complete
- `ComplexCartesian` must support `sin`, `cos`, `sinh`, `cosh`, `exp`
  operations — **new work required in algebra layer**

---

## Summary

| Layer | Location | Effort |
|---|---|---|
| 1: Complex trig in algebra | `ComplexCartesian` or `ComplexMath` | Medium |
| 2: `applyComplex` on mono/bi functions | `ExpressionMonoFunction`, `ExpressionBiFunction` | Small |
| 3: `approximationComplex` on Expression | `UniFunction`, `BiFunction`, `ValueExpression`, `Euler` | Small |
| 4: Updated `materialize` | `Expression` | Trivial |