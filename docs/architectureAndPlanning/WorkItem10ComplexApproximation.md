# Work Item 10 — Complex Approximation (Tier 3)

*Number Library — Architecture & Planning — March 2026*
*Milestone: Version 1.9 — **COMPLETE***

---

## Overview

Un-pending the approximate evaluation tests in `ComplexFunctionSpec`:

```scala
it should "evaluate sin(1 + i) approximately"   // ≈ 1.2985 + 0.6350i  ✅
it should "evaluate cos(1 + i) approximately"   // ≈ 0.8337 - 0.9889i  ✅
it should "evaluate sinh(1 + i) approximately"  // ≈ 0.6350 + 1.2985i  ✅
it should "evaluate cosh(1 + i) approximately"  // ≈ 0.8337 + 0.9889i  ✅
it should "evaluate sin(i) approximately"       // ≈ 1.1752i            ✅
it should "evaluate cos(i) approximately"       // ≈ 1.5431             ✅
it should "evaluate exp(i) approximately"       // ≈ 0.5403 + 0.8415i  ✅
it should "satisfy sin(ix) = i·sinh(x)"                                 ✅
it should "satisfy cos(ix) = cosh(x)"                                   ✅
it should "evaluate i * 2"                                               ✅
it should "evaluate cosh(iπ/2) = 0"                                     ✅
```

---

## Root Cause

`Expression.materialize` previously tried:

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

The fix required a parallel complex approximation path.

---

## Design

### Principle

Preserve the existing `approximation: Option[Real]` contract for all
real-valued cases. Add a parallel `approximationComplex: Option[Eager]`
path for complex results. This is strictly additive — nothing in the
existing code changes behaviour.

Note: the return type is `Option[Eager]` rather than `Option[Complex]`
because real-valued results (e.g. `cos(i) = cosh(1)`) are handled
naturally — `Real` extends `Eager` and composes cleanly with `materialize`.

---

## Implementation

### Layer 1 — Complex trig/hyperbolic in the algebra layer ✅

Implemented as lazy vals on `BaseComplex` using exponential definitions:

```
sin(a+bi)  =  sin(a)·cosh(b)  +  i·cos(a)·sinh(b)
cos(a+bi)  =  cos(a)·cosh(b)  -  i·sin(a)·sinh(b)
sinh(a+bi) =  sinh(a)·cos(b)  +  i·cosh(a)·sin(b)
cosh(a+bi) =  cosh(a)·cos(b)  +  i·sinh(a)·sin(b)
exp(a+bi)  =  e^a·cos(b)      +  i·e^a·sin(b)
```

Polar form delegates to Cartesian throughout.

### Layer 2 — `applyComplex` on `ExpressionMonoFunction` ✅

```scala
def applyComplex(z: algebra.eager.Complex): Option[Eager] = {
  val result: Field = this match {
    case Sine       => z.complex.sin
    case Cosine     => z.complex.cos
    case Sinh       => z.complex.sinh
    case Cosh       => z.complex.cosh
    case Exp        => z.complex.exp
    case Negate     => -eagerToField(z)
    case Reciprocal => eagerToField(z).invert
    case Ln         => z.complex.ln
    case _          => return None
  }
  Some(Eager(result))
}
```

The parameter is `algebra.eager.Complex` (the `Eager` subtype); `.complex`
unwraps to `core.numerical.BaseComplex` where the algebra lives.

### Layer 3 — `approximationComplex` on `Approximate` and subtypes ✅

**`Approximate` trait** — default implementation:
```scala
def approximationComplex(force: Boolean = false): Option[Eager] = None
```

**`ValueExpression`** — covers all real-valued atomics (`One`, `Two`, etc.)
as well as complex ones:
```scala
override def approximationComplex(force: Boolean = false): Option[Eager] = value match {
  case c: algebra.eager.Complex => Some(c)
  case _                        => approximation(force)
}
```

**`case object I`** — special case since `value` is `InversePower`, not `Complex`:
```scala
override def approximationComplex(force: Boolean = false): Option[Eager] =
  asComplex
```

**`UniFunction`**:
```scala
override def approximationComplex(force: Boolean = false): Option[Eager] =
  x.approximationComplex(force) flatMap {
    case z: algebra.eager.Complex => f.applyComplex(z)
    case _                        => None
  }
```

**`BiFunction`**:
```scala
override def approximationComplex(force: Boolean = false): Option[Eager] =
  for {
    za <- a.approximationComplex(force)
    zb <- b.approximationComplex(force)
    result <- f match {
      case Sum     => Some(Eager(eagerToField(za) + eagerToField(zb)))
      case Product => Some(Eager(eagerToField(za) `multiply` eagerToField(zb)))
      case Power   => Some(Eager(eagerToField(za) `power` eagerToField(zb)))
      case _       => None
    }
  } yield result
```

**`Expression` trait** — lazy val alongside `approximation`:
```scala
lazy val approximationComplex: Option[Eager] = simplify.approximationComplex(true)
```

### Layer 4 — Updated `materialize` ✅

```scala
lazy val materialize: Eager = {
  val asIs = simplify.evaluateAsIs
  val maybeValuable = asIs.map(normalizeIfAppropriate)
    .orElse(approximation)
    .orElse(approximationComplex)
  recover(maybeValuable)(ExpressionException(s"materialize: logic error on $this"))
}
```

### Bonus — `IsImaginaryExpression` extractor and `n * i` rule ✅

The `i * 2` materialisation test (#149) required a new `Expression`-level
extractor and a new rule in `BiFunction.identitiesMatcher`:

```scala
object IsImaginaryExpression {
  def unapply(x: Expression): Option[Eager] =
    x.evaluateAsIs flatMap (IsImaginary.unapply(_))
}
```

```scala
// in BiFunction.identitiesMatcher, Product cases:
case (IsImaginaryExpression(m), x) =>
  em.matchIfDefined(
    for {
      xv <- x.evaluateAsIs
      n  <- (xv `multiply` m).toOption
      z  <- Eager.eagerToField(n) match {
              case Real(x) => Some(x)
              case _       => None
            }
    } yield Literal(algebra.eager.Complex(ComplexCartesian(numerical.Number.zero, z)))
  )(x)
// (and symmetric case)
```

---

## Test Notes

- Tests comparing symbolic expression equality use `.simplify` on both
  sides (not `materialize`) — this is the established convention for
  `Expression` equality assertions.
- Tests checking approximate complex values use `isSame` on the unwrapped
  `core.numerical.Complex`:
  ```scala
  result.materialize.asInstanceOf[Complex].complex
    .isSame(ComplexCartesian(1.2985, 0.6350)) shouldBe true
  ```
- `fuzzy.toDouble` returns the modulus for complex values — do not use it
  to check the real part of a complex result.

---

## Not in Scope

- Symbolic Pythagorean identities (Issue C #193 — deferred)
- `fuzzy` on complex expressions (separate work item)
- Complex `Log` for non-unit-modulus arguments (extension of Work Item 8)

---

## Dependencies

- Work Item 8 (complex log) — ✅ complete
- Work Item 9 (complex trig structural rules) — ✅ complete
- `ComplexCartesian` complex trig/hyperbolic — ✅ complete (Layer 1)

---

## Summary

| Layer | Location | Status |
|---|---|---|
| 1: Complex trig in algebra | `BaseComplex` | ✅ Complete |
| 2: `applyComplex` on `ExpressionMonoFunction` | `ExpressionFunction.scala` | ✅ Complete |
| 3: `approximationComplex` on `Approximate` and subtypes | `Approximate`, `ValueExpression`, `UniFunction`, `BiFunction`, `Expression` | ✅ Complete |
| 4: Updated `materialize` | `Expression` | ✅ Complete |
| Bonus: `IsImaginaryExpression` + `n*i` rule | `ExpressionFunction`, `BiFunction` | ✅ Complete |