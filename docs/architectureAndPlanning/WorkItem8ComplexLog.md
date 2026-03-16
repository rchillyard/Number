# Work Item 8 — Complex Log Support

*Number Library — Architecture & Planning — March 2026*
*Milestone: Version 1.8*

---

## Overview

Un-pending the two `log` tests in `ComplexFunctionSpec`:

```scala
it should "evaluate log(-1) = iπ"
it should "evaluate log(i) = iπ/2"
```

The target is `BiFunction.matchBiFunctionIdentitiesLog` in `BiFunction.scala`.
The simplification pipeline already delivers `x.log` as `BiFunction(x, E, Log)`,
so no changes to `UniFunction` are needed.

---

## Design

### General rule

The principal of complex logarithm is:

```
log(r·e^(iθ))  =  ln(r) + i·θ
```

For unit-modulus values this reduces to:

```
log(e^(iθ))  =  i·θ
```

`MinusOne` is `e^(iπ)` and `I` is `e^(iπ/2)`, so both cases fall out of the
general rule once the argument is recognised as an `Euler` form.

### No `Ln` expansion

`UniFunction(x, Ln)` is deliberately **not** expanded to `BiFunction(x, E, Log)`.
The existing rule in `matchBiFunctionIdentitiesLog` already goes the other way:

```scala
case (a, E) => em.Match(UniFunction(a, Ln))
```

Adding the reverse expansion would create an infinite simplification loop.
Since `x.log` already produces `BiFunction(x, E, Log)` directly, no expansion
is needed.

---

## Changes

### 1. `Euler.scala` — `IsEuler` extractor (top-level in package)

Add as a top-level object in the `expression.expr` package (same file as
`Euler`, after the companion object), since it needs access to the package-level
constants `MinusOne`, `I`, `One`, `Pi`, `Half`:

```scala
object IsEuler {
  def unapply(e: Expression): Option[Euler] = e match {
    case MinusOne =>
      Some(Euler(One, Pi))
    case I =>
      Some(Euler(One, BiFunction(Pi, Half, Product)))
    case UniFunction(IsEuler(eu), Negate) =>
      Some(Euler(eu.r, UniFunction(eu.θ, Negate)))
    case _ =>
      None
  }
}
```

The third case is recursive — `UniFunction(IsEuler(eu), Negate)` — handling
`-i` → `Euler(1, -π/2)` for free, and is the natural extension point for
`IsEager(Complex(...))` later (Issue C).

### 2. `UniFunction.scala` — `identitiesMatcher`

Add one new case to `UniFunction.identitiesMatcher`. The rule belongs here
because `.ln` desugars to `UniFunction(x, Ln)`, not `BiFunction(x, E, Log)`,
so `matchBiFunctionIdentitiesLog` never sees it.

```scala
case UniFunction(IsEuler(Euler(r, θ)), Ln) =>
  em.Match((UniFunction(r, Ln) + (I * θ)).simplify)
```

This is a strictly simplifying step — complex argument → sum of simpler terms
— with no risk of cycling back into `Ln` expansion.

`BiFunction.matchBiFunctionIdentitiesLog` is **not changed**.

### 3. `ComplexFunctionSpec.scala` — rewrite the two pending tests

Remove `pending` and replace the `Literal(Eager.iPi)` assertions with
symbolic expression equality:

```scala
it should "evaluate log(-1) = iπ" in {
  MinusOne.log.simplify shouldBe (I * Pi).simplify
}

it should "evaluate log(i) = iπ/2" in {
  I.log.simplify shouldBe (I * Pi * Half).simplify
}
```

Using `.simplify` on both sides avoids sensitivity to the internal `Product`
tree ordering.

---

## Trace

```
MinusOne.log
  = BiFunction(MinusOne, E, Log)
  → matchBiFunctionIdentitiesLog: (MinusOne, E)
  → toEuler(MinusOne) = Some(Euler(One, Pi))
  → (UniFunction(One, Ln) + (I * Pi)).simplify
  → (Zero + (I * Pi)).simplify          // ln(1) = 0
  → (I * Pi).simplify
  ✓ shouldBe (I * Pi).simplify

I.log
  = BiFunction(I, E, Log)
  → toEuler(I) = Some(Euler(One, BiFunction(Pi, Half, Product)))
  → (UniFunction(One, Ln) + (I * Pi * Half)).simplify
  → (Zero + (I * Pi * Half)).simplify   // ln(1) = 0
  → (I * Pi * Half).simplify
  ✓ shouldBe (I * Pi * Half).simplify
```

---

## Not in scope

- General `log(Euler(r, θ))` for non-unit-modulus `r` — `toEuler` returns
  `None` for those cases; the existing `em.Match(UniFunction(a, Ln))` fires
  instead, which is the correct fallback for real `a`.
- `IsEager(Complex(...))` recognition in `toEuler` — deferred to Issue C.
- `sin(ix)`, `cos(ix)`, `sinh(ix)`, `cosh(ix)` — Issue B remainder,
  separate work item.

---

## Summary

| Location | Change |
|---|---|
| `Euler.scala` | Add `IsEuler` extractor as top-level object in package |
| `UniFunction.scala` | Add `IsEuler` case to `identitiesMatcher` |
| `ComplexFunctionSpec.scala` | Un-pending + rewrite 2 log tests |