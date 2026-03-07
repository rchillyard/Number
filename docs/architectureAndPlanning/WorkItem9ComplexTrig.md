# Work Item 9 — Complex Trig/Hyperbolic Identities

*Number Library — Architecture & Planning — March 2026*
*Milestone: Version 1.8*

---

## Overview

Un-pending the symbolic trig/hyperbolic tests in `ComplexFunctionSpec` that
follow from purely imaginary arguments:

```
sin(i·x)  =  i·sinh(x)
cos(i·x)  =  cosh(x)
sinh(i·x) =  i·sin(x)
cosh(i·x) =  cos(x)
```

Plus the special-value tests that fall out of these rules feeding into
existing identities:

```
sinh(iπ/2) = i
sinh(iπ)   = 0
cosh(iπ)   = -1
cosh(iπ/2) = 0
```

---

## Design

### Four structural rules

All four identities share the same argument shape: `BiFunction(I, x, Product)`
(or commuted). They belong in `UniFunction.structuralMatcher` alongside the
existing `exp(i·θ) → Euler(1, θ)` rules, since they are purely structural —
they depend only on the *shape* of the argument, not its value.

```
UniFunction(BiFunction(I, x, Product), Sin)  →  BiFunction(I, UniFunction(x, Sinh), Product)
UniFunction(BiFunction(I, x, Product), Cos)  →  UniFunction(x, Cosh)
UniFunction(BiFunction(I, x, Product), Sinh) →  BiFunction(I, UniFunction(x, Sin), Product)
UniFunction(BiFunction(I, x, Product), Cosh) →  UniFunction(x, Cos)
```

Each needs both argument orderings (`I * x` and `x * I`) handled, either via
two cases or via a `CommutativeExtractor`. Given there are only four functions
and two orderings, eight explicit cases is cleaner than a new extractor.

### Special values fall out automatically

Once the four rules are in place:

```
sinh(iπ/2)
  → i·sin(π/2)       [sinh(ix) = i·sin(x)]
  → i·1              [sin(π/2) = 1, existing identity]
  → i                ✓

sinh(iπ)
  → i·sin(π)         [sinh(ix) = i·sin(x)]
  → i·0              [sin(π) = 0, existing identity]
  → 0                ✓

cosh(iπ)
  → cos(π)           [cosh(ix) = cos(x)]
  → -1               [existing identity]    ✓

cosh(iπ/2)
  → cos(π/2)         [cosh(ix) = cos(x)]
  → 0                [existing identity]    ✓
```

No additional rules needed for special values.

### Not in scope

- `sin(1+i)`, `cos(1+i)`, `sinh(1+i)`, `cosh(1+i)` approximately — these
  require materialisation of `Euler` arguments and are a separate work item.
- `sin²(z) + cos²(z) = 1` — Issue C (#193), separate work item.

---

## Changes

### `UniFunction.scala` — `structuralMatcher`

Add eight cases (four identities × two argument orderings) after the existing
`exp(i·θ)` recognition rules:

```scala
// sin(i·x) = i·sinh(x)
case UniFunction(BiFunction(I, x, Product), Sine) =>
  em.Match(I * UniFunction(x, Sinh))
case UniFunction(BiFunction(x, I, Product), Sine) =>
  em.Match(I * UniFunction(x, Sinh))

// cos(i·x) = cosh(x)
case UniFunction(BiFunction(I, x, Product), Cosine) =>
  em.Match(UniFunction(x, Cosh))
case UniFunction(BiFunction(x, I, Product), Cosine) =>
  em.Match(UniFunction(x, Cosh))

// sinh(i·x) = i·sin(x)
case UniFunction(BiFunction(I, x, Product), Sinh) =>
  em.Match(I * UniFunction(x, Sine))
case UniFunction(BiFunction(x, I, Product), Sinh) =>
  em.Match(I * UniFunction(x, Sine))

// cosh(i·x) = cos(x)
case UniFunction(BiFunction(I, x, Product), Cosh) =>
  em.Match(UniFunction(x, Cosine))
case UniFunction(BiFunction(x, I, Product), Cosh) =>
  em.Match(UniFunction(x, Cosine))
```

### `ComplexFunctionSpec.scala` — un-pending tests

Remove `pending` from the following tests (rewriting assertions where needed):

**sin:**
```scala
it should "evaluate sin(i) = i·sinh(1)" in {
  val actual = I.sin.simplify
  val expected = (I * One.sinh).simplify
  actual shouldBe expected
}

it should "satisfy sin(ix) = i·sinh(x)" in {
  val x = Two
  val lhs = (I * x).sin.simplify
  val rhs = (I * x.sinh).simplify
  lhs shouldBe rhs
}
```

**cos:**
```scala
it should "evaluate cos(i) = cosh(1)" in {
  val result = I.cos.simplify
  result shouldBe One.cosh.simplify
}

it should "satisfy cos(ix) = cosh(x)" in {
  val x = Two
  val lhs = (I * x).cos.simplify
  val rhs = x.cosh.simplify
  lhs shouldBe rhs
}
```

**sinh:**
```scala
it should "evaluate sinh(iπ/2) = i" in {
  iPiBy2.sinh.simplify shouldBe I
}

it should "evaluate sinh(iπ) = 0" in {
  iPi.sinh.simplify shouldBe Zero
}

it should "satisfy sinh(ix) = i·sin(x)" in {
  val x = Two
  val lhs = (I * x).sinh.simplify
  val rhs = (I * x.sin).simplify
  lhs shouldBe rhs
}
```

**cosh:**
```scala
it should "evaluate cosh(iπ) = -1" in {
  iPi.cosh.simplify shouldBe MinusOne
}

it should "evaluate cosh(iπ/2) = 0" in {
  iPiBy2.cosh.simplify shouldBe Zero
}

it should "satisfy cosh(ix) = cos(x)" in {
  val x = Two
  val lhs = (I * x).cosh.simplify
  val rhs = x.cos.simplify
  lhs shouldBe rhs
}
```

Note: `iPi` and `iPiBy2` are defined in `ComplexFunctionSpec` as
`Literal(Eager.iPi)` and `iPi / Two` respectively. These are concrete
`Literal` values, not symbolic `BiFunction(I, Pi, Product)` — so
`UniFunction(iPi, Sinh)` will NOT match `UniFunction(BiFunction(I, x, Product), Sinh)`.
The special-value tests (`sinh(iπ/2)` etc.) therefore need a different path.
See the trace below.

---

## Trace

### `sinh(iπ/2) = i` via `iPiBy2 = Literal(Eager.iPi) / Two`

`iPiBy2` is a `Literal` — its argument to `sinh` is not structurally
`BiFunction(I, x, Product)`. The structural rules above won't fire.

These tests may need `iPiBy2` redefined as a symbolic expression:

```scala
private lazy val iPiBy2 = BiFunction(I, BiFunction(Pi, Half, Product), Product)
```

or handled via `IsEuler` recognition in `identitiesMatcher` for `Sinh`/`Cosh`.
**This needs verification during implementation** — run the special-value tests
first and see whether they pass via the structural route or need additional work.

---

## Summary

| Location | Change |
|---|---|
| `UniFunction.scala` | Add 8 structural cases to `structuralMatcher` |
| `ComplexFunctionSpec.scala` | Un-pending + rewrite 10 tests |
| `ComplexFunctionSpec.scala` | Possibly redefine `iPiBy2` as symbolic expression |