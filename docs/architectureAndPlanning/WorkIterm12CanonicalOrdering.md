# Work Item 12 — Canonical Ordering of `BiFunction` Operands

*Number Library — Architecture & Planning — March 2026*
*Milestone: Version 1.9.2 — **COMPLETE***

---

## Overview

Add a rule in `BiFunction.structuralMatcher` to canonicalise commutative
`BiFunction`s so that operands always appear in a defined canonical order:

```
BiFunction(x, y, f)  ->  BiFunction(y, x, f)   when isCommutative(f) && x > y
```

where `>` is defined by a new `Ordering[Expression]` in the `Expression`
companion object.

This reduces the burden on downstream rules — in particular `identitiesMatcher`
— which currently must handle both orderings of commutative operands via
`CommutativeExtractor`. After this work item, rules can be written in one
canonical form for the atom-vs-composite and atom-vs-atom cases.

---

## Design

### Commutative Function Extractor

`IsCommutative` introduced as a new extractor matching on the entire `BiFunction`
and exposing all three components:

```scala
object IsCommutative {
  def unapply(e: Expression): Option[(Expression, Expression, ExpressionBiFunction)] = e match {
    case BiFunction(x, y, f @ (Sum | Product)) => Some((x, y, f))
    case _                                      => None
  }
}
```

`Power` is explicitly excluded — it is not commutative.

### `Ordering[Expression]`

A single `given Ordering[Expression]` in the `Expression` companion object,
covering all expression subtypes. The rule in `structuralMatcher` has no
special-casing of its own; it simply delegates all ordering decisions here.

#### Primary ordering key: subtype class

```
AtomExpression  <  CompositeExpression
```

Atoms are always considered simpler than composites and sort first.

#### Within `AtomExpression`: type-class order

```
Noop < Literal < ScalarConstant < E < I < Infinity
     < PiTranscendental < ETranscendental < L2 < LgE < EulerMascheroni
     < LinearRoot < QuadraticRoot < ValueExpression
```

#### Within `CompositeExpression`: structural depth

When both operands are composite, the ordering falls back to the existing
`depth` method. A shallower expression sorts before a deeper one. This is a
defined tiebreaker rather than a mathematically meaningful order.

#### Equal expressions

When `compare` returns `0`, no swap is performed. The `structuralMatcher`
guard uses strict `gt`, not `gteq`, ensuring the rule does not loop.

### `structuralMatcher` Rule

```scala
case IsCommutative(x, y, f) if summon[Ordering[Expression]].gt(x, y) =>
  em.Match(BiFunction(y, x, f))
```

### Interaction with `CommutativeExtractor`

`CommutativeExtractor` remains necessary for cases where both operands are
composite (e.g. the Pythagorean identities `sin²(z) + cos²(z)`), since both
have the same depth and the ordering will not produce a unique canonical form
in general. The two mechanisms are complementary:

- `Ordering[Expression]` handles the atom-vs-composite and atom-vs-atom cases
  upstream, at the structural phase.
- `CommutativeExtractor` handles composite-vs-composite cases in
  `identitiesMatcher`, as before.

### Known Interaction: Equal-depth Composites

The depth-based tiebreaker for equal-depth composites can produce unpredictable
ordering, which may interfere with `SumSymmetricCommutative` matching for
commuted Pythagorean identity cases (e.g. `cos²(z) + sin²(z)`). This is
a documented limitation; the non-commuted form is always tested.

---

## Bug Fixes Uncovered During Implementation

The following bugs were discovered and fixed as a direct consequence of WI12
exposing previously-untested expression orderings:

### 1. `BiFunction.equals` used `==` in `operandsMatch`
Replaced with `===` (Cats `Eq`) to correctly handle mathematically equivalent
but differently-constructed expressions (e.g. `MinusOne` vs `Literal(Number(-1))`).

### 2. `matchBiFunctionIdentitiesPower` missing `1∧x -> 1`
Added explicit cases:
```scala
case (_, IsZero(_)) | (IsUnity(_), _) => em.Match(One)
case (x, IsUnity(_))                  => em.Match(x)
```

### 3. `isSame` polar/Cartesian conversion was backwards
`ComplexPolar.isSame(ComplexCartesian)` was converting Cartesian to polar
before subtracting. Fixed to convert polar to Cartesian instead, avoiding
unnecessary precision loss.

### 4. `BiFunction.leaveOperandsAsIs` too narrow
Extended to protect all `BiFunction`s where both operands are hyperbolic
functions, not just `IsCoshSquared`/`IsSinhSquared`. Introduced `IsHyperbolic`
extractor:
```scala
object IsHyperbolic {
  def unapply(e: Expression): Option[Expression] = e match {
    case UniFunction(z, Sinh | Cosh) => Some(z)
    case _                           => None
  }
}
```

### 5. `shouldStaySymbolic` too broad for trig/hyperbolic with complex arguments
Added case to `shouldStaySymbolic` to exempt trig/hyperbolic functions when
their argument contains `I`, allowing `simplifyExpand` to fire correctly:
```scala
case UniFunction(x, Sine | Cosine | Sinh | Cosh) => !x.containsI
```
Introduced `containsI` method on `Expression` for recursive `I`-detection.

### 6. `IsImaginaryExpressionCommutative` — partially resolved
The commutative extractor was introduced but the rule is currently commented
out pending resolution of a pre-existing core bug (Issue #197): asymmetric
`Complex + Real` addition throws an exception when operands are reordered.

---

## Known Issues / Deferred

- **Issue #196** — `Box`/`Gaussian` fuzz combination in subtraction too strict.
  Workaround: use `Number("x.xxxx(20)")` notation in affected tests.
- **Issue #197** — `Complex + Real` asymmetric addition bug exposed by WI12
  reordering. `IsImaginaryExpressionCommutative` rule commented out pending fix.
- **`FuzzyNumber` with `None` fuzz** — should be `ExactNumber`. Separate issue.
- **`IsImaginary` too narrow** — should match any `Complex(0, x)`, not just `i`.
  Currently uses `HasImaginary` as workaround in `BiFunctionTableSpec`.

---

## Implementation Steps

### Step 1 — `IsCommutative` extractor ✅
### Step 2 — `Ordering[Expression]` ✅
### Step 3 — `structuralMatcher` rule ✅
### Step 4 — Regression check and fixes ✅
### Step 5 — `BiFunctionTableSpec` ✅

---

## Dependencies

- Work Item 9 (structural rules framework) — ✅ complete
- Work Item 11 (`SumSymmetricCommutative`, `CommutativeExtractor` for Sum) — ✅ complete

---

## Summary

| Step | Location | Status |
|---|---|---|
| 1: `IsCommutative` extractor | `Extractors.scala` | ✅ |
| 2: `Ordering[Expression]` | `Expression` companion object | ✅ |
| 3: `structuralMatcher` rule | `BiFunction.structuralMatcher` | ✅ |
| 4: Regression check and fixes | Full test suite | ✅ |
| 5: Tests | `BiFunctionTableSpec` | ✅ |