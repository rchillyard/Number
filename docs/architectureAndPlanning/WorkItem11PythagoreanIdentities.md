# Work Item 11 — Pythagorean Identities (Issue C #193)

*Number Library — Architecture & Planning — March 2026*
*Milestone: Version 1.9.2*

---

## Overview

Symbolic simplification of the Pythagorean and hyperbolic identities:

```
sin²(z) + cos²(z)   →  1         (for any expression z)
cosh²(z) - sinh²(z) →  1         (for any expression z)
cos²(z) - sin²(z)   →  cos(2z)   (for any expression z)
tan²(z) + 1         →  1/cos²(z) (see note below)
```

These are currently verified numerically (via `materialize`) but not
symbolically. The three pending tests in `ComplexFunctionSpec` (Issue C)
are the acceptance criterion for this work item.

---

## Design

### Pipeline Phase

These rules belong in `identitiesMatcher`. The rationale:

- The identity fires when a specific *relationship* holds between two
  subexpressions (same argument `z` in both trig functions) — this is
  the spirit of identity matching, not purely structural transformation.
- Contrast with `simplifyStructural`, which transforms based solely on
  function type (e.g. `sin(ix) → i·sinh(x)`) with no cross-operand
  constraint.

### Confirmed AST Shape

`sin²(z)` is represented as:

```
BiFunction(UniFunction(z, Sine), Two, Power)
```

Confirmed from inspection of `sin²(π/4)`:

```
BiFunction(UniFunction(BiFunction(Pi, UniFunction(Literal(WholeNumber(4), None), Reciprocal), Product), Sine), Two, Power)
```

The exponent is the `Two` case object (not `ValueExpression(WholeNumber(2))`).
All squared-trig extractors match against this form.

`cosh²(z) - sinh²(z)` after `simplifyOperands`:

```
Sum(
  BiFunction(UniFunction(z, Cosh), Two, Power),
  UniFunction(BiFunction(UniFunction(z, Sinh), Two, Power), Negate)
)
```

The `Negate` wraps the entire `sinh²(z)` term, not just `sinh(z)`.

### Extractors

Dedicated extractors per trig function, each returning the argument `z`:

```scala
object IsSinSquared {
  def unapply(e: Expression): Option[Expression] = e match {
    case BiFunction(UniFunction(z, Sine), Two, Power) => Some(z)
    case _ => None
  }
}
```

Analogously for `IsCosSquared`, `IsSinhSquared`, `IsCoshSquared`,
`IsTanSquared`. The `Negate`-wrapped forms are matched inline as
`UniFunction(IsSinhSquared(z), Negate)` etc.

### Symmetry

`CommutativeExtractor` is not currently wired up for Sum in
`matchBiFunctionIdentitiesSum` (there is an existing `TODO` for the
`Literal`/`QuadraticRoot` case). Step 1b introduces it. Once in place,
each Pythagorean rule needs to be written only once — both orderings
are presented by the extractor.

---

## Implementation Plan

### Step 1a — AST shape ✅

Confirmed (see above).

### Step 1b — Wire `CommutativeExtractor` into `matchBiFunctionIdentitiesSum`

The current implementation:

```scala
private def matchBiFunctionIdentitiesSum(a: Expression, b: Expression): em.MatchResult[Expression] =
  (a, b) match {
    case AdditiveIdentityCommutative(a, _) =>
      em.Match(a)
    case (q1@QuadraticRoot(quadratic: QuadraticEquation, b1, _), q2@QuadraticRoot(e2, b2, _)) if quadratic == e2 && b1 != b2 =>
      em.Match(quadratic.conjugateSum)
    case (l: Literal, q: QuadraticRoot) => // TODO this should use the commutative extractor
      matchLiteral(l, q, Sum)
    case _ =>
      em.Miss[Expression, Expression]("BiFunction: matchBiFunctionSum: no trivial simplification for Sum", this)
  }
```

Introduce a `SumCommutativeExtractor` (analogous to the existing Product
one) and route the match through it, resolving the existing `TODO` as a
side benefit. The `AdditiveIdentityCommutative` case may already handle
its own symmetry — confirm before restructuring.

### Step 2 — Circular identity rule

In `matchBiFunctionIdentitiesSum`, via `SumCommutativeExtractor`:

```scala
// sin²(z) + cos²(z) = 1
case (IsSinSquared(z1), IsCosSquared(z2)) if z1 === z2 => em.Match(One)
```

### Step 3 — Hyperbolic identity rule

```scala
// cosh²(z) - sinh²(z) = 1
case (IsCoshSquared(z1), UniFunction(IsSinhSquared(z2), Negate)) if z1 === z2 => em.Match(One)
```

> **Status: blocked for complex `z`.** The rule is in place but cannot fire
> when `z` is complex (e.g. `z = 1+i`), because `sinh(z)` and `cosh(z)` are
> unconditionally expanded to exponential form by `Expression.simplifyLazy`
> before the enclosing Sum ever reaches `identitiesMatcher`. The pipeline has
> no mechanism to suppress lazy expansion of a subexpression based on its
> parent context. The test for complex `z` remains pending. See Work Item 13.

### Step 4 — Double-angle and secant rules

```scala
// cos²(z) - sin²(z) = cos(2z)
case (IsCosSquared(z1), UniFunction(IsSinSquared(z2), Negate)) if z1 === z2 =>
  em.Match(UniFunction(BiFunction(Two, z1, Product), Cosine))

// tan²(z) + 1 = 1/cos²(z)
// Note: sec is not a first-class function; RHS is Power(cos(z), -2).
// Direction: tan²+1 → 1/cos² is the useful direction for calculus substitutions.
case (IsTanSquared(z1), One) =>
  em.Match(BiFunction(UniFunction(z1, Cosine), MinusTwo, Power))
```

The `tan²+1` rule does not require `CommutativeExtractor` since `One`
is an `AtomExpression` and will be in canonical position once Work Item
12 is complete; in the interim, write both orderings if needed.

### Step 5 — Tests

Un-pending the three Issue C tests in `ComplexFunctionSpec` is the
acceptance criterion. Additionally add tests (in `BiFunctionSpec` or a
new `PythagoreanIdentitySpec`) covering:

- Real `z` and complex `z`
- Both orderings for the circular identity (confirming `CommutativeExtractor`)
- The double-angle result is structurally correct: `cos(2z)`
- The secant result is structurally correct: `1/cos²(z)`
- Non-matching cases do not simplify (regression guard)

---

## Dependencies

- Work Item 9 (structural rules framework) — ✅ complete
- Work Item 10 (complex approximation) — ✅ complete
- `CommutativeExtractor` for Product — ✅ in place
- `CommutativeExtractor` for Sum — introduced in Step 1b

---

## Summary

| Step | Location | Notes |
|---|---|---|
| 1a: Confirm AST shape | — | ✅ Done |
| 1b: `CommutativeExtractor` for Sum | `matchBiFunctionIdentitiesSum` | ✅ Done |
| 2: Circular identity | `matchBiFunctionIdentitiesSum` | ✅ `sin²+cos²=1` |
| 3: Hyperbolic identity | `matchBiFunctionIdentitiesSum` | Rule in place; complex `z` test pending (Work Item 13) |
| 4: Double-angle + secant | `matchBiFunctionIdentitiesSum` | `cos²-sin²=cos(2z)`, `tan²+1=1/cos²` — deferred |
| 5: Tests | `ComplexFunctionSpec` + new spec | Circular identity ✅; hyperbolic pending |

---

# Work Item 12 — Canonical Ordering of `BiFunction` Operands

*Number Library — Architecture & Planning — March 2026*
*Scope: small, separate work item*

## Overview

Add a rule in `structuralMatcher` to canonicalise commutative `BiFunction`s
so that the simpler operand always appears first:

```
BiFunction(CompositeExpression, AtomExpression, f)
  →  BiFunction(AtomExpression, CompositeExpression, f)
```

where `f` is commutative (Sum, Product).

## Rationale

Pushing the ordering concern upstream means rules in `identitiesMatcher`
(and elsewhere) can be written in one canonical form rather than relying
on `CommutativeExtractor` for every case. It also produces a more
predictable, readable expression tree.

## Limitation

Does not help when both operands are `CompositeExpression` (as in the
Pythagorean identities — both `sin²(z)` and `cos²(z)` are composite).
`CommutativeExtractor` remains necessary for those cases. This is why
Work Item 12 is placed *after* Work Item 11 rather than before it.

## Ordering Key

`AtomExpression < CompositeExpression`. A finer-grained ordering (e.g.
by depth or by a canonical type order) could be considered but is out
of scope here.

## Implementation

Single rule in `BiFunction.structuralMatcher`, Product and Sum cases:

```scala
case (c: CompositeExpression, a: AtomExpression) => em.Match(make(a, c))
```

where `make` reconstructs the `BiFunction` with operands swapped.

---

# Work Item 13 — Context-Sensitive Simplification ("Keep It Symbolic")

*Number Library — Architecture & Planning — March 2026*
*Scope: significant design work*

## Overview

The current simplification pipeline processes each expression bottom-up,
with no awareness of parent context. This means that `sinh(z)` and `cosh(z)`
are unconditionally expanded to exponential form by `Expression.simplifyLazy`
whenever they are simplified — even when they appear as operands of a `Power`
inside a `Sum` that could match a Pythagorean identity.

This is a fundamental architectural limitation, encountered in Work Item 11
Step 3 (hyperbolic identity `cosh²(z) - sinh²(z) = 1` for complex `z`).

## The Problem

The pipeline is:

```
simplifyOperands → simplifyStructural → simplifyIdentities → ... → simplifyLazy
```

`simplifyOperands` calls `.simplify` on each child expression independently.
By the time the parent `Sum` reaches `simplifyIdentities`, its children have
already been expanded into exponential form and the `IsCoshSquared`/`IsSinhSquared`
pattern no longer matches.

## Potential Approaches

**Option A — Symbolic mode flag:** A parameter or context object passed through
`simplify` that suppresses `simplifyLazy` for designated function types. Invasive
change to the `Matchers`/pipeline API.

**Option B — Lazy wrapper type:** A `Symbolic(expr)` expression subtype that
suppresses lazy expansion of its contents. The enclosing expression opts in by
wrapping its operands before simplification.

**Option C — Two-pass simplification:** A preliminary pass that recognises
high-level patterns (Pythagorean etc.) before operand simplification, followed
by the normal pipeline. Requires duplicating or pre-empting the identity rules.

**Option D — Rewrite identities in expanded form:** Match the fully-expanded
exponential form of `cosh²-sinh²` directly. Unreadable and brittle.

## Status

Deferred. The hyperbolic Pythagorean identity test for complex `z` remains
pending until this work item is addressed. Options A or B appear most
consistent with the existing `Matchers` design philosophy.