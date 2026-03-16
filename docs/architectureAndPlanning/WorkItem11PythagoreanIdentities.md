# Work Item 11 — Pythagorean Identities (Issue C #193)

*Number Library — Architecture & Planning — March 2026*
*Milestone: Version 1.9.2 — **COMPLETE (partial)***

---

## Overview

Symbolic simplification of the Pythagorean and hyperbolic identities:

```
sin²(z) + cos²(z)   →  1         (for any expression z)
cosh²(z) - sinh²(z) →  1         (for any expression z)
cos²(z) - sin²(z)   →  cos(2z)   (for any expression z)
tan²(z) + 1         →  1/cos²(z) (see note below)
```

The three originally pending Issue C tests are resolved as follows:

- `sin²(z)+cos²(z)=1` for complex `z` — ✅ green
- `cosh²(z)-sinh²(z)=1` for complex `z` — ✅ green (via `leaveOperandsAsIs`)
- `cosh²(x)-sinh²(x)=1` for concrete numeric `x` — pending; symbolic
  simplification not possible when argument is fully evaluable. Verified
  numerically via `materialize`. Symbolic case deferred.

Steps 4 (double-angle and secant rules) remain deferred.

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

Dedicated extractors in `Extractors.scala`, each returning the argument `z`:

```scala
object IsSinSquared {
  def unapply(e: Expression): Option[Expression] = e match {
    case BiFunction(UniFunction(z, Sine), Two, Power) => Some(z)
    case _ => None
  }
}
```

Analogously for `IsCosSquared`, `IsSinhSquared`, `IsCoshSquared`.
The `Negate`-wrapped forms are matched inline as
`UniFunction(IsSinhSquared(z), Negate)` etc.

### Symmetry

`SumSymmetricCommutative` extractor added to `BiFunction.scala` and wired
into `matchBiFunctionIdentitiesSum`, resolving the existing `TODO`. Each
Pythagorean rule is written once; both orderings are handled automatically.

### `leaveOperandsAsIs` Mechanism

The hyperbolic identity required a new general mechanism to prevent
`sinh`/`cosh` from being eagerly expanded to exponential form by
`Expression.simplifyLazy` before `identitiesMatcher` can fire.

**`CompositeExpression.leaveOperandsAsIs`** — default implementation,
propagates upward through any wrapper:

```scala
def leaveOperandsAsIs: Boolean = terms.exists {
  case n: Nameable => n.keepSymbolic
  case c: CompositeExpression => c.leaveOperandsAsIs
  case _ => false
}
```

**`BiFunction.leaveOperandsAsIs`** — base case, delegates to super:

```scala
override def leaveOperandsAsIs: Boolean = this match {
  case IsCoshSquared(_) | IsSinhSquared(_) => true
  case _ => super.leaveOperandsAsIs
}
```

**`simplifyOperands`** — checks `leaveOperandsAsIs` before recursing:

```scala
case c: CompositeExpression if c.leaveOperandsAsIs =>
  em.Miss("Expression:simplifyOperands: leaving operands as-is", c)
```

**`componentsSimplifier`** — also checks per operand:

```scala
xs.map {
  case c: CompositeExpression if c.leaveOperandsAsIs => c
  case x => x.simplify
}
```

This prevents `cosh(z)` and `sinh(z)` from expanding when they appear
as operands of `Power(..., Two)`, allowing `identitiesMatcher` to see
`IsCoshSquared`/`IsSinhSquared` intact.

**Limitation:** Does not help when the argument `z` is a concrete numeric
value (e.g. `Literal(Rational(3,2))`), since `cosh(z)` is fully evaluated
before squaring. The symbolic test for concrete `x` remains pending.

### `shouldStaySymbolic` Fix

Moving `shouldStaySymbolic` from a static method to an instance method
introduced a subtle regression: the `CompositeExpression` case was not
checking `Nameable` terms, only `CompositeExpression` ones. Fixed by
adding the `Nameable` case:

```scala
case c: CompositeExpression =>
  c.terms.exists {
    case n: Nameable => n.keepSymbolic
    case x: CompositeExpression => x.shouldStaySymbolic
    case _ => false
  }
```

---

## Implementation Steps

### Step 1a — AST shape ✅
### Step 1b — `SumSymmetricCommutative` + wire into `matchBiFunctionIdentitiesSum` ✅
### Step 2 — Circular identity `sin²+cos²=1` ✅
### Step 3 — Hyperbolic identity `cosh²-sinh²=1` ✅ (for non-evaluable `z`)
### Step 4 — Double-angle and secant rules — deferred
### Step 5 — Tests ✅ (concrete numeric `x` case pending)

---

## Dependencies

- Work Item 9 (structural rules framework) — ✅ complete
- Work Item 10 (complex approximation) — ✅ complete
- `CommutativeExtractor` for Product — ✅ in place
- `CommutativeExtractor` for Sum — ✅ introduced in Step 1b

---

## Summary

| Step | Location | Status |
|---|---|---|
| 1a: Confirm AST shape | — | ✅ |
| 1b: `SumSymmetricCommutative` | `matchBiFunctionIdentitiesSum` | ✅ |
| 2: Circular identity | `matchBiFunctionIdentitiesSum` | ✅ |
| 3: Hyperbolic identity | `matchBiFunctionIdentitiesSum` | ✅ (complex `z`); pending (numeric `x`) |
| 4: Double-angle + secant | `matchBiFunctionIdentitiesSum` | Deferred |
| 5: Tests | `ComplexFunctionSpec` | ✅ (numeric pending) |

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

The `leaveOperandsAsIs` mechanism (introduced in Work Item 11) solves the
specific case of `cosh²`/`sinh²` by suppressing eager expansion when these
expressions appear as operands. However, it does not solve the more general
case where the *argument* of `cosh`/`sinh` is a concrete numeric value — in
that case, `cosh(z)` is fully evaluated before squaring, so `IsCoshSquared`
never matches.

More generally, the simplification pipeline processes each expression
bottom-up with no awareness of parent context, making it impossible to
suppress expansion based on what an expression's parent intends to do with it.

## Remaining Limitation

`cosh²(x) - sinh²(x) = 1` for concrete numeric `x` (e.g. `Rational(3,2)`)
cannot be simplified symbolically. The test is pending. Verified numerically
via `materialize` (result is fuzzy since `e^(3/2)` is irrational).

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

Partially addressed by `leaveOperandsAsIs` (Work Item 11). The remaining
case (concrete numeric argument) is deferred. Options A or B appear most
consistent with the existing `Matchers` design philosophy.