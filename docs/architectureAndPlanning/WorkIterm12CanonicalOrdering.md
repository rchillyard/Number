# Work Item 12 — Canonical Ordering of `BiFunction` Operands

*Number Library — Architecture & Planning — March 2026*
*Milestone: Version 1.9.2 — **PLANNED***

---

## Overview

Add a rule in `BiFunction.structuralMatcher` to canonicalise commutative
`BiFunction`s so that operands always appear in a defined canonical order:

```
BiFunction(x, y, f)  →  BiFunction(y, x, f)   when isCommutative(f) && x > y
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

Check whether `IsCommutative` (or equivalent) already exists in the codebase.
If not, introduce it:

```scala
object IsCommutative {
  def unapply(f: ExpressionBiFunction): Boolean = f match {
    case Sum | Product => true
    case _             => false
  }
}
```

`Power` is explicitly excluded — it is not commutative.

### `Ordering[Expression]`

A single `given Ordering[Expression]` in the `Expression` companion object,
covering all expression subtypes. The rule in `structuralMatcher` then has no
special-casing of its own; it simply delegates all ordering decisions here.

#### Primary ordering key: subtype class

```
AtomExpression  <  CompositeExpression
```

Atoms are always considered simpler than composites and sort first.

#### Within `AtomExpression`: type-class order

A defined total order over atom types, from simplest/most-specific to
most-general:

```
Zero < One < Two < Half < Pi < Literal < Variable < ...
```

The precise sequence should be established during implementation by inspecting
all `AtomExpression` subtypes (including any named constants beyond `Pi`).
The principle is: named mathematical constants with fixed values precede
general literals, which precede symbolic variables.

#### Within `CompositeExpression`: structural depth

When both operands are composite, the ordering falls back to a structural
measure — for example, expression depth (number of nested levels). A shallower
expression sorts before a deeper one. This is a defined tiebreaker rather than
a mathematically meaningful order, and is sufficient for the canonicalisation
goal.

#### Equal expressions

When `compare` returns `0` (structurally identical subexpressions), no swap
is performed. The `structuralMatcher` guard uses strict `gt`, not `gteq`,
ensuring the rule does not loop.

### `structuralMatcher` Rule

A single rule, added to `BiFunction.structuralMatcher` for commutative
functions:

```scala
case BiFunction(x, y, IsCommutative()) if summon[Ordering[Expression]].gt(x, y) =>
  em.Match(make(y, x))
```

No additional special-casing for atom-vs-composite, atom-vs-atom, or
composite-vs-composite — all cases are handled by `Ordering[Expression]`.

### Interaction with `CommutativeExtractor`

`CommutativeExtractor` remains necessary for cases where both operands are
composite (e.g. the Pythagorean identities `sin²(z) + cos²(z)`), since both
have the same depth and the ordering will not produce a unique canonical form
in general. The two mechanisms are complementary:

- `Ordering[Expression]` handles the atom-vs-composite and atom-vs-atom cases
  upstream, at the structural phase.
- `CommutativeExtractor` handles composite-vs-composite cases in
  `identitiesMatcher`, as before.

### Scope Limitation

This work item does not attempt to define a canonical order between two
arbitrary composite expressions of equal depth. A finer-grained ordering
(e.g. by full structural hash or recursive type ordering) is out of scope.

---

## Implementation Steps

### Step 1 — `IsCommutative` extractor
Check for existing extractor; add if absent. Cover `Sum` and `Product`.

### Step 2 — `Ordering[Expression]`
Implement `given Ordering[Expression]` in the `Expression` companion object.
- Enumerate all `AtomExpression` subtypes and assign type-class rank.
- Implement depth measure for `CompositeExpression` fallback.
- Unit-test the ordering directly (not via simplification).

### Step 3 — `structuralMatcher` rule
Add the canonicalisation rule to `BiFunction.structuralMatcher`.
Verify with `matchSimpler`-style tests for representative cases.

### Step 4 — Regression check
Run the full test suite. Confirm no previously-green tests are broken by
the reordering. Adjust any tests whose expected forms depended on the old
(non-canonical) ordering.

### Step 5 — Tests
Add targeted tests:
- Atom before composite after canonicalisation.
- Canonical atom ordering (e.g. `Two + Pi` → `Two + Pi`, not `Pi + Two`).
- Composite-vs-composite: confirm no reordering occurs (handled by
  `CommutativeExtractor` downstream).
- `Power`: confirm no reordering occurs.

---

## Dependencies

- Work Item 9 (structural rules framework) — ✅ complete
- Work Item 11 (`SumSymmetricCommutative`, `CommutativeExtractor` for Sum) — ✅ complete

---

## Summary

| Step | Location | Status |
|---|---|---|
| 1: `IsCommutative` extractor | `Extractors.scala` (or existing location) | Planned |
| 2: `Ordering[Expression]` | `Expression` companion object | Planned |
| 3: `structuralMatcher` rule | `BiFunction.structuralMatcher` | Planned |
| 4: Regression check | Full test suite | Planned |
| 5: Tests | `BiFunctionSpec` or equivalent | Planned |