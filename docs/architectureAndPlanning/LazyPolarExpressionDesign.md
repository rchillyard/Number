# Lazy Polar Expression Canonical Form

*Number Library — Architecture & Planning — March 2026*
*Milestone: Version 1.8*

---

## 1. Motivation

The Number library currently supports lazy expressions for transcendental functions such as `exp`, `sin`, `cos`, `sinh`,
and `cosh`. However, when these expressions need to be combined algebraically, there is no common canonical form that
allows the simplification pipeline to recognise and exploit the relationships between them.

The concrete acceptance criterion for this work is **un-pending all pending tests in `ComplexFunctionSpec`**, which
covers complex logarithm, polar arithmetic, and the full range of trigonometric and hyperbolic identities. Version 1.8
is considered complete when that spec is green with no `pending` entries.

Euler's formula provides exactly such a canonical form. Every complex exponential or trigonometric expression can be
written as:

```
r · e^(iθ)
```

This unifies all of the following in one lazy representation:

| Expression          | Canonical Polar Form | Notes                              |
|---------------------|----------------------|------------------------------------|
| `cos(θ) + i·sin(θ)` | `UnitPolar(θ)`       | Euler's formula directly           |
| `sin(θ)`            | `Im(UnitPolar(θ))`   | Imaginary part extracted on demand |
| `cos(θ)`            | `Re(UnitPolar(θ))`   | Real part extracted on demand      |
| `e^(a + ib)`        | `Polar(exp(a), b)`   | Full complex exponential           |
| `e^(ix)`            | `UnitPolar(x)`       | Pure imaginary exponent            |
| `cosh(x)`           | `Re(UnitPolar(ix))`  | Simplifies to real                 |
| `sinh(x)`           | `Im(UnitPolar(ix))`  | Simplifies to real                 |
| `e^x` (real)        | stays as `exp(x)`    | Only converts when meeting complex |

---

## 2. Design Overview

### 2.1 New Expression Types

Two new `Expression` subtypes are added to the `expression` module (not `core`). The unit-modulus case is kept separate
because it is by far the most common (all pure trigonometric functions produce it) and allows simplification rules to
avoid carrying a redundant `r = 1` factor.

```scala
// r·e^(iθ) — general polar form
case class PolarExpression(r: Expression, θ: Expression) extends Expression

// e^(iθ) — unit modulus (r = 1 implicit)
case class UnitPolarExpression(θ: Expression) extends Expression
```

Both types reside in the `expression` module alongside the existing `Expression` hierarchy and materialise to
`ComplexPolar` (from the `core` module) when evaluated.

### 2.2 Key Design Decisions

**Lazy conversion only.** Expressions such as `sin(x)` and `exp(x)` remain in their natural form until they encounter
another complex expression in the simplification pipeline. This preserves readability: `sin(π/7)` stays as `sin(π/7)` in
isolation, not `Im(UnitPolar(π/7))`.

**No explicit `Im`/`Re` expression types.** The imaginary and real projections (`sin(θ) = Im(e^(iθ))`,
`cos(θ) = Re(e^(iθ))`) do not require dedicated expression types. The simplification rules pattern-match directly on
`BiFunction` structures such as `BiFunction(cos(θ), BiFunction(i, sin(θ), Product), Sum)` and convert to `UnitPolar(θ)`
in one step.

**Materialisation target.** Both `PolarExpression` and `UnitPolarExpression` materialise to the existing eager type
`ComplexPolar(r: Number, θ: Number)` in the `core` module. No changes to `core` are required.

---

## 3. Simplification Rules

All rules belong to the structural simplification phases of the existing pipeline. They are grouped into three
categories.

### 3.1 Euler Recognition (converting into polar)

These rules fire when the simplifier encounters a `BiFunction` that matches the Euler pattern:

```scala
// cos(θ) + i·sin(θ)  →  UnitPolar(θ)
BiFunction(cos(θ), BiFunction(i, sin(θ), Product), Sum)
    ⇒  UnitPolar(θ)

// cos(θ) - i·sin(θ)  →  UnitPolar(-θ)  (complex conjugate)
BiFunction(cos(θ), BiFunction(i, sin(θ), Product), Difference)
    ⇒  UnitPolar(Negate(θ))

// exp(ix)  →  UnitPolar(x)
exp(BiFunction(i, x, Product))
    ⇒  UnitPolar(x)

// exp(a + ib)  →  Polar(exp(a), b)
exp(BiFunction(a, BiFunction(i, b, Product), Sum))
    ⇒  PolarExpression(exp(a), b)
```

### 3.2 Polar Arithmetic (combining polar forms)

These rules allow polar forms to be combined efficiently without converting back to Cartesian:

```scala
// Multiplication: add angles, multiply moduli
UnitPolar(θ₁) * UnitPolar(θ₂)  ⇒  UnitPolar(θ₁ + θ₂)
Polar(r₁, θ₁) * Polar(r₂, θ₂)  ⇒  Polar(r₁·r₂, θ₁ + θ₂)

// Division: subtract angles, divide moduli
UnitPolar(θ₁) / UnitPolar(θ₂)  ⇒  UnitPolar(θ₁ - θ₂)
Polar(r₁, θ₁) / Polar(r₂, θ₂)  ⇒  Polar(r₁/r₂, θ₁ - θ₂)

// Power (de Moivre's theorem)
UnitPolar(θ) ^ n  ⇒  UnitPolar(n·θ)
Polar(r, θ)  ^ n  ⇒  Polar(r^n, n·θ)

// Addition: conjugate pair → real
UnitPolar(θ) + UnitPolar(-θ)  ⇒  2·cos(θ)

// Subtraction: conjugate pair → pure imaginary
UnitPolar(θ) - UnitPolar(-θ)  ⇒  2i·sin(θ)
```

### 3.3 Materialisation (identity simplification phase)

These rules fire when a polar expression resolves to a real, imaginary, or other special value:

```scala
UnitPolar(0)    ⇒  1
UnitPolar(π)    ⇒  -1
UnitPolar(π/2)  ⇒  i
UnitPolar(-π/2) ⇒  -i
Polar(r, 0)     ⇒  r          // result is real
Polar(r, π)     ⇒  Negate(r)  // result is negative real
Polar(0, θ)     ⇒  0          // zero modulus
```

---

## 4. Work Items

Listed in suggested implementation order. Each is independent enough to be developed and tested incrementally.

1. Add `PolarExpression(r, θ)` and `UnitPolarExpression(θ)` to the `expression` module, with `materialize`
   implementations producing `ComplexPolar`.
2. Add clean `unapply` extractors for `sin`, `cos`, and `exp` expressions to make pattern matching in rule sets concise
   and readable.
3. Implement Euler recognition rules (§3.1) in the structural simplification phase.
4. Implement polar arithmetic rules (§3.2) for multiplication, division, and power.
5. Implement conjugate-pair addition/subtraction rules producing real or imaginary results.
6. Implement materialisation rules (§3.3) in the identity simplification phase.
7. Add parsing rules to the LaTeX parser grammar to accept Cartesian (`a + bi`) and Polar (`r·e^(iθ)`) input forms,
   producing `ComplexCartesian`/`PolarExpression` respectively, as well as adding sinh, cosh, and tanh rules.
8. Fix the simplification/materialization rules for complex Log expressions.
9. Write a comprehensive test suite: unit tests for each rule, integration tests for multi-step simplifications (e.g.
   `(cos(θ) + i·sin(θ))^n = UnitPolar(nθ)`), and LaTeX rendering tests.

---

## 5. Relationship to Existing Architecture

### 5.1 Module boundaries

All new types (`PolarExpression`, `UnitPolarExpression`) are placed in the `expression` module. They depend on
`ComplexPolar` from the `core` module for materialisation, but no changes to `core` are required.

### 5.2 Simplification pipeline phases

| Phase              | New Rules Added                                             |
|--------------------|-------------------------------------------------------------|
| Structural (early) | Euler recognition: `cosθ + i·sinθ → UnitPolar`              |
| Structural         | Polar arithmetic: multiply, divide, power                   |
| Identity           | Materialisation: `UnitPolar(0) → 1`, `Polar(r,0) → r`, etc. |
| Other phases       | No changes required                                         |

### 5.3 Interaction with existing `ComplexPolar` / `ComplexCartesian`

The existing eager types `ComplexPolar` and `ComplexCartesian` in the `core` module are unchanged. `PolarExpression` is
their lazy counterpart in the `expression` module. When a `PolarExpression` is asked to materialise (e.g. during
rendering or numeric evaluation), it produces a `ComplexPolar`.

---

## 6. Open Questions

- Should `UnitPolarExpression` be a subtype of `PolarExpression`, or kept as a sibling? A subtype relationship (with `r`
  defaulting to `Expression.one`) would simplify some of the arithmetic rules at the cost of a slightly less explicit
  distinction.
- How should `render` and `toLatex` behave for `PolarExpression`? Options: always render as `r·e^(iθ)`, or convert back
  to `cos`/`sin` form for display.
- The zero case: `ComplexPolar(0, 0)` is arguably undefined or degenerate. The materialisation rule `Polar(0, θ) → 0`
  should handle this, but the edge case deserves a dedicated test.
- For the parsing work item (item 7), should `r·e^(iθ)` be parsed as a `PolarExpression` directly, or as a `BiFunction`
  that the simplifier subsequently recognises as polar?