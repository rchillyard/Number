# Lazy Polar Expression Canonical Form

*Number Library — Architecture & Planning — March 2026*
*Milestone: Version 1.8*

---

## 1. Motivation

The Number library currently supports lazy expressions for transcendental functions such as `exp`, `sin`, `cos`, `sinh`, and `cosh`. However, when these expressions need to be combined algebraically, there is no common canonical form that allows the simplification pipeline to recognise and exploit the relationships between them.

The concrete acceptance criterion for this work is **un-pending all pending tests in `ComplexFunctionSpec`**, which covers complex logarithm, polar arithmetic, and the full range of trigonometric and hyperbolic identities. Version 1.8 is considered complete when that spec is green with no `pending` entries.

Euler's formula provides exactly such a canonical form. Every complex exponential or trigonometric expression can be written as:

```
r · e^(iθ)
```

This unifies all of the following in one lazy representation:

| Expression | Canonical Euler Form | Notes |
|---|---|---|
| `cos(θ) + i·sin(θ)` | `Euler(1, θ)` | Euler's formula directly |
| `sin(θ)` | imaginary part of `Euler(1, θ)` | extracted by simplification rule |
| `cos(θ)` | real part of `Euler(1, θ)` | extracted by simplification rule |
| `e^(a + ib)` | `Euler(exp(a), b)` | full complex exponential |
| `e^(ix)` | `Euler(1, x)` | pure imaginary exponent |
| `cosh(x)` | real part of `Euler(1, ix)` | simplifies to real |
| `sinh(x)` | imaginary part of `Euler(1, ix)` | simplifies to real |
| `e^x` (real) | stays as `exp(x)` | only converts when meeting complex |

---

## 2. Design Overview

### 2.1 New Expression Type

One new `CompositeExpression` subtype is added to the `expression` module (not `core`), sitting alongside `BiFunction` and similar types:

```scala
// r·e^(iθ) — the canonical lazy complex form
case class Euler(r: Expression, θ: Expression) extends CompositeExpression
```

The `r = 1` case (unit modulus, covering all pure trigonometric functions) is represented as `Euler(Expression.one, θ)` — there is no separate subtype. Simplification rules detect and exploit the `r = 1` case where useful.

`Euler` resides in the `expression` module and materialises to `ComplexPolar` (from the `core` module) when evaluated.

### 2.2 Key Design Decisions

**Single type, not two.** The earlier design proposed `PolarExpression` and `UnitPolarExpression` as separate types. This is rejected in favour of a single `Euler(r, θ)` where the `r = 1` case is handled by simplification rules rather than the type system.

**Lazy conversion only.** Expressions such as `sin(x)` and `exp(x)` remain in their natural form until they encounter another complex expression in the simplification pipeline. This preserves readability: `sin(π/7)` stays as `sin(π/7)` in isolation.

**No explicit `Im`/`Re` expression types.** The imaginary and real projections do not require dedicated expression types. The simplification rules pattern-match directly on `BiFunction` structures and convert to `Euler` in one step.

**Parser stays dumb.** The input form `r·e^(iθ)` is parsed as a `BiFunction` tree. A simplification rule then recognises the pattern and converts it to `Euler`. Mathematical knowledge lives exclusively in the simplifier, not the parser.

**`render`/`toLatex` form.** `Euler(r, θ)` always renders as `r·e^(iθ)`.

**Materialisation target.** `Euler` materialises to the existing eager type `ComplexPolar(r: Number, θ: Number)` in the `core` module. No changes to `core` are required.

---

## 3. Simplification Rules

All rules belong to the structural simplification phases of the existing pipeline. They are grouped into three categories.

### 3.1 Euler Recognition (converting into Euler form)

These rules fire when the simplifier encounters a `BiFunction` that matches the Euler pattern:

```scala
// cos(θ) + i·sin(θ)  →  Euler(1, θ)
BiFunction(cos(θ), BiFunction(i, sin(θ), Product), Sum)
    ⇒  Euler(one, θ)

// cos(θ) - i·sin(θ)  →  Euler(1, -θ)  (complex conjugate)
BiFunction(cos(θ), BiFunction(i, sin(θ), Product), Difference)
    ⇒  Euler(one, Negate(θ))

// r * exp(i * θ)  →  Euler(r, θ)  (parser output recognised here)
BiFunction(r, Exp(BiFunction(i, θ, Product)), Product)
    ⇒  Euler(r, θ)

// exp(ix)  →  Euler(1, x)
Exp(BiFunction(i, x, Product))
    ⇒  Euler(one, x)

// exp(a + ib)  →  Euler(exp(a), b)
Exp(BiFunction(a, BiFunction(i, b, Product), Sum))
    ⇒  Euler(Exp(a), b)
```

### 3.2 Euler Arithmetic (combining Euler forms)

These rules allow `Euler` expressions to be combined efficiently without converting back to Cartesian:

```scala
// Multiplication: add angles, multiply moduli
Euler(r₁, θ₁) * Euler(r₂, θ₂)  ⇒  Euler(r₁·r₂, θ₁ + θ₂)

// Division: subtract angles, divide moduli
Euler(r₁, θ₁) / Euler(r₂, θ₂)  ⇒  Euler(r₁/r₂, θ₁ - θ₂)

// Power (de Moivre's theorem)
Euler(r, θ) ^ n  ⇒  Euler(r^n, n·θ)

// Addition: conjugate pair → real
Euler(r, θ) + Euler(r, -θ)  ⇒  2·r·cos(θ)

// Subtraction: conjugate pair → pure imaginary
Euler(r, θ) - Euler(r, -θ)  ⇒  2i·r·sin(θ)
```

### 3.3 Materialisation (identity simplification phase)

These rules fire when an `Euler` expression resolves to a real, imaginary, or other special value:

```scala
Euler(r, 0)    ⇒  r                              // result is real
Euler(r, π)    ⇒  Negate(r)                      // result is negative real
Euler(r, π/2)  ⇒  BiFunction(i, r, Product)      // result is pure imaginary
Euler(r, -π/2) ⇒  BiFunction(i, Negate(r), Product)
Euler(1, 0)    ⇒  1                              // special cases of r = 1
Euler(1, π)    ⇒  -1
Euler(1, π/2)  ⇒  i
Euler(1, -π/2) ⇒  -i
Euler(0, θ)    ⇒  0                              // zero modulus
```

---

## 4. Work Items

Listed in suggested implementation order. Each is independent enough to be developed and tested incrementally.

1. Add `Euler(r, θ)` as a `CompositeExpression` subtype in the `expression` module, with `materialize` producing `ComplexPolar`.
2. Add clean `unapply` extractors for `sin`, `cos`, and `exp` expressions to make pattern matching in rule sets concise and readable.
3. Implement Euler recognition rules (§3.1) in the structural simplification phase.
4. Implement Euler arithmetic rules (§3.2) for multiplication, division, and power.
5. Implement conjugate-pair addition/subtraction rules producing real or imaginary results.
6. Implement materialisation rules (§3.3) in the identity simplification phase.
7. Add parsing rules to the LaTeX parser grammar to accept Cartesian (`a + bi`) and Polar (`r·e^(iθ)`) input forms as `BiFunction` trees, as well as `sinh`, `cosh`, and `tanh` rules. The simplifier (§3.1) will then convert the polar `BiFunction` tree to `Euler`.
8. Fix the simplification/materialisation rules for complex `Log` expressions.
9. Write a comprehensive test suite: unit tests for each rule, integration tests for multi-step simplifications (e.g. `(cos(θ) + i·sin(θ))^n = Euler(1, nθ)`), and LaTeX rendering tests.

---

## 5. Relationship to Existing Architecture

### 5.1 Module boundaries

`Euler` is placed in the `expression` module. It depends on `ComplexPolar` from the `core` module for materialisation, but no changes to `core` are required.

### 5.2 Simplification pipeline phases

| Phase | New Rules Added |
|---|---|
| Structural (early) | Euler recognition: `cosθ + i·sinθ → Euler(1, θ)` |
| Structural | Euler arithmetic: multiply, divide, power |
| Identity | Materialisation: `Euler(1, 0) → 1`, `Euler(r, 0) → r`, etc. |
| Other phases | No changes required |

### 5.3 Interaction with existing `ComplexPolar` / `ComplexCartesian`

The existing eager types `ComplexPolar` and `ComplexCartesian` in the `core` module are unchanged. `Euler` is their lazy counterpart in the `expression` module. When an `Euler` expression is asked to materialise (e.g. during rendering or numeric evaluation), it produces a `ComplexPolar`.

---

## 6. Resolved Design Questions

For reference, the questions raised during initial design and their resolutions:

- **One type or two?** One: `Euler(r, θ)`. The `r = 1` case uses `Expression.one` and is handled by simplification rules, not a separate `UnitPolarExpression` type.
- **`render`/`toLatex` form?** Always `r·e^(iθ)`.
- **Parsing strategy?** Parser produces `BiFunction` trees; the simplifier converts to `Euler`. Mathematical knowledge stays out of the parser.
- **The zero case.** `Euler(0, θ) → 0` is handled as a materialisation rule; deserves a dedicated test.