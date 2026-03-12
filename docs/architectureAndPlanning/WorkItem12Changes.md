# Work Item 12 — Required and Desirable Changes

*Number Library — March 2026*

This document lists all changes identified during the audit of `structuralMatcher`
and `identitiesMatcher` following introduction of the canonical `Ordering[Expression]`
rule. Changes are classified as **Required** (active regression or dead code masking
a bug) or **Desirable** (dead code cleanup or latent issues).

---

## New Extractor

### `IsImaginaryExpressionCommutative` — **Required**

`IsImaginaryExpression` can match any `Expression` regardless of type, so the
two separate arms in `matchBiFunctionIdentitiesProduct` cannot be reliably ordered.
Replace with a single commutative extractor:

```scala
object IsImaginaryExpressionCommutative {
  def unapply(e: (Expression, Expression)): Option[(Eager, Expression)] = e match {
    case (IsImaginaryExpression(m), x) => Some((m, x))
    case (x, IsImaginaryExpression(m)) => Some((m, x))
    case _                             => None
  }
}
```

Then replace the two arms in `matchBiFunctionIdentitiesProduct` with one:

```scala
case IsImaginaryExpressionCommutative(m, x) =>
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
```

This is almost certainly the cause of the `cosh(1+i)` regression.

---

## `structuralMatcher` Changes

### 1. Non-canonical `Match` result — **Required**

```scala
// BEFORE
case BiFunction(BiFunction(x, k1, Product), BiFunction(y, k2, Product), Sum) if k1 == k2 =>
  em.Match(BiFunction(BiFunction(x, y, Sum), k1, Product))
```

The result places a `CompositeExpression` on the left and `k1` on the right,
which is non-canonical if `k1` is an atom. This will trigger the canonicalisation
rule again unnecessarily (or loop if canonicalisation is not idempotent here).

```scala
// AFTER
case BiFunction(BiFunction(x, k1, Product), BiFunction(y, k2, Product), Sum) if k1 == k2 =>
  em.Match(BiFunction(k1, BiFunction(x, y, Sum), Product))
```

Note: the pattern itself also has `k1`/`k2` on the right of their respective
`Product` — verify this is still canonical after WI12 (i.e. that `x` and `y`
are always composite relative to `k1`/`k2`).

### 2. Unreachable second `Negate/Product` arm — **Desirable**

```scala
// BEFORE
case BiFunction(a, UniFunction(b, Negate), Product) if a == b => minusXSquared(a)
case BiFunction(UniFunction(b, Negate), a, Product) if b == a => minusXSquared(a)
```

`UniFunction` is composite, so under canonical ordering it always appears on
the right. The second arm is unreachable. However, verify that `a` is always
an `AtomExpression` here — if `a` can be composite, neither arm is guaranteed
to fire and a commutative extractor is needed instead.

```scala
// AFTER (if a is always atomic)
case BiFunction(a, UniFunction(b, Negate), Product) if a == b => minusXSquared(a)
// second arm removed
```

### 3. Unreachable second `r * Euler` arm — **Desirable**

```scala
// BEFORE
case BiFunction(r, Euler(One, θ), Product) => em.Match(Euler(r, θ))
case BiFunction(Euler(One, θ), r, Product) => em.Match(Euler(r, θ))
```

`Euler` is composite, so it always appears on the right under canonical ordering.
The second arm is unreachable.

```scala
// AFTER
case BiFunction(r, Euler(One, θ), Product) => em.Match(Euler(r, θ))
// second arm removed
```

### 4. Unreachable second `exp(i*θ)` arm — **Desirable**

```scala
// BEFORE
case BiFunction(UniFunction(BiFunction(I, θ, Product), Exp), n, Power) =>
  em.Match(Euler(One, θ * n).simplify)
case BiFunction(UniFunction(BiFunction(θ, I, Product), Exp), n, Power) =>
  em.Match(Euler(One, θ * n).simplify)
```

`Power` is not commutative so the outer `BiFunction` is unaffected. However,
the inner `BiFunction(I, θ, Product)` vs `BiFunction(θ, I, Product)` — `I` is
an atom (rank 4) and `θ` is generally composite, so `BiFunction(I, θ, Product)`
is canonical. The second inner pattern is now unreachable.

```scala
// AFTER
case BiFunction(UniFunction(BiFunction(I, θ, Product), Exp), n, Power) =>
  em.Match(Euler(One, θ * n).simplify)
// second arm removed
```

### 5. `Match` result in de Moivre sum rules — **Needs verification**

```scala
case BiFunction(Euler(r1, t), Euler(r2, UniFunction(t2, Negate)), Sum) if r1 == r2 && t == t2 =>
  em.Match(Two * r1 * UniFunction(t, Cosine))
case BiFunction(Euler(r1, t), UniFunction(Euler(r2, UniFunction(t2, Negate)), Negate), Sum) if r1 == r2 && t == t2 =>
  em.Match(Two * I * r1 * UniFunction(t, Sine))
```

These chain `*` via `ExpressionOps`. Verify that the intermediate `BiFunction`
nodes produced are in canonical order — in particular that `Two` (atom, rank 2)
ends up on the left of any product with a composite operand.

---

## `matchBiFunctionIdentitiesProduct` Changes

### 6. `(Zero, _) | (_, Zero)` — **Desirable**

```scala
case (Zero, _) | (_, Zero) => em.Match(Zero)
```

`Zero` is a `ScalarConstant` (atom, rank 2), so it will always appear on the
left under canonical ordering. The `(_, Zero)` arm is now unreachable. Harmless
but worth cleaning up for clarity.

```scala
// AFTER
case (Zero, _) => em.Match(Zero)
```

---

## Summary Table

| # | Location | Classification | Nature |
|---|---|---|---|
| — | `IsImaginaryExpressionCommutative` (new) | **Required** | Active regression (`cosh(1+i)`) |
| 1 | `structuralMatcher`: factor-sum result | **Required** | Non-canonical `Match` result |
| 2 | `structuralMatcher`: `Negate/Product` second arm | **Desirable** | Unreachable dead code (verify `a` type) |
| 3 | `structuralMatcher`: `Euler(One,θ)` second arm | **Desirable** | Unreachable dead code |
| 4 | `structuralMatcher`: `exp(i*θ)` second arm | **Desirable** | Unreachable dead code |
| 5 | `structuralMatcher`: de Moivre `Match` results | **Needs verification** | Possible non-canonical intermediate forms |
| 6 | `matchBiFunctionIdentitiesProduct`: `(_, Zero)` | **Desirable** | Unreachable dead code |