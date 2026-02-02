# Exact vs Approximate Arithmetic

Number maintains **exact arithmetic** by default, only using approximate (fuzzy) values when explicitly requested or
when precision is inherently limited.

## Key Methods

### `materialize` - Returns the "eager" equivalent.

```scala
val expr = Expression(7) ^ Rational(3).invert // Cube root of 7
val result = expr.materialize
// result: InversePower(7, 3) - eager value is semi-symbolic
```

The `materialize` method evaluates an expression to its simplest exact form. When a result cannot be represented
exactly (like irrational roots), it remains in symbolic form rather than converting to a floating-point approximation.

### `fuzzy` - Forces conversion to approximate `Real` value

```scala
val expr = Expression(7) ^ Rational(3).invert
val result = expr.materialize.fuzzy
// result: Real(1.9129..., Some(Fuzz(...)))
```

The `fuzzy` method forces any exact value to convert to its approximate floating-point representation as a `Real` number
with associated uncertainty information.

## Special Mathematical Constants

The library provides exact representations of important mathematical constants that remain symbolic until explicitly
evaluated:

- **π (pi)** - Exact representation, not approximated to 3.14159...
- **e (Euler's number)** - Exact representation, not approximated to 2.71828...
- **φ (golden ratio)** - Represented as `QuadraticSolution`, not 1.61803...
- **ψ (conjugate golden ratio)** - Exact quadratic solution
- **Physical constants** - Maintained with full precision and dimensional analysis

These constants participate in algebraic simplification:

```scala
// π stays symbolic through calculations
val expr = Expression.pi * 2
expr.materialize // 2π (symbolic), not 6.28318...

// Golden ratio exact form
val phi = QuadraticSolution.phi
phi.materialize // QuadraticSolution - exact (1 + √5)/2

// Only becomes approximate when forced
Expression.pi.materialize.fuzzy // Real(3.141592653589793, ...)
phi.fuzzy // Real(1.618033988749895, ...)
```

This ensures that expressions like `sin(π/4)` can be simplified exactly to `√2/2` rather than computing
`sin(3.14159.../4)` ≈ 0.707. Special values are protected from premature evaluation, enabling exact symbolic
computation.

## Design Philosophy

1. **Exact by default**: Operations like `√2`, `π`, and `⅓` remain symbolic
    - Symbolic representations preserve mathematical meaning
    - No loss of precision during intermediate calculations
    - Enables algebraic simplification and pattern matching

2. **Lazy approximation**: Numeric evaluation only when explicitly requested via `fuzzy`
    - Users must consciously choose to leave the exact domain
    - Prevents silent precision loss
    - Makes the distinction between exact and approximate values explicit

3. **Transparent precision**: `Real` values carry uncertainty (`Fuzz`) information
    - Every approximate value includes its error bounds
    - Uncertainty propagates through calculations
    - Users can make informed decisions about precision requirements

4. **Type safety**: The type system distinguishes exact from approximate values
    - `Rational`, `InversePower`, `QuadraticSolution` - exact types
    - `Real` - approximate type with uncertainty
    - Compile-time distinction prevents accidental mixing

## Value Type Hierarchy

```
Value (trait)
├── Eager (trait) - Evaluated values
│   ├── PureNumber (trait) - Concrete numeric values
│   │   ├── Rational - Exact rational numbers (p/q)
│   │   ├── Real - Approximate floating-point with fuzz
│   │   └── Complex - Complex numbers (may contain Real)
│   ├── InversePower - Exact nth roots (ⁿ√x)
│   └── QuadraticSolution - Exact quadratic roots
└── Expression - Symbolic unevaluated expressions
```

## Example Workflows

### Perfect Powers - Exact Results

```scala
// Perfect square root
val expr1 = Expression(16) ^ Rational.half
expr1.materialize // Number(2) - exact integer

// Perfect cube root  
val expr2 = Expression(27) ^ Rational(1, 3)
expr2.materialize // Number(3) - exact integer

// Perfect fourth root
val expr3 = Expression(16) ^ Rational(1, 4)
expr3.materialize // Number(2) - exact integer
```

### Irrational Roots - Stay Symbolic

```scala
// Non-perfect root stays symbolic
val expr = Expression(7) ^ Rational(1, 3)
val exact = expr.materialize
// exact: InversePower(7, 3) - symbolic representation

// Force to approximate when needed
val approx = exact.fuzzy
// approx: Real(1.9129311827723892, Some(AbsoluteFuzz(...)))
```

### Golden Ratio - Quadratic Solution

```scala
// Golden ratio: (1 + √5) / 2
val phi = (Expression(1) + Expression(5).sqrt) / 2
val exact = phi.materialize
// exact: QuadraticSolution - exact symbolic form

// Convert to decimal approximation
val approx = exact.fuzzy
// approx: Real(1.618033988749895, Some(AbsoluteFuzz(...)))
```

### Algebraic Simplification

```scala
// Difference of squares: (√3 + 1)(√3 - 1) = 3 - 1 = 2
val sqrt3 = Expression.sqrt(3)
val expr = (sqrt3 + 1) * (sqrt3 - 1)
val result = expr.materialize
// result: Number(2) - exact through algebraic simplification
```

### Rational Arithmetic - Always Exact

```scala
// Rational arithmetic stays exact
val third = Rational(1, 3)
val twoThirds = Rational(2, 3)
val sum = third + twoThirds
// sum: Rational(1, 1) = 1 - exact

// Only fuzzy when explicitly requested
sum.fuzzy // Real(1.0, Some(AbsoluteFuzz(0.0, ...)))
```

## When to Use `fuzzy`

Use `fuzzy` when you need:

1. **Numeric output for display**: Showing decimal approximations to users
   ```scala
   val result = calculation.materialize
   println(s"Answer: ${result.fuzzy.x}")  // Display as decimal
   ```

2. **Interfacing with numeric libraries**: Passing to non-symbolic math functions
   ```scala
   val angle = piOver4.materialize.fuzzy.x
   val sine = Math.sin(angle)
   ```

3. **Comparison with tolerances**: When exact equality isn't meaningful
   ```scala
   val measured = Real(1.913)
   val expected = Expression(7).cubeRoot.materialize.fuzzy
   measured.x should be (expected.x +- 0.001)
   ```

4. **Performance-critical paths**: When symbolic computation is too expensive
   ```scala
   // Symbolic: slower but exact
   val exact = complexExpression.materialize
   
   // Numeric: faster but approximate
   val approx = complexExpression.materialize.fuzzy
   ```

## When to Stay Exact

Keep values exact when:

1. **Intermediate calculations**: Preserve precision through multi-step computations
2. **Pattern matching**: Enable algebraic simplifications and identities
3. **User-facing formulas**: Display symbolic mathematical expressions
4. **Testing**: Verify exact mathematical relationships without tolerance

## Implementation Details

### `approximation` Method

The `fuzzy` method is implemented in terms of `approximation`:

```scala
def fuzzy: Real = approximation(force = true).getOrElse(
  throw new IllegalStateException(s"Cannot convert $this to Real")
)
```

The `approximation(force: Boolean)` method:

- When `force = false`: Only approximates if already approximate (returns `None` for exact values)
- When `force = true`: Forces conversion of exact values to `Real`
- Returns `Option[Real]` to handle cases where approximation might fail

### Root Normalization

Roots are automatically normalized using prime factorization:

```scala
// ⁴√16 = ⁴√(2⁴) = 2
val expr = Expression(16) ^ Rational(1, 4)
expr.materialize // Number(2) - fully simplified

// ⁴√32 = ⁴√(2⁴ · 2) = 2 · ⁴√2
val expr2 = Expression(32) ^ Rational(1, 4)
expr2.materialize // InversePower(2, 4, coefficient=2) - partially simplified
```

## Testing Considerations

When writing tests, be explicit about exact vs approximate:

```scala
// Test exact result
val exact = expression.materialize
exact shouldBe Number(2) // Exact equality

// Test approximate result
val approx = expression.materialize.fuzzy
approx.x should be(1.913 +- 0.001) // Tolerance-based comparison
```

## Summary

The distinction between exact and approximate arithmetic is fundamental to the Number library's design:

- **`materialize`** keeps you in the exact domain as long as possible
- **`fuzzy`** is your explicit opt-in to the approximate domain
- The type system helps prevent accidental precision loss
- Symbolic representations enable powerful algebraic simplifications

This design ensures that numeric computations maintain maximum precision unless the user explicitly chooses otherwise.