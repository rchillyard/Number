# Valuable Architecture: Solution, Expression, and Complex

## Overview

This document describes the architectural design decisions for representing mathematical values in the Number library, specifically addressing the distinction between `Solution`, `Expression`, and `Complex` types.

## Core Design Principle

Mathematical values fall into three categories based on their representation and evaluation characteristics:

1. **Eager values** - Can be simplified to a concrete numeric form
2. **Algebraic solutions** - Have explicit algebraic structure but can't always be combined
3. **Symbolic expressions** - Must remain symbolic until materialized

## Type Hierarchy

```
Valuable (root trait)
├── Eager (immediately evaluable)
│   ├── Structure
│   │   ├── Number
│   │   ├── Scalar
│   │   ├── Monotone
│   │   └── ...
│   ├── Solution (algebraic structure)
│   │   ├── LinearSolution
│   │   ├── QuadraticSolution
│   │   ├── CubicSolution (future)
│   │   └── AlgebraicRoot (degree ≥ 5)
│   ├── Complex (any complex number)
│   │   ├── ComplexCartesian (a + bi)
│   │   └── ComplexPolar (r·e^(iθ))
│   └── Nat (natural numbers)
└── Expression (symbolic, lazy evaluation)
    ├── AtomicExpression
    ├── ExpressionMonoFunction
    │   ├── Sin, Cos, Log, Exp, etc.
    │   ├── Bessel (special functions)
    │   ├── Erf (error function)
    │   └── LambertW, etc.
    └── CompositExpression
```

## Solution: Algebraic Structure

### Purpose

`Solution` represents solutions to polynomial equations where the algebraic structure is known and can be represented explicitly. The key characteristic is that terms cannot always be combined into a single closed form.

### Examples

**QuadraticSolution**: x = base ± √offset
```scala
case class QuadraticSolution(
  base: Monotone,      // The rational part: -b/2a
  offset: Monotone,    // The radical part: Δ/4a²
  branch: Branch,      // Which root: Plus or Minus
  conjugate: Boolean   // Real vs complex conjugate
) extends Solution
```

For the quadratic equation ax² + bx + c = 0:
- `base` = -b/2a (the rational part)
- `offset` = (b² - 4ac)/4a² (what's under the square root)
- Two solutions differ only by the `branch` (± sign)

**LinearSolution**: x = value
```scala
case class LinearSolution(value: Monotone) extends Solution
```

### Why Solution Can't Be Combined

Consider: x = 3 + √2
- The `3` is rational (base)
- The `√2` is irrational (offset)
- They cannot be combined into a single `Monotone`
- But we know the exact algebraic structure

### Future Extensions

**CubicSolution** (if needed):
```scala
case class CubicSolution(
  base: Monotone,
  cubeRootTerm: Monotone,
  cubeRadicand: Monotone,
  branch: CubicBranch  // 3 possible complex cube roots
) extends Solution
```

**AlgebraicRoot** (for degree ≥ 5 or complex cases):
```scala
case class AlgebraicRoot(
  polynomial: Polynomial,
  numericalApproximation: Complex,
  rootIndex: Int  // Which root (1st, 2nd, 3rd...)
) extends Solution
```

For polynomials of degree ≥ 5, the Abel-Ruffini theorem tells us there's no general radical formula, so we store the defining polynomial and a numerical approximation.

## Complex: Any Complex Number

### Purpose

`Complex` represents **any** complex number, whether it comes from an algebraic solution, a transcendental function, or direct construction. Unlike `Solution`, `Complex` doesn't preserve algebraic structure—it's just a + bi.

### Representation

```scala
sealed trait Complex extends Eager

case class ComplexCartesian(
  real: Number,
  imaginary: Number
)(val maybeName: Option[String] = None) extends Complex

case class ComplexPolar(
  radius: Number,
  angle: Angle,
  branch: Int = 0
)(val maybeName: Option[String] = None) extends Complex
```

### Relationship to Solution

- **Solution → Complex**: Solutions can materialize to Complex when evaluated numerically
- **Complex ⊄ Solution**: Not all Complex numbers come from algebraic solutions
- **Complex is more general**: Includes transcendental values like e^(iπ), sin(1) + i·cos(1)

### Examples

```scala
// From algebraic solution
val quadratic: QuadraticSolution = QuadraticSolution(
  base = Scalar(1),
  offset = Scalar(-1),  // √(-1) = i
  branch = Plus,
  conjugate = false
)
val asComplex: Complex = quadratic.materialize  // Complex(1, 1) = 1 + i

// Direct complex construction
val eulerIdentity: Complex = ComplexPolar(1, Angle.pi, 0)  // e^(iπ) = -1

// From expression evaluation
val transcendental: Expression = Exp(Multiply(I, Pi))
val evaluated: Complex = transcendental.materialize.asInstanceOf[Complex]
```

## Expression: Symbolic Representation

### Purpose

`Expression` represents mathematical values that must remain symbolic until explicitly materialized. This includes:
- Special functions (Bessel, error function, elliptic integrals)
- Transcendental values that can't simplify (sin(π/7), log(3))
- Complex symbolic expressions
- Anything that doesn't fit the algebraic structure of `Solution`

### Key Characteristic

Expressions support:
- **Symbolic manipulation**: Simplification, normalization, pattern matching
- **Lazy evaluation**: Computation deferred until `materialize` is called
- **Infinite extensibility**: New functions added without touching core types

### Examples

**Special Functions**:
```scala
// Bessel function J_n(x)
case class BesselJ(
  order: Expression,
  argument: Expression
) extends ExpressionMonoFunction {
  override def materialize: Eager = {
    // Use numerical library to compute
    val n = order.materialize.toDouble
    val x = argument.materialize.toDouble
    Complex(numericalBessel(n, x))
  }
}

// Error function erf(x)
case class Erf(argument: Expression) extends ExpressionMonoFunction {
  override def materialize: Eager = {
    val x = argument.materialize.toDouble
    Scalar(numericalErf(x))
  }
}

// Lambert W function
case class LambertW(
  branch: Int,
  argument: Expression
) extends ExpressionMonoFunction
```

**Transcendentals**:
```scala
// These stay as expressions until forced to materialize
val sinPi7: Expression = Sin(Divide(Pi, Number(7)))
val log3: Expression = Log(Number(3))
val sqrtPi: Expression = Power(Pi, Rational(1, 2))
```

## Design Boundaries

### When to Use Solution

Use `Solution` when:
- ✅ Solving a polynomial equation
- ✅ You can represent the algebraic structure explicitly
- ✅ The solution involves radicals with known indices (√, ∛, etc.)
- ✅ Terms cannot be combined but structure is known

Examples: Quadratic formula, cubic formula (Cardano), simple algebraic numbers

### When to Use Complex

Use `Complex` when:
- ✅ You have a concrete complex number (a + bi)
- ✅ You don't need to preserve algebraic structure
- ✅ Numerical form is sufficient
- ✅ Result of materializing a Solution or Expression

Examples: e^(iπ), 3 + 4i, result of numerical computation

### When to Use Expression

Use `Expression` when:
- ✅ Must stay symbolic for pattern matching/simplification
- ✅ Special functions (Bessel, erf, elliptic, hypergeometric)
- ✅ Transcendental values that won't simplify
- ✅ Degree ≥ 5 polynomials (if not using AlgebraicRoot)
- ✅ Anything that doesn't fit Solution's structure

Examples: BesselJ(2, x), sin(π/7), log(log(3)), algebraic expressions

## Materialization Flow

```
Expression (symbolic)
    │
    ├─→ materialize() → Solution (if algebraic with known structure)
    │                       │
    │                       └─→ materialize() → Complex (if needed)
    │
    └─→ materialize() → Complex (if transcendental/special function)
                            │
                            └─→ toDouble() → Double (if real)
```

### Examples

```scala
// Quadratic expression → Solution → Complex
val expr: Expression = Quadratic(a=1, b=2, c=2)  // x² + 2x + 2 = 0
val soln: Solution = expr.solve(0)  // QuadraticSolution(-1, √(-1), Plus)
val complex: Complex = soln.materialize  // Complex(-1, 1) = -1 + i

// Special function → Complex directly
val bessel: Expression = BesselJ(Number(0), Number(1))
val value: Complex = bessel.materialize  // ≈ 0.7652 (real, but returns Complex)

// Real solution → Scalar
val simple: Expression = Quadratic(a=1, b=-3, c=2)  // x² - 3x + 2 = 0
val realSoln: Solution = simple.solve(0)  // QuadraticSolution(1.5, 0.25, Plus)
val scalar: Scalar = realSoln.materialize  // 2.0 (real number)
```

## Extensibility

### Adding New Special Functions

To add a new special function (e.g., Airy function):

```scala
case class AiryAi(argument: Expression) extends ExpressionMonoFunction("Ai", ...) {
  override def materialize: Eager = {
    val x = argument.materialize.toDouble
    Scalar(numericalAiryAi(x))  // Use numerical library
  }
  
  // Optional: symbolic derivatives, simplifications, etc.
  override def derivative(variable: Variable): Expression = 
    AiryAiPrime(argument)  // Ai'(x)
}
```

No changes needed to `Solution` or `Complex` hierarchies!

### Adding New Algebraic Solutions

To add cubic solutions (if needed):

```scala
case class CubicSolution(
  base: Monotone,
  cubeRootCoeff: Monotone,
  cubeRadicand: Monotone,
  branch: CubicBranch
) extends Solution {
  override def materialize: Eager = {
    // Cardano's formula implementation
    val cubeRoot = cubeRadicand.nthRoot(3, branch.index)
    base + (cubeRootCoeff * cubeRoot)
  }
}
```

## Rationale

### Why This Separation?

1. **Solution is type-safe and structured**
    - Compiler knows about base, offset, branch
    - Pattern matching works cleanly
    - Normalization can reason about structure

2. **Complex is simple and universal**
    - Every numerical system needs complex numbers
    - Doesn't pollute with algebraic details
    - Natural result of materialization

3. **Expression is infinitely extensible**
    - Add Bessel functions without touching Solution
    - Special functions don't complicate core types
    - Symbolic manipulation independent of evaluation

4. **Clear mental model**
    - "Can I write the algebraic structure?" → Solution
    - "Do I have a concrete a + bi?" → Complex
    - "Must it stay symbolic?" → Expression

### Benefits

- **Maintainability**: Each type has a focused purpose
- **Extensibility**: Add new special functions without core changes
- **Type Safety**: Compiler catches structural mismatches
- **Performance**: Solutions avoid symbolic overhead
- **Clarity**: Users understand when values are exact vs approximate

## Migration Notes

### From Expression to Solution

When simplifying expressions, if the result is an algebraic solution with known structure, convert to `Solution`:

```scala
def simplify(expr: Expression): Valuable = expr match {
  case Quadratic(a, b, c) if canSolveExactly(a, b, c) =>
    QuadraticSolution(...)  // Promote to Solution
  case other =>
    other.normalize  // Stay as Expression
}
```

### From Solution to Complex

Solutions materialize to `Complex` when:
- The discriminant is negative (imaginary roots)
- User explicitly requests numerical value
- Passing through APIs that expect Complex

```scala
val solution: QuadraticSolution = ...
val asComplex: Complex = solution.materialize

// Or check first:
if (solution.isReal) {
  val real: Scalar = solution.materialize.asInstanceOf[Scalar]
} else {
  val complex: Complex = solution.materialize.asInstanceOf[Complex]
}
```

## Future Considerations

### Polynomial Class

A full `Polynomial` class would enable:
```scala
case class AlgebraicRoot(
  polynomial: Polynomial,  // x⁵ - x - 1 = 0
  approximation: Complex,  // Numerical root
  index: Int              // Which root
) extends Solution
```

### Symbolic Differentiation

Expressions can support automatic differentiation:
```scala
trait Expression {
  def derivative(variable: Variable): Expression
}

// Example:
val f: Expression = BesselJ(n, x)
val df: Expression = f.derivative(x)  // ∂J_n/∂x
```

### Integration with Computer Algebra Systems

Future integration with systems like SymPy or Mathematica for:
- Symbolic integration
- Series expansions
- Exact simplification of special functions

## References

- **Abel-Ruffini Theorem**: No general algebraic solution for degree ≥ 5 polynomials
- **Cardano's Formula**: Cubic equation solution
- **Special Functions**: NIST Digital Library of Mathematical Functions
- **Complex Analysis**: Branch cuts, Riemann surfaces for multi-valued functions

---

*Document Version: 1.0*  
*Last Updated: 2025-01-28*  
*Library Version: 1.6.2*