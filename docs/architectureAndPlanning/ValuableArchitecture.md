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
## Key Architectural Points

### Expression vs Solution

**Expression** (symbolic, lazy):
- Has `materialize: Eager` method
- Root is an Expression
- Represents unevaluated mathematical expressions

**Solution** (eager, already evaluated):
- NO `materialize` method - it's already eager!
- Represents algebraic structure of equation solutions
- QuadraticSolution, LinearSolution, Complex all extend Solution

### The Root → Solution Relationship

```scala
// Root is an Expression (symbolic)
sealed trait Root extends AtomicExpression {
  def equation: Equation
  def branch: Int
  
  // Computed lazily, but returns eager Solution
  lazy val solution: Solution = equation.solve(branch)
}

// Solution is Eager (already evaluated)
trait Solution extends Eager {
  def toMonotone: Option[Monotone]  // Conversion
  def toComplex: Complex             // Conversion (for QuadraticSolution)
  def conjugate: Solution
  // NO materialize method!
}
```

**Key insight:** When you access `root.solution`, the lazy val is evaluated once and caches the Solution. The Solution itself is already eager - it doesn't need further materialization.
```

## Solution: Algebraic Structure

### Purpose

`Solution` represents solutions to polynomial equations where the algebraic structure is known and can be represented explicitly. The key characteristic is that terms cannot always be combined into a single closed form.

### Examples

**QuadraticSolution**: x = base ± √offset (real) or x = base ± i√|offset| (complex)
```scala
case class QuadraticSolution(
                              base: Monotone,      // The rational part: -b/2a
                              offset: Monotone,    // The radical part: |Δ|/4a²
                              coefficient: Int,    // Which root: +1 or -1 (the ± sign)
                              imaginary: Boolean   // false for real solutions, true for complex
                            ) extends Solution
```

For the quadratic equation ax² + bx + c = 0:
- `base` = -b/2a (the rational part)
- `offset` = |b² - 4ac|/4a² (absolute value of discriminant / 4a²)
- `coefficient` = +1 or -1 (which of the two roots)
- `imaginary` = true when discriminant < 0 (complex roots)
- Two solutions differ by the `coefficient` (±1)

**LinearSolution**: x = value
```scala
case class LinearSolution(value: Monotone) extends Solution
```

### Why Solution Can't Be Combined

Consider: x = 3 + √2
- The `3` is rational (base)
- The `√2` is irrational (offset with coefficient = +1)
- They cannot be combined into a single `Monotone`
- But we know the exact algebraic structure

For complex solutions: x = 1 + i√2
- The `1` is the real part (base)
- The `√2` is what's under the imaginary radical (offset with imaginary = true)
- `coefficient` = +1 gives 1 + i√2, coefficient = -1 gives 1 - i√2
- These are complex conjugate pairs

The key insight: **algebraic structure is preserved** - we know it's "base ± [i]√offset" even though we can't simplify it to a single number.

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

`Complex` represents **any** complex number, whether it comes from an algebraic solution, a transcendental function, or direct construction. Complex numbers extend the real numbers by including the imaginary unit i (where i² = -1).

### Current Design: Wrapper Pattern (Recommended)

The algebra module wraps `core.numerical.Complex` to integrate it with the `Solution` interface:

```scala
// In algebra.eager
case class Complex(
  complex: numerical.Complex  // Wraps core.numerical.Complex
)(val maybeName: Option[String] = None) extends Solution {
  
  // Delegates to wrapped complex with inline for performance
  @inline def modulus: Number = complex.modulus
  @inline def argument: Number = complex.argument
  @inline def real: Number = complex.real
  @inline def imaginary: Number = complex.imag
  
  // Solution interface implementations
  def conjugate: Solution = copy(complex = complex.conjugate)(None)
  def toMonotone: Option[Monotone] = 
    if (complex.isReal) complex.asReal.map(Eager(_).asMonotone) else None
  def +(other: Solution): Solution = other match {
    case Complex(c) => Complex(complex + c)
    case _ => throw AlgebraException(...)
  }
  def scale(r: Rational): Solution = Complex(complex.scale(r))
  def negate: Solution = Complex(complex.negate)
}
```

**Why the wrapper pattern?**
- **Module dependencies**: `algebra` depends on `core` (for Rational, Factor, Context, etc.)
- **Circular dependencies forbidden**: Can't have `core` also depend on `algebra`
- **Core owns fundamentals**: `Complex` is a fundamental numerical type, belongs in core
- **Minimal overhead**: JVM inlining makes delegation virtually free
- **Clear separation**: Core defines types, algebra extends them with algebraic interfaces

### Underlying Type: numerical.Complex

The wrapped type in `core.numerical` has two representations:

```scala
// In core.numerical
sealed trait Complex {
  def real: Number
  def imag: Number
  def modulus: Number
  def argument: Number
  def conjugate: Complex
  // ... other operations
}

case class ComplexCartesian(real: Number, imag: Number) extends Complex
case class ComplexPolar(radius: Number, angle: Number, branch: Int = 0) extends Complex
```

### Why Not Eliminate the Wrapper?

Several architectural constraints make the wrapper pattern the best choice:

1. **No circular dependencies**: Scala/JVM prohibits `algebra ↔ core` mutual dependencies
2. **Core independence**: Core should remain independent of algebra (it's lower-level)
3. **Shared types**: Core provides fundamental types (Rational, Complex) used by multiple modules
4. **Performance**: Modern JVM inlining makes wrapper overhead negligible
5. **Clarity**: Separation between numerical types (core) and algebraic structures (algebra) is clean

### Relationship to QuadraticSolution

When a quadratic has complex roots (discriminant < 0), the `QuadraticSolution` can be converted to `Complex`:

```scala
// For x² + 1 = 0:
val solution: QuadraticSolution = QuadraticSolution(
  base = Scalar(0),       // Real part is 0
  offset = Scalar(1),     // |Δ|/4a² = 1
  coefficient = 1,        // The + root
  imaginary = true        // Complex solution
)

// Convert to Complex:
val asComplex: Complex = solution.toComplex
// → ComplexCartesian(0, 1) = i

// The conjugate:
val conjugateSolution = solution.conjugate  // coefficient = -1
val conjugateComplex = conjugateSolution.toComplex
// → ComplexCartesian(0, -1) = -i
```

### Integration with QuadraticSolution

```scala
// From algebraic solution with complex roots
case class QuadraticSolution(..., imaginary: Boolean) extends Algebraic {

  // NO materialize method - QuadraticSolution is already Eager!

  def toComplex: Complex = {
    val radical = offset.sqrt
    val term = radical.scale(coefficient)

    if (imaginary) {
      // Complex roots: base ± i√offset
      ComplexCartesian(base.toNumber, term.toNumber)()
    } else {
      // Real roots converted to complex: (base ± √offset) + 0i
      val realPart = base + term
      ComplexCartesian(realPart.toNumber, Number.zero)()
    }
  }

  def toMonotone: Option[Monotone] = {
    if (!imaginary && (offset.isZero || base.isZero)) {
      Some((base + offset.sqrt.scale(coefficient)).normalize)
    } else {
      None
    }
  }
}
```

### Examples

```scala
// From algebraic solution with complex roots
val quadratic: QuadraticSolution = QuadraticSolution(
  base = Scalar(1),
  offset = Scalar(1),
  coefficient = 1,
  imaginary = true        // Complex solution
)  // Represents: 1 + i

val asComplex: Complex = quadratic.materialize
// → ComplexCartesian(1, 1) = 1 + i

val conjugate: QuadraticSolution = quadratic.conjugate  // coefficient = -1
val conjugateComplex: Complex = conjugate.materialize
// → ComplexCartesian(1, -1) = 1 - i

// Direct complex construction
val eulerIdentity: Complex = ComplexPolar(Number.one, Angle.pi, 0)  // e^(iπ) = -1

// From expression evaluation
val transcendental: Expression = Exp(Multiply(I, Pi))
val evaluated: Complex = transcendental.materialize.asInstanceOf[Complex]
```

## Complex vs QuadraticSolution: When to Use Each

### QuadraticSolution (Algebraic Structure Preserved)

Use `QuadraticSolution` when:
- ✅ You're solving a quadratic equation
- ✅ You need to preserve the exact algebraic form: base ± √offset or base ± i√offset
- ✅ You want to perform algebraic operations that respect the structure
- ✅ You need the conjugate (just flip coefficient)

```scala
// Solving x² - 2 = 0, solutions are ±√2
val plus: QuadraticSolution = QuadraticSolution(
  base = Scalar(0),
  offset = Scalar(2),
  coefficient = 1,
  imaginary = false
)  // Represents: +√2 (exactly)

val minus = plus.conjugate  // Represents: -√2 (exactly)

// Algebraic operations preserve exactness
val doubled = plus.scale(Rational(2))  // 2√2 (still exact)
```

### Complex (Numerical/General Form)

Use `Complex` when:
- ✅ You need a general complex number (not necessarily from quadratic)
- ✅ You're doing transcendental computations (e^(iθ), sin + i·cos)
- ✅ You want Cartesian (a + bi) or Polar (r·e^(iθ)) form
- ✅ Algebraic structure is not important

```scala
// General complex number
val z: Complex = ComplexCartesian(3, 4)()  // 3 + 4i

// From transcendental
val euler: Complex = ComplexPolar(1, Angle(theta))()  // e^(iθ)

// Numerical operations
val magnitude = z.modulus  // 5
val angle = z.argument     // atan2(4, 3)
```

### Conversion Between Them

```scala
// QuadraticSolution → Complex (convert via toComplex)
val quad: QuadraticSolution = QuadraticSolution(
  Scalar(2), Scalar(3), 1, imaginary = true
)  // 2 + i√3

val complex: Complex = quad.toComplex
// → ComplexCartesian(2, √3)

// Complex → QuadraticSolution (rare, only for specific forms)
// Not generally possible - Complex loses algebraic structure
```

### Real-World Example: Solving Quadratics

```scala
// Solve: x² + 2x + 5 = 0
// Discriminant: 4 - 20 = -16 < 0 (complex roots)
// x = (-2 ± √(-16))/2 = -1 ± 2i

val solution1: QuadraticSolution = QuadraticSolution(
  base = Scalar(-1),
  offset = Scalar(4),    // |Δ|/4a² = 16/4 = 4
  coefficient = 1,
  imaginary = true
)  // -1 + 2i (exact algebraic form)

val solution2 = solution1.conjugate  // -1 - 2i (exact)

// For numerical work, convert to Complex:
val z1: Complex = solution1.toComplex  // ComplexCartesian(-1, 2)
val z2: Complex = solution2.toComplex  // ComplexCartesian(-1, -2)

// Verify: both should satisfy the original equation
// (z1)² + 2(z1) + 5 = 0 ✓
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

# Polynomial and Series Architecture Addendum

This content should be inserted into ValueableArchitecture.md after the "Expression: Symbolic Representation" section and before "Design Boundaries".

---

## Polynomial: Function Representation

### Purpose

`Polynomial[X]` represents polynomial functions - mathematical expressions of the form:

```
p(x) = a₀ + a₁x + a₂x² + ... + aₙxⁿ
```

Polynomials are **function types** (extending `X => X`), not `Valuable` - they transform inputs to outputs rather than representing values themselves.

### Design

Located in `core.numerical`, polynomials are fundamental mathematical structures:

```scala
trait Polynomial[X] extends (X => X) {
  def numeric: Numeric[X]
  def degree: Int
  def coefficients: Seq[X]
  
  // Evaluation
  def apply(x: X): X
  
  // Calculus
  def derivative: Polynomial[X]
  def derivativeN(n: Int): Polynomial[X]
  def nthDerivative(n: Int, x: X): X
  
  // Construction
  def unit(degree: Int, coefficients: Seq[X])(implicit xn: Numeric[X]): Polynomial[X]
}

// Concrete implementations
case class NumberPolynomial(
  degree: Int,
  coefficients: Seq[Number]
)(implicit ev: Numeric[Number]) extends Polynomial[Number]

case class RationalPolynomial(
  degree: Int,
  coefficients: Seq[Rational]
)(implicit ev: Numeric[Rational]) extends Polynomial[Rational]
```

### Key Operations

**Evaluation**: Efficient computation using implicit Numeric operations
```scala
val p = NumberPolynomial(2, 3, 1)  // 1·x² + 3·x + 2
val result = p(Number(5))  // 42
```

**Differentiation**: Automatic derivative computation
```scala
val p = NumberPolynomial(0, 0, 1, 1)  // x³ + x²
val dp = p.derivative  // 3x² + 2x
val d2p = p.derivativeN(2)  // 6x + 2
val value = p.nthDerivative(2, Number(1))  // 8
```

**Factory methods**:
```scala
// Variable number of coefficients
NumberPolynomial(1, 2, 3)  // 3x² + 2x + 1
RationalPolynomial(Rational(1,2), Rational(3,4))  // (3/4)x + (1/2)
```

### Relationship to Solution: AlgebraicRoot

For polynomials of degree ≥ 5, the Abel-Ruffini theorem tells us there's no general radical formula. We use `AlgebraicRoot`:

```scala
case class AlgebraicRoot(
  polynomial: Polynomial[Number],
  numericalApproximation: Complex,
  rootIndex: Int  // Which root (1st, 2nd, 3rd...)
) extends Solution {
  
  def toMonotone: Option[Monotone] = None  // Can't simplify to closed form
  
  def conjugate: Solution = 
    // Find conjugate root of the same polynomial
    AlgebraicRoot(polynomial, numericalApproximation.conjugate, conjugateIndex)
  
  // Verify this is actually a root
  def verify(tolerance: Double): Boolean = 
    polynomial(numericalApproximation.toNumber).abs < tolerance
  
  def scale(r: Rational): Solution = {
    // Scaling a root: if p(α) = 0, then p(α/r)·rⁿ = 0
    val scaledApprox = numericalApproximation.scale(r)
    AlgebraicRoot(polynomial, scaledApprox, rootIndex)
  }
}
```

**Example**: Solving quintic equations
```scala
// x⁵ - x - 1 = 0 (no radical formula exists by Abel-Ruffini theorem)
val quintic = NumberPolynomial(-1, -1, 0, 0, 0, 1)

// Find numerical root using Newton's method or similar
val numericalRoot: Complex = findRootNumerically(quintic)  // ≈ 1.1673...

// Represent as AlgebraicRoot
val solution: AlgebraicRoot = AlgebraicRoot(
  quintic,
  numericalRoot,
  rootIndex = 1
)

// This is a Solution with exact algebraic definition
solution.isExact  // true - defined exactly by polynomial
solution.toMonotone  // None - can't express in radicals
solution.verify(1e-10)  // true - it's actually a root

// Can still do algebraic operations
val doubled = solution.scale(Rational(2))  // 2α where α is the root
```

### Why Polynomial is Not Valuable

Polynomials are **functions**, not **values**:
- `Polynomial[X]` is `X => X` (function type)
- `Valuable` represents mathematical values
- A polynomial evaluated at a point produces a value: `p(3)` → `Number`

However, polynomials are **used by** Valuable types:
- `AlgebraicRoot` stores a `Polynomial` to define which number it represents
- `TaylorSeries` uses polynomial approximations
- Expression simplification uses polynomial algebra

**Relationship diagram**:
```
Polynomial[X] (function)
    ↓ apply(x)
X (value - could be Number, Rational, etc.)
    ↓ wrap
Valuable (Number, Scalar, etc.)

But also:
Polynomial[X] (stored in AlgebraicRoot)
    → defines which algebraic number
    → AlgebraicRoot extends Solution extends Valuable
```

## Series: Infinite and Finite Sequences

### Purpose

`Series[X]` represents mathematical series - sums of sequences of terms:

```
S = a₀ + a₁ + a₂ + ... + aₙ [+ ...]
```

Series are **evaluated values** that can become Eager values when their sum is computed.

### Design

Located in `core.numerical`:

```scala
trait Series[X] {
  def terms: Seq[X]  // May be List or LazyList
  def nTerms: Option[Int]  // Some(n) for finite, None for infinite
  def term(n: Int): Option[X]
  
  // Evaluation methods
  def evaluate(maybeN: Option[Int]): Option[X]
  def evaluateToTolerance(epsilon: Double): Try[X]
  def convergenceRate: Double
  
  def render(n: Int): String
}

// Finite series - exact sums
case class FiniteSeries[X: Numeric](terms: Seq[X]) extends AbstractSeries[X] {
  def nTerms: Option[Int] = Some(terms.length)
  val convergenceRate: Double = 1.0
}

// Infinite series - lazy evaluation with convergence
case class InfiniteSeries[X: Numeric](
  terms: LazyList[X],
  convergenceRate: Double
) extends AbstractInfiniteSeries[X] {
  def nTerms: Option[Int] = None
}
```

### Key Features

**1. Lazy evaluation**: Infinite series compute terms on demand
```scala
// Geometric series: 1 + 1/2 + 1/4 + 1/8 + ...
val geometric: InfiniteSeries[Number] = InfiniteSeries(
  LazyList.iterate(Number.one)(x => x / 2),
  convergenceRate = 2.0
)

val sum = geometric.evaluateToTolerance(0.001)  // Success(~2.0)
```

**2. Convergence tracking**: Series maintain convergence rate for error bounds
```scala
// Error bounds = tolerance / convergenceRate
val result: Try[Fuzz[Double]] = series.evaluateToTolerance(epsilon)
// Result includes uncertainty: ±(epsilon / convergenceRate)
```

**3. Finite series**: Exact sums with known number of terms
```scala
val finite = FiniteSeries(Seq(
  Number(1), Number(2), Number(3), Number(4), Number(5)
))
val sum = finite.evaluate(None)  // Some(15)
```

**4. Partial evaluation**: Sum first n terms
```scala
val infinite = InfiniteSeries(terms, convergence = 1.5)
val partial = infinite.evaluate(Some(10))  // Sum of first 10 terms
val full = infinite.evaluateToTolerance(0.01)  // Sum until convergence
```

### Series as Eager Values

Once evaluated, a series becomes a concrete value:

```scala
val series: InfiniteSeries[Number] = InfiniteSeries(terms, 2.0)

// Evaluation produces Number (which is Eager)
val value: Try[Number] = series.evaluateToTolerance(0.001)
value match {
  case Success(n) => 
    // n is a Number with fuzziness tracking error
    val eager: Eager = Scalar(n)
  case Failure(e) => 
    // Series didn't converge
}
```

**Integration with Valuable hierarchy**:
```
Series[X] (has evaluation methods)
    ↓ evaluateToTolerance
Try[X] (where X is often Number)
    ↓ Success(number)
Number → Scalar → Structure → Eager → Valuable
```

## PowerSeries: Symbolic Series Generators

### Purpose

`PowerSeries[X, Y]` is a **function** that generates `Series[Y]` when given input `X`:

```scala
trait PowerSeries[X, Y] extends (X => Series[Y]) {
  def apply(x: X): Series[Y]
}
```

PowerSeries are **symbolic/lazy** - they don't evaluate until you give them an input value.

### Types of PowerSeries

**1. LazyPowerSeries**: Infinite coefficients, lazy evaluation
```scala
abstract class LazyPowerSeries[X: Numeric, Y: Numeric](
  coefficients: LazyList[Y]
)(f: X => Y) extends PowerSeries[X, Y] {
  
  def apply(x: X): Series[Y] = {
    val powers: LazyList[X] = LazyList.iterate(one)(z => z * x)
    val terms: LazyList[Y] = (powers zip coefficients).map {
      case (p, c) => c * f(p)
    }
    InfiniteSeries(terms, convergence)
  }
}
```

**2. FinitePowerSeries**: Polynomials as power series
```scala
case class FinitePowerSeries[X: Numeric, Y: Numeric](
  coefficients: Seq[Y]
)(f: X => Y) extends PowerSeries[X, Y] {
  
  def apply(x: X): Series[Y] = {
    val powers: LazyList[X] = LazyList.iterate(one)(z => z * x)
    val terms: Seq[Y] = (powers zip coefficients).map {
      case (p, c) => c * f(p)
    }.toList
    FiniteSeries(terms)
  }
}
```

**3. TaylorSeries**: Function approximation around a point
```scala
case class TaylorSeries(
  point: Number,
  startFunction: SeriesFunction,
  derivative: SeriesFunction => SeriesFunction,
  convergence: Double
) extends PowerSeries[Number, Number] {
  
  // Generate coefficients from derivatives at point
  lazy val functions: LazyList[SeriesFunction] =
    LazyList.iterate(startFunction)(derivative)
  
  lazy val coefficients: LazyList[Number] = 
    functions.map(f => f(point))
  
  lazy val terms: LazyList[Number] = 
    coefficients.zipWithIndex.map {
      case (c, i) => c / Factorial(i)
    }
  
  def apply(x: Number): Series[Number] = {
    val xs = LazyList.iterate(Number.one)(_ * x)
    val seriesTerms = (terms zip xs).map { case (a, b) => a * b }
    InfiniteSeries(seriesTerms, convergence)
  }
}
```

### TaylorSeries Example: Sine Function

```scala
// Create Taylor series for sin(x) around x = 0
val sineSeries: TaylorSeries = TaylorSeries.createSine(Number.zero)

// Evaluate at x = π/6
val series: Series[Number] = sineSeries(Number.pi / 6)

// Get approximate value
val approx: Number = series.evaluateToTolerance(0.001).get  // ≈ 0.5

// Or evaluate first n terms
val partial: Number = series.evaluate(Some(5)).get
```

**How it works**:
```
TaylorSeries(sin, point=0)
    → coefficients: [sin(0), cos(0), -sin(0), -cos(0), ...]
                  = [0, 1, 0, -1, 0, 1, ...]
    → divided by factorials: [0, 1, 0, -1/6, 0, 1/120, ...]
    
apply(π/6):
    → powers of π/6: [1, π/6, (π/6)², (π/6)³, ...]
    → multiply: [0, π/6, 0, -(π/6)³/6, 0, (π/6)⁵/120, ...]
    → sum: π/6 - (π/6)³/6 + (π/6)⁵/120 - ... ≈ 0.5
```

### Use Cases for PowerSeries

**1. Function Approximation**

Approximate smooth functions using Taylor series:
```scala
val sineSeries = TaylorSeries.createSine(Number.zero)
val cosineSeries = TaylorSeries.createCosine(Number.zero)

// Evaluate at any point
val sin30 = sineSeries(Number.pi / 6).evaluateToTolerance(0.001)
```

**2. Special Functions in Expressions**

Special functions use series for evaluation:
```scala
case class BesselJ(order: Expression, argument: Expression) 
  extends ExpressionMonoFunction {
  
  override def materialize: Eager = {
    val n = order.materialize.toDouble
    val x = argument.materialize.toNumber
    
    // Create power series for Bessel function
    val series = createBesselSeries(n, x)
    
    // Evaluate to get Number
    val value: Number = series.evaluateToTolerance(0.001).get
    Scalar(value)
  }
  
  private def createBesselSeries(n: Double, x: Number): InfiniteSeries[Number] = {
    // Bessel series: J_n(x) = Σ ((-1)^k / (k! Γ(n+k+1))) (x/2)^(n+2k)
    val terms: LazyList[Number] = LazyList.from(0).map { k =>
      val sign = if (k % 2 == 0) 1 else -1
      val coeff = sign / (factorial(k) * gamma(n + k + 1))
      coeff * math.pow(x.toDouble / 2, n + 2*k)
    }
    InfiniteSeries(terms, convergenceRate = 2.0)
  }
}
```

**3. Symbolic Computation**

Series expansions remain symbolic until needed:
```scala
// Expression that represents a Taylor expansion
case class SeriesExpansion(
  function: Expression,
  point: Expression,
  order: Int
) extends Expression {
  
  def materialize: Eager = {
    // Generate symbolic TaylorSeries
    val taylorSeries = generateTaylorSeries(function, point)
    
    // Evaluate at the expansion point
    val series: Series[Number] = taylorSeries(point.materialize.toNumber)
    
    // Sum terms up to specified order
    val sum: Number = series.evaluate(Some(order))
      .getOrElse(throw AlgebraException("Series expansion failed"))
    
    Scalar(sum)
  }
}
```

### Series in the Type Hierarchy

PowerSeries and Series occupy different positions:

```
PowerSeries[X, Y] (function: X → Series[Y])
    ↓ apply(x)
Series[Y] (lazy sequence with evaluation rules)
    ↓ evaluateToTolerance
Y (concrete value - typically Number)
    ↓ wrap
Scalar → Structure → Eager → Valuable
```

**Flow example**:
1. `TaylorSeries(sin, 0)` is a `PowerSeries[Number, Number]`
2. `sineSeries(π/6)` generates `InfiniteSeries[Number]`
3. `series.evaluateToTolerance(0.001)` computes sum → `Try[Number]`
4. `Success(number)` where `number: Number` with fuzziness
5. Wrap in `Scalar(number)` → `Eager` → `Valuable`

## Polynomial, Series, and PowerSeries Summary

| Type | Category | Purpose | Produces | Location |
|------|----------|---------|----------|----------|
| `Polynomial[X]` | Function | Evaluate polynomial at x | X | `core.numerical` |
| `PowerSeries[X,Y]` | Function Generator | Generate Series[Y] from x | Series[Y] | `core.numerical` |
| `Series[X]` | Lazy Sequence | Sum of terms | X (when evaluated) | `core.numerical` |
| `TaylorSeries` | PowerSeries | Approximate functions | Series[Number] | `core.numerical` |
| `AlgebraicRoot` | Solution | Root of polynomial | Eager value | `algebra.eager` |

### Key Relationships

**Polynomial → AlgebraicRoot**:
```scala
val quintic: Polynomial[Number] = NumberPolynomial(-1, -1, 0, 0, 0, 1)
val root: AlgebraicRoot = AlgebraicRoot(quintic, approximation, 1)
// Polynomial defines the algebraic number
```

**PowerSeries → Series → Number**:
```scala
val taylor: TaylorSeries = TaylorSeries.createSine(0)
val series: Series[Number] = taylor(x)
val value: Number = series.evaluateToTolerance(0.001).get
// Pipeline from symbolic to concrete
```

**Series → Eager**:
```scala
val series: InfiniteSeries[Number] = InfiniteSeries(terms, convergence)
val number: Number = series.evaluateToTolerance(epsilon).get
val scalar: Scalar = Scalar(number)  // Now it's Eager
```

**Expression → PowerSeries → Series**:
```scala
// Special functions can use series internally
case class Erf(argument: Expression) extends ExpressionMonoFunction {
  def materialize: Eager = {
    val x = argument.materialize.toNumber
    val series = createErfSeries(x)  // InfiniteSeries
    val value = series.evaluateToTolerance(0.001).get
    Scalar(value)
  }
}
```

### When to Use Each

**Use Polynomial when**:
- ✅ You need to evaluate p(x) for various x
- ✅ You need derivatives of a function
- ✅ You're defining an algebraic number (AlgebraicRoot)
- ✅ You have explicit coefficients

**Use Series when**:
- ✅ You have a sequence of terms to sum
- ✅ You need lazy evaluation (infinite series)
- ✅ You're working with convergent sequences
- ✅ You need error bounds on sums

**Use PowerSeries when**:
- ✅ You need to generate different series for different x values
- ✅ You're approximating a function (TaylorSeries)
- ✅ You want symbolic series that evaluate on demand
- ✅ You need series expansion of special functions

---

*This completes the Polynomial and Series addendum. Insert this content before the "Design Boundaries" section in the main ValueableArchitecture.md document.*

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

## Expression to Solution Flow
```
Root (Expression, symbolic)
    │
    └─→ lazy val solution: Solution (computed on first access)
            │
            ├─→ QuadraticSolution(imaginary=false) → real roots
            │       │
            │       ├─→ base ± √offset (exact algebraic form)
            │       └─→ toMonotone: Option[Monotone] (if offset is zero)
            │
            ├─→ QuadraticSolution(imaginary=true) → complex roots
            │       │
            │       ├─→ base ± i√offset (exact algebraic form)
            │       └─→ toComplex: Complex (for numerical work)
            │
            └─→ LinearSolution → always real
                    └─→ value: Monotone

Other Expression types
    │
    └─→ materialize: Eager (evaluates to concrete value)
            │
            ├─→ Number, Scalar (real values)
            ├─→ Complex (transcendental/special functions)
            └─→ Solution (from solving equations)

Note: Only Expression has materialize - Solution is already Eager!
```

### Examples

```scala
// Root (Expression) containing real solution
val realRoot: Root = QuadraticRoot(QuadraticEquation(-3, 2), 0)  // x² - 3x + 2 = 0
val realSolution: QuadraticSolution = realRoot.solution  // Lazy evaluation
// QuadraticSolution(1.5, 0.25, 1, imaginary=false)
val realValue: Monotone = realSolution.toMonotone.get  // 2.0

// Root (Expression) containing complex solution  
val complexRoot: Root = QuadraticRoot(QuadraticEquation(0, 1), 0)  // x² + 1 = 0
val complexSolution: QuadraticSolution = complexRoot.solution
// QuadraticSolution(0, 1, 1, imaginary=true)
val complexValue: Complex = complexSolution.toComplex  // ComplexCartesian(0, 1) = i

// Other Expression types materialize
val bessel: Expression = BesselJ(Number(0), Number(1))
val besselValue: Eager = bessel.materialize  // Returns Number or Complex

// Transcendental expression
val euler: Expression = Exp(Multiply(I, Pi))
val eulerValue: Eager = euler.materialize  // Returns Complex(-1, 0) = -1
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
    val discriminant = b*b - 4*a*c
    val base = -b / (2*a)
    val offset = discriminant.abs / (4*a*a)
    val coefficient = 1  // Plus root by default
    val imaginary = discriminant < 0
    
    QuadraticSolution(base, offset, coefficient, imaginary)
    
  case other =>
    other.normalize  // Stay as Expression
}
```

### From QuadraticSolution to Complex

Solutions with imaginary roots can be converted to `Complex`:

```scala
val solution: QuadraticSolution = QuadraticSolution(
  base = Scalar(2),
  offset = Scalar(3),
  coefficient = 1,
  imaginary = true
)  // Represents: 2 + i√3

// Convert to Complex for numerical work
val asComplex: Complex = solution.toComplex
// → ComplexCartesian(2, √3)

// Or check before converting:
if (solution.imaginary) {
  val complex: Complex = solution.toComplex
  // Work with complex number
} else {
  val real: Option[Monotone] = solution.toMonotone
  // Work with real number
}
```

### Wrapper Elimination Strategy

To eliminate the `Complex` wrapper:

1. **Phase 1**: Create native `ComplexCartesian` and `ComplexPolar` in algebra module
2. **Phase 2**: Update all `QuadraticSolution.toComplex` conversions to use new types
3. **Phase 3**: Replace `Complex(numerical.Complex)` wrapper with direct types
4. **Phase 4**: Add type aliases in core.numerical for backward compatibility
5. **Phase 5**: Migrate core module code to use algebra types

```scala
// Before (wrapper):
case class Complex(complex: numerical.Complex) extends Solution

// After (native):
sealed trait Complex extends Solution
case class ComplexCartesian(real: Number, imaginary: Number) extends Complex
case class ComplexPolar(radius: Number, angle: Angle) extends Complex
```

## Why the Wrapper Pattern is Optimal

### Architectural Constraints

The Number library has a clear module dependency hierarchy:

```
core (fundamental types: Rational, Factor, Context, Value, Number, Complex)
  ↑
algebra (algebraic structures: Solution, Algebraic, extending core types)
  ↑
expression (symbolic expressions, lazy evaluation)
  ↑
parse (LaTeX parsing, string interpolators)
  ↑
top (top-level API, integrations)
```

**Key principle**: Dependencies flow upward. Lower modules don't depend on higher ones.

### Why Circular Dependencies Are Impossible

Scala/JVM doesn't allow circular module dependencies. If `algebra` depends on `core`, then `core` cannot depend on `algebra`. This is enforced at compile time and is actually a **good architectural constraint** that prevents tangled dependencies.

### Why Complex Belongs in Core

`numerical.Complex` is a **fundamental numerical type**, like `Rational` or `Number`:
- Used by multiple modules (core, algebra, expression)
- Represents basic mathematical concept (complex numbers)
- No dependencies on algebraic structures like `Solution` or `Algebraic`
- Belongs at the foundation with other numerical primitives

### Why the Wrapper is Minimal Overhead

Modern JVM optimizations make the wrapper virtually free:

```scala
case class Complex(complex: numerical.Complex) extends Solution {
  @inline def modulus: Number = complex.modulus
  @inline def argument: Number = complex.argument
  // JIT compiler will inline these calls, eliminating indirection
}
```

Performance characteristics:
- **Inlining**: JIT removes method call overhead
- **Object allocation**: Wrapper object is short-lived, GC handles efficiently
- **Memory**: Single pointer overhead per instance
- **Runtime cost**: Negligible in practice

### Alternative Approaches (and Why They Don't Work)

#### ❌ Option 1: Move Complex to Algebra
**Problem**: Core module needs Complex for its own operations. Moving it to algebra would require `core → algebra` dependency, creating a cycle.

```
core → algebra (for Complex)
algebra → core (for Rational, Factor, etc.)
❌ Circular dependency - won't compile!
```

#### ❌ Option 2: Create "Common" Module
**Problem**: Adds architectural complexity for minimal gain.

```
common (Rational, Factor, Complex, ...)
  ↑
  ├── core
  └── algebra
```

This just moves the problem - now you have another module to maintain, and it's unclear what belongs in "common" vs "core".

#### ❌ Option 3: Duplicate Complex
**Problem**: Two implementations means code duplication, synchronization issues, and confusion.

```scala
core.numerical.Complex      // Original
algebra.eager.Complex       // Duplicate
// Which one is authoritative? How do they stay in sync?
```

### The Wrapper Pattern Benefits

✅ **Clean architecture**: Maintains unidirectional dependencies  
✅ **Type safety**: `algebra.eager.Complex` is a `Solution`, `numerical.Complex` is not  
✅ **Flexibility**: Algebra can add Solution-specific methods without modifying core  
✅ **Performance**: JVM inlining eliminates overhead  
✅ **Clarity**: Clear ownership - core owns numerical types, algebra owns algebraic interfaces

### Making the Wrapper Efficient

To ensure minimal overhead:

```scala
case class Complex(complex: numerical.Complex) extends Solution {
  // Use @inline for delegation methods
  @inline def modulus: Number = complex.modulus
  @inline def argument: Number = complex.argument
  @inline def real: Number = complex.real
  @inline def imaginary: Number = complex.imag
  
  // Minimize object allocations
  def conjugate: Solution = {
    // Reuse existing complex conjugate method
    copy(complex = complex.conjugate)(None)
  }
  
  // Delegate algebraic operations to underlying complex
  def +(other: Solution): Solution = other match {
    case Complex(c) => 
      Complex((complex + c).asInstanceOf[numerical.Complex])()
    case _ => 
      throw AlgebraException(s"Complex.+: unsupported $other")
  }
}

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

*Document Version: 3.2*  
*Last Updated: 2025-01-28*  
*Library Version: 1.6.2*

## Changelog

**Version 3.2** (2025-01-28):
- **Critical fix**: Corrected Expression → Solution flow
- Removed incorrect `materialize` method from Solution
- Clarified Root contains `lazy val solution: Solution`
- Updated all examples to show `toComplex` instead of `materialize`
- Added section distinguishing Expression (has materialize) from Solution (already eager)
- Fixed flow diagrams to show actual architecture

**Version 3.1** (2025-01-28):
- Added comprehensive Polynomial section
- Added comprehensive Series and PowerSeries sections
- Documented AlgebraicRoot for degree ≥ 5 polynomials
- Explained integration with TaylorSeries
- Added decision matrices for when to use each type
- Updated type hierarchy diagram to include Series/PowerSeries

**Version 3.0** (2025-01-28):
- **Major revision**: Wrapper pattern is now the recommended approach
- Added architectural analysis of module dependencies
- Explained why circular dependencies are impossible
- Documented why Complex belongs in core module
- Added performance analysis of wrapper overhead (@inline)
- Removed "eliminating wrapper" as a goal
- Clarified that wrapper is optimal given constraints
- Updated all sections to reflect this architectural decision

**Version 2.0** (2025-01-28):
- Corrected QuadraticSolution structure: `imaginary: Boolean` instead of `conjugate: Boolean`
- Added comprehensive section on eliminating Complex wrapper
- Clarified Complex vs QuadraticSolution usage patterns
- Added native Complex implementation design
- Updated all examples with correct parameter names
- Added materialization flow diagrams
- Expanded migration strategy

**Version 1.0** (2025-01-28):
- Initial architecture document
- Defined Solution, Expression, and Complex separation
- Established design boundaries and extensibility patterns