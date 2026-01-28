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

When a quadratic has complex roots (discriminant < 0), the `QuadraticSolution` can materialize to `Complex`:

```scala
// For x² + 1 = 0:
val solution: QuadraticSolution = QuadraticSolution(
  base = Scalar(0),       // Real part is 0
  offset = Scalar(1),     // |Δ|/4a² = 1
  coefficient = 1,        // The + root
  imaginary = true        // Complex solution
)

// Materialize to Complex:
val asComplex: Complex = solution.materialize
// → ComplexCartesian(0, 1) = i

// The conjugate:
val conjugateSolution = solution.conjugate  // coefficient = -1
val conjugateComplex = conjugateSolution.materialize
// → ComplexCartesian(0, -1) = -i
```

### Integration with QuadraticSolution

```scala
case class QuadraticSolution(..., imaginary: Boolean) extends Algebraic {
  
  def materialize: Eager = {
    val radical = offset.sqrt  // √|offset|
    val term = radical.scale(coefficient)  // ±√|offset|
    
    if (imaginary) {
      // Complex result: base ± i√offset
      ComplexCartesian(base, term)()
    } else {
      // Real result: base ± √offset
      (base + term).normalize
    }
  }
  
  def toComplex: Complex = {
    if (imaginary) {
      ComplexCartesian(base, offset.sqrt.scale(coefficient))()
    } else {
      ComplexCartesian(materialize.asInstanceOf[Monotone], Number.zero)()
    }
  }
}

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
// QuadraticSolution → Complex (materialize)
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

// For numerical work, materialize to Complex:
val z1: Complex = solution1.materialize  // ComplexCartesian(-1, 2)
val z2: Complex = solution2.materialize  // ComplexCartesian(-1, -2)

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
    ├─→ solve/simplify → QuadraticSolution (if quadratic with known structure)
    │                       │
    │                       ├─→ toComplex() → Complex (if imaginary = true)
    │                       │
    │                       └─→ materialize() → Monotone (if imaginary = false)
    │
    └─→ materialize() → Complex (if transcendental/special function)
                            │
                            └─→ toDouble() → Double (if real)
```

### Examples

```scala
// Quadratic with real roots → Monotone
val realExpr: Expression = Quadratic(a=1, b=-3, c=2)  // x² - 3x + 2 = 0
val realSoln: QuadraticSolution = realExpr.solve(0)
// QuadraticSolution(1.5, 0.25, 1, imaginary=false)
val realValue: Monotone = realSoln.materialize  // 2.0

// Quadratic with complex roots → Complex
val complexExpr: Expression = Quadratic(a=1, b=0, c=1)  // x² + 1 = 0
val complexSoln: QuadraticSolution = complexExpr.solve(0)
// QuadraticSolution(0, 1, 1, imaginary=true)
val complexValue: Complex = complexSoln.toComplex  // ComplexCartesian(0, 1) = i

// Special function → Complex
val bessel: Expression = BesselJ(Number(0), Number(1))
val besselValue: Complex = bessel.materialize  // ≈ 0.7652 (real, but returns Complex)

// Transcendental → Complex
val euler: Expression = Exp(Multiply(I, Pi))
val eulerValue: Complex = euler.materialize  // ComplexPolar(1, π) = -1
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

Solutions materialize to `Complex` when they have imaginary roots:

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
  val real: Monotone = solution.materialize.asInstanceOf[Monotone]
  // Work with real number
}
```

### Wrapper Elimination Strategy

To eliminate the `Complex` wrapper:

1. **Phase 1**: Create native `ComplexCartesian` and `ComplexPolar` in algebra module
2. **Phase 2**: Update all `QuadraticSolution.materialize` to return new types
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

*Document Version: 3.0*  
*Last Updated: 2025-01-28*  
*Library Version: 1.6.2*

## Changelog

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