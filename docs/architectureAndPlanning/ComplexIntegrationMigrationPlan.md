# Complex Integration Migration Plan

## Overview

This document provides a step-by-step plan for migrating from the current wrapper pattern (`algebra.eager.Complex` wrapping `core.numerical.Complex`) to a more integrated design where Complex types are native to the algebra module.

**Current State:** Version 1.6.2 with wrapper pattern  
**Target State:** Native Complex in algebra module  
**Estimated Effort:** Large (affects multiple modules, extensive testing required)  
**Risk Level:** High (touches fundamental types)

---

## Decision Point: Should You Do This?

### Reasons TO Migrate
- ✅ Single source of truth for Complex
- ✅ No wrapper indirection (though @inline makes this negligible)
- ✅ Clearer type ownership (algebra owns algebraic types)
- ✅ Easier to add Solution-specific operations to Complex

### Reasons NOT TO Migrate
- ❌ Current wrapper pattern works well
- ❌ Performance overhead is negligible with JVM inlining
- ❌ Would create circular dependency issues if not careful
- ❌ Large refactoring effort for minimal practical gain
- ❌ Risk of introducing bugs in fundamental types
- ❌ Core module would lose independence

**Recommendation:** Only migrate if you have a compelling reason (performance profiling shows issues, architectural refactor needed anyway, or preparing for major 3.0.0 release).

---

## Prerequisites

Before starting, ensure:
- [ ] All 3,974+ tests passing
- [ ] Clean working directory (git status clean)
- [ ] Branch created for migration work
- [ ] Backup/tag current stable version
- [ ] Time allocated (estimate 2-3 days minimum)

---

## Migration Strategy Overview

We'll use a **parallel implementation** strategy:
1. Create new native Complex types alongside old ones
2. Gradually migrate code to use new types
3. Keep old types as deprecated wrappers during transition
4. Remove old types once migration complete

This minimizes risk and allows incremental testing.

---

## Phase 1: Create Native Complex Types in Algebra Module

### Step 1.1: Create algebra/eager/ComplexTypes.scala

**File:** `algebra/src/main/scala/com/phasmidsoftware/number/algebra/eager/ComplexTypes.scala`

```scala
package com.phasmidsoftware.number.algebra.eager

import cats.Show
import cats.kernel.Eq
import com.phasmidsoftware.number.algebra.core._
import com.phasmidsoftware.number.algebra.util.{AlgebraException, LatexRenderer}
import com.phasmidsoftware.number.core.inner.{Factor, Rational, PureNumber}

/**
  * Native Complex number implementation in algebra module.
  * Represents any complex number a + bi.
  */
sealed trait ComplexNumber extends Solution {
  /** Real component */
  def real: Number
  
  /** Imaginary component */
  def imaginary: Number
  
  /** Magnitude |z| = √(a² + b²) */
  def modulus: Number
  
  /** Argument arg(z) = atan2(b, a) */
  def argument: Number
  
  /** Complex conjugate: a + bi → a - bi */
  def conjugate: ComplexNumber
  
  /** Multiply by i: a + bi → -b + ai */
  def rotate: ComplexNumber
  
  /** Convert to Cartesian form */
  def toCartesian: ComplexCartesian
  
  /** Convert to Polar form */
  def toPolar: ComplexPolar
  
  // Solution interface
  def toMonotone: Option[Monotone] = 
    if (imaginary.isZero) Some(Scalar(real)) else None
  
  def +(other: Solution): Solution = other match {
    case c: ComplexNumber => 
      ComplexCartesian(
        (real.asInstanceOf[CanAdd[Number, Number]] + c.real),
        (imaginary.asInstanceOf[CanAdd[Number, Number]] + c.imaginary)
      )()
    case _ => 
      throw AlgebraException(s"ComplexNumber.+: unsupported $other")
  }
  
  def scale(r: Rational): Solution = 
    ComplexCartesian(
      real.scale(r),
      imaginary.scale(r)
    )()
  
  def negate: Solution = 
    ComplexCartesian(
      real.negate,
      imaginary.negate
    )()
  
  def isZero: Boolean = real.isZero && imaginary.isZero
  
  def signum: Int = if (isZero) 0 else 1
  
  def isExact: Boolean = real.isExact && imaginary.isExact
  
  def normalize: ComplexNumber = this
  
  def maybeFactor(context: Context): Option[Factor] = 
    if (isZero) Some(PureNumber)
    else if (imaginary.isZero) real.maybeFactor(context)
    else None
  
  lazy val render: String = maybeName.getOrElse {
    (real.isZero, imaginary.isZero) match {
      case (true, true) => "0"
      case (true, false) => 
        if (imaginary.isUnity) "i"
        else if (imaginary == Number.negOne) "-i"
        else s"${imaginary.render}i"
      case (false, true) => 
        real.render
      case (false, false) => 
        val sign = if (imaginary.signum < 0) "-" else "+"
        val imagPart = imaginary.abs.render
        s"${real.render} $sign ${imagPart}i"
    }
  }
  
  // Comparison methods
  override def eqv(that: Eager): Try[Boolean] = that match {
    case c: ComplexNumber =>
      for {
        realEq <- real.eqv(c.real)
        imagEq <- imaginary.eqv(c.imaginary)
      } yield realEq && imagEq
    case _ =>
      Failure(AlgebraException(s"ComplexNumber.eqv: cannot compare $this with $that"))
  }
  
  override def fuzzyEqv(p: Double)(that: Eager): Try[Boolean] = that match {
    case c: ComplexNumber =>
      for {
        realEq <- real.fuzzyEqv(p)(c.real)
        imagEq <- imaginary.fuzzyEqv(p)(c.imaginary)
      } yield realEq && imagEq
    case _ =>
      super.fuzzyEqv(p)(that)
  }
}

/**
  * Cartesian representation: a + bi
  */
case class ComplexCartesian(
  real: Number,
  imaginary: Number
)(val maybeName: Option[String] = None) extends ComplexNumber {
  
  lazy val modulus: Number = {
    val realSq = real.power(Rational(2))
    val imagSq = imaginary.power(Rational(2))
    (realSq + imagSq).sqrt
  }
  
  lazy val argument: Number = 
    Number.atan2(imaginary, real)
  
  def conjugate: ComplexNumber = 
    copy(imaginary = imaginary.negate)(None)
  
  def rotate: ComplexNumber = 
    ComplexCartesian(-imaginary, real)()
  
  def toCartesian: ComplexCartesian = this
  
  def toPolar: ComplexPolar = 
    ComplexPolar(modulus, Angle(argument))()
  
  // Complex arithmetic
  def *(other: ComplexCartesian): ComplexCartesian = {
    // (a + bi)(c + di) = (ac - bd) + (ad + bc)i
    val newReal = real * other.real - imaginary * other.imaginary
    val newImag = real * other.imaginary + imaginary * other.real
    ComplexCartesian(newReal, newImag)()
  }
}

/**
  * Polar representation: r·e^(iθ)
  */
case class ComplexPolar(
  radius: Number,
  angle: Angle,
  branch: Int = 0
)(val maybeName: Option[String] = None) extends ComplexNumber {
  
  lazy val real: Number = radius * angle.cos
  lazy val imaginary: Number = radius * angle.sin
  
  def modulus: Number = radius
  def argument: Number = angle.toNumber
  
  def conjugate: ComplexNumber = 
    copy(angle = angle.negate)(None)
  
  def rotate: ComplexNumber = 
    copy(angle = angle + Angle.rightAngle)()
  
  def toCartesian: ComplexCartesian = 
    ComplexCartesian(real, imaginary)()
  
  def toPolar: ComplexPolar = this
  
  // Complex arithmetic in polar form
  def *(other: ComplexPolar): ComplexPolar = {
    // (r₁e^(iθ₁))(r₂e^(iθ₂)) = (r₁r₂)e^(i(θ₁+θ₂))
    ComplexPolar(
      radius * other.radius,
      angle + other.angle
    )()
  }
}

object ComplexNumber {
  
  def apply(real: Number, imaginary: Number): ComplexNumber = 
    ComplexCartesian(real, imaginary)()
  
  def polar(radius: Number, angle: Number): ComplexNumber = 
    ComplexPolar(radius, Angle(angle))()
  
  val zero: ComplexNumber = ComplexCartesian(Number.zero, Number.zero)(Some("0"))
  val one: ComplexNumber = ComplexCartesian(Number.one, Number.zero)(Some("1"))
  val i: ComplexNumber = ComplexCartesian(Number.zero, Number.one)(Some("i"))
  
  given DyadicOperator[ComplexNumber] = new DyadicOperator[ComplexNumber] {
    def op[B <: ComplexNumber, Z](f: (ComplexNumber, B) => Try[Z])(x: ComplexNumber, y: B): Try[Z] = 
      f(x, y)
  }
  
  given Eq[ComplexNumber] = Eq.instance { (x, y) =>
    x.eqv(y).getOrElse(false)
  }
  
  given FuzzyEq[ComplexNumber] = FuzzyEq.instance { (x, y, p) =>
    x === y || x.fuzzyEqv(p)(y).getOrElse(false)
  }
  
  implicit val showComplex: Show[ComplexNumber] = Show.show(_.render)
  
  implicit val latexRenderer: LatexRenderer[ComplexNumber] = LatexRenderer.instance { c =>
    val cart = c.toCartesian
    val r = cart.real
    val i = cart.imaginary
    
    (r.isZero, i.isZero) match {
      case (true, true) => "0"
      case (true, false) =>
        if (i.isUnity) "i"
        else if (i == Number.negOne) "-i"
        else s"${i.toLatex}i"
      case (false, true) =>
        r.toLatex
      case (false, false) =>
        val sign = if (i.signum < 0) "-" else "+"
        s"${r.toLatex} $sign ${i.abs.toLatex}i"
    }
  }
}
```

**Tests to create:** `ComplexTypesSpec.scala` with comprehensive tests for:
- Construction (Cartesian, Polar)
- Arithmetic (+, *, conjugate, rotate)
- Conversions (toCartesian, toPolar)
- Edge cases (zero, purely real, purely imaginary)
- Equality and fuzzy equality
- LaTeX rendering

---

## Phase 2: Update QuadraticSolution to Use Native Types

### Step 2.1: Modify QuadraticSolution.toComplex

**File:** `algebra/src/main/scala/com/phasmidsoftware/number/algebra/eager/Algebraic.scala`

**CHANGE FROM:**
```scala
def toComplex: Complex = {
  // Returns wrapper type
  Complex(numerical.ComplexCartesian(real, imag))
}
```

**CHANGE TO:**
```scala
def toComplex: ComplexNumber = {
  val radical = offset.sqrt
  val term = radical.scale(coefficient)
  
  if (imaginary) {
    ComplexCartesian(base.toNumber, term.toNumber)()
  } else {
    val realPart = (base + term).toNumber
    ComplexCartesian(realPart, Number.zero)()
  }
}
```

**Tests to update:**
- `AlgebraicSpec.scala` - update return types
- `QuadraticSolutionSpec.scala` - verify toComplex returns ComplexCartesian
- Add tests for imaginary=true and imaginary=false cases

---

## Phase 3: Create Type Aliases in Core Module

To maintain backward compatibility during transition:

### Step 3.1: Add type aliases

**File:** `core/src/main/scala/com/phasmidsoftware/number/core/numerical/ComplexAliases.scala`

```scala
package com.phasmidsoftware.number.core.numerical

// Type aliases for backward compatibility
type Complex = com.phasmidsoftware.number.algebra.eager.ComplexNumber
type ComplexCartesian = com.phasmidsoftware.number.algebra.eager.ComplexCartesian
type ComplexPolar = com.phasmidsoftware.number.algebra.eager.ComplexPolar

object Complex {
  // Factory methods maintain API compatibility
  def apply(real: Number, imaginary: Number): Complex = 
    com.phasmidsoftware.number.algebra.eager.ComplexCartesian(real, imaginary)()
  
  def polar(radius: Number, angle: Number): Complex = 
    com.phasmidsoftware.number.algebra.eager.ComplexPolar(
      radius, 
      com.phasmidsoftware.number.algebra.eager.Angle(angle)
    )()
  
  val zero: Complex = ComplexNumber.zero
  val one: Complex = ComplexNumber.one
  val i: Complex = ComplexNumber.i
}
```

### Step 3.2: Deprecate old Complex implementation

**File:** `core/src/main/scala/com/phasmidsoftware/number/core/numerical/Complex.scala`

Add deprecation warnings to existing `numerical.Complex`:

```scala
@deprecated("Use algebra.eager.ComplexNumber instead", "1.7.0")
sealed trait Complex {
  // ... existing implementation
}

@deprecated("Use algebra.eager.ComplexCartesian instead", "1.7.0")
case class ComplexCartesian(...) extends Complex

@deprecated("Use algebra.eager.ComplexPolar instead", "1.7.0")
case class ComplexPolar(...) extends Complex
```

**Important:** Keep the old implementation around for at least one major version to allow users to migrate.

---

## Phase 4: Update Core Module to Use Type Aliases

### Step 4.1: Find all uses of numerical.Complex in core module

```bash
cd core/src/main/scala
grep -r "numerical\.Complex\|: Complex" . --include="*.scala" > complex_usage.txt
```

### Step 4.2: Update imports

**FROM:**
```scala
import com.phasmidsoftware.number.core.numerical.Complex
import com.phasmidsoftware.number.core.numerical.ComplexCartesian
```

**TO:**
```scala
import com.phasmidsoftware.number.algebra.eager.{ComplexNumber => Complex}
import com.phasmidsoftware.number.algebra.eager.ComplexCartesian
```

**OR** (if using type aliases):
```scala
import com.phasmidsoftware.number.core.numerical.Complex  // Now aliases to algebra types
```

### Step 4.3: Update method signatures

Any methods in core that return or accept Complex:

```scala
// Before
def asComplex: Option[numerical.Complex]

// After
def asComplex: Option[algebra.eager.ComplexNumber]
// OR if using aliases:
def asComplex: Option[Complex]  // Where Complex is aliased
```

### Step 4.4: Handle construction

**FROM:**
```scala
numerical.ComplexCartesian(real, imag)
```

**TO:**
```scala
algebra.eager.ComplexCartesian(real, imag)()
// OR
Complex(real, imag)  // Using factory in aliases
```

**Tests to update:**
- All core module tests that use Complex
- Verify type aliases work correctly
- Check that no runtime ClassCastExceptions occur

---

## Phase 5: Update Algebra Module

### Step 5.1: Remove wrapper from Solution.scala

**File:** `algebra/src/main/scala/com/phasmidsoftware/number/algebra/eager/Solution.scala`

**DELETE:**
```scala
case class Complex(complex: numerical.Complex)(val maybeName: Option[String] = None) extends Solution {
  // ... all the wrapper delegation code
}

object Complex {
  def apply(x: numerical.Complex): Complex = new Complex(x)()
  // ...
}
```

**REPLACE WITH:**
```scala
// Complex is now in ComplexTypes.scala
// Just add a type alias for convenience if needed:
type Complex = ComplexNumber
```

### Step 5.2: Update all algebra code using Complex wrapper

Find all uses:
```bash
cd algebra/src/main/scala
grep -r "Complex(numerical\|Complex(.*Complex)" . --include="*.scala"
```

**FROM:**
```scala
Complex(numerical.ComplexCartesian(r, i))
```

**TO:**
```scala
ComplexCartesian(r, i)()
```

### Step 5.3: Update QuadraticSolution (already done in Phase 2)

Verify `toComplex` returns `ComplexNumber` (or `ComplexCartesian` specifically).

**Tests to update:**
- `SolutionSpec.scala`
- `AlgebraicSpec.scala`
- Any tests that construct Complex via wrapper

---

## Phase 6: Update Expression Module

### Step 6.1: Update Expression types that return Complex

**File:** `expression/src/main/scala/com/phasmidsoftware/number/expression/expr/*.scala`

Any Expression that materializes to Complex:

**FROM:**
```scala
override def materialize: Eager = {
  val result = computeComplex(...)
  Complex(numerical.ComplexCartesian(real, imag))
}
```

**TO:**
```scala
override def materialize: Eager = {
  val result = computeComplex(...)
  ComplexCartesian(real, imag)()
}
```

### Step 6.2: Update imports

```scala
// Add import
import com.phasmidsoftware.number.algebra.eager.{ComplexNumber, ComplexCartesian, ComplexPolar}

// OR use type alias from core
import com.phasmidsoftware.number.core.numerical.Complex  // Now aliases to algebra type
```

**Tests to update:**
- Expression materialization tests
- Any tests checking types of materialized values

---

## Phase 7: Update Parse Module

### Step 7.1: Update LaTeX parser if it handles complex numbers

**File:** `parse/src/main/scala/com/phasmidsoftware/number/parse/*.scala`

If LaTeX parser creates Complex numbers:

**FROM:**
```scala
numerical.ComplexCartesian(real, imag)
```

**TO:**
```scala
algebra.eager.ComplexCartesian(real, imag)()
```

**Tests to update:**
- LaTeX parsing tests for complex numbers
- String interpolator tests

---

## Phase 8: Comprehensive Testing

### Step 8.1: Run full test suite

```bash
sbt clean test
```

Expected: All 3,974+ tests should still pass.

### Step 8.2: Type-specific test verification

Create a test that verifies type hierarchy:

```scala
class ComplexMigrationSpec extends AnyFlatSpec with Matchers {
  
  "ComplexCartesian" should "extend Solution" in {
    val c = ComplexCartesian(Number.one, Number.zero)()
    c shouldBe a[Solution]
    c shouldBe a[ComplexNumber]
  }
  
  "QuadraticSolution.toComplex" should "return ComplexNumber" in {
    val q = QuadraticSolution(Scalar(1), Scalar(1), 1, imaginary = true)
    val c = q.toComplex
    c shouldBe a[ComplexNumber]
    c shouldBe a[ComplexCartesian]
  }
  
  "Type aliases" should "work correctly" in {
    import com.phasmidsoftware.number.core.numerical.Complex
    val c: Complex = ComplexNumber(Number.one, Number.zero)
    c shouldBe a[ComplexNumber]
  }
}
```

### Step 8.3: Integration testing

Test workflows that span modules:

```scala
// Expression → Solution → Complex
val root: Root = QuadraticRoot(QuadraticEquation(0, 1), 0)
val solution: QuadraticSolution = root.solution
val complex: ComplexNumber = solution.toComplex
complex.imaginary shouldBe Number.one

// Expression.materialize → Complex
val expr: Expression = Exp(Multiply(I, Pi))
val result = expr.materialize
result shouldBe a[ComplexNumber]
```

---

## Phase 9: Update Documentation

### Step 9.1: Update scaladoc

Review and update scaladoc comments in:
- `ComplexTypes.scala` - comprehensive documentation
- `Solution.scala` - remove references to old wrapper
- `Algebraic.scala` - update toComplex return type docs
- All Expression types that return Complex

### Step 9.2: Update README.md

Add migration notes:
```markdown
## Version 1.7.0 - Complex Type Migration

**Breaking Change:** Complex numbers have been moved from `core.numerical` to `algebra.eager`.

**Migration Guide:**
- Old: `import com.phasmidsoftware.number.core.numerical.Complex`
- New: `import com.phasmidsoftware.number.algebra.eager.ComplexNumber`
- Type aliases provided for compatibility
- Old Complex types deprecated, will be removed in 2.0.0

**Code Changes:**
- `Complex(numerical.Complex(...))` → `ComplexCartesian(...)()` or `ComplexPolar(...)()`
- `QuadraticSolution.toComplex` now returns `ComplexNumber` instead of wrapper
- All Complex arithmetic uses native types
```

### Step 9.3: Update ValueableArchitecture.md

Update the document to reflect:
- Complex is now native in algebra.eager
- No wrapper pattern (remove that section)
- Update all code examples
- Bump version to 4.0

---

## Phase 10: Cleanup and Finalization

### Step 10.1: Remove deprecated wrapper (in version 2.0.0)

After one full version with deprecation warnings:

**DELETE:**
- Old wrapper Complex in `Solution.scala`
- Old `numerical.Complex` implementation in core
- Type aliases (users should import directly)

### Step 10.2: Final verification

```bash
sbt clean test
sbt publishLocal
# Test in a separate project
```

### Step 10.3: Update version numbers

- `version.sbt` → "1.7.0" (for migration)
- Later: "2.0.0" (after deprecation period)

---

## Rollback Plan

If migration fails or causes too many problems:

### Rollback Steps:
1. `git checkout <branch-before-migration>`
2. Delete new `ComplexTypes.scala`
3. Remove type aliases
4. Restore old wrapper pattern
5. Re-run tests to verify stability

### Keep These Commits Separate:
1. Commit after Phase 1 (new types created)
2. Commit after Phase 4 (core migrated)
3. Commit after Phase 5 (algebra migrated)
4. Commit after Phase 8 (all tests passing)

This allows fine-grained rollback if needed.

---

## Estimated Effort

| Phase | Effort | Risk | Dependency |
|-------|--------|------|------------|
| 1. Create native types | 4 hours | Low | None |
| 2. Update QuadraticSolution | 1 hour | Low | Phase 1 |
| 3. Type aliases | 1 hour | Low | Phase 1 |
| 4. Migrate core | 6 hours | Medium | Phase 3 |
| 5. Migrate algebra | 4 hours | Medium | Phase 4 |
| 6. Migrate expression | 3 hours | Low | Phase 5 |
| 7. Migrate parse | 2 hours | Low | Phase 5 |
| 8. Testing | 4 hours | High | All above |
| 9. Documentation | 2 hours | Low | Phase 8 |
| 10. Cleanup | 1 hour | Low | Phase 9 |

**Total:** ~28 hours over 3-4 days

**Risk points:**
- Circular dependency if not careful with imports
- Type inference issues with implicit conversions
- Breaking changes for any external users

---

## Success Criteria

✅ All tests pass (3,974+)  
✅ No deprecation warnings in library code  
✅ Type aliases work for backward compatibility  
✅ No runtime ClassCastExceptions  
✅ Performance unchanged (benchmark if concerned)  
✅ Documentation updated  
✅ Clean `git diff` showing intentional changes only

---

## Alternative: Keep Wrapper Pattern

**Recommendation:** Given the effort and risk, consider keeping the wrapper pattern unless you have a specific compelling reason to migrate.

The current architecture is sound:
- Performance is fine with @inline
- Clear separation of concerns
- No circular dependencies
- Well-tested and stable

Migration would be "nice to have" but not necessary for 2.0.0 or Typelevel submission.

---

## Decision Matrix

| Factor | Keep Wrapper | Migrate to Native |
|--------|--------------|-------------------|
| **Effort** | None | ~28 hours |
| **Risk** | None | Medium |
| **Performance** | Negligible overhead | Marginally better |
| **Architecture** | Good | Slightly cleaner |
| **Maintenance** | Simple | Simpler long-term |
| **For 2.0.0** | ✅ Ready now | ⏳ Delays release |

**Recommendation for Now:** Keep wrapper, consider migration post-2.0.0 if you have compelling reasons.

---

*This migration plan is comprehensive but labor-intensive. Evaluate whether the benefits justify the effort and risk for your timeline and goals.*