/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.algebraic

import com.phasmidsoftware.number.algebra.eager.*
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.expression.expr.ExpressionException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class QuadraticEquationSpec extends AnyFlatSpec with Matchers {

  behavior of "QuadraticEquation"

  // =========================
  // Constructor and Basic Properties
  // =========================

  it should "create a quadratic equation with given p and q coefficients" in {
    val equation = QuadraticEquation(Rational(3), Rational(2))
    equation.p shouldBe Rational(3)
    equation.q shouldBe Rational(2)
  }

  it should "always have 2 branches" in {
    val equation = QuadraticEquation(Rational(1), Rational(1))
    equation.branches shouldBe 2
  }

  it should "format toString correctly for positive coefficients" in {
    val equation = QuadraticEquation(Rational(3), Rational(2))
    equation.render should include("x∧2")
    equation.render should include("+ ")
  }

  it should "format toString correctly for negative coefficients" in {
    val equation = QuadraticEquation(Rational(-3), Rational(-2))
    equation.render should include("x∧2")
    equation.render should include("- ")
  }

  // =========================
  // Discriminant
  // =========================

  it should "compute discriminant correctly for positive case" in {
    val equation = QuadraticEquation(Rational(5), Rational(6))
    equation.discriminant shouldBe Rational(1) // 25 - 24 = 1
  }

  it should "compute discriminant correctly for zero case (repeated roots)" in {
    val equation = QuadraticEquation(Rational(4), Rational(4))
    equation.discriminant shouldBe Rational.zero // 16 - 16 = 0
  }

  it should "compute discriminant correctly for negative case (complex roots)" in {
    val equation = QuadraticEquation(Rational(2), Rational(5))
    equation.discriminant shouldBe Rational(-16) // 4 - 20 = -16
  }

  // =========================
  // Solving - Real Roots
  // =========================

  it should "solve equation with two distinct real roots" in {
    // x² - 3x + 2 = 0, roots are x = 1 and x = 2
    val equation = QuadraticEquation(Rational(-3), Rational(2))
    val root0 = equation.solve(0)
    val root1 = equation.solve(1)

    // Both should be QuadraticSolution instances
    root0 shouldBe a[QuadraticSolution]
    root1 shouldBe a[QuadraticSolution]

    // Check the round-trip
    equation.evaluate(root0).isZero shouldBe true
    equation.evaluate(root1).isZero shouldBe true
  }

  it should "solve equation with repeated root" in {
    // x² + 4x + 4 = 0, root is x = -2 (repeated)
    val equation = QuadraticEquation(Rational(4), Rational(4))
    val root0 = equation.solve(0)
    val root1 = equation.solve(1)

    root0 shouldBe a[QuadraticSolution]
    root1 shouldBe a[QuadraticSolution]
    // Both branches should give the same root

    // Check the round-trip
    equation.evaluate(root0).isZero shouldBe true
    equation.evaluate(root1).isZero shouldBe true
  }

  it should "solve simple square root equation x² - 4 = 0" in {
    // Roots are ±2
    val equation = QuadraticEquation(Rational.zero, Rational(-4))
    val root0 = equation.solve(0)
    val root1 = equation.solve(1)

    root0 shouldBe a[QuadraticSolution]
    root1 shouldBe a[QuadraticSolution]

    // Check the round-trip
    equation.evaluate(root0).isZero shouldBe true
    equation.evaluate(root1).isZero shouldBe true
  }

  // =========================
  // Solving - Complex Roots
  // =========================

  it should "solve equation with complex roots" in {
    // x² + x + 1 = 0, discriminant = -3 (negative)
    val equation = QuadraticEquation(Rational(1), Rational(1))
    val root0 = equation.solve(0)
    val root1 = equation.solve(1)

    root0 shouldBe a[Complex]
    root1 shouldBe a[Complex]

    // Check the round-trip
    equation.evaluate(root0).isZero shouldBe true
    equation.evaluate(root1).isZero shouldBe true
  }

  it should "solve equation x² + 1 = 0 (pure imaginary roots)" in {
    // Roots are ±i
    val equation = QuadraticEquation(Rational.zero, Rational(1))
    val root0 = equation.solve(0)
    val root1 = equation.solve(1)

    root0 shouldBe a[Complex]
    root1 shouldBe a[Complex]

    // Check the round-trip
    equation.evaluate(root0).isZero shouldBe true
    equation.evaluate(root1).isZero shouldBe true
  }

  // =========================
  // Solve - Error Cases
  // =========================

  it should "throw exception for invalid branch index -1" in {
    val equation = QuadraticEquation(Rational(1), Rational(1))
    an[IllegalArgumentException] should be thrownBy {
      equation.solve(-1)
    }
  }

  it should "throw exception for invalid branch index 2" in {
    val equation = QuadraticEquation(Rational(1), Rational(1))
    an[IllegalArgumentException] should be thrownBy {
      equation.solve(2)
    }
  }

  // =========================
  // Branched Method
  // =========================

  it should "return Expression for valid branch 0" in {
    val equation = QuadraticEquation(Rational(-3), Rational(2))
    val expr = equation.branched(0)
    expr shouldBe a[com.phasmidsoftware.number.expression.expr.Literal]
  }

  it should "return Expression for valid branch 1" in {
    val equation = QuadraticEquation(Rational(-3), Rational(2))
    val expr = equation.branched(1)
    expr shouldBe a[com.phasmidsoftware.number.expression.expr.Literal]
  }

  // =========================
  // Conjugate Sum and Product (Vieta's Formulas)
  // =========================

  it should "compute conjugate sum correctly" in {
    val equation = QuadraticEquation(Rational(5), Rational(6))
    equation.conjugateSum shouldBe Rational(-5)
  }

  it should "compute conjugate sum for negative p" in {
    val equation = QuadraticEquation(Rational(-3), Rational(2))
    equation.conjugateSum shouldBe Rational(3)
  }

  it should "compute conjugate product correctly" in {
    val equation = QuadraticEquation(Rational(5), Rational(6))
    equation.conjugateProduct shouldBe Rational(6)
  }

  it should "compute conjugate product for negative q" in {
    val equation = QuadraticEquation(Rational(3), Rational(-2))
    equation.conjugateProduct shouldBe Rational(-2)
  }

  // =========================
  // Shift Origin
  // =========================

  it should "shift origin by c = 1" in {
    val equation = QuadraticEquation(Rational(4), Rational(3))
    val shifted = equation.shiftOrigin(Rational(1))

    // New p = p - 2c = 4 - 2 = 2
    // New q = c² - p*c + q = 1 - 4 + 3 = 0
    shifted.p shouldBe Rational(2)
    shifted.q shouldBe Rational.zero
  }

  it should "shift origin by c = -1" in {
    val equation = QuadraticEquation(Rational(2), Rational(1))
    val shifted = equation.shiftOrigin(Rational(-1))

    // New p = p - 2c = 2 - (-2) = 4
    // New q = c² - p*c + q = 1 + 2 + 1 = 4
    shifted.p shouldBe Rational(4)
    shifted.q shouldBe Rational(4)
  }

  it should "shift origin by c = 0 (identity)" in {
    val equation = QuadraticEquation(Rational(3), Rational(2))
    val shifted = equation.shiftOrigin(Rational.zero)

    shifted shouldBe equation
  }

  // =========================
  // Invert
  // =========================

  it should "invert equation correctly" in {
    val equation = QuadraticEquation(Rational(4), Rational(2))
    val inverted = equation.invert

    // New p = p/q = 4/2 = 2
    // New q = 1/q = 1/2
    inverted.p shouldBe Rational(2)
    inverted.q shouldBe Rational(1, 2)
  }

  it should "handle invert with negative coefficients" in {
    val equation = QuadraticEquation(Rational(-6), Rational(-3))
    val inverted = equation.invert

    // New p = p/q = -6/-3 = 2
    // New q = 1/q = 1/-3 = -1/3
    inverted.p shouldBe Rational(2)
    inverted.q shouldBe Rational(-1, 3)
  }

  // =========================
  // Transform
  // =========================

  it should "apply arbitrary transformation" in {
    val equation = QuadraticEquation(Rational(4), Rational(2))
    val transformed = equation.transform(
      (p, q) => p + q,
      (p, q) => p * q
    )

    transformed.p shouldBe Rational(6) // 4 + 2
    transformed.q shouldBe Rational(8) // 4 * 2
  }

  it should "transform with identity functions" in {
    val equation = QuadraticEquation(Rational(3), Rational(5))
    val transformed = equation.transform(
      (p, _) => p,
      (_, q) => q
    )

    transformed shouldBe equation
  }

  // =========================
  // Scaling (multiplication)
  // =========================

  it should "scale equation by positive factor" in {
    val equation = QuadraticEquation(Rational(2), Rational(3))
    val scaled = equation * Rational(2)

    // New p = x * p = 2 * 2 = 4
    // New q = x² * q = 4 * 3 = 12
    scaled shouldBe a[QuadraticEquation]
    val scaledQuad = scaled.asInstanceOf[QuadraticEquation]
    scaledQuad.p shouldBe Rational(4)
    scaledQuad.q shouldBe Rational(12)
  }

  it should "scale equation by negative factor" in {
    val equation = QuadraticEquation(Rational(4), Rational(2))
    val scaled = equation * Rational(-1)

    // New p = -1 * 4 = -4
    // New q = 1 * 2 = 2
    scaled shouldBe a[QuadraticEquation]
    val scaledQuad = scaled.asInstanceOf[QuadraticEquation]
    scaledQuad.p shouldBe Rational(-4)
    scaledQuad.q shouldBe Rational(2)
  }

  it should "scale equation by fractional factor" in {
    val equation = QuadraticEquation(Rational(6), Rational(8))
    val scaled = equation * Rational(1, 2)

    // New p = 1/2 * 6 = 3
    // New q = 1/4 * 8 = 2
    scaled shouldBe a[QuadraticEquation]
    val scaledQuad = scaled.asInstanceOf[QuadraticEquation]
    scaledQuad.p shouldBe Rational(3)
    scaledQuad.q shouldBe Rational(2)
  }

  // =========================
  // Evaluate
  // =========================

  it should "evaluate real QuadraticSolution to zero for its equation" in {
    val equation = QuadraticEquation(Rational.zero, Rational.negOne)
    val sol = equation.solve(0)

    sol match {
      case qs@com.phasmidsoftware.number.algebra.eager.QuadraticSolution(base, offset, coefficient, imaginary) =>
        val result = equation.evaluate(qs)
        result.isZero shouldBe true
      case _ => fail("Expected QuadraticSolution")
    }
  }

  it should "evaluate Complex solution to zero for its equation" in {
    val equation = QuadraticEquation(Rational.zero, Rational.one)
    val sol = equation.solve(0)

    sol match {
      case c: Complex =>
        val result = equation.evaluate(c)
        result.isZero shouldBe true
      case _ => fail("Expected Complex solution")
    }
  }

  it should "throw exception when evaluating invalid solution type" in {
    val eq = QuadraticEquation(Rational.one, Rational.one)
    val invalidSol = LinearSolution(RationalNumber.one)

    an[ExpressionException] should be thrownBy {
      eq.evaluate(invalidSol)
    }
  }

  // =========================
  // Equality and CanEqual
  // =========================

  it should "support canEqual for QuadraticEquation instances" in {
    val equation = QuadraticEquation(Rational(1), Rational(1))
    equation.canEqual(QuadraticEquation(Rational(2), Rational(2))) shouldBe true
  }

  it should "reject canEqual for non-QuadraticEquation instances" in {
    val equation = QuadraticEquation(Rational(1), Rational(1))
    equation.canEqual("not a quadratic") shouldBe false
    equation.canEqual(42) shouldBe false
  }

  it should "be equal to itself" in {
    val equation = QuadraticEquation(Rational(3), Rational(2))
    equation shouldBe equation
  }

  it should "be equal to another equation with same coefficients" in {
    val eq1 = QuadraticEquation(Rational(3), Rational(2))
    val eq2 = QuadraticEquation(Rational(3), Rational(2))
    eq1 shouldBe eq2
  }

  it should "not be equal to equation with different coefficients" in {
    val eq1 = QuadraticEquation(Rational(3), Rational(2))
    val eq2 = QuadraticEquation(Rational(3), Rational(5))
    eq1 should not be eq2
  }

  // =========================
  // Special Equations from Companion Object
  // =========================

  behavior of "QuadraticEquation companion object"

  it should "create square root equation" in {
    val sqrtEq = QuadraticEquation.squareRootEquation(Rational(4))
    sqrtEq.p shouldBe Rational.zero
    sqrtEq.q shouldBe Rational(-4)
  }

  it should "provide golden ratio equation" in {
    val goldenEq = QuadraticEquation.goldenRatioEquation
    goldenEq.p shouldBe Rational(-1)
    goldenEq.q shouldBe Rational(-1)
  }

  it should "provide root two equation" in {
    val rootTwoEq = QuadraticEquation.rootTwoEquation
    rootTwoEq.p shouldBe Rational.zero
    rootTwoEq.q shouldBe Rational(-2)
  }

  it should "provide root three equation" in {
    val rootThreeEq = QuadraticEquation.rootThreeEquation
    rootThreeEq.p shouldBe Rational.zero
    rootThreeEq.q shouldBe Rational(-3)
  }

  it should "provide phi approximation" in {
    val phi = QuadraticEquation.phiApprox
    phi shouldBe a[com.phasmidsoftware.number.algebra.eager.Real]
  }

  it should "solve golden ratio equation correctly" in {
    val goldenEq = QuadraticEquation.goldenRatioEquation
    val root0 = goldenEq.solve(0)
    val root1 = goldenEq.solve(1)

    // Golden ratio φ = (1 + √5)/2 ≈ 1.618...
    // Conjugate ψ = (1 - √5)/2 ≈ -0.618...
    root0 shouldBe a[QuadraticSolution]
    root1 shouldBe a[QuadraticSolution]

    // Check the round-trip
    val eager = goldenEq.evaluate(root0)
    eager.isZero shouldBe true
    goldenEq.evaluate(root1).isZero shouldBe true
  }

  it should "solve root two equation correctly" in {
    val rootTwoEq = QuadraticEquation.rootTwoEquation
    val root0 = rootTwoEq.solve(0)
    val root1 = rootTwoEq.solve(1)

    // Roots are ±√2
    root0 shouldBe a[QuadraticSolution]
    root1 shouldBe a[QuadraticSolution]

    // Check the round-trip
    rootTwoEq.evaluate(root0).isZero shouldBe true
    rootTwoEq.evaluate(root1).isZero shouldBe true

  }

  // =========================
  // Edge Cases
  // =========================

  it should "handle equation with zero p coefficient" in {
    val equation = QuadraticEquation(Rational.zero, Rational(4))
    equation.discriminant shouldBe Rational(-16)
    val root0 = equation.solve(0)
    root0 shouldBe a[Complex] // Pure imaginary roots

    // Check the round-trip
    equation.evaluate(root0).isZero shouldBe true
  }

  it should "handle equation with zero q coefficient" in {
    val equation = QuadraticEquation(Rational(3), Rational.zero)
    equation.discriminant shouldBe Rational(9)
    val root0 = equation.solve(0)
    root0 shouldBe a[QuadraticSolution] // Real roots: 0 and -3

    equation.evaluate(root0).isZero shouldBe true
  }

  it should "handle equation with both coefficients negative" in {
    val equation = QuadraticEquation(Rational(-4), Rational(-3))
    val root0 = equation.solve(0)
    val root1 = equation.solve(1)

    root0 shouldBe a[QuadraticSolution]
    root1 shouldBe a[QuadraticSolution]

    equation.evaluate(root0).isZero shouldBe true
    equation.evaluate(root1).isZero shouldBe true
  }

  it should "handle very small coefficients" in {
    val equation = QuadraticEquation(Rational(1, 100), Rational(1, 1000))
    val root0 = equation.solve(0)
    root0 shouldBe a[Complex]

    equation.evaluate(root0) match {
      case Complex(c) =>
        // TODO this is not exactly a resoundingly confident test. Something is up!
        c.modulus.signum(0.0001) shouldBe -1
      case x =>
        x.isZero shouldBe true
    }
  }

  it should "handle very large coefficients" in {
    val equation = QuadraticEquation(Rational(1000), Rational(100000))
    val root0 = equation.solve(0)
    root0 shouldBe a[QuadraticSolution]
  }
}

// =========================
// LinearEquation Tests
// =========================

class LinearEquationSpec extends AnyFlatSpec with Matchers {

  behavior of "LinearEquation"

  // =========================
  // Constructor and Basic Properties
  // =========================

  it should "create a linear equation with rational coefficient" in {
    val eq = LinearEquation(Rational.one)
    eq.r shouldBe Rational.one
  }

  it should "create linear equation with negative coefficient" in {
    val eq = LinearEquation(Rational(-5))
    eq.r shouldBe Rational(-5)
  }

  it should "create linear equation with fractional coefficient" in {
    val eq = LinearEquation(Rational(3, 4))
    eq.r shouldBe Rational(3, 4)
  }

  // =========================
  // Branches
  // =========================

  it should "report 1 branch" in {
    val eq = LinearEquation(Rational.two)
    eq.branches shouldBe 1
  }

  // =========================
  // Solve
  // =========================

  it should "solve x + 2 = 0 correctly" in {
    val eq = LinearEquation(Rational.two)
    val sol = eq.solve(0)

    sol shouldBe a[LinearSolution]
    val ls = sol.asInstanceOf[LinearSolution]
    ls.value shouldBe RationalNumber(-2)
  }

  it should "solve x + 0 = 0 correctly" in {
    val eq = LinearEquation(Rational.zero)
    val sol = eq.solve(0)

    sol shouldBe a[LinearSolution]
    val ls = sol.asInstanceOf[LinearSolution]
    ls.value shouldBe RationalNumber.zero
  }

  it should "solve x - 5 = 0 correctly" in {
    val eq = LinearEquation(Rational(-5))
    val sol = eq.solve(0)

    sol shouldBe a[LinearSolution]
    val ls = sol.asInstanceOf[LinearSolution]
    ls.value shouldBe RationalNumber(5)
  }

  it should "solve x + 1/2 = 0 correctly" in {
    val eq = LinearEquation(Rational(1, 2))
    val sol = eq.solve(0)

    sol shouldBe a[LinearSolution]
    val ls = sol.asInstanceOf[LinearSolution]
    ls.value shouldBe RationalNumber(Rational(-1, 2))
  }

  it should "throw exception for invalid branch index 1" in {
    val eq = LinearEquation(Rational.one)

    an[IllegalArgumentException] should be thrownBy {
      eq.solve(1)
    }
  }

  it should "throw exception for invalid branch index -1" in {
    val eq = LinearEquation(Rational.one)

    an[IllegalArgumentException] should be thrownBy {
      eq.solve(-1)
    }
  }

  // =========================
  // Branched Method
  // =========================

  it should "return Literal expression for the single branch" in {
    val eq = LinearEquation(Rational.three)
    val expr = eq.branched(0)

    expr shouldBe a[com.phasmidsoftware.number.expression.expr.Literal]
  }

  // =========================
  // Evaluate
  // =========================

  it should "evaluate LinearSolution correctly" in {
    val eq = LinearEquation(Rational(3))
    val sol = LinearSolution(RationalNumber(5))

    val result = eq.evaluate(sol)
    result shouldBe WholeNumber(8) // 5 + 3
  }

  it should "evaluate LinearSolution with negative values" in {
    val eq = LinearEquation(Rational(-2))
    val sol = LinearSolution(RationalNumber(7))

    val result = eq.evaluate(sol)
    result shouldBe WholeNumber(5) // 7 + (-2)
  }

  it should "evaluate to zero when solution is correct" in {
    val eq = LinearEquation(Rational(4))
    val sol = eq.solve(0).asInstanceOf[LinearSolution]

    val result = eq.evaluate(sol)
    result.isZero shouldBe true
  }

  // =========================
  // Transform
  // =========================

  it should "transform with custom functions" in {
    val eq = LinearEquation(Rational(4))
    val transformed = eq.transform(
      (p, q) => p + q, // Not used for linear equations
      (p, q) => q * Rational.two
    )

    transformed shouldBe LinearEquation(Rational(8)) // 4 * 2
  }

  it should "transform ignoring fP function" in {
    val eq = LinearEquation(Rational(3))
    val transformed = eq.transform(
      (p, q) => Rational(999), // Should be ignored
      (p, q) => q + Rational.one
    )

    transformed shouldBe LinearEquation(Rational(4)) // 3 + 1
  }

  it should "transform with complex function" in {
    val eq = LinearEquation(Rational(6))
    val transformed = eq.transform(
      (p, q) => p, // Ignored
      (p, q) => q / Rational(2)
    )

    transformed shouldBe LinearEquation(Rational(3)) // 6 / 2
  }

  // =========================
  // Scale (multiplication)
  // =========================

  it should "scale equation by positive factor" in {
    val eq = LinearEquation(Rational(3))
    val scaled = eq * Rational(4)

    scaled shouldBe LinearEquation(Rational(12))
  }

  it should "scale equation by negative factor" in {
    val eq = LinearEquation(Rational(2))
    val scaled = eq * Rational(-3)

    scaled shouldBe LinearEquation(Rational(-6))
  }

  it should "scale equation by fractional factor" in {
    val eq = LinearEquation(Rational(10))
    val scaled = eq * Rational(1, 5)

    scaled shouldBe LinearEquation(Rational(2))
  }

  it should "scale by zero (edge case)" in {
    val eq = LinearEquation(Rational(5))
    val scaled = eq * Rational.zero

    scaled shouldBe LinearEquation(Rational.zero)
  }

  // =========================
  // Invert
  // =========================

  it should "invert equation correctly" in {
    val eq = LinearEquation(Rational(4))
    val inverted = eq.invert

    // For x + r = 0 with solution x = -r
    // Inverted should have solution 1/x = -1/r
    // So equation is x - 1/r = 0
    inverted shouldBe LinearEquation(Rational(-1, 4))
  }

  it should "invert equation with negative coefficient" in {
    val eq = LinearEquation(Rational(-2))
    val inverted = eq.invert

    inverted shouldBe LinearEquation(Rational(1, 2))
  }

  it should "invert equation with fractional coefficient" in {
    val eq = LinearEquation(Rational(1, 3))
    val inverted = eq.invert

    inverted shouldBe LinearEquation(Rational(-3))
  }

  it should "handle inversion of zero coefficient (degenerate case)" in {
    val eq = LinearEquation(Rational.zero)
    eq.invert shouldBe LinearEquation(Rational.negInfinity)
  }

  // =========================
  // ShiftOrigin
  // =========================

  it should "shift origin correctly" in {
    val eq = LinearEquation(Rational(5))
    val shifted = eq.shiftOrigin(Rational(2))

    // Original: x + 5 = 0, solution x = -5
    // After shift by c=2: y + (5-2) = 0, solution y = -3
    // Check: -3 = -5 + 2 ✓
    shifted shouldBe LinearEquation(Rational(3))
  }

  it should "shift origin by negative value" in {
    val eq = LinearEquation(Rational(3))
    val shifted = eq.shiftOrigin(Rational(-4))

    // Original: x + 3 = 0, solution x = -3
    // After shift by c=-4: y + (3-(-4)) = 0 = y + 7 = 0, solution y = -7
    // Check: -7 = -3 + (-4) ✓
    shifted shouldBe LinearEquation(Rational(7))
  }

  it should "shift origin by zero (identity)" in {
    val eq = LinearEquation(Rational(2))
    val shifted = eq.shiftOrigin(Rational.zero)

    shifted shouldBe eq // No change
  }

  it should "shift origin by fractional value" in {
    val eq = LinearEquation(Rational(5))
    val shifted = eq.shiftOrigin(Rational(1, 2))

    // y + (5 - 1/2) = 0, y + 9/2 = 0
    shifted shouldBe LinearEquation(Rational(9, 2))
  }

  it should "compose shift operations correctly" in {
    val eq = LinearEquation(Rational(10))
    val shift1 = eq.shiftOrigin(Rational(3))
    val shift2 = shift1.shiftOrigin(Rational(2))
    val directShift = eq.shiftOrigin(Rational(5))

    shift2 shouldBe directShift
  }

  it should "verify shift preserves solution relationship" in {
    val eq = LinearEquation(Rational(7))
    val originalSol = eq.solve(0).asInstanceOf[LinearSolution]

    val c = Rational(3)
    val shifted = eq.shiftOrigin(c)
    val shiftedSol = shifted.solve(0).asInstanceOf[LinearSolution]

    // originalSol.value + c should equal shiftedSol.x
    originalSol.value.add(RationalNumber(c)) shouldBe scala.util.Success(shiftedSol.normalize)
  }

  // =========================
  // Equality and CanEqual
  // =========================

  it should "support canEqual for LinearEquation instances" in {
    val eq = LinearEquation(Rational(1))
    eq.canEqual(LinearEquation(Rational(2))) shouldBe true
  }

  it should "reject canEqual for non-LinearEquation instances" in {
    val eq = LinearEquation(Rational(1))
    eq.canEqual("not a linear equation") shouldBe false
    eq.canEqual(42) shouldBe false
  }

  it should "be equal to itself" in {
    val eq = LinearEquation(Rational(3))
    eq shouldBe eq
  }

  it should "be equal to another equation with same coefficient" in {
    val eq1 = LinearEquation(Rational(5))
    val eq2 = LinearEquation(Rational(5))
    eq1 shouldBe eq2
  }

  it should "not be equal to equation with different coefficient" in {
    val eq1 = LinearEquation(Rational(3))
    val eq2 = LinearEquation(Rational(5))
    eq1 should not be eq2
  }

  // =========================
  // toString/render
  // =========================

  it should "render correctly for positive coefficient" in {
    val eq: LinearEquation = LinearEquation(Rational(3))
    val rendered = eq.render
    rendered should include("x")
    rendered should include("+")
  }

  it should "render correctly for negative coefficient" in {
    val eq = LinearEquation(Rational(-4))
    val rendered = eq.render
    rendered should include("x")
    rendered should include("-")
  }

  // =========================
  // Edge Cases
  // =========================

  it should "handle very small coefficient" in {
    val eq = LinearEquation(Rational(1, 1000))
    val sol = eq.solve(0)

    sol shouldBe a[LinearSolution]
    eq.evaluate(sol.asInstanceOf[LinearSolution]).isZero shouldBe true
  }

  it should "handle very large coefficient" in {
    val eq = LinearEquation(Rational(1000000))
    val sol = eq.solve(0)

    sol shouldBe a[LinearSolution]
    val ls = sol.asInstanceOf[LinearSolution]
    ls.value shouldBe RationalNumber(Rational(-1000000))
  }
}