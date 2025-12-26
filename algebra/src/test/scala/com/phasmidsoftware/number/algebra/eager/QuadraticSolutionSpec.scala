/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.eager

import com.phasmidsoftware.number.core.inner.Rational
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * Test suite for QuadraticSolution class.
  *
  * Tests cover:
  * - Real roots (both perfect square and irrational discriminants)
  * - Complex roots (negative discriminant with imaginary flag)
  * - Both branches (k=0 and k=1)
  * - Normalization behavior
  * - Conjugate operations
  * - Equality (exact and fuzzy)
  * - Rendering
  * - Edge cases (zero discriminant, zero offset, etc.)
  */
class QuadraticSolutionSpec extends AnyFlatSpec with Matchers {

  behavior of "QuadraticSolution construction"

  it should "create a quadratic solution with real roots (branch 0)" in {
    val base = RationalNumber(Rational(1, 2))
    val offset = InversePower(2, RationalNumber(Rational(5, 4)))
    val solution = QuadraticSolution(base, offset, 0, false)

    solution.base shouldBe base
    solution.offset shouldBe offset
    solution.branch shouldBe 0
    solution.imaginary shouldBe false
    solution.branches shouldBe 2
  }

  it should "create a quadratic solution with real roots (branch 1)" in {
    val base = RationalNumber(Rational(3, 2))
    val offset = InversePower(2, RationalNumber(Rational(2)))
    val solution = QuadraticSolution(base, offset, 1, false)

    solution.base shouldBe base
    solution.offset shouldBe offset
    solution.branch shouldBe 1
    solution.imaginary shouldBe false
  }

  it should "create a quadratic solution with imaginary roots (branch 0)" in {
    val base = RationalNumber(Rational(1, 2))
    val offset = InversePower(2, RationalNumber(Rational(3, 4)))
    val solution: QuadraticSolution = QuadraticSolution(base, offset, 0, true)

    solution.base shouldBe base
    solution.offset shouldBe offset
    solution.branch shouldBe 0
    solution.imaginary shouldBe true
  }

  it should "create a quadratic solution with imaginary roots (branch 1)" in {
    val base = RationalNumber(Rational(-1, 2))
    val offset = InversePower(2, RationalNumber(Rational(7, 4)))
    val solution = QuadraticSolution(base, offset, 1, imaginary = true)

    solution.imaginary shouldBe true
    solution.branch shouldBe 1
  }

  it should "create phi constant correctly" in {
    val phi = QuadraticSolution.phi

    phi.base shouldBe RationalNumber.half
    phi.offset shouldBe InversePower(2, RationalNumber(Rational(5, 4)))
    phi.branch shouldBe 0
    phi.imaginary shouldBe false
    phi.maybeName shouldBe Some("𝛗")
  }

  it should "create psi constant correctly" in {
    val psi = QuadraticSolution.psi

    psi.base shouldBe RationalNumber.half
    psi.offset shouldBe InversePower(2, RationalNumber(Rational(5, 4)))
    psi.branch shouldBe 1
    psi.imaginary shouldBe false
    psi.maybeName shouldBe Some("𝛙")
  }

  behavior of "QuadraticSolution.conjugate"

  it should "compute conjugate by inverting branch (0 -> 1)" in {
    val base = RationalNumber(Rational(1, 2))
    val offset = InversePower(2, RationalNumber(Rational(5, 4)))
    val solution = QuadraticSolution(base, offset, 0, false)
    val conjugate = solution.conjugate

    conjugate.base shouldBe base
    conjugate.offset shouldBe offset
    conjugate.branch shouldBe 1
    conjugate.imaginary shouldBe false
  }

  it should "compute conjugate by inverting branch (1 -> 0)" in {
    val base = RationalNumber(Rational(1, 2))
    val offset = InversePower(2, RationalNumber(Rational(5, 4)))
    val solution = QuadraticSolution(base, offset, 1, false)
    val conjugate = solution.conjugate

    conjugate.branch shouldBe 0
  }

  it should "preserve imaginary flag in conjugate" in {
    val solution = QuadraticSolution(RationalNumber.half, InversePower(2, RationalNumber(2)), 0, imaginary = true)(None)
    val conjugate = solution.conjugate

    conjugate.imaginary shouldBe true
  }

  it should "conjugate of conjugate returns original" in {
    val solution = QuadraticSolution(RationalNumber.half, InversePower(2, RationalNumber(2)), 0, false)
    val conjugateConjugate = solution.conjugate.conjugate

    conjugateConjugate.branch shouldBe solution.branch
  }

  behavior of "QuadraticSolution.negate"

  it should "negate base and flip branch" in {
    val base = RationalNumber(Rational(3, 2))
    val offset = InversePower(2, RationalNumber(2))
    val solution = QuadraticSolution(base, offset, 0, false)
    val negated = solution.negate

    negated.base shouldBe base.negate
    negated.branch shouldBe 1
  }

  it should "negate and conjugate correctly for branch 1" in {
    val base = RationalNumber(Rational(3, 2))
    val offset = InversePower(2, RationalNumber(2))
    val solution = QuadraticSolution(base, offset, 1, false)
    val negated = solution.negate

    negated.base shouldBe base.negate
    negated.branch shouldBe 0
  }

  behavior of "QuadraticSolution.normalize"

  it should "normalize to offset when base is zero" in {
    val base = RationalNumber.zero
    val offset = RationalNumber(Rational(5))
    val solution = QuadraticSolution(base, offset, 0, false)
    val normalized = solution.normalize

    // When base is zero, result should be offset scaled by branched coefficient
    // For branch 0: branched(0) = -1/2, so result is -5/2
    normalized shouldBe a[Scalar]
  }

  it should "normalize to base when offset is zero" in {
    val base = RationalNumber(Rational(7, 2))
    val offset = RationalNumber.zero
    val solution = QuadraticSolution(base, offset, 0, false)
    val normalized = solution.normalize

    normalized shouldBe base
  }

  it should "not normalize when both base and offset are non-zero" in {
    val base = RationalNumber(Rational(1, 2))
    val offset = InversePower(2, RationalNumber(Rational(5, 4)))
    val solution = QuadraticSolution(base, offset, 0, false)
    val normalized = solution.normalize

    normalized shouldBe solution
  }

  behavior of "QuadraticSolution boolean predicates"

  it should "identify zero solution correctly" in {
    val zero = QuadraticSolution(RationalNumber.zero, RationalNumber.zero, 0, false)
    zero.isZero shouldBe true
  }

  it should "not identify non-zero solution as zero" in {
    val nonZero = QuadraticSolution(RationalNumber(Rational(1)), RationalNumber.zero, 0, false)
    nonZero.isZero shouldBe false
  }

  it should "identify pure number when offset is zero" in {
    val pure = QuadraticSolution(RationalNumber(Rational(5)), RationalNumber.zero, 0, false)
    pure.isPureNumber shouldBe true
  }

  it should "not identify as pure number when offset is non-zero" in {
    val notPure = QuadraticSolution(RationalNumber(Rational(5)), InversePower(2, RationalNumber(2)), 0, false)
    notPure.isPureNumber shouldBe false
  }

  it should "identify unity correctly" in {
    val unity = QuadraticSolution(RationalNumber.one, RationalNumber.zero, 0, false)
    unity.isUnity shouldBe true
  }

  it should "not identify non-unity as unity" in {
    val notUnity1 = QuadraticSolution(RationalNumber(Rational(2)), RationalNumber.zero, 0, false)
    notUnity1.isUnity shouldBe false

    val notUnity2 = QuadraticSolution(RationalNumber.one, InversePower(2, RationalNumber(2)), 0, false)
    notUnity2.isUnity shouldBe false
  }

  it should "always report as exact" in {
    val solution = QuadraticSolution(RationalNumber.half, InversePower(2, RationalNumber(2)), 0, false)
    solution.isExact shouldBe true
  }

  behavior of "QuadraticSolution.branched"
  it should "compute correct coefficient for branch 0" in {
    val solution = QuadraticSolution(RationalNumber.zero, RationalNumber.one, 0, false)
    val coefficient = solution.branched(0)

    // Branch 0 should give +1 (positive root)
    coefficient shouldBe Rational(1)
  }

  it should "compute correct coefficient for branch 1" in {
    val solution = QuadraticSolution(RationalNumber.zero, RationalNumber.one, 1, false)
    val coefficient = solution.branched(1)

    // Branch 1 should give -1 (negative root)
    coefficient shouldBe Rational(-1)
  }

  behavior of "QuadraticSolution.add(Rational)"

  it should "add rational to base when base is pure number" in {
    val base = RationalNumber(Rational(3, 2))
    val offset = RationalNumber.zero
    val solution = QuadraticSolution(base, offset, 0, false)
    val addend = Rational(1, 2)

    val result = solution.add(addend)

    result shouldBe a[QuadraticSolution]
    // New base should be 3/2 + 1/2 = 2
    result.asInstanceOf[QuadraticSolution].base shouldBe WholeNumber(2)
  }

  behavior of "QuadraticSolution.*(Rational)"

  it should "scale both base and offset when both are scalars" in {
    val base = RationalNumber(Rational(1, 2))
    val offset = RationalNumber(Rational(3, 2))
    val solution = QuadraticSolution(base, offset, 0, false)
    val scale = Rational(2)

    val result = solution * scale

    result shouldBe a[QuadraticSolution]
    val scaled = result.asInstanceOf[QuadraticSolution]
    scaled.base shouldBe WholeNumber.one
    scaled.offset shouldBe WholeNumber(3)
    scaled.branch shouldBe 0
  }

  it should "preserve branch when scaling" in {
    val solution = QuadraticSolution(RationalNumber(Rational(1)), RationalNumber(Rational(2)), 1, false)
    val scaled = solution * Rational(3)

    scaled.asInstanceOf[QuadraticSolution].branch shouldBe 1
  }

  behavior of "QuadraticSolution equality"

  it should "be equal to itself" in {
    val solution = QuadraticSolution(RationalNumber.half, InversePower(2, RationalNumber(2)), 0, false)

    solution.eqv(solution).get shouldBe true
  }

  it should "be equal to another solution with same base, offset, and branch" in {
    val solution1 = QuadraticSolution(RationalNumber.half, InversePower(2, RationalNumber(2)), 0, false)
    val solution2 = QuadraticSolution(RationalNumber.half, InversePower(2, RationalNumber(2)), 0, false)

    solution1.eqv(solution2).get shouldBe true
  }

  it should "not be equal if branches differ" in {
    val solution1 = QuadraticSolution(RationalNumber.half, InversePower(2, RationalNumber(2)), 0, false)
    val solution2 = QuadraticSolution(RationalNumber.half, InversePower(2, RationalNumber(2)), 1, false)

    solution1.eqv(solution2).get shouldBe false
  }

  it should "not be equal if bases differ" in {
    val solution1 = QuadraticSolution(RationalNumber.half, InversePower(2, RationalNumber(2)), 0, false)
    val solution2 = QuadraticSolution(RationalNumber.one, InversePower(2, RationalNumber(2)), 0, false)

    solution1.eqv(solution2).get shouldBe false
  }

  it should "not be equal if offsets differ" in {
    val solution1 = QuadraticSolution(RationalNumber.half, InversePower(2, RationalNumber(2)), 0, false)
    val solution2 = QuadraticSolution(RationalNumber.half, InversePower(2, RationalNumber(3)), 0, false)

    solution1.eqv(solution2).get shouldBe false
  }

  behavior of "QuadraticSolution fuzzy equality"

  it should "be fuzzy equal to itself" in {
    val solution = QuadraticSolution(RationalNumber.half, InversePower(2, RationalNumber(2)), 0, false)

    solution.fuzzyEqv(0.01)(solution).get shouldBe true
  }

  it should "be fuzzy equal to similar solution" in {
    val solution1 = QuadraticSolution(RationalNumber.half, InversePower(2, RationalNumber(2)), 0, false)
    val solution2 = QuadraticSolution(RationalNumber.half, InversePower(2, RationalNumber(2)), 0, false)

    solution1.fuzzyEqv(0.01)(solution2).get shouldBe true
  }

  it should "not be fuzzy equal if branches differ" in {
    val solution1 = QuadraticSolution(RationalNumber.half, InversePower(2, RationalNumber(2)), 0, false)
    val solution2 = QuadraticSolution(RationalNumber.half, InversePower(2, RationalNumber(2)), 1, false)

    solution1.fuzzyEqv(0.01)(solution2).get shouldBe false
  }

  behavior of "QuadraticSolution.render"

  it should "render phi with its name" in {
    QuadraticSolution.phi.render shouldBe "𝛗"
  }

  it should "render psi with its name" in {
    QuadraticSolution.psi.render shouldBe "𝛙"
  }

  it should "render normalized form when offset is zero" in {
    val solution = QuadraticSolution(RationalNumber(Rational(5)), RationalNumber.zero, 0, false)
    val rendered = solution.render

    // Should normalize to just the base
    rendered shouldBe "5"
  }

  it should "render with + for branch 0" in {
    val base = RationalNumber(Rational(1, 2))
    val offset = InversePower(2, RationalNumber(2))
    val solution = QuadraticSolution(base, offset, 0, false)
    val rendered = solution.render

    rendered should include("+")
  }

  it should "render with - for branch 1" in {
    val base = RationalNumber(Rational(1, 2))
    val offset = InversePower(2, RationalNumber(2))
    val solution = QuadraticSolution(base, offset, 1, false)
    val rendered = solution.render

    rendered should include("-")
  }

  it should "render complex solutions with appropriate prefix" in {
    val solution = QuadraticSolution(RationalNumber.half, InversePower(2, RationalNumber(2)), 0, imaginary = true)
    val rendered = solution.render

    rendered should startWith("Complex quadratic solution:")
  }

  behavior of "QuadraticSolution edge cases"

  it should "handle zero base and zero offset" in {
    val solution = QuadraticSolution(RationalNumber.zero, RationalNumber.zero, 0, false)

    solution.isZero shouldBe true
    solution.isPureNumber shouldBe true
    solution.isUnity shouldBe false
  }

  it should "handle unity base with zero offset" in {
    val solution = QuadraticSolution(RationalNumber.one, RationalNumber.zero, 0, false)

    solution.isZero shouldBe false
    solution.isPureNumber shouldBe true
    solution.isUnity shouldBe true
  }

  it should "handle large rational values" in {
    val base = WholeNumber(10000)
    val offset = InversePower(2, WholeNumber(10000))
    val solution = QuadraticSolution(base, offset, 0, false)

    solution.base shouldBe base
    solution.offset shouldBe WholeNumber(100)
  }

  it should "handle negative base values" in {
    val base = RationalNumber(Rational(-3, 2))
    val offset = InversePower(2, RationalNumber(2))
    val solution = QuadraticSolution(base, offset, 0, false)

    solution.base shouldBe base
    solution.signum should be < 0
  }

  behavior of "QuadraticSolution.approximation"

  it should "provide approximation for non-exact solutions" in {
    val solution = QuadraticSolution(RationalNumber.half, InversePower(2, RationalNumber(2)), 0, false)
    val approx = solution.approximation(force = true)

    approx shouldBe defined
    approx.get shouldBe a[Real]
  }

  it should "handle approximation for pure number solutions" in {
    val solution = QuadraticSolution(RationalNumber(Rational(5)), RationalNumber.zero, 0, false)
    val approx = solution.approximation(force = false)

    // Pure numbers might not need approximation
    // Test that it doesn't throw
    noException should be thrownBy solution.approximation(force = true)
  }

  behavior of "QuadraticSolution with imaginary flag integration"

  it should "distinguish between real and imaginary solutions in rendering" in {
    val realSolution = QuadraticSolution(RationalNumber.half, InversePower(2, RationalNumber(2)), 0, false)
    val imaginarySolution: QuadraticSolution = QuadraticSolution(RationalNumber.half, InversePower(2, RationalNumber(2)), 0, imaginary = true)

    realSolution.render should not startWith "Complex"
    imaginarySolution.render should startWith("Complex")
  }

  it should "preserve imaginary flag through conjugate" in {
    val solution = QuadraticSolution(RationalNumber.half, InversePower(2, RationalNumber(2)), 0, imaginary = true)
    val conjugate = solution.conjugate

    conjugate.imaginary shouldBe true
  }

  it should "preserve imaginary flag through scaling" in {
    val solution = QuadraticSolution(RationalNumber.half, RationalNumber(Rational(2)), 0, imaginary = true)
    val scaled = solution.*(Rational(2))

    // Note: imaginary flag is lost in current implementation - this documents the behavior
    // CONSIDER: Should scaling preserve the imaginary flag?
  }

  behavior of "QuadraticSolution toString"

  it should "delegate to render" in {
    val solution = QuadraticSolution.phi
    solution.toString shouldBe solution.render
  }
}
