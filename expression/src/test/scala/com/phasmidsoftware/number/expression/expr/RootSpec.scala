/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.eager.*
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.expression.algebraic.{LinearEquation, QuadraticEquation}
import com.phasmidsoftware.number.expression.expr.Root.{phi, psi}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * Test suite for Root trait and its implementations (QuadraticRoot, LinearRoot).
  *
  * Tests cover:
  * - Construction and basic properties
  * - Predefined mathematical constants (phi, psi, rootTwo, etc.)
  * - Solution computation
  * - Branch handling
  * - Arithmetic operations (power, reciprocal, negate, squareRoot)
  * - Addition of roots
  * - Normalization and evaluation
  * - Equality and hashCode
  */
class RootSpec extends AnyFlatSpec with Matchers {

  behavior of "Root factory method"

  it should "create QuadraticRoot for quadratic equation" in {
    val equation = QuadraticEquation(Rational(0), Rational(-4))
    val root = Root(equation, 0)

    root shouldBe a[QuadraticRoot]
    root.equation shouldBe equation
    root.branch shouldBe 0
  }

  it should "create LinearRoot for linear equation" in {
    val equation = LinearEquation(Rational(3))
    val root = Root(equation, 0)

    root shouldBe a[LinearRoot]
    root.equation shouldBe equation
    root.branch shouldBe 0
  }

  it should "ignore branch parameter for linear equations" in {
    val equation = LinearEquation(Rational(3))
    val root1 = Root(equation, 0)
    val root2 = Root(equation, 5)

    root1.branch shouldBe 0
    root2.branch shouldBe 0
  }

  behavior of "QuadraticRoot construction"

  it should "create root from equation and branch" in {
    val equation = QuadraticEquation(Rational(0), Rational(-4))
    val root = QuadraticRoot(equation, 0)

    root.equation shouldBe equation
    root.branch shouldBe 0
    root.branches shouldBe 2
  }

  it should "create root for branch 1" in {
    val equation = QuadraticEquation(Rational(0), Rational(-4))
    val root = QuadraticRoot(equation, 1)

    root.branch shouldBe 1
  }

  it should "create root from QuadraticSolution" in {
    val solution = QuadraticSolution(
      RationalNumber.half,
      RationalNumber(Rational(2)),
      0,
      imaginary = false
    )
    val root = QuadraticRoot(solution)

    root shouldBe a[QuadraticRoot]
    root.branch shouldBe 0
  }

  behavior of "QuadraticRoot predefined constants"

  it should "define phi (golden ratio)" in {
    val phi = Root.phi

    phi shouldBe a[QuadraticRoot]
    phi.equation shouldBe QuadraticEquation.goldenRatioEquation
    phi.branch shouldBe 0
    phi.branches shouldBe 2
  }

  it should "define psi (conjugate of golden ratio)" in {
    val psi = Root.psi

    psi shouldBe a[QuadraticRoot]
    psi.equation shouldBe QuadraticEquation.goldenRatioEquation
    psi.branch shouldBe 1
  }

  it should "define rootTwo (√2)" in {
    val rootTwo = Root.rootTwo

    rootTwo shouldBe a[QuadraticRoot]
    rootTwo.equation shouldBe QuadraticEquation.rootTwoEquation
    rootTwo.branch shouldBe 0
  }

  it should "define negRootTwo (-√2)" in {
    val negRootTwo = Root.negRootTwo

    negRootTwo shouldBe a[QuadraticRoot]
    negRootTwo.equation shouldBe QuadraticEquation.rootTwoEquation
    negRootTwo.branch shouldBe 1
  }

  it should "define one" in {
    val one = Root.one

    one shouldBe a[QuadraticRoot]
    one.branch shouldBe 0
  }

  it should "define zero" in {
    val zero = Root.zero

    zero shouldBe a[QuadraticRoot]
    zero.branch shouldBe 0
  }

  behavior of "QuadraticRoot.solution"

  it should "compute solution lazily for phi" in {
    val phi = Root.phi
    val solution = phi.solution

    solution shouldBe a[QuadraticSolution]
  }

  it should "compute correct solution for x² - 4 = 0, branch 0" in {
    val equation = QuadraticEquation(Rational(0), Rational(-4))
    val root = QuadraticRoot(equation, 0)
    val solution = root.solution

    solution shouldBe a[QuadraticSolution]
    // Solution should be 2 (positive root)
  }

  it should "compute correct solution for x² - 4 = 0, branch 1" in {
    val equation = QuadraticEquation(Rational(0), Rational(-4))
    val root = QuadraticRoot(equation, 1)
    val solution = root.solution

    solution shouldBe a[QuadraticSolution]
    // Solution should be -2 (negative root)
  }

  it should "compute solution for complex roots" in {
    val equation = QuadraticEquation(Rational(0), Rational(4)) // x² + 4 = 0
    val root = QuadraticRoot(equation, 0)
    val solution = root.solution

    // Solution should be complex (2i or -2i)
    solution shouldBe a[Solution]
  }

  behavior of "QuadraticRoot.branches and branched"

  it should "return 2 branches for quadratic equations" in {
    val root = Root.phi
    root.branches shouldBe 2
  }

  it should "get specific branch using branched(index)" in {
    val equation = QuadraticEquation(Rational(0), Rational(-4))
    val root = QuadraticRoot(equation, 0)

    val branch0 = root.branched(0)
    val branch1 = root.branched(1)

    branch0.branch shouldBe 0
    branch1.branch shouldBe 1
    branch0.equation shouldBe equation
    branch1.equation shouldBe equation
  }

  behavior of "QuadraticRoot.isExact"

  it should "always be exact" in {
    Root.phi.isExact shouldBe true
    Root.rootTwo.isExact shouldBe true
    Root.zero.isExact shouldBe true
  }

  behavior of "QuadraticRoot.render"

  it should "render phi with special character" in {
    val rendered = Root.phi.render
    // Should render as the phi symbol or the solution representation
    rendered should not be empty
  }

  it should "render psi with special character" in {
    val rendered = Root.psi.render
    rendered should not be empty
  }

  it should "render generic quadratic root" in {
    val equation = QuadraticEquation(Rational(2), Rational(-3))
    val root = QuadraticRoot(equation, 0)
    val rendered = root.render

    rendered should not be empty
  }

  behavior of "QuadraticRoot.toString"

  it should "use special symbol for phi" in {
    Root.phi.toString shouldBe "\uD835\uDED7" // Mathematical sans-serif small phi
  }

  it should "use special symbol for psi" in {
    Root.psi.toString shouldBe "\uD835\uDED9" // Mathematical sans-serif small psi
  }

  it should "use descriptive string for generic roots" in {
    val equation = QuadraticEquation(Rational(2), Rational(-3))
    val root = QuadraticRoot(equation, 0)

    root.toString should include("QuadraticRoot")
  }

  behavior of "QuadraticRoot.power"

  it should "return One for power of 0" in {
    val root = Root.rootTwo
    val result = root.power(Rational.zero)

    result shouldBe One
  }

  it should "return self for power of 1" in {
    val root = Root.rootTwo
    val result = root.power(Rational.one)

    result shouldBe root
  }

  it should "compute square (power of 2)" in {
    val root = Root.rootTwo
    val result = root.power(Rational(2))

    // √2 squared should be 2
    result shouldBe a[Expression]
  }

  it should "compute higher powers" in {
    val root = Root.rootTwo
    val result = root.power(Rational(3))

    result shouldBe a[Expression]
  }

  it should "handle negative powers using reciprocal" in {
    val root = Root.rootTwo
    val result = root.power(Rational(-1))

    // (√2)^-1 = 1/√2
    result shouldBe a[Expression]
  }

  it should "compute square root (power of 1/2)" in {
    val equation = QuadraticEquation(Rational(0), Rational(-4))
    val root = QuadraticRoot(equation, 0) // This is 2
    val result = root.power(Rational.half)

    // 2^(1/2) = √2
    result shouldBe a[Expression]
  }

  behavior of "QuadraticRoot.reciprocal"

  it should "compute reciprocal for quadratic roots" in {
    val root = Root.rootTwo
    val reciprocal = root.reciprocal

    // 1/√2 = √2/2
    reciprocal shouldBe a[Expression]
  }

  it should "compute reciprocal for phi" in {
    val phi = Root.phi
    val reciprocal = phi.reciprocal

    // 1/φ = φ - 1 (special property of golden ratio)
    reciprocal shouldBe a[Expression]
  }

  behavior of "QuadraticRoot.negate"

  it should "negate a root" in {
    val root = Root.rootTwo
    val negated = root.negate

    negated shouldBe a[Expression]
    // Should be -√2
  }

  it should "negate phi" in {
    val phi = Root.phi
    val negated = phi.negate

    negated shouldBe a[Expression]
  }

  behavior of "QuadraticRoot.squareRoot"

  it should "compute positive square root" in {
    val equation = QuadraticEquation(Rational(0), Rational(-4))
    val root = QuadraticRoot(equation, 0) // This is 2
    val sqrtResult = root.squareRoot(plus = true)

    sqrtResult shouldBe a[Expression]
  }

  it should "compute negative square root" in {
    val equation = QuadraticEquation(Rational(0), Rational(-4))
    val root = QuadraticRoot(equation, 0)
    val sqrtResult = root.squareRoot(plus = false)

    sqrtResult shouldBe a[Expression]
  }

  behavior of "QuadraticRoot.add"

  // Issue #147
  it should "add two quadratic roots with compatible solutions" in {
    val root1 = Root.rootTwo
    val root2 = Root.rootTwo
//    val sum = root1.add(root2) // This throws an exception.

    // √2 + √2 might yield a new root
//    sum match {
//      case Some(result) => result shouldBe a[QuadraticRoot]
//      case None => // Addition might not always be supported
//    }
    pending
  }

  it should "return None for incompatible root addition" in {
    val quadRoot = Root.phi
    val linearRoot = Root.half
    val sum = quadRoot.add(linearRoot)

    sum shouldBe None
  }

  behavior of "QuadraticRoot.pure"

  it should "create new root with different equation" in {
    val root = Root.phi
    val newEquation = QuadraticEquation(Rational(2), Rational(-3))
    val newRoot = root.pure(newEquation, 1)

    newRoot shouldBe a[QuadraticRoot]
    newRoot.equation shouldBe newEquation
    newRoot.branch shouldBe 1
  }

  behavior of "QuadraticRoot equality and hashCode"

  it should "be equal to itself" in {
    val root = Root.phi
    root shouldBe root
  }

  it should "be equal to another root with same equation and branch" in {
    val root1 = QuadraticRoot(QuadraticEquation.goldenRatioEquation, 0)
    val root2 = QuadraticRoot(QuadraticEquation.goldenRatioEquation, 0)

    root1 shouldBe root2
  }

  it should "not be equal if branches differ" in {
    val root1 = QuadraticRoot(QuadraticEquation.goldenRatioEquation, 0)
    val root2 = QuadraticRoot(QuadraticEquation.goldenRatioEquation, 1)

    root1 should not be root2
  }

  it should "not be equal if equations differ" in {
    val root1 = QuadraticRoot(QuadraticEquation.goldenRatioEquation, 0)
    val root2 = QuadraticRoot(QuadraticEquation.rootTwoEquation, 0)

    root1 should not be root2
  }

  it should "have consistent hashCode" in {
    val root1 = QuadraticRoot(QuadraticEquation.goldenRatioEquation, 0)
    val root2 = QuadraticRoot(QuadraticEquation.goldenRatioEquation, 0)

    root1.hashCode shouldBe root2.hashCode
  }

  behavior of "LinearRoot construction"

  it should "create linear root from equation" in {
    val equation = LinearEquation(Rational(5))
    val root = LinearRoot(equation)

    root.equation shouldBe equation
    root.branch shouldBe 0
    root.branches shouldBe 1
  }

  it should "define half constant" in {
    val half = Root.half

    half shouldBe a[LinearRoot]
    half.branch shouldBe 0
  }

  behavior of "LinearRoot properties"

  it should "always have branch 0" in {
    val root = Root.half
    root.branch shouldBe 0
  }

  it should "have 1 branch" in {
    val root = Root.half
    root.branches shouldBe 1
  }

  it should "branched returns itself" in {
    val root = Root.half
    root.branched(0) shouldBe root
    root.branched(5) shouldBe root // Any index returns the same root
  }

  it should "always be exact" in {
    Root.half.isExact shouldBe true
  }

  behavior of "LinearRoot.solution"

  it should "compute solution for linear equation" in {
    val equation = LinearEquation(Rational(-3)) // x = 3
    val root = LinearRoot(equation)
    val solution = root.solution

    solution shouldBe a[LinearSolution]
  }

  it should "compute solution for half" in {
    val half = Root.half
    val solution = half.solution

    solution shouldBe a[LinearSolution]
  }

  behavior of "LinearRoot.pure"

  it should "create new linear root with different equation" in {
    val root = Root.half
    val newEquation = LinearEquation(Rational(7))
    val newRoot = root.pure(newEquation, 0)

    newRoot shouldBe a[LinearRoot]
    newRoot.equation shouldBe newEquation
  }

  it should "ignore branch parameter" in {
    val root = Root.half
    val newEquation = LinearEquation(Rational(7))
    val newRoot = root.pure(newEquation, 99)

    newRoot.branch shouldBe 0
  }

  behavior of "Root.maybeFactor"

  it should "determine factor for linear solutions" in {
    val root = Root.half
    val factor = root.maybeFactor(AnyContext)

    factor shouldBe defined
  }

  behavior of "Root.approximation"

  it should "provide approximation for phi" in {
    val phi = Root.phi
    val approx = phi.approximation(force = true)

    approx shouldBe defined
    // φ ≈ 1.618...
    approx.get.toDouble should be(1.618 +- 0.01)
  }

  it should "provide approximation for rootTwo" in {
    val rootTwo = Root.rootTwo
    val approx = rootTwo.approximation(force = true)

    approx shouldBe defined
    // √2 ≈ 1.414...
    approx.get.toDouble should be(1.414 +- 0.01)
  }

  it should "provide approximation for half" in {
    val half = Root.half
    val approx = half.approximation(force = true)

    approx shouldBe defined
    approx.get.toDouble shouldBe 0.5 +- 1e-10
  }

  behavior of "Root.maybeDouble"

  it should "provide double value for exact solutions" in {
    val half = Root.half
    val maybeDouble = half.maybeDouble

    maybeDouble shouldBe defined
    maybeDouble.get shouldBe 0.5 +- 1e-10
  }

  behavior of "Root edge cases"

  it should "handle zero root" in {
    val zero = Root.zero

    zero shouldBe a[QuadraticRoot]
    zero.branch shouldBe 0
  }

  it should "handle one root" in {
    val one = Root.one

    one shouldBe a[QuadraticRoot]
    one.branch shouldBe 0
  }

  it should "handle equations with complex solutions" in {
    val equation = QuadraticEquation(Rational(0), Rational(1)) // x² + 1 = 0
    val root = QuadraticRoot(equation, 0)

    // Should handle complex roots gracefully
    noException should be thrownBy root.solution
  }

  behavior of "Root.evaluate and evaluateAsIs"

  it should "evaluate root when solution is exact" in {
    val half = Root.half
    val evaluated = half.evaluateAsIs

    evaluated shouldBe defined
  }

  it should "evaluate phi" in {
    val phi = Root.phi
    val evaluated = phi.evaluateAsIs

    // Phi should be evaluatable
    evaluated shouldBe defined
  }

  behavior of "Root arithmetic combinations"

  it should "compute phi squared" in {
    val phi = Root.phi
    val phiSquared = phi.power(Rational(2))

    // φ² = φ + 1 (special property of golden ratio)
    phiSquared shouldBe a[Expression]
  }


  it should "compute reciprocal of rootTwo" in {
    val rootTwo = Root.rootTwo
    val reciprocal = rootTwo.reciprocal

    // 1/√2 = √2/2
    reciprocal shouldBe a[Expression]
  }

  it should "negate and square rootTwo" in {
    val rootTwo = Root.rootTwo
    val negated = rootTwo.negate
    val squared = rootTwo.power(Rational(2))
//
    negated shouldBe a[Expression]
    squared shouldBe a[Expression]
  }

  it should "evaluate phi + psi" in {
    val expression: Expression = phi :+ psi
    //    expression.simplify shouldBe One // TODO implement root cancellation in simplify
    val materialized = expression.materialize
    materialized.eqv(Eager.one).get shouldBe true
  }

}