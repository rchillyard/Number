/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.eager.{QuadraticSolution, WholeNumber}
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.expression.algebraic.QuadraticEquation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class QuadraticRootSpec extends AnyFlatSpec with should.Matchers {

  behavior of "QuadraticRoot"

  it should "create quadratic root" in {
    // x² - 3x + 2 in monic form: p = -3, q = 2
    val equation = QuadraticEquation(Rational(-3), Rational(2))
    val root = QuadraticRoot(equation, 0) // positive branch
    root.equation shouldBe equation
    root.branch shouldBe 0
  }

  behavior of "normalize - simple integer roots"

  it should "normalize x² - 3x + 2 (roots: 1, 2) - positive branch" in {
    // x² - 3x + 2 = 0 → x = 2 or x = 1
    val equation = QuadraticEquation(Rational(-3), Rational(2))
    val root = QuadraticRoot(equation, 0)
    val normalized = root.normalize
    normalized shouldBe a[QuadraticSolution]
    val solution: QuadraticSolution = normalized.asInstanceOf[QuadraticSolution]
    solution.base.toDouble shouldBe 1.5 +- 1e-10
    solution.offset.toDouble shouldBe 0.5 +- 1e-10
    solution.branch shouldBe 0
  }

  it should "normalize x² - 3x + 2 - negative branch" in {
    val equation = QuadraticEquation(Rational(-3), Rational(2))
    val root = QuadraticRoot(equation, 1)
    val normalized = root.normalize
    normalized shouldBe a[QuadraticSolution]
    val solution = normalized.asInstanceOf[QuadraticSolution]
    solution.base.toDouble shouldBe 1.5 +- 1e-10
    solution.offset.toDouble shouldBe 0.5 +- 1e-10
    solution.branch shouldBe 1
  }

  it should "normalize x² - 4 (roots: -2, 2) - positive branch" in {
    // x² + 0x - 4 = 0 → x = ±2
    val equation = QuadraticEquation(Rational(0), Rational(-4))
    val root = QuadraticRoot(equation, 0)
    val normalized = root.normalize
    normalized shouldBe a[WholeNumber]
    val solution = normalized.asInstanceOf[WholeNumber]
    solution.toDouble shouldBe 2.0 +- 1e-10
  }

  it should "normalize x² - 4 - negative branch" in {
    val equation = QuadraticEquation(Rational(0), Rational(-4))
    val root = QuadraticRoot(equation, 1)
    val normalized = root.normalize
    normalized shouldBe a[WholeNumber]
    val solution = normalized.asInstanceOf[WholeNumber]
    solution.toDouble shouldBe -2.0 +- 1e-10
  }

  behavior of "normalize - golden ratio"

  ignore should "normalize x² - x - 1 to golden ratio (positive branch)" in {
    // x² - x - 1 = 0 → φ = (1 + √5)/2
    val equation = QuadraticEquation(Rational(-1), Rational(-1))
    val root = QuadraticRoot(equation, 0)
    val normalized = root.normalize
    normalized shouldBe QuadraticSolution.phi
  }

  ignore should "normalize x² - x - 1 (negative branch)" in {
    val equation = QuadraticEquation(Rational(-1), Rational(-1))
    val root = QuadraticRoot(equation, 1)
    val normalized = root.normalize
    normalized shouldBe a[QuadraticSolution]
    val solution = normalized.asInstanceOf[QuadraticSolution]
    // Negative root: (1 - √5)/2 ≈ -0.618
    solution.offset.toDouble shouldBe -0.618 +- 0.001
  }

  behavior of "normalize - irrational roots"

  ignore should "normalize x² - 2 (roots: ±√2)" in {
    // x² + 0x - 2 = 0 → x = ±√2
    val equation = QuadraticEquation(Rational(0), Rational(-2))
    val root = QuadraticRoot(equation, 0)
    val normalized = root.normalize
    normalized shouldBe a[QuadraticSolution]
    val solution = normalized.asInstanceOf[QuadraticSolution]
    solution.base.toDouble shouldBe Math.sqrt(2.0) +- 1e-10
  }

  ignore should "normalize x² + x - 1 (irrational roots)" in {
    // x² + x - 1 = 0 → x = (-1 ± √5)/2
    val equation = QuadraticEquation(Rational(1), Rational(-1))
    val root = QuadraticRoot(equation, 0)
    val normalized = root.normalize
    normalized shouldBe a[QuadraticSolution]
    val solution = normalized.asInstanceOf[QuadraticSolution]
    // Positive root: (-1 + √5)/2 ≈ 0.618
    solution.base.toDouble shouldBe 0.618 +- 0.001
  }

  behavior of "normalize - rational roots"

  ignore should "normalize 2x² - 5x + 2 converted to monic form" in {
    // Divide by 2: x² - (5/2)x + 1 = 0 → roots: 2 and 1/2
    val equation = QuadraticEquation(Rational(-5, 2), Rational(1))
    val root = QuadraticRoot(equation, 0)
    val normalized = root.normalize
    normalized shouldBe a[QuadraticSolution]
    val solution = normalized.asInstanceOf[QuadraticSolution]
    solution.base.toDouble shouldBe 2.0 +- 1e-10
  }

  behavior of "normalize - double root"

  ignore should "normalize x² - 4x + 4 (double root: 2)" in {
    // x² - 4x + 4 = (x-2)² → x = 2 (multiplicity 2)
    val equation = QuadraticEquation(Rational(-4), Rational(4))
    val root = QuadraticRoot(equation, 0)
    val normalized = root.normalize
    normalized shouldBe a[QuadraticSolution]
    val solution = normalized.asInstanceOf[QuadraticSolution]
    solution.base.toDouble shouldBe 2.0 +- 1e-10
//    solution.multiplicity shouldBe 2
  }

  behavior of "render"

  it should "render quadratic root correctly" in {
    val equation = QuadraticEquation(Rational(-1), Rational(-1))
    val root = QuadraticRoot(equation, 0)
    val rendered = root.render
    rendered should not be empty
  }

  behavior of "materialize"

  it should "materialize to QuadraticSolution" in {
    val equation = QuadraticEquation(Rational(-3), Rational(2))
    val root = QuadraticRoot(equation, 0)
    val normalized = root.normalize
    normalized shouldBe a[QuadraticSolution]
    val materialized = root.materialize
    materialized shouldBe a[QuadraticSolution]
    materialized shouldBe QuadraticSolution(Rational(3, 2), Rational(1, 2), 0)
  }

  it should "have correct isAtomic" in {
    val equation = QuadraticEquation(Rational(-1), Rational(-1))
    val root = QuadraticRoot(equation, 0)
    root.isAtomic shouldBe true
  }
}