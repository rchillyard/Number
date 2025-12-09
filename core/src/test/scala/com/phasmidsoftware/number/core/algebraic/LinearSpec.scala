/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.algebraic

import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.numerical.{FuzzyEquality, Real}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LinearSpec extends AnyFlatSpec with Matchers with FuzzyEquality {

  behavior of "Linear"
  val half: Algebraic = Algebraic.half
  val one: Algebraic = Algebraic_Linear(LinearEquation(Rational.negOne))
  it should "add 1" in {
    val target = half
    target.add(target) shouldBe one
  }
  it should "branches" in {
    half.equation.branches shouldBe 1
  }
  it should "isExact" in {
    half.isExact shouldBe true
  }
  it should "render" in {
    half.render shouldBe "½"
  }
  it should "approximation" in {
    val approximation = half.approximation
    approximation.isDefined shouldBe true
    approximation.get.isSame(Real(0.5)) shouldBe true
  }
  it should "signum" in {
    half.signum shouldBe 1
  }
  it should "abs" in {
    val target = Algebraic_Linear(LinearEquation(Rational.half))
    val actual = target.abs
    actual shouldBe Real(Rational.half)
  }
  it should "scale" in {
    val actual = half.scale(2)
    val expected = Algebraic_Linear(LinearEquation(-1))
    actual shouldBe expected
  }
  it should "negate" in {
    val target = Algebraic_Linear(LinearEquation(Rational.half))
    target.negate shouldBe half
  }
  it should "product" in {
    // TODO implement test
  }
  it should "solutionSquared" in {
    // TODO implement test
  }
  it should "invert" in {
    val actual = half.invert
    actual shouldBe Real(Rational.two)
  }
  it should "add 2" in {
    // TODO implement test
  }
  it should "multiply" in {
    // TODO implement test
  }
  it should "power 2" in {
    // TODO implement test
  }
  it should "apply(Solution) to get half" in {
    val target = half
    val solution: LinearSolution = target.solve.asInstanceOf[LinearSolution]
    val algebraic = Algebraic_Linear.create(solution)
    algebraic.get shouldBe target
  }
}
