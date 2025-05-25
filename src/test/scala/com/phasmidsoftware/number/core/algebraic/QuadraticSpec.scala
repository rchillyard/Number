/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.algebraic

import com.phasmidsoftware.number.core.Number.{negOne, one, two}
import com.phasmidsoftware.number.core.algebraic.Algebraic_Quadratic.{phi, psi, zero}
import com.phasmidsoftware.number.core.inner.{Rational, SquareRoot, Value}
import com.phasmidsoftware.number.core.{Constants, Field, FuzzyEquality, Real}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class QuadraticSpec extends AnyFlatSpec with Matchers with FuzzyEquality {

  behavior of "Quadratic"

  it should "normalize" in {
    val expected: Real = Constants.phi
    val phi = Algebraic_Quadratic.phi.normalize
    println(s"phi = $phi, expected = $expected")
    phi.isSame(expected) shouldBe true
    val psi: Field = Algebraic_Quadratic.psi.normalize
    println(s"psi = $psi")
    psi.isSame(Real(-0.618033)) shouldBe true
  }

  it should "branches" in {
    phi.equation.branches shouldBe 2
  }

  it should "isExact" in {
    phi.isExact shouldBe true
    psi.isExact shouldBe true
  }

  it should "value 1" in {
    val p: Algebraic_Quadratic = phi
    println(p.render)
    val solution: Solution = p.solve
    solution.base shouldBe Value.fromRational(Rational.half)
    solution.negative shouldBe false
    solution.factor shouldBe SquareRoot
    import Rational._
    solution.offset shouldBe Value.fromRational(r"5/4")
  }

  it should "value 2" in {
    val otherEquation = Quadratic(1, -1)
    val target = Algebraic_Quadratic(otherEquation, pos = true)
    val expected: Solution = phi.equation.invert.solve(0)
    val solution: Solution = target.solve
    solution shouldBe expected
    println(s"solution = $solution, expected = $expected")
    println(s"solution = ${solution.asField}")
    target.normalize.isSame(Real(0.618033)) shouldBe true
  }

//  it should "value 3" in {
//    val equation = Quadratic(Rational.three.negate, Rational.one)
//    val target = Algebraic_Quadratic(equation, pos = true)
//    val (n1, r, (n2)) = target.function
//    n1 shouldBe Real(Rational(3, 2))
//    val resultingValue: Field = r * n2
//    val expected = (root5 divide Real(2))
//    resultingValue shouldBe expected
//  }

  it should "render" in {
    phi.render shouldBe "\uD835\uDED7"
    psi.render shouldBe "\uD835\uDED9"
  }

  it should "approximation" in {
    val approximation = phi.approximation
    approximation.isDefined shouldBe true
    approximation.get.isSame(Real(1.6180339887498948)) shouldBe true
  }

  it should "signum" in {
    phi.signum shouldBe 1
    psi.signum shouldBe -1
  }

  ignore should "abs" in {
    phi.abs shouldBe phi
    val actual = psi.abs
    val expected = psi.scale(-1)
    println(s"psi.abs = $actual, expected = $expected")
    actual shouldBe expected
  }

  it should "scale" in {
    val actual = phi.scale(2)
    val expected = Algebraic_Quadratic(Quadratic(-2, -4), pos = true)
    actual shouldBe expected
  }

  it should "negate" in {
    phi.negate shouldBe Algebraic_Quadratic(Quadratic(1, -1), pos = true)
  }

  it should "product" in {
    val actual = phi * psi
    val expected = phi.equation.q
    actual shouldBe Algebraic_Linear(LinearEquation(Rational.negOne))
  }

  it should "solutionSquared" in {
    val actual: Solution = phi.solutionSquared
    // XXX phi^2 = phi + 1 (see https://en.wikipedia.org/wiki/Golden_ratio)
    val expected: Solution = (phi).solve.add(Rational.one) // NOTE this only works here
    actual shouldBe expected
  }

  it should "invert" in {
    val actual = phi.invert
    // XXX 1/phi = phi - 1 (see https://en.wikipedia.org/wiki/Golden_ratio)
    val expected = phi add Real(-1)
    actual.isSame(expected) shouldBe true
  }

  // TODO find out why this does not work correctly
  ignore should "add phi" in {
    val actual = phi.add(phi)
    val expected = phi.scale(2)
    println(s"phi.scale(2) = ${actual.normalize}, expected = ${expected.normalize}")
    actual shouldBe expected
  }
  it should "add psi" in {
    val actual = phi.add(psi)
    val expected = zero
    println(s"phi.scale(2) = ${actual.normalize}, expected = ${expected.normalize}")
    actual shouldBe expected
  }

  it should "add 1" in {
    println(s"phi = $phi")
    val actual = phi add Real(one)
    println(s"phi add Real(one) = $actual")
    actual.normalize should ===(Constants.phi + Real(1))
    actual.normalize.isSame(Constants.phi + Real(1)) shouldBe true
  }

  it should "add -1" in {
    println(s"phi = $phi")
    val actual = phi add Real(negOne)
    println(s"phi add Real(one) = $actual")
    actual.normalize should ===(Constants.phi + Real(-1))
    actual.normalize.isSame(Constants.phi + Real(-1)) shouldBe true
  }

  it should "add 2" in {
    println(s"phi = $phi")
    val actual = phi add Real(two)
    println(s"phi add Real(one) = $actual")
    actual.normalize should ===(Constants.phi + Real(2))
    actual.normalize.isSame(Constants.phi + Real(2)) shouldBe true
  }

  it should "zero" in {
    val actual = Algebraic_Quadratic.zero.solve
    actual should matchPattern { case QuadraticSolution(Value.zero, Value.zero, SquareRoot, false) => }
  }

  it should "one" in {
    val actual = Algebraic_Quadratic.one.solve
    actual should matchPattern { case QuadraticSolution(Value.one, Value.zero, SquareRoot, false) => }
  }

  it should "power 0" in {
    val actual = phi.power(0)
    val expected = Real(1)
    actual shouldBe (expected)
    actual.normalize shouldBe Constants.one
  }

  it should "power 1" in {
    val actual = phi.power(1)
    val expected = phi
    println(s"phi=$phi; phi.power(1) = $actual")
    actual shouldBe (expected)
    actual.normalize should ===(Constants.phi)
  }

  it should "power 2" in {
    val actual = phi.power(2)
    val expected = phi.square
//    actual shouldBe (expected) // TODO recreate test such that this works.
    actual.normalize should ===(Constants.phi + Real(1))
    actual.normalize.isSame(Constants.phi + Real(1)) shouldBe true
  }

  it should "power 3" in {
    val actual = phi.power(3)
    val scaled: Algebraic = phi.scale(2)
    val expected = scaled.equation.shiftOrigin(Rational.one).solve(0)
    println(s"phi^3 = $actual. expected=$expected")
    val expectedValue = Constants.phi * Rational.two + Real(1)
    println(s"expectedValue = $expectedValue")
    actual.normalize should ===(expectedValue)
    actual.normalize.isSame(expectedValue) shouldBe true
  }

  it should "power 4" in {
    val actual = phi.power(4)
    val expected = phi.square.square
    println(s"phi^3 = $actual. expected=$expected")
    actual.normalize should ===(expected)
    actual.normalize.isSame(expected) shouldBe true
  }

  it should "transform" in {
    val pFunc: (Rational, Rational) => Rational = (p, q) => (2 * q) - (p ∧ 2)
    val qFunc: (Rational, Rational) => Rational = (_, q) => q ∧ 2
    val equation = Quadratic.goldenRatioEquation
    println(s"equation = $equation")
    val quadratic = equation.transform(pFunc, qFunc)
    println(s"quadratic (transformed equation) = $quadratic")
    quadratic shouldBe Quadratic(-Rational.three, Rational.one)
    println(s"equation = $equation")
    println(quadratic.solve(0))
    println(quadratic.solve(1))
    println(s"quadratic.value = ${quadratic.solve(0)}")
  }
}
