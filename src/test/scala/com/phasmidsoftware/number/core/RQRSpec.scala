/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Number.{convertInt, negOne, one, root5, two}
import com.phasmidsoftware.number.core.Solution_RQR.{phi, psi}
import com.phasmidsoftware.number.core.inner.Rational
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RQRSpec extends AnyFlatSpec with Matchers with FuzzyEquality {

  behavior of "RQR"

  it should "normalize" in {
    val expected: Real = Constants.phi
    val phi = Solution_RQR.phi.normalize
    phi.isSame(expected) shouldBe true
    val psi = Solution_RQR.psi.normalize
    psi.isSame(Real(-0.618033)) shouldBe true
  }

  it should "branches" in {
    phi.equation.branches shouldBe 2
  }

  it should "isExact" in {
    phi.isExact shouldBe true
    psi.isExact shouldBe false
  }

  it should "value 1" in {
    val p = phi
    println(p.render)
    val (n1, r, (n2)) = p.value
    n1 shouldBe Real(Number.half)
    val resultingValue: Field = r * (2 multiply n2)
    val expected = Real(root5)
    resultingValue.isSame(expected) shouldBe true
    resultingValue shouldBe expected
  }

  it should "value 2" in {
    val equation = RQR.goldenRatioEquation
    val otherEquation = RQR(1, -1)
    val target = Solution_RQR(otherEquation, pos = true)
    val expected = phi.invert
    target.value shouldBe expected.value
    target.normalize.isSame(Real(0.618033)) shouldBe true
  }

  it should "value 3" in {
    val equation = RQR(Rational.three.negate, Rational.one)
    val target = Solution_RQR(equation, pos = true)
    val (n1, r, (n2)) = target.value
    n1 shouldBe Real(Rational(3, 2))
    val resultingValue: Field = r * n2
    val expected = (root5 divide Real(2))
    resultingValue shouldBe expected
  }

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

  it should "abs" in {
    phi.abs shouldBe phi
    psi.abs shouldBe psi.scale(-1)
  }

  it should "scale" in {
    val actual = phi.scale(2)
    val expected = Solution_RQR(RQR(-2, -4), pos = true)
    actual shouldBe expected
  }

  it should "negate" in {
    phi.negate shouldBe Solution_RQR(RQR(1, -1), pos = true)
  }

  it should "product" in {
    val actual = phi * psi
    val expected = phi.equation.asInstanceOf[RQR].q
    actual shouldBe Real(expected)
  }

  it should "square" in {
    val actual: Solution = phi.square
    // XXX phi^2 = phi + 1 (see https://en.wikipedia.org/wiki/Golden_ratio)
    val expected: Solution = (phi add Real(1)).asInstanceOf[Solution] // NOTE this only works here
    println(s"phi.square = ${actual.normalize}")
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
    val actual = Solution_RQR.zero
    actual.rationalValue should matchPattern { case (Rational.zero, _, Some(Rational.zero)) => }
  }

  it should "one" in {
    val actual = Solution_RQR.one
    actual.rationalValue should matchPattern { case (Rational.one, _, Some(Rational.zero)) => }
  }

  it should "power 0" in {
    val actual = phi.power(0)
    val expected = Solution_RQR.one
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
    actual shouldBe (expected)
    actual.normalize should ===(Constants.phi + Real(1))
    actual.normalize.isSame(Constants.phi + Real(1)) shouldBe true
  }

  it should "power 3" in {
    val actual = phi.power(3)
    val expected = phi.scale(2).add(Rational.one)
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
    val equation = RQR.goldenRatioEquation
    println(s"equation = $equation")
    val rqr = equation.transform(pFunc, qFunc)
    println(s"rqr (transformed equation) = $rqr")
    rqr shouldBe RQR(-Rational.three, Rational.one)
    println(s"equation = $equation")
    rqr.solve(0) foreach println
    rqr.solve(1) foreach println
    println(s"rqr.value = ${rqr.solve(0)}")
  }
}
