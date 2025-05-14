/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Number.{convertInt, root5}
import com.phasmidsoftware.number.core.inner.Rational
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RQRSpec extends AnyFlatSpec with Matchers {

  behavior of "RQR"

  it should "normalize" in {
    val expected: Real = Constants.phi
    val phi = Solution_RQR.phi.normalize
    phi.isSame(expected) shouldBe true
    val psi = Solution_RQR.psi.normalize
    psi.isSame(Real(-0.618033)) shouldBe true
  }

  it should "branches" in {
    Solution_RQR.phi.equation.branches shouldBe 2
  }

  it should "isExact" in {
    Solution_RQR.phi.isExact shouldBe true
    Solution_RQR.psi.isExact shouldBe false
  }

  it should "value 1" in {
    val p = Solution_RQR.phi
    val (n1, r, n2) = p.value
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
    val expected = Solution_RQR.phi.invert
    target.value shouldBe expected.value
    target.normalize.isSame(Real(0.618033)) shouldBe true

  }
  it should "value 3" in {
    val equation = RQR(Rational.three.negate, Rational.one)
    val target = Solution_RQR(equation, pos = true)
    val (n1, r, n2) = target.value
    n1 shouldBe Real(Rational(3, 2))
    val resultingValue: Field = r * n2
    val expected = (root5 divide Real(2))
    resultingValue shouldBe expected
  }

  it should "render" in {
    Solution_RQR.phi.render shouldBe "\uD835\uDED7"
    Solution_RQR.psi.render shouldBe "\uD835\uDED9"
  }

  it should "approximation" in {
    val approximation = Solution_RQR.phi.approximation
    approximation.isDefined shouldBe true
    approximation.get.isSame(Real(1.6180339887498948)) shouldBe true
  }

  it should "product" in {
    val actual = Solution_RQR.phi * Solution_RQR.psi
    val expected = Solution_RQR.phi.equation.asInstanceOf[RQR].q
    actual shouldBe Real(expected)
  }

  it should "square" in {
    val actual = Solution_RQR.phi.square
    // XXX phi^2 = phi + 1 (see https://en.wikipedia.org/wiki/Golden_ratio)
    val expected = Solution_RQR.phi add Real(1)
    actual shouldBe expected
  }

  it should "invert" in {
    val actual = Solution_RQR.phi.invert
    // XXX 1/phi = phi - 1 (see https://en.wikipedia.org/wiki/Golden_ratio)
    val expected = Solution_RQR.phi add Real(-1)
    actual.isSame(expected) shouldBe true
  }
}
