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
    val phi = RQR.phi.normalize
    println(s"$phi")
    phi.isSame(expected) shouldBe true
    val psi = RQR.psi.normalize
    println(s"$psi")
    psi.isSame(Real(-0.618033)) shouldBe true
  }

  it should "branches" in {
    RQR.phi.branches shouldBe 2
  }

  it should "isExact" in {
    RQR.phi.isExact shouldBe true
    RQR.psi.isExact shouldBe false
  }

  it should "value 1" in {
    val p = RQR.phi
    val (n1, r, n2) = p.value
    n1 shouldBe Real(Number.half)
    val resultingValue: Field = r * (2 multiply n2)
    val expected = Real(root5)
    resultingValue.isSame(expected) shouldBe true
    resultingValue shouldBe expected
  }
  it should "value 2" in {
    val target = RQR("test", Rational.one, Rational.negOne, pos = true)
    val expected = RQR.phi.invert
    println(s"${target.normalize}, ${expected.normalize}")
    target shouldBe expected
    target.normalize.isSame(Real(0.618033)) shouldBe true

  }
  it should "value 3" in {
    val target = RQR("test", Rational.three.negate, Rational.one, pos = true)
    println(s"${target.normalize}")
    val (n1, r, n2) = target.value
    n1 shouldBe Real(Rational(3, 2))
    val resultingValue: Field = r * n2
    val expected = (root5 divide Real(2))
    resultingValue shouldBe expected
  }

  it should "render" in {
    RQR.phi.render shouldBe "\uD835\uDED7"
    RQR.psi.render shouldBe "\uD835\uDED9"
  }

  it should "approximation" in {
    val approximation = RQR.phi.approximation
    approximation.isDefined shouldBe true
    approximation.get.isSame(Real(1.6180339887498948)) shouldBe true
  }

  it should "product" in {
    val actual = RQR.phi * RQR.psi
    val expected = RQR.phi.q
    println(s"$actual, $expected")
    actual shouldBe Real(expected)
  }

  it should "square" in {
    val actual = RQR.phi.square
    val expected = RQR.phi add Real(1)
    actual shouldBe expected
    println(s"${actual.normalize}")
  }

  it should "invert" in {
    val actual = RQR.phi.invert
    val expected = RQR.phi add Real(-1)
    println(s"$actual, $expected")
    actual.isSame(expected) shouldBe true
  }
}
