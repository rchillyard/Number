/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.numerical

import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.inner.Rational.RationalFractional
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PolynomialSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Polynomial"
  val p2: RationalPolynomial = RationalPolynomial(1, 1, 1)
  val p4: RationalPolynomial = RationalPolynomial(3, 2, 5, 4, 1)
  it should "apply 0" in {
    p2.apply(0) shouldBe Rational.one
    p4.apply(0) shouldBe Rational.three
  }
  it should "apply 1" in {
    p2.apply(1) shouldBe Rational.three
    p4.apply(1) shouldBe Rational(15)
  }
  it should "apply 2" in {
    p2.apply(2) shouldBe (Rational.two ∧ 3) - 1
    p4.apply(2) shouldBe Rational.three + 2 * 2 + Rational(5) * 4 + Rational.four * 8 + 16
  }
  it should "degree" in {
    p2.degree shouldBe 2
    p4.degree shouldBe 4
  }
  it should "derivative 1" in {
    p2.derivative shouldBe RationalPolynomial(1, 2)
    p4.derivative shouldBe RationalPolynomial(2, 10, 12, 4)
  }
  it should "derivative 2" in {
    p2.derivative.derivative shouldBe RationalPolynomial(2)
    p4.derivative.derivative shouldBe RationalPolynomial(10, 24, 12)
  }
  it should "coefficients" in {
    p2.coefficients shouldBe Seq(Rational.one, Rational.one, Rational.one)
    p4.coefficients shouldBe Seq(Rational.three, Rational.two, Rational(5), Rational.four, Rational.one)
  }
  it should "derivativeN" in {
    p2.derivativeN(1) shouldBe RationalPolynomial(1, 2)
  }
  it should "nthDerivative" in {

  }
}
