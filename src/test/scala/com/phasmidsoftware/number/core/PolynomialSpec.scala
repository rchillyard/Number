/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.inner.Rational.RationalFractional
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class PolynomialSpec extends AnyFlatSpec {

  behavior of "Polynomial"

  val p2: RationalPolynomial = RationalPolynomial(1, 1, 1)
  val p4: RationalPolynomial = RationalPolynomial(3, 2, 5, 4, 1)

  it should "apply" in {
    p2.apply(0) shouldBe Rational.zero
    p4.apply(0) shouldBe Rational.zero
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
