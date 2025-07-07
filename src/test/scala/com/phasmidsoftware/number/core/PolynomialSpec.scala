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

  val p: RationalPolynomial = RationalPolynomial(2, Seq(Rational(1), Rational(2), Rational(3)))

  it should "apply" in {
    p.apply(Rational.zero) shouldBe Rational.zero
  }

  it should "degree" in {
    p.degree shouldBe 2
  }

  it should "derivative" in {
    val actual = p.derivative
    val expected = RationalPolynomial(1, Seq(Rational(2), Rational(6)))
    actual shouldBe expected
  }

  it should "coefficients" in {
    p.coefficients shouldBe Seq(Rational(1), Rational(2), Rational(3))
  }

  ignore should "derivativeN" in {
    p.derivativeN(0) shouldBe RationalPolynomial(1, Seq(Rational(2), Rational(6)))
  }

  it should "nthDerivative" in {

  }

}
