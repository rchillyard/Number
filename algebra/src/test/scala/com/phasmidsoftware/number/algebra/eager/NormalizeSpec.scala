/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.eager

import com.phasmidsoftware.number.algebra.eager.{Angle, BinaryExponential, Complex, Exponential, InversePower, NaturalExponential, RationalNumber, Real, WholeNumber}
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.numerical.ComplexCartesian
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * Comprehensive test suite for the normalize method across all Valuable types.
  */
class NormalizeSpec extends AnyFlatSpec with Matchers {

  behavior of "WholeNumber.normalize"

  it should "return itself (already in simplest form)" in {
    val w = WholeNumber(42)
    w.normalize shouldBe w
    w.normalize should be theSameInstanceAs w
  }

  it should "work for zero" in {
    WholeNumber.zero.normalize shouldBe WholeNumber.zero
  }

  it should "work for negative numbers" in {
    WholeNumber(-5).normalize shouldBe WholeNumber(-5)
  }

  behavior of "RationalNumber.normalize"

  it should "reduce to WholeNumber when denominator is 1" in {
    val r = RationalNumber(Rational(5, 1))
    r.normalize shouldBe WholeNumber(5)
  }

  it should "reduce fraction and then to WholeNumber" in {
    // Assuming Rational constructor auto-simplifies 4/2 to 2/1
    val r = RationalNumber(Rational(4, 2))
    r.normalize shouldBe WholeNumber(2)
  }

  it should "keep as RationalNumber when not reducible to whole" in {
    val r = RationalNumber(Rational(2, 3))
    val result = r.normalize
    result shouldBe a[RationalNumber]
    result.asInstanceOf[RationalNumber].r shouldBe Rational(2, 3)
  }

  it should "return same instance if already normalized and not whole" in {
    val r = RationalNumber(Rational(3, 4))
    val result = r.normalize
    result should be theSameInstanceAs r
  }

  it should "preserve asPercentage flag when not reducing to whole" in {
    val r = new RationalNumber(Rational(3, 4), percentage = true)()
    val result = r.normalize
    result shouldBe a[RationalNumber]
    result.asInstanceOf[RationalNumber].percentage shouldBe true
  }

  it should "work with negative rationals" in {
    val r = RationalNumber(Rational(-6, 2))
    r.normalize shouldBe WholeNumber(-3)
  }

  it should "work with zero" in {
    val r = RationalNumber(Rational(0, 5))
    r.normalize shouldBe WholeNumber(0)
  }

  behavior of "Real.normalize"

  it should "return itself (fuzzy numbers don't reduce)" in {
    val real = Real("3.14*")
    real.normalize shouldBe real
    real.normalize should be theSameInstanceAs real
  }

  it should "keep exact Reals as Real" in {
    val real = Real(5.0, None)
    real.normalize shouldBe real
  }

  behavior of "InversePower.normalize"

  it should "reduce to base when n=1" in {
    val ip = InversePower(1, WholeNumber(5))
    ip.normalize shouldBe WholeNumber(5)
  }

  it should "normalize the base number" in {
    val ip = InversePower(2, RationalNumber(Rational(4, 2)))
    val result = ip.normalize
    result shouldBe a[InversePower]
    val normalized = result.asInstanceOf[InversePower]
    normalized.n shouldBe 2
    normalized.number shouldBe WholeNumber(2)
  }

  it should "return same instance if base doesn't normalize" in {
    val ip = InversePower(3, WholeNumber(7))
    val result = ip.normalize
    result should be theSameInstanceAs ip
  }

  it should "handle nested normalization" in {
    // InversePower(1, RationalNumber(6, 2)) should reduce all the way to WholeNumber(3)
    val ip = InversePower(1, RationalNumber(Rational(6, 2)))
    ip.normalize shouldBe WholeNumber(3)
  }

  behavior of "NaturalExponential.normalize"

  it should "reduce NaturalExponential(0) to WholeNumber(1)" in {
    // e^0 = 1
    val nl = NaturalExponential(WholeNumber.zero)
    nl.normalize shouldBe WholeNumber.one
  }

  it should "normalize the number parameter" in {
    val nl = NaturalExponential(RationalNumber(Rational(2, 2)))
    val result = nl.normalize
    result shouldBe a[NaturalExponential]
    result.asInstanceOf[NaturalExponential].number shouldBe WholeNumber(1)
  }

  it should "return same instance if number doesn't normalize" in {
    val nl = NaturalExponential(WholeNumber(5))
    val result = nl.normalize
    result should be theSameInstanceAs nl
  }

  behavior of "Exponential.normalize"

  it should "reduce Exponential(base, 0) to WholeNumber(1)" in {
    // base^0 = 1 for any base
    val log = NaturalExponential(WholeNumber.zero)
    log.normalize shouldBe WholeNumber.one
  }

  it should "normalize the number parameter" in {
    val log = BinaryExponential(RationalNumber(Rational(4, 2)))
    val result = log.normalize
    result shouldBe a[Exponential]
    result.asInstanceOf[Exponential].number shouldBe WholeNumber(2)
  }

  it should "return same instance if number doesn't normalize" in {
    val log = NaturalExponential(WholeNumber(3))
    val result = log.normalize
    result should be theSameInstanceAs log
  }

  behavior of "Angle.normalize"

  it should "return itself (angles have semantic meaning)" in {
    val angle = new Angle(WholeNumber(0), degrees = false)()
    angle.normalize shouldNot be theSameInstanceAs angle
  }

  it should "normalize the radians parameter" in {
    val angle = new Angle(RationalNumber(Rational(6, 2)), degrees = false)()
    val result: Angle = angle.normalize
    result shouldBe a[Angle]
    result.number shouldBe WholeNumber(1)
  }

  behavior of "Complex.normalize"

  it should "reduce to Real when imaginary part is zero" in {
    // This test depends on how your Complex type works
    // Adjust based on actual Complex implementation
    val complex = Complex(ComplexCartesian(3.14, 0.0))
    val actual = complex.normalize
    actual shouldBe RationalNumber(157, 50)
  }

  it should "reduce further to WholeNumber if possible" in {
    val complex = Complex(ComplexCartesian(5, 0))
    complex.normalize shouldBe WholeNumber(5)
  }

  it should "stay as Complex when imaginary part is non-zero" in {
    val complex = Complex(ComplexCartesian(3.14, 1.0))
    complex.normalize shouldBe complex
  }

  behavior of "normalize integration tests"

  it should "handle chains of normalization" in {
    // InversePower(1, RationalNumber(4, 2)) -> RationalNumber(4,2) -> WholeNumber(2)
    val complex = InversePower(1, RationalNumber(Rational(4, 2)))
    complex.normalize shouldBe WholeNumber(2)
  }

  it should "not work in raw arithmetic operations" in {
    RationalNumber(1, 2) + RationalNumber(1, 2) shouldBe RationalNumber.one
  }

  behavior of "type discovery methods"

  it should "return correct typeName" in {
    WholeNumber(5).typeName shouldBe "WholeNumber"
    RationalNumber(Rational(2, 3)).typeName shouldBe "RationalNumber"
    Real(3.14, None).typeName shouldBe "Real"
  }

  it should "return correct category" in {
    WholeNumber(5).category shouldBe "Monotone"
    RationalNumber(Rational(2, 3)).category shouldBe "Monotone"
    // Add more once Lazy/Expression types are available
  }

  it should "return correct describe string" in {
    WholeNumber(5).describe shouldBe "Monotone.WholeNumber"
    RationalNumber(Rational(2, 3)).describe shouldBe "Monotone.RationalNumber"
  }
}
