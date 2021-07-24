package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Complex.{convertToCartesian, convertToPolar}
import com.phasmidsoftware.number.core.Field.convertToNumber
import com.phasmidsoftware.number.core.Rational.RationalHelper
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ComplexSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Complex"

  private val c1_2 = ComplexCartesian(GeneralNumber.one, GeneralNumber.two)
  private val c2_0 = ComplexCartesian(GeneralNumber.two, GeneralNumber.zero)
  private val p1_pi = ComplexPolar(GeneralNumber.one, GeneralNumber.pi)
  private val p1_pi_2 = ComplexPolar(GeneralNumber.one, convertToNumber((GeneralNumber.pi / GeneralNumber.two).materialize))

  it should "real" in {
    c1_2.real shouldBe GeneralNumber.one
    c2_0.real shouldBe GeneralNumber.two
    p1_pi.real shouldBe GeneralNumber.one
  }

  it should "imag" in {
    c1_2.imag shouldBe GeneralNumber.two
    c2_0.imag shouldBe GeneralNumber.zero
    p1_pi.imag shouldBe GeneralNumber.pi
  }

  it should "isAtomic" in {
    c2_0.isAtomic shouldBe true
  }

  it should "isExact" in {
    c1_2.isExact shouldBe true
    c2_0.isExact shouldBe true
    p1_pi.isExact shouldBe true
    (c1_2 add c2_0).materialize.isExact shouldBe true
    // FIXME Caused by July 1st commit
//    (c1_2 add p1_pi_2).materialize.isExact shouldBe false
  }

  it should "asNumber" in {
    c2_0.asNumber shouldBe Some(GeneralNumber.two)
    c1_2.asNumber shouldBe None
  }

  it should "add1" in {
    val c3 = c1_2 add c2_0
    c3 should matchPattern { case ComplexCartesian(ExactNumber(Right(3), Scalar), ExactNumber(Right(2), Scalar)) => }
  }
  it should "add2" in {
    val c3 = p1_pi add c2_0
    c3 shouldBe ComplexCartesian(GeneralNumber.one, GeneralNumber.zero)
  }

  it should "multiply" in {
    val z1 = c1_2 * c2_0
    z1.materialize shouldBe ComplexCartesian(GeneralNumber.two, GeneralNumber(4))
  }

  it should "unary_$minus" in {
    val z = -c1_2
    z shouldBe ComplexCartesian(GeneralNumber.one, GeneralNumber(-2))
  }

  it should "divide" in {
    val z = ComplexCartesian(GeneralNumber.two, GeneralNumber(4))
    val z1 = z / c2_0
    z1.materialize shouldBe c1_2

  }

  it should "power1" in {
    val z = c1_2 power 2
    z.materialize should matchPattern { case ComplexCartesian(ExactNumber(Right(-3), Scalar), ExactNumber(Right(4), Scalar)) => }
  }

  it should "power2" in {
    val z = c1_2 power 0
    z.materialize should matchPattern { case ComplexCartesian(ExactNumber(Right(1), Scalar), ExactNumber(Right(0), Scalar)) => }
  }

  it should "power3" in {
    val z = c1_2 power -1
    z.materialize shouldBe ComplexCartesian(GeneralNumber(r"1/5"), GeneralNumber(r"-2/5"))
  }

  it should "maybeFactor" in {
    c1_2.maybeFactor shouldBe Some(Scalar)
    ComplexCartesian(GeneralNumber.one, GeneralNumber.pi).maybeFactor shouldBe None
    p1_pi.maybeFactor shouldBe Some(Scalar)
    ComplexPolar(GeneralNumber.one, GeneralNumber.one).maybeFactor shouldBe None
  }

  it should "modulus" in {
    c2_0.modulus shouldBe GeneralNumber.two
    p1_pi.modulus shouldBe GeneralNumber.one
  }

  it should "complement (1)" in {
    c2_0.complement shouldBe c2_0
    c1_2.complement shouldBe ComplexCartesian(1, -2)
    (p1_pi_2.complement doAdd p1_pi_2).modulus shouldBe GeneralNumber.zero
  }

  it should "complement (2)" in {
    p1_pi.complement shouldBe p1_pi
  }

  it should "doAdd" in {

  }

  it should "invert" in {
    val z = c1_2.invert
    z.materialize shouldBe ComplexCartesian(GeneralNumber(r"1/5"), GeneralNumber(r"-2/5"))
  }

  it should "apply" in {

  }

  ignore should "convertToPolar" in {
    convertToPolar(c1_2) shouldBe ComplexPolar(GeneralNumber(5).sqrt, GeneralNumber.pi doDivide GeneralNumber(3))
  }

  it should "unapply" in {

  }

  it should "narrow" in {

  }

  it should "convertToCartesian" in {
    convertToCartesian(p1_pi) shouldBe ComplexCartesian(GeneralNumber(-1), GeneralNumber.zero)
  }

  it should "magnitudeSquared" in {
    val c3 = p1_pi add c2_0
    convertToNumber(c3.materialize.magnitudeSquared.materialize) shouldBe GeneralNumber.one

  }

}
