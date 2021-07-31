package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Complex.{convertToCartesian, convertToPolar}
import com.phasmidsoftware.number.core.Field.convertToNumber
import com.phasmidsoftware.number.core.Rational.RationalHelper
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ComplexSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Complex"

  private val c1_2 = ComplexCartesian(Number.one, Number.two)
  private val c2_0 = ComplexCartesian(Number.two, Number.zero)
  private val p1_pi = ComplexPolar(Number.one, Number.pi)
  private val p1_pi_2 = ComplexPolar(Number.one, convertToNumber((Number.pi / Number.two).materialize))

  it should "real" in {
    c1_2.real shouldBe Number.one
    c2_0.real shouldBe Number.two
    p1_pi.real shouldBe Number.one
  }

  it should "imag" in {
    c1_2.imag shouldBe Number.two
    c2_0.imag shouldBe Number.zero
    p1_pi.imag shouldBe Number.pi
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
    c2_0.asNumber shouldBe Some(Number.two)
    c1_2.asNumber shouldBe None
  }

  it should "add1" in {
    val c3 = c1_2 add c2_0
    c3 should matchPattern { case ComplexCartesian(ExactNumber(Right(3), Scalar), ExactNumber(Right(2), Scalar)) => }
  }
  it should "add2" in {
    val c3 = p1_pi add c2_0
    c3 shouldBe ComplexCartesian(Number.one, Number.zero)
  }

  it should "multiply" in {
    val z1 = c1_2 * c2_0
    z1.materialize shouldBe ComplexCartesian(Number.two, Number(4))
  }

  it should "unary_$minus" in {
    val z = -c1_2
    z shouldBe ComplexCartesian(Number.one, Number(-2))
  }

  it should "divide" in {
    val z = ComplexCartesian(Number.two, Number(4))
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
    z.materialize shouldBe ComplexCartesian(Number(r"1/5"), Number(r"-2/5"))
  }

  it should "maybeFactor" in {
    c1_2.maybeFactor shouldBe Some(Scalar)
    ComplexCartesian(Number.one, Number.pi).maybeFactor shouldBe None
    p1_pi.maybeFactor shouldBe Some(Scalar)
    ComplexPolar(Number.one, Number.one).maybeFactor shouldBe None
  }

  it should "modulus" in {
    c2_0.modulus shouldBe Number.two
    p1_pi.modulus shouldBe Number.one
  }

  it should "complement (1)" in {
    c2_0.complement shouldBe c2_0
    c1_2.complement shouldBe ComplexCartesian(1, -2)
    (p1_pi_2.complement doAdd p1_pi_2).modulus shouldBe Number.zero
  }

  it should "complement (2)" in {
    p1_pi.complement shouldBe p1_pi
  }

  it should "doAdd" in {

  }

  it should "invert" in {
    val z = c1_2.invert
    z.materialize shouldBe ComplexCartesian(Number(r"1/5"), Number(r"-2/5"))
  }

  it should "apply" in {

  }

  ignore should "convertToPolar" in {
    convertToPolar(c1_2) shouldBe ComplexPolar(Number(5).sqrt, Number.pi doDivide Number(3))
  }

  it should "unapply" in {

  }

  it should "narrow" in {

  }

  it should "convertToCartesian" in {
    convertToCartesian(p1_pi) shouldBe ComplexCartesian(Number(-1), Number.zero)
  }

  it should "magnitudeSquared" in {
    val c3 = p1_pi add c2_0
    convertToNumber(c3.materialize.magnitudeSquared.materialize) shouldBe Number.one

  }

  behavior of "render"
  it should "work" in {
    c2_0.render shouldBe "(2+i0)"
    c1_2.render shouldBe "(1+i2)"
    p1_pi.render shouldBe "1e^i1\uD835\uDED1"
    p1_pi_2.render shouldBe "1e^i0.5\uD835\uDED1"
    p1_pi_2.complement.render shouldBe "1e^i1.5\uD835\uDED1"
  }

}
