package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Complex.{convertToCartesian, convertToPolar}
import com.phasmidsoftware.number.core.Field.convertToNumber
import com.phasmidsoftware.number.core.Rational.RationalHelper
import org.scalactic.Equality
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ComplexSpec extends AnyFlatSpec with should.Matchers {

  implicit object ComplexEquality extends Equality[Complex] {
    def areEqual(a: Complex, b: Any): Boolean = b match {
      case n: Complex => (Literal(a) - n).materialize.isZero
      case _ => false
    }
  }

  behavior of "Complex"

  private val c1_2 = ComplexCartesian(Number.one, Number.two)
  private val c2_0 = Complex(Number.two)
  private val p1_pi = ComplexPolar(Number.one, Number.pi)
  private val p1_pi_2 = ComplexPolar(Number.one, Number.pi doDivide Number.two)

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

  it should "isExact" in {
    c1_2.isExact(None) shouldBe true
    c2_0.isExact(None) shouldBe true
    p1_pi.isExact(None) shouldBe true
    (c1_2 add c2_0).isExact shouldBe true
    // FIXME Caused by July 1st commit
    //    (c1_2 add p1_pi_2).materialize.isExact shouldBe false
  }

  it should "isInfinite" in {

    p1_pi_2.divide(Number.zero).isInfinite shouldBe true
    p1_pi.isInfinite shouldBe false
    c2_0.divide(Number.one).isInfinite shouldBe false
    c1_2.isInfinite shouldBe false
  }

  it should "asNumber" in {
    c2_0.asNumber shouldBe Some(Number.two)
    c1_2.asNumber shouldBe None
    p1_pi_2.asNumber shouldBe Some(Number.i)
    ComplexCartesian(0, Number.two).asNumber shouldBe Some(Number(-4, Root2))
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
    val z1 = Expression(c1_2) * c2_0
    z1.materialize shouldBe ComplexCartesian(Number.two, Number(4))
  }

  it should "unary_$minus" in {
    val z = -c1_2
    z shouldBe ComplexCartesian(Number.one, Number(-2))
  }

  it should "divide" in {
    val z = ComplexCartesian(Number.two, Number(4))
    val z1 = Literal(z) / c2_0
    z1.materialize shouldBe c1_2
  }

  it should "power0" in {
    val z: Field = c1_2 power 1
    z shouldBe c1_2
  }

  it should "power1" in {
    val z: Field = c1_2 power 2
    z should matchPattern { case ComplexCartesian(ExactNumber(Right(-3), Scalar), ExactNumber(Right(4), Scalar)) => }
  }

  it should "power2" in {
    val z = c1_2 power 0
    z should matchPattern { case ComplexCartesian(ExactNumber(Right(1), Scalar), ExactNumber(Right(0), Scalar)) => }
  }

  it should "power3" in {
    val z = c1_2 power -1
    z shouldBe ComplexCartesian(Number(r"1/5"), Number(r"-2/5"))
  }

  it should "maybeFactor" in {
    c1_2.maybeFactor shouldBe Some(Scalar)
    ComplexCartesian(Number.one, Number.pi).maybeFactor shouldBe None
    p1_pi.maybeFactor shouldBe None
    ComplexPolar(Number.one, Number.one).maybeFactor shouldBe Some(Scalar)
  }

  it should "modulus" in {
    c2_0.modulus shouldBe Number.two
    p1_pi.modulus shouldBe Number.one
  }

  it should "argument" in {
    c2_0.argument.isZero shouldBe true
    p1_pi.argument shouldBe Number.pi
  }

  it should "complement (1)" in {
    c2_0.complement shouldBe c2_0
    c1_2.complement shouldBe ComplexCartesian(1, -2)
    (p1_pi_2.complement doAdd p1_pi_2).modulus shouldBe Number.zero
  }

  it should "complement (2)" in {
    p1_pi.complement shouldBe p1_pi
  }

  it should "invert" in {
    val z = c1_2.invert
    z shouldBe ComplexCartesian(Number(r"1/5"), Number(r"-2/5"))
  }

  it should "convertToPolar" in {
    val expected: Complex = ComplexPolar(Number(5).sqrt, Number(0.35241638235, Radian))
    val actual: Complex = convertToPolar(c1_2)
    actual shouldEqual expected
  }

  it should "check that math.atan really works" in {
    val tangent = 2.0
    val theta = math.atan2(tangent, 1)
    val result = math.tan(theta)
    result shouldBe tangent +- 1E-8
    // TODO understand why the match.atan method seems to be so imprecise
    theta * 3 shouldBe math.Pi +- 2E-1
  }

  it should "check that math.atan really works for many fractions" in {
    for (i <- 0 to 10; j <- 0 to 10)
      if (i != 0 || j != 0) checkAtan2(i, j)
  }

  private def checkAtan2(opposite: Int, adjacent: Int): Unit = {
    val theta = math.atan2(opposite, adjacent)
    //noinspection ScalaUnusedSymbol
    // NOTE this isn't used
    val thetaAsFraction = theta / math.Pi
    //    println(s"atan2($opposite/$adjacent) is $thetaAsFraction")
    val result = math.tan(theta)
    val expected = opposite * 1.0 / adjacent
    // CONSIDER what's going on here? When might it be pos inf?
    if (expected != Double.PositiveInfinity)
      result shouldBe expected +- 1E-6
  }

  it should "convertToCartesian" in {
    convertToCartesian(p1_pi) shouldBe ComplexCartesian(Number(-1), Number.zero)
  }

  behavior of "render"
  it should "work for various Complex values" in {
    c2_0.render shouldBe "(2+i0)"
    c1_2.render shouldBe "(1+i2)"
    p1_pi.render shouldBe "1e^i\uD835\uDED1"
    p1_pi_2.render shouldBe "1e^i0.5\uD835\uDED1"
    p1_pi_2.complement.render shouldBe "1e^i1.5\uD835\uDED1"
  }
  it should "work for iPi" in {
    val target = Number.iPi
    target.toString shouldBe "ComplexCartesian(0,\uD835\uDED1)"
  }

  behavior of "Number.asComplex"
  it should "work" in {
    Number(2).asComplex shouldBe c2_0
  }

  behavior of "numberProduct"
  it should "work" in {
    ComplexCartesian(1, 0).numberProduct(Number.two) shouldBe c2_0
    ComplexCartesian(0, -2).numberProduct(Number.i) shouldBe c2_0
  }
  it should "work when i is scaled by two (prefix)" in {
    Number.two multiply Number.i shouldBe ComplexCartesian(0, 2)
  }
  it should "work when i is scaled by two (postfix)" in {
    Number.i multiply Number.two shouldBe ComplexCartesian(0, 2)
  }

  behavior of "i"
  it should "render as √-1" in {
    // CONSIDER should we have it render as "i" instead?
    Number.i.render shouldBe "√-1"
  }
  it should "convertToNumber" in {
    convertToNumber(Number.i) shouldBe Number.i
  }
  it should "scale(Scalar)" in {
    a[NumberException] should be thrownBy Number.i.scale(Scalar)
  }
  it should "normalize" in {
    Number.i.normalize shouldBe ComplexCartesian(0, 1)
  }
}
