package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Complex.{ComplexHelper, convertToCartesian, convertToPolar}
import com.phasmidsoftware.number.core.Expression.convertFieldToExpression
import com.phasmidsoftware.number.core.Field.convertToNumber
import com.phasmidsoftware.number.core.Number.{half, zeroR}
import com.phasmidsoftware.number.core.Rational.RationalHelper
import org.scalactic.Equality
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ComplexSpec extends AnyFlatSpec with should.Matchers {

  implicit object ComplexCartesianEquality extends Equality[ComplexCartesian] {
    def areEqual(a: ComplexCartesian, b: Any): Boolean = b match {
      case n: Complex =>
        (Literal(a) - n).materialize.isZero
      case _ => false
    }
  }

  implicit object ComplexPolarEquality extends Equality[ComplexPolar] {
    def areEqual(a: ComplexPolar, b: Any): Boolean = b match {
      case n: ComplexPolar =>
        (Literal(a) - n).materialize.isZero
      case _ => false
    }
  }

  behavior of "Complex"

  /**
    * (1,2)
    */
  private val c1_2 = ComplexCartesian(Number.one, Number.two)
  /**
    * 2
    */
  private val c2_0 = Complex(Number.two)
  /**
    * -1
    */
  private val p1_pi = ComplexPolar(Number.one, Number.pi)
  /**
    * 1
    */
  private val p1_0 = ComplexPolar(Number.one, Number.zeroR)
  /**
    * i
    */
  private val p1_pi_2 = ComplexPolar(Number.one, Number.piBy2)
  /**
    * e to the power of i
    */
  private val p1_1 = ComplexPolar(Number.one, ExactNumber(1, Radian))

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
    (c1_2 add p1_pi_2).materialize.isExact shouldBe true
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
    z shouldBe ComplexCartesian(Number.negOne, Number(-2))
  }

  it should "divide" in {
    val z = ComplexCartesian(Number.two, Number(4))
    val z1: Expression = Literal(z) / c2_0
    (z1 * Expression(c2_0)).materialize shouldBe z
    z1.materialize shouldBe c1_2
  }

  behavior of "power"

  it should "c1_2^1" in {
    val z: Field = c1_2 power 1
    z shouldBe c1_2
  }

  it should "c1_2^2" in {
    val z: Field = c1_2 power 2
    z should matchPattern { case ComplexCartesian(ExactNumber(Right(-3), Scalar), ExactNumber(Right(4), Scalar)) => }
  }

  it should "c1_2^0" in {
    val z = c1_2 power 0
    z shouldBe Number.one
  }

  it should "c1_2^-1" in {
    val z = c1_2 power -1
    z shouldBe ComplexCartesian(Number(r"1/5"), Number(r"-2/5"))
  }

  it should "c1_2^1/2" in {
    val result: Field = c1_2 power half
    // result should be 1.27201965 + i0.786151378
    result should matchPattern { case ComplexPolar(_, _, _) => }
    result match {
      case c@ComplexPolar(_, _, _) => c.modulus.toDouble.get shouldBe 1.495348781221220 +- 1E-9
    }
    (result * result).compare(c1_2) shouldBe 0
  }
  it should "c2_0^1/2" in {
    val result: Field = c2_0 power half
    result should matchPattern { case ComplexPolar(_, _, 2) => }
    (result * result).compare(c2_0) shouldBe 0
  }

  it should "p1_pi_2^1" in {
    val z: Field = p1_pi_2 power 1
    z shouldBe p1_pi_2
  }

  it should "p1_pi_2^2" in {
    val z: Field = p1_pi_2 power 2
    z shouldBe p1_pi
  }

  it should "p1_pi_2^0" in {
    val z = p1_pi_2 power 0
    z shouldBe Number.one
  }

  it should "p1_pi_2^-1" in {
    val z: Field = p1_pi_2 power -1
    (z * p1_pi_2).normalize shouldBe Number.one
    val normalized = z.normalize
    normalized shouldBe p1_pi_2.conjugate
  }

  it should "p1_1^-1/2" in {
    p1_1 power half shouldBe ComplexPolar(Number.one, Number.piBy2, 2)
  }

  it should "c2_0^1/3" in {
    val cubeRootOfTwo = c2_0.power(Number(Rational(3).invert))
    cubeRootOfTwo shouldBe ComplexPolar(Number(2, Root3), Number.zeroR, 3)
  }

  behavior of "other"

  it should "maybeFactor" in {
    c1_2.maybeFactor shouldBe Some(Scalar)
    ComplexCartesian(Number.one, Number.pi).maybeFactor shouldBe None
    p1_pi.maybeFactor shouldBe None
    ComplexPolar(Number.one, Number.pi).maybeFactor shouldBe None
  }

  it should "modulus" in {
    c2_0.modulus shouldBe Number.two
    p1_pi.modulus shouldBe Number.one
  }

  it should "argument" in {
    c2_0.argument.isZero shouldBe true
    p1_pi.argument shouldBe Number.pi
  }

  it should "conjugate (1)" in {
    c2_0.conjugate shouldBe c2_0
    c1_2.conjugate shouldBe ComplexCartesian(1, -2)
    (p1_pi_2.conjugate doAdd p1_pi_2).modulus shouldBe Number.zero
  }

  it should "conjugate (2)" in {
    p1_pi.conjugate shouldBe p1_pi
  }

  it should "invert" in {
    val z = c1_2.invert
    z shouldBe ComplexCartesian(Number(r"1/5"), Number(r"-2/5"))
  }

  it should "numberProduct" in {
    ComplexCartesian(1,1).numberProduct(Number.two) shouldBe ComplexCartesian(2,2)
    Number.i.asComplex.numberProduct(Number.two) shouldBe ComplexCartesian(0,2)
    Number.i.asComplex.numberProduct(Number.i) shouldBe ComplexCartesian(-1,0)
  }

  it should "convertToPolar" in {
    val expected: Complex = ComplexPolar(Number(5).sqrt, Number(0.35241638235, Radian))
    val actual: ComplexPolar = convertToPolar(c1_2).asInstanceOf[ComplexPolar]
    convertToCartesian(actual).compare(c1_2) shouldBe 0
    actual.compare(c1_2) shouldBe 0
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
    c2_0.render shouldBe "2"
    c1_2.render shouldBe "(1+i2)"
    p1_0.render shouldBe "1"
    p1_pi.render shouldBe "-1"
    p1_pi_2.render shouldBe "1e^i0.5\uD835\uDED1"
  }
  it should "work for negative polar angle" in {
    val conjugate = p1_pi_2.conjugate
    conjugate.render shouldBe "1e^-i0.5\uD835\uDED1"
  }
  it should "work for iPi" in {
    val target = Constants.iPi
    target.toString shouldBe "ComplexCartesian(0,\uD835\uDED1)"
  }
  it should "work for zero" in {
    p1_pi.add(Number.one).render shouldBe "0"
  }
  it should "work for root 2 in c2_0^1/2" in {
    val result: Field = c2_0 power half
    result.render shouldBe "±√2"
  }
  it should "render c2_0^1/3" in {
    val cubeRootOfTwo = c2_0.power(Number(Rational(3).invert))
    cubeRootOfTwo.render shouldBe "{³√2, ±³√2e^i0.<6>\uD835\uDED1}"
  }
  it should "render root2s" in {
    val target = Constants.root2s
    target.render shouldBe "±√2"
  }

  behavior of "Number.asComplex"
  it should "work" in {
    Number(2).asComplex shouldBe ComplexPolar(2, zeroR, 1)
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

  behavior of "C interpolator"
  it should "parse" in {
    C"1i0" should matchPattern { case ComplexCartesian(Number.one, Number.zero) => }
    C"1i1" should matchPattern { case ComplexCartesian(Number.one, Number.one) => }
    C"0i1" should matchPattern { case ComplexCartesian(Number.zero, Number.one) => }
    C"1ipi" should matchPattern { case ComplexPolar(Number.one, Number.pi, 1) => }
    C"1i0.5pi" should matchPattern { case ComplexPolar(Number.one, Number.piBy2, 1) => }
    C"0ipi" should matchPattern { case ComplexPolar(Number.zero, Number.pi, 1) => }

    C"1+i0" should matchPattern { case ComplexCartesian(Number.one, Number.zero) => }
    C"1+i1" should matchPattern { case ComplexCartesian(Number.one, Number.one) => }
    C"0+i1" should matchPattern { case ComplexCartesian(Number.zero, Number.one) => }
    C"1+ipi" should matchPattern { case ComplexPolar(Number.one, Number.pi, 1) => }
    C"1i0.5pi" should matchPattern { case ComplexPolar(Number.one, Number.piBy2, 1) => }
    C"0+ipi" should matchPattern { case ComplexPolar(Number.zero, Number.pi, 1) => }

    C"1-i0" should matchPattern { case ComplexCartesian(Number.one, Number.zero) => }
    C"1-i1" should matchPattern { case ComplexCartesian(Number.one, Number.negOne) => }
    C"1-i-1" should matchPattern { case ComplexCartesian(Number.one, Number.one) => }
    C"0-i1" should matchPattern { case ComplexCartesian(Number.zero, Number.negOne) => }
    C"1-ipi" should matchPattern { case ComplexPolar(Number.one, Number.pi, 1) => }
//    C"1-i0.5pi" should matchPattern { case (ComplexPolar(Number.one, negate(Number.piBy2), 1)) => }
    C"0-ipi" should matchPattern { case ComplexPolar(Number.zero, Number.pi, 1) => }

  }
}
