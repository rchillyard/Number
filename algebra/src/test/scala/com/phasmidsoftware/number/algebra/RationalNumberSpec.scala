package com.phasmidsoftware.number.algebra

import algebra.ring.Field
import com.phasmidsoftware.number.algebra.RationalNumber.rationalNumberIsField
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.inner.Rational.RationalHelper
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RationalNumberSpec extends AnyFlatSpec with Matchers with AssertionHelpers {

  private val rf: Field[RationalNumber] = implicitly[Field[RationalNumber]]

  behavior of "RationalNumber"

  import com.phasmidsoftware.number.core.inner.Rational.RationalOps

  it should "apply" in {
    RationalNumber(42) shouldBe RationalNumber(Rational(42))
    RationalNumber(22, 7) shouldBe RationalNumber(Rational(22, 7))
    val r = 22 :/ 7
    RationalNumber(r) shouldBe RationalNumber(Rational(22, 7))
    RationalNumber(r"22/7") shouldBe RationalNumber(Rational(22, 7))
  }
  it should "assertEq" in {
    assertEq(RationalNumber(42), RationalNumber(42))
    assertEq(new RationalNumber(42 :/ 100, true)(), new RationalNumber(42 :/ 100, true)())
    assertEq(RationalNumber(42 :/ 100), new RationalNumber(42 :/ 100, true)())
  }
  it should "render" in {
    RationalNumber(42).render shouldBe "42"
    RationalNumber.percentage(7).render shouldBe "7%"
    RationalNumber.percentage(42).render shouldBe "42%"
    RationalNumber.percentage(r"85/2").render shouldBe "42.5%"
    RationalNumber.percentage(r"171/4").render shouldBe "42.75%"
  }
  it should "toString" in {
    RationalNumber(42).toString shouldBe "RationalNumber(42,false)"
    RationalNumber(42, true)().toString shouldBe "RationalNumber(42,true)"
  }
  it should "parse" in {
    val maybeTheAnswer: Option[Number] = RationalNumber.parse("42")
    maybeTheAnswer shouldBe Some(RationalNumber(42))
    RationalNumber.parse("84/2") shouldBe Some(RationalNumber(42))
    RationalNumber.parse("14/2%") shouldBe Some(RationalNumber(Rational(7, 100), true)())
    RationalNumber.parse("7%") shouldBe Some(RationalNumber(Rational(7, 100), true)())
  }
  it should "percentage" in {
    RationalNumber.percentage(42) shouldBe RationalNumber(Rational(21, 50), true)()
  }
//  it should "plus RationalNumber" in {
//    val x = RationalNumber(1)
//    val y = RationalNumber(2)
//    x plus y shouldBe Some(RationalNumber(3))
//    val z = Angle.pi
//    val expected = Some(Real(4.141592653589793, Some(AbsoluteFuzz(5.02654824574367E-16, Box))))
//    x plus z shouldBe expected
//    z plus x shouldBe expected
//  }

//  it should "plus Angle" in {
//    val x = Angle.pi
//    val y = Angle.piBy2
//    x plus y shouldBe Some(-y)
//    val z = Angle.pi
//    x plus z shouldBe Some(Angle.zero)
//    z plus x shouldBe Some(Angle.zero)
//  }

//  it should "plus Real" in {
//    val x = Real(42)
//    val y = Angle.pi
//    x plus y shouldBe Some(Real(45.1415926535898, Some(AbsoluteFuzz(5.02654824574367E-16, Box))))
//  }

  // Basic arithmetic operations
  it should "perform addition correctly" in {
    val x: RationalNumber = RationalNumber(1)
    val y: RationalNumber = RationalNumber(2)
    x + y shouldBe RationalNumber(3)
  }

  it should "perform subtraction correctly" in {
    val x = RationalNumber(5)
    val y = RationalNumber(3)
    x - y shouldBe RationalNumber(2)
  }

  it should "perform multiplication correctly" in {
    val x = RationalNumber(2)
    val y = RationalNumber(3)
    x * y shouldBe RationalNumber(6)
  }

  it should "perform division correctly" in {
    val x = RationalNumber(6)
    val y = RationalNumber(2)
    x / y shouldBe RationalNumber(3)
  }

  it should "perform reciprocal correctly" in {
    val x = RationalNumber(6)
    x.reciprocal shouldBe RationalNumber(Rational(1, 6))
  }

  // Comparison operations
  it should "compare numbers correctly" in {
    val x = RationalNumber(1)
    val y = RationalNumber(2)
    x.compare(y) shouldBe -1
    y.compare(x) shouldBe 1
    x.compare(x) shouldBe 0
  }

  // Conversion operations
  it should "convert to different number types" in {
    val x = RationalNumber(5)
    x.convert(Real.zero) shouldBe Some(Real(5, None))
//    x.convert(Angle.zero) shouldBe None
  }

  // Edge cases and special values
  it should "handle zero correctly" in {
    val x = RationalNumber(0)
    x.isZero shouldBe true
  }

  behavior of "Field[RationalNumber]"

  it should "plus" in {
    rf.plus(-RationalNumber(3, 5), RationalNumber(8, 5)) shouldBe RationalNumber.one
  }
  it should "negate" in {
    rf.negate(RationalNumber(3, 5)) shouldBe RationalNumber(-3, 5)
  }
}
