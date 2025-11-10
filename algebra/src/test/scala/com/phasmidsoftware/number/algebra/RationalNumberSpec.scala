package com.phasmidsoftware.number.algebra

import algebra.ring.Field
import com.phasmidsoftware.number.algebra.RationalNumber.rationalNumberIsField
import com.phasmidsoftware.number.core.inner.Rational
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RationalNumberSpec extends AnyFlatSpec with Matchers {

  private val rf: Field[RationalNumber] = implicitly[Field[RationalNumber]]

  behavior of "RationalNumber"

//  it should "doPlus RationalNumber" in {
//    val x = RationalNumber(1)
//    val y = RationalNumber(2)
//    x doPlus y shouldBe Some(RationalNumber(3))
//    val z = Angle.pi
//    val expected = Some(Real(4.141592653589793, Some(AbsoluteFuzz(5.02654824574367E-16, Box))))
//    x doPlus z shouldBe expected
//    z doPlus x shouldBe expected
//  }

//  it should "doPlus Angle" in {
//    val x = Angle.pi
//    val y = Angle.piBy2
//    x doPlus y shouldBe Some(-y)
//    val z = Angle.pi
//    x doPlus z shouldBe Some(Angle.zero)
//    z doPlus x shouldBe Some(Angle.zero)
//  }

//  it should "doPlus Real" in {
//    val x = Real(42)
//    val y = Angle.pi
//    x doPlus y shouldBe Some(Real(45.1415926535898, Some(AbsoluteFuzz(5.02654824574367E-16, Box))))
//  }

  // Basic arithmetic operations
  it should "perform addition correctly" in {
    val x = RationalNumber(1)
    val y = RationalNumber(2)
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
