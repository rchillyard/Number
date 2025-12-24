package com.phasmidsoftware.number.algebra

import com.phasmidsoftware.number.algebra.eager.{Angle, Real, Scalar}
import com.phasmidsoftware.number.core.numerical.{AbsoluteFuzz, Box}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NumberSpec extends AnyFlatSpec with Matchers {

  behavior of "Number"

  private val one = Real(1)
  private val two = Real(2)
  private val three = Real(3)
  private val pi: Scalar = Angle.pi

  // Basic arithmetic operations
  it should "perform addition correctly" in {
    val x: Real = one
    val y: Real = two
    val z: eager.Number = 2
    x + y shouldBe three
    y + x shouldBe three
    x doPlus z shouldBe Some(three)
    val expected2plusPi = Some(Real(5.141592653589793, Some(AbsoluteFuzz(5.02654824574367E-16, Box))))
//    summon[AdditiveCommutativeMonoid[Number]].plus(z, pi) shouldBe expected2plusPi
//    plus(pi, z) shouldBe expected2plusPi
  }

  it should "perform subtraction correctly" in {
    val x = Real(5)
    val y = three
    x + -y shouldBe two
  }

  it should "perform multiplication correctly" in {
    val x = two
    val y = three
    x * y shouldBe Real(6)
  }

  it should "perform division correctly" in {
    val x = Real(6)
    val y = two
    x / y shouldBe Real(3)
  }

  // Comparison operations
  it should "compare numbers correctly" in {
    val x: eager.Number = one
    val y = two
    x < y shouldBe true
    x > y shouldBe false
    x <= y shouldBe true
    x >= y shouldBe false
    x == Real.one shouldBe true
    x <= Real.one shouldBe true
    x >= Real.one shouldBe true
    three.compare(pi) shouldBe -1
    pi.convert(three).map(x => x.compare(three)) shouldBe Some(1)
    x < Real(1.5) shouldBe true
    x > Real(1.5) shouldBe false
  }

  // Conversion operations
  it should "convert to different number types" in {
    val x = Real(5)
    x.convert(Real.zero) shouldBe Some(x)
    pi.convert(Real.zero) shouldBe Some(Real.pi)
    // NOTE that Angle can be converted to Real, but not the other way around.
    x.convert(Angle.zero) shouldBe None
  }

  it should "isExact" in {
    Real(0.0).isExact shouldBe false
    Real.zero.isExact shouldBe true
    Angle.zero.isExact shouldBe true
  }

  // Edge cases and special values
  it should "handle zero correctly" in {
    Real(0.0).isZero shouldBe true
    Real.zero.isZero shouldBe true
    Angle.zero.isZero shouldBe true
  }
}
