package com.phasmidsoftware.number.algebra

import algebra.CommutativeGroup
import algebra.ring.AdditiveCommutativeGroup
import com.phasmidsoftware.number.core.{Box, RelativeFuzz}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AngleSpec extends AnyFlatSpec with Matchers {

  private val zero: Angle = Angle.zero
  private val pi: Angle = Angle.pi
  private val piBy2: Angle = Angle.piBy2

  behavior of "Angle"

  it should "test creation" in {
    Angle(RationalNumber(1)) shouldBe pi
    Angle(RationalNumber(1)) shouldBe Angle.𝛑
  }

  it should "test render" in {
    zero.render shouldBe "0\uD835\uDED1"
    pi.render shouldBe "\uD835\uDED1"
  }

  it should "test conversion to other Structures" in {
    pi.convert(Real.zero) shouldBe Some(Real(3.141592653589793, Some(RelativeFuzz(1.6E-16, Box))))
    pi.convert(RationalNumber.zero) shouldBe None
  }

  it should "isExact" in {
    pi.isExact shouldBe true
    piBy2.isExact shouldBe true
    Angle(Real(1)).isExact shouldBe true
    Angle(Real(0.5)).isExact shouldBe false
  }

  it should "maybeDouble" in {
    pi.maybeDouble shouldBe Some(3.141592653589793)
    piBy2.maybeDouble shouldBe Some(1.5707963267948966)
    Angle(Real(1)).maybeDouble shouldBe Some(3.141592653589793)
    Angle(Real(0.5)).maybeDouble shouldBe None
  }

  it should "isZero" in {
    pi.isZero shouldBe false
    zero.isZero shouldBe true
  }

  it should "test comparison" in {
    // TODO implement test
  }

  it should "test compareExact" in {
    val xo: Option[Scalar] = piBy2.doPlus(piBy2)
    xo flatMap (x => pi.compareExact(x)) shouldBe Some(0)
  }
  it should "test arithmetic operations" in {
    pi + pi shouldBe zero
    pi + -pi shouldBe zero
    piBy2 + piBy2 shouldBe pi
    piBy2 + -piBy2 shouldBe zero
  }

  behavior of "AdditiveCommutativeGroup[Angle]"
  private val ac: AdditiveCommutativeGroup[Angle] = implicitly[AdditiveCommutativeGroup[Angle]]

  it should "combine" in {
    ac.plus(-pi, pi) shouldBe zero
  }
  it should "inverse" in {
    ac.negate(pi) shouldBe -pi
  }
}
