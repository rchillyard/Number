package com.phasmidsoftware.number.algebra

import algebra.ring.CommutativeRing
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WholeNumberSpec extends AnyFlatSpec with Matchers {

  private val zero: WholeNumber = WholeNumber.zero
  private val one: WholeNumber = WholeNumber(1)

  behavior of "WholeNumber"

  it should "test creation" in {
    WholeNumber(0) shouldBe zero
  }

  it should "test render" in {
    zero.render shouldBe "0"
  }

  it should "test conversion to other Structures" in {
    zero.convert(Real.zero) shouldBe Some(Real(0, None))
//    zero.convert(RationalNumber.zero) shouldBe Some(RationalNumber.zero)
  }

  it should "test comparison" in {
    // TODO implement test
  }

  it should "test compareExact" in {
  }

  it should "test arithmetic operations" in {
    zero + one shouldBe one
    one + -one shouldBe zero
//    one - one shouldBe zero
  }

  behavior of "CommutativeGroup[WholeNumber]"
  private val ac: CommutativeRing[WholeNumber] = implicitly[CommutativeRing[WholeNumber]]

  it should "plus" in {
    ac.plus(zero, one) shouldBe one
    ac.plus(one, zero) shouldBe one
  }
  it should "times" in {
    ac.times(zero, one) shouldBe zero
    ac.times(one, one) shouldBe one
    ac.times(one, zero) shouldBe zero
  }
  it should "negate" in {
    ac.negate(one) shouldBe WholeNumber(-1)
  }
}
