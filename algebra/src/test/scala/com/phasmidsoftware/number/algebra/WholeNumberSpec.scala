package com.phasmidsoftware.number.algebra

import algebra.ring.CommutativeRing
import com.phasmidsoftware.number.core.inner.Rational
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
    zero.convert(RationalNumber.zero) shouldBe Some(RationalNumber.zero)
  }

  it should "test comparison" in {
    // TODO implement test
  }

  it should "test compareExact" in {
  }

  it should "test arithmetic operations" in {
    zero + one shouldBe one
    one + -one shouldBe zero
    one - one shouldBe zero
  }

  behavior of "WholeNumberSpec 2"
  it should "$plus" in {
    WholeNumber.zero + WholeNumber.one shouldBe WholeNumber.one
  }
  it should "$times" in {
    WholeNumber(6) * WholeNumber(7) shouldBe WholeNumber(42)
  }
  it should "$minus" in {
    WholeNumber(6) - WholeNumber(7) shouldBe WholeNumber.minusOne
  }
  it should "apply" in {
  }
  it should "approximation" in {
  }
  it should "asJavaNumber" in {
  }
  it should "asRational" in {
    WholeNumber(42).asRational shouldBe Rational(42)
  }
  it should "asT" in {
    WholeNumber(42).asT shouldBe WholeNumber(42)
  }
  it should "compare" in {
  }
  it should "compareExact" in {
  }
  it should "compareTo" in {
  }
  it should "convert" in {
  }
  it should "doPlus" in {
    WholeNumber.zero doPlus WholeNumber.one shouldBe Some(WholeNumber.one)
  }
  it should "doTimes" in {
    WholeNumber.one doTimes WholeNumber.minusOne shouldBe Some(WholeNumber.minusOne)
  }
  it should "doScale" in {
  }
  it should "doScaleInt" in {
  }
  it should "isExact" in {
  }
  it should "isZero" in {
  }
  it should "maybeDouble" in {
  }
  it should "maybeFactor" in {
  }
  it should "minusOne" in {
  }
  it should "negate" in {
    WholeNumber.one.negate shouldBe WholeNumber.minusOne
    WholeNumber.minusOne.negate shouldBe WholeNumber.one
  }
  it should "one" in {
    WholeNumber.one.one shouldBe one
  }
  it should "render" in {
    WholeNumber(42).render shouldBe "42"
  }
  it should "scale" in {
  }
  it should "scaleFactor" in {
  }
  it should "showWholeNumber" in {
  }
  it should "signum" in {
    WholeNumber.zero.signum shouldBe 0
    WholeNumber.one.signum shouldBe 1
    WholeNumber.minusOne.signum shouldBe -1
  }
  it should "toInt" in {
    WholeNumber(42).toInt shouldBe Some(42)
  }
  it should "toString" in {
    // toString should really show what we've got
//    WholeNumber(42).toString shouldBe "WholeNumber(42)"
    WholeNumber(42).toString shouldBe "42"
  }
  it should "zero" in {
    WholeNumber.zero.zero shouldBe zero
    WholeNumber.one.zero shouldBe zero
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
