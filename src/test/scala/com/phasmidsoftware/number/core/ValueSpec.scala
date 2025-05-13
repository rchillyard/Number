package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Value._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

/**
  * Specification for Value.
  *
  * CONSIDER flesh this out.
  */
class ValueSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Value"

  it should "signum" in {
    signum(fromInt(1)) shouldBe 1
    signum(fromInt(0)) shouldBe 0
    signum(fromInt(-1)) shouldBe -1
  }

  it should "isEqual" in {
    isEqual(fromInt(1), fromInt(1)) shouldBe true
    isEqual(fromInt(1), fromRational(Rational.one)) shouldBe true
    isEqual(fromInt(1), fromDouble(Some(1.0))) shouldBe true
    isEqual(fromInt(1), fromInt(2)) shouldBe false
  }

  it should "maybeDouble" in {
    maybeDouble(fromDouble(Some(0.5))) shouldBe Some(0.5)
    maybeDouble(fromDouble(None)) shouldBe None
  }

  it should "valueToString 1" in {
    valueToString(fromRational(Rational("22/7"))) shouldBe "3.<142857>"
  }
  it should "valueToString 2" in {
    valueToString(fromRational(Rational("1/524287"))) shouldBe "0.000001907352270798245998851773932979456..."
  }

  it should "maybeInt" in {
    maybeInt(fromInt(1)) shouldBe Some(1)
  }

  it should "maybeRational" in {
    maybeRational(fromRational(Rational("1/2"))) shouldBe Some(Rational.half)
  }

  it should "abs" in {

  }

  it should "scaleDouble" in {

  }
}
