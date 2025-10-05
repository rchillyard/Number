/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.inner

import com.phasmidsoftware.number.core.inner.Value._
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

  it should "isZero" in {
    isZero(fromInt(0)) shouldBe true
    isZero(fromRational(Rational.zero)) shouldBe true
    isZero(fromDouble(Some(0))) shouldBe true
    isZero(fromInt(1)) shouldBe false
    isZero(fromRational(Rational.one)) shouldBe false
    isZero(fromDouble(Some(1))) shouldBe false
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
    valueToString(fromRational(Rational("22/7")), skipOne = false) shouldBe "3.<142857>"
  }

  it should "valueToString 2" in {
    valueToString(fromRational(Rational("1/524287")), skipOne = false) shouldBe "0.000001907352270798245998851773932979456..."
  }

  it should "valueToString 3" in {
    valueToString(fromInt(1), skipOne = true) shouldBe ""
  }

  it should "maybeInt" in {
    maybeInt(fromInt(1)) shouldBe Some(1)
  }

  it should "maybeNominalRational" in {
    maybeRational(fromRational(Rational("1/2"))) shouldBe Some(Rational.half)
  }

  it should "abs" in {

  }

  it should "scaleDouble" in {

  }
}
