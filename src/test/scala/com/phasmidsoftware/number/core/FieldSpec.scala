package com.phasmidsoftware.number.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class FieldSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Field"

  it should "unary_$minus" in {

  }

  it should "add" in {

  }

  it should "divide" in {

  }

  it should "isZero" in {

  }

  it should "magnitudeSquared" in {

  }

  it should "power1" in {

  }

  it should "power2" in {

  }

  it should "invert" in {

  }

  it should "asNumber" in {

  }

  it should "isInfinite" in {

  }

  it should "isExact" in {
    //    GeneralNumber(4).power(convertToNumber((GeneralNumber.one / 2).materialize)).isExact shouldBe true
    //    GeneralNumber(2).power(convertToNumber((GeneralNumber.one / 2).materialize)).isExact shouldBe false
    val x = (GeneralNumber.pi / 2).materialize
    x.isExact shouldBe true
  }

  it should "multiply" in {

  }

  it should "recover" in {

  }

  it should "convertToNumber" in {

  }

}
