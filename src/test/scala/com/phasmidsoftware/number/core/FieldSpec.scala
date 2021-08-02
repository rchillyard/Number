package com.phasmidsoftware.number.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class FieldSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Field"

  it should "isExact" in {
    //    Number(4).power(convertToNumber((Number.one / 2).materialize)).isExact shouldBe true
    //    Number(2).power(convertToNumber((Number.one / 2).materialize)).isExact shouldBe false
    val x = (Number.pi / 2).materialize
    x.isExact shouldBe true
  }

  it should "multiply i by itself correctly" in {
    val z = Constants.i * Constants.i
    z.materialize.asComplex shouldBe ComplexCartesian(-1, 0)
  }

}
