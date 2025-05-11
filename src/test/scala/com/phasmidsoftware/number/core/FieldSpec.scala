package com.phasmidsoftware.number.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class FieldSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Field"

  it should "isExact" in {
    //    Number(4).power(convertToNumber((Number.one / 2).simplifyAndEvaluate)).isExact shouldBe true
    //    Number(2).power(convertToNumber((Number.one / 2).simplifyAndEvaluate)).isExact shouldBe false
    val x = (ConstPi / 2).materialize
    x.isExact shouldBe true
  }

  it should "multiply i by itself correctly" in {
    val z = Expression(Constants.i) * Constants.i
    val result = z.materialize
    result shouldBe Constants.minusOne
  }

  it should "add" in {
    val one = Number.one
    val result = one add Constants.i
    result shouldBe ComplexCartesian(1, 1)
  }

}
