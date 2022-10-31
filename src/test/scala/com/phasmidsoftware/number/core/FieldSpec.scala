package com.phasmidsoftware.number.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class FieldSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Field"

  it should "isExact" in {
    //    Number(4).power(convertToNumber((Number.one / 2).materialize)).isExact shouldBe true
    //    Number(2).power(convertToNumber((Number.one / 2).materialize)).isExact shouldBe false
    val x = (ConstPi / 2).materialize
    x.isExact(None) shouldBe true
  }

  it should "multiply i by itself correctly" in {
    val z = Expression(Constants.i) * Constants.i
    z.materialize shouldBe ComplexCartesian(-1, 0)
  }

}
