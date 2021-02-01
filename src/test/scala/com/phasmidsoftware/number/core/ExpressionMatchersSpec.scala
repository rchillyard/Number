package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Number.one
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ExpressionMatchersSpec extends AnyFlatSpec with should.Matchers {

  private val p = new ExpressionMatchers {}

  behavior of "value"
  it should "work with value on Literal" in {
    val f = p.value
    f(Literal(one)).success shouldBe true
  }
  it should "work with value on One" in {
    val f = p.value
    f(One).success shouldBe true
  }
  it should "work with value on Number.one" in {
    val f = p.value
    f(one).success shouldBe true
  }
  it should "work with value on FuzzyNumber" in {
    val f = p.value
    f(FuzzyNumber(Right(1), Scalar, None)).success shouldBe true
  }

  behavior of "matchValue"
  it should "work with value 1" in {
    val f = p.matchValue(one)
    val e = Literal(one)
    f(e).success shouldBe true
  }
}
