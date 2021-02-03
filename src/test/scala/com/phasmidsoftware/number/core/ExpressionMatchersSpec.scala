package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Number.one
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ExpressionMatchersSpec extends AnyFlatSpec with should.Matchers {

  private val p = new ExpressionMatchers {}

  behavior of "value"
  it should "work with value on Literal" in {
    val f: p.ExpressionMatcher[Number] = p.value
    f(Literal(one)).successful shouldBe true
  }
  it should "work with value on One" in {
    val f = p.value
    f(One).successful shouldBe true
  }
  it should "work with value on Number.one" in {
    val f = p.value
    f(one).successful shouldBe true
  }
  it should "work with value on FuzzyNumber" in {
    val f = p.value
    f(FuzzyNumber(Right(1), Scalar, None)).successful shouldBe true
  }

  behavior of "matchValue"
  it should "work with value 1" in {
    val f: p.ExpressionMatcher[Number] = p.matchValue(one)
    val e = Literal(one)
    f(e).successful shouldBe true
  }

  behavior of "|"
  it should "work with | 1 or 2" in {
    val q = new ExpressionMatchers {}
    val f = q.matchValue(one)
    val g = f | q.matchValue(Number.pi)
    f(Literal(one)).successful shouldBe true
    g(Literal(Number.pi)).successful shouldBe true
    g(Literal(Number.e)).successful shouldBe false
  }

}