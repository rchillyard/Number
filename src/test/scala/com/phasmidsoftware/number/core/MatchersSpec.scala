package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Number.one
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MatchersSpec extends AnyFlatSpec with should.Matchers {

  private val p = new Matchers {}

  behavior of "Matcher method"

  it should "work with fixed success result" in {
    val f: p.Matcher[Expression, Number] = p.Matcher(_ => p.Match(one))
    val e = Literal(one)
    f(e).success shouldBe true
  }
  it should "work with fixed fail result" in {
    val f: p.Matcher[Expression, Number] = p.Matcher(e => p.Miss(e))
    val e = Literal(Number.one)
    f(e).success shouldBe false
  }

  behavior of "success"
  it should "work with Number.one" in {
    val f = p.success[Expression, Number](one)
    f(Literal(one)).success shouldBe true
  }

  behavior of "fail"
  it should "work with fixed fail result" in {
    val f = p.fail[Expression, Number]
    f(Literal(one)).success shouldBe false
  }

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

  behavior of "|"
  it should "work with | 1 or 2" in {
    val f = p.matchValue(one)
    val g = f | p.matchValue(Number.pi)
    f(Literal(one)).success shouldBe true
    g(Literal(Number.pi)).success shouldBe true
    g(Literal(Number.e)).success shouldBe false
  }
}
