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

  behavior of "|"
  it should "work with | 1 or 2" in {
    val q = new ExpressionMatchers {}
    val f = q.matchValue(one)
    val g = f | q.matchValue(Number.pi)
    f(Literal(one)).success shouldBe true
    g(Literal(Number.pi)).success shouldBe true
    g(Literal(Number.e)).success shouldBe false
  }
}
