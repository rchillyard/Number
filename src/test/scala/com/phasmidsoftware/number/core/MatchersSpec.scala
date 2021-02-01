package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Number.one
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MatchersSpec extends AnyFlatSpec with should.Matchers {

  private val m = new Matchers {}

  behavior of "Matcher method"

  it should "work with fixed success result" in {
    val f: m.Matcher[Expression, Number] = m.Matcher(_ => m.Match(one))
    val e = Literal(one)
    f(e).success shouldBe true
  }
  it should "work with fixed fail result" in {
    val f: m.Matcher[Expression, Number] = m.Matcher(e => m.Miss(e))
    val e = Literal(Number.one)
    f(e).success shouldBe false
  }

  behavior of "success"
  it should "work with Number.one" in {
    val f = m.success[Expression, Number](one)
    f(Literal(one)).success shouldBe true
  }

  behavior of "fail"
  it should "work with fixed fail result" in {
    val f = m.fail[Expression, Number]
    f(Literal(one)).success shouldBe false
  }

  behavior of "|"
  it should "work with | 1 or 2" in {
    // TODO move this to ExpressionMatchers and replace with Matchers-based test
    val q = new ExpressionMatchers {}
    val f = q.matchValue(one)
    val g = f | q.matchValue(Number.pi)
    f(Literal(one)).success shouldBe true
    g(Literal(Number.pi)).success shouldBe true
    g(Literal(Number.e)).success shouldBe false
  }

  behavior of "match2Any"
  it should "succeed with toInt and 0" in {
    val p: m.Matcher[String, Int] = m.Matcher(w => m.Match(w.toInt))
    val q: m.Matcher[String, Int] = m.Matcher(_ => m.Match(0))
    val r: m.Matcher[(String, String), Int] = m.match2Any(p, q)
    val tuple = ("1", "")
    r(tuple).success shouldBe true
  }
  it should "succeed with toInt and fail" in {
    val p: m.Matcher[String, Int] = m.Matcher(w => m.Match(w.toInt))
    val q: m.Matcher[String, Int] = m.fail
    val r: m.Matcher[(String, String), Int] = m.match2Any(p, q)
    val tuple = ("1", "")
    r(tuple).success shouldBe true
  }
  it should "succeed with fail and toInt" in {
    val p: m.Matcher[String, Int] = m.Matcher(w => m.Match(w.toInt))
    val q: m.Matcher[String, Int] = m.fail
    val r: m.Matcher[(String, String), Int] = m.match2Any(q, p)
    val tuple = ("", "1")
    r(tuple).success shouldBe true
  }
  it should "fail with fail and fail" in {
    val p: m.Matcher[String, Int] = m.fail
    val q: m.Matcher[String, Int] = m.fail
    val r: m.Matcher[(String, String), Int] = m.match2Any(q, p)
    val tuple = ("1", "")
    r(tuple).success shouldBe false
  }

  behavior of "match2All"
  it should "succeed with toInt and 0" in {
    val p: m.Matcher[String, Int] = m.Matcher(w => m.Match(w.toInt))
    val q: m.Matcher[String, Int] = m.Matcher(_ => m.Match(0))
    val r: m.Matcher[(String, String), (Int, Int)] = m.match2All(p, q)
    val tuple = ("1", "")
    r(tuple).success shouldBe true
  }
  it should "fail with toInt and fail" in {
    val p: m.Matcher[String, Int] = m.Matcher(w => m.Match(w.toInt))
    val q: m.Matcher[String, Int] = m.fail
    val r: m.Matcher[(String, String), (Int, Int)] = m.match2All(p, q)
    val tuple = ("1", "")
    r(tuple).success shouldBe false
  }
  it should "fail with fail and toInt" in {
    val p: m.Matcher[String, Int] = m.Matcher(w => m.Match(w.toInt))
    val q: m.Matcher[String, Int] = m.fail
    val r: m.Matcher[(String, String), (Int, Int)] = m.match2All(q, p)
    val tuple = ("", "1")
    r(tuple).success shouldBe false
  }
  it should "fail with fail and fail" in {
    val p: m.Matcher[String, Int] = m.fail
    val q: m.Matcher[String, Int] = m.fail
    val r: m.Matcher[(String, String), (Int, Int)] = m.match2All(q, p)
    val tuple = ("", "1")
    r(tuple).success shouldBe false
  }

  behavior of "opt"
  it should "succeed with a match" in {
    val p: m.Matcher[String, Int] = m.Matcher(w => m.Match(w.toInt))
    val q = m.opt(p)
    q("1") should matchPattern { case m.Match(Some(1)) => }
  }
}
