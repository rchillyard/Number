package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Number.one
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Success, Try}

class MatchersSpec extends AnyFlatSpec with should.Matchers {

  private val m = new Matchers {}

  behavior of "Match"

  it should "support success" in {
    val target = new m.Match[Int](0)
    val value1 = target.success(1)
    value1.successful shouldBe true
    value1.get shouldBe 1
  }
  it should "support successful" in {
    m.success(0)("").successful shouldBe true
    m.fail("").successful shouldBe false
  }
  it should "support isEmpty" in {
    m.success(0)("").isEmpty shouldBe false
    m.fail("").isEmpty shouldBe true
  }
  it should "support always" in {
    val target = m.always[Unit]
    target(()).successful shouldBe true
  }
  it should "support fail" in {
    val target = m.fail[Unit, Unit]
    target(()).successful shouldBe false
  }
  it should "support |" in {
    val result = m.fail("") | m.success(0)
    result.successful shouldBe true
  }
  it should "support ||" in {
    val result = m.fail("") || m.success(0)("")
    result.successful shouldBe true
  }
  it should "support &" in {
    val result = m.success(0)("") & m.success[Any, Int](0)
    result.successful shouldBe true
  }
  it should "support &&" in {
    val result = m.success(0)("") && m.success(1)("")
    result.successful shouldBe true
    result.get shouldBe 0 -> 1
  }
  it should "support map" in {
    val result = m.success(0)("").map(_.toString)
    result.successful shouldBe true
    result.get shouldBe "0"
  }
  it should "support flatMap" in {
    val result = m.success(0)("").flatMap(x => m.Match(x.toString))
    result.successful shouldBe true
    result.get shouldBe "0"
  }

  behavior of "Matcher class"

  it should "support map" in {
    val target = new m.Matcher[String, Int] {
      def apply(v1: String): m.MatchResult[Int] = m.Match(v1.toInt)
    }
    val q: m.Matcher[String, Double] = target map (_.toDouble)
    q("1") shouldBe m.Match(1.0)
  }
  it should "support ^^" in {
    val target = new m.Matcher[String, Int] {
      def apply(v1: String): m.MatchResult[Int] = m.Match(v1.toInt)
    }
    val q: m.Matcher[String, Double] = target ^^ (_.toDouble)
    q("1") shouldBe m.Match(1.0)
  }

  behavior of "Matcher method"

  it should "work with fixed success result" in {
    val f: m.Matcher[Expression, Number] = m.Matcher(_ => m.Match(one))
    val e = Literal(one)
    f(e).successful shouldBe true
  }
  it should "work with fixed fail result" in {
    val f: m.Matcher[Expression, Number] = m.Matcher(e => m.Miss(e))
    val e = Literal(Number.one)
    f(e).successful shouldBe false
  }

  behavior of "success"
  it should "work with Number.one" in {
    val f = m.success[Expression, Number](one)
    f(Literal(one)).successful shouldBe true
  }

  behavior of "fail"
  it should "work with fixed fail result" in {
    val f = m.fail[Expression, Number]
    f(Literal(one)).successful shouldBe false
  }

  behavior of "|"
  it should "work with | 1 or 2" in {
    // TODO move this to ExpressionMatchers and replace with Matchers-based test
    val q = new ExpressionMatchers {}
    val f = q.matchValue(one)
    val g = f | q.matchValue(Number.pi)
    f(Literal(one)).successful shouldBe true
    g(Literal(Number.pi)).successful shouldBe true
    g(Literal(Number.e)).successful shouldBe false
  }

  behavior of "not"

  it should "work" in {
    val p = m.matches(1)
    val q = m.not(p, 0)
    p(1).successful shouldBe true
    q(1).successful shouldBe false
    q(2).successful shouldBe true
    q(2).get shouldBe 0
  }

  behavior of "having"

  case class Wrapper(i: Int)

  it should "work" in {
    val p = m.matches(1)
    val q: m.Matcher[Wrapper, Int] = m.having(p)(_.i)
    q(Wrapper(1)).successful shouldBe true
  }

  behavior of "matchProduct2Any"

  case class StringPair(t1: String, t2: String)

  it should "succeed with toInt and 0" in {
    val p: m.Matcher[String, Int] = m.Matcher(w => m.Match(w.toInt))
    val q: m.Matcher[String, Int] = m.Matcher(_ => m.Match(0))
    val f: (String, String) => StringPair = StringPair.apply
    val r: m.Matcher[StringPair, Int] = m.matchProduct2Any(p, q)(f)
    val tuple = StringPair("1", "")
    r(tuple).successful shouldBe true
  }
  it should "succeed with toInt and fail" in {
    val p: m.Matcher[String, Int] = m.Matcher(w => m.Match(w.toInt))
    val q: m.Matcher[String, Int] = m.fail
    val f: (String, String) => StringPair = StringPair.apply
    val r: m.Matcher[StringPair, Int] = m.matchProduct2Any(p, q)(f)
    val tuple = StringPair("1", "")
    r(tuple).successful shouldBe true
  }
  it should "succeed with fail and toInt" in {
    val p: m.Matcher[String, Int] = m.Matcher(w => m.Match(w.toInt))
    val q: m.Matcher[String, Int] = m.fail
    val f: (String, String) => StringPair = StringPair.apply
    val r: m.Matcher[StringPair, Int] = m.matchProduct2Any(q, p)(f)
    val tuple = StringPair("", "1")
    r(tuple).successful shouldBe true
  }
  it should "fail with fail and fail" in {
    val p: m.Matcher[String, Int] = m.fail
    val q: m.Matcher[String, Int] = m.fail
    val f: (String, String) => StringPair = StringPair.apply
    val r: m.Matcher[StringPair, Int] = m.matchProduct2Any(p, q)(f)
    val tuple = StringPair("1", "")
    r(tuple).successful shouldBe false
  }

  behavior of "match2Any"
  it should "succeed with toInt and 0" in {
    val p: m.Matcher[String, Int] = m.Matcher(w => m.Match(w.toInt))
    val q: m.Matcher[String, Int] = m.Matcher(_ => m.Match(0))
    val r: m.Matcher[(String, String), Int] = m.match2Any(p, q)
    val tuple = ("1", "")
    r(tuple).successful shouldBe true
  }
  it should "succeed with toInt and fail" in {
    val p: m.Matcher[String, Int] = m.Matcher(w => m.Match(w.toInt))
    val q: m.Matcher[String, Int] = m.fail
    val r: m.Matcher[(String, String), Int] = m.match2Any(p, q)
    val tuple = ("1", "")
    r(tuple).successful shouldBe true
  }
  it should "succeed with fail and toInt" in {
    val p: m.Matcher[String, Int] = m.Matcher(w => m.Match(w.toInt))
    val q: m.Matcher[String, Int] = m.fail
    val r: m.Matcher[(String, String), Int] = m.match2Any(q, p)
    val tuple = ("", "1")
    r(tuple).successful shouldBe true
  }
  it should "fail with fail and fail" in {
    val p: m.Matcher[String, Int] = m.fail
    val q: m.Matcher[String, Int] = m.fail
    val r: m.Matcher[(String, String), Int] = m.match2Any(q, p)
    val tuple = ("1", "")
    r(tuple).successful shouldBe false
  }

  behavior of "match3Any"
  it should "succeed with toInt and 0" in {
    val p: m.Matcher[String, Int] = m.Matcher(w => m.Match(w.toInt))
    val q: m.Matcher[String, Int] = m.Matcher(_ => m.Match(0))
    val z: m.Matcher[String, Int] = m.success(1 / 0)
    val r: m.Matcher[(String, String, String), Int] = m.match3Any(p, q, z)
    val tuple = ("1", "", "junk")
    r(tuple).successful shouldBe true
  }
  it should "succeed with toInt and fail" in {
    val p: m.Matcher[String, Int] = m.Matcher(w => m.Match(w.toInt))
    val q: m.Matcher[String, Int] = m.fail
    val z: m.Matcher[String, Int] = m.success(1 / 0)
    val r: m.Matcher[(String, String, String), Int] = m.match3Any(p, q, z)
    val tuple = ("1", "", "junk")
    r(tuple).successful shouldBe true
  }
  it should "succeed with fail and toInt" in {
    val p: m.Matcher[String, Int] = m.Matcher(w => m.Match(w.toInt))
    val q: m.Matcher[String, Int] = m.fail
    val z: m.Matcher[String, Int] = m.success(1 / 0)
    val r: m.Matcher[(String, String, String), Int] = m.match3Any(p, q, z)
    val tuple = ("1", "", "junk")
    r(tuple).successful shouldBe true
  }
  it should "fail with fail and fail" in {
    val p: m.Matcher[String, Int] = m.fail
    val q: m.Matcher[String, Int] = m.fail
    val z: m.Matcher[String, Int] = m.success(1 / 0)
    val r: m.Matcher[(String, String, String), Int] = m.match3Any(p, q, z)
    val tuple = ("1", "", "junk")
    an[ArithmeticException] shouldBe thrownBy(r(tuple))
  }

  behavior of "match2All"
  it should "succeed with toInt and 0" in {
    val p: m.Matcher[String, Int] = m.Matcher(w => m.Match(w.toInt))
    val q: m.Matcher[String, Int] = m.success(0)
    val r: m.Matcher[(String, String), (Int, Int)] = m.match2All(p, q)
    val tuple = ("1", "")
    r(tuple).successful shouldBe true
  }
  it should "fail with toInt and fail" in {
    val p: m.Matcher[String, Int] = m.Matcher(w => m.Match(w.toInt))
    val q: m.Matcher[String, Int] = m.fail
    val r: m.Matcher[(String, String), (Int, Int)] = m.match2All(p, q)
    val tuple = ("1", "")
    r(tuple).successful shouldBe false
  }
  it should "fail with fail and toInt" in {
    val p: m.Matcher[String, Int] = m.Matcher(w => m.Match(w.toInt))
    val q: m.Matcher[String, Int] = m.fail
    val r: m.Matcher[(String, String), (Int, Int)] = m.match2All(q, p)
    val tuple = ("", "1")
    r(tuple).successful shouldBe false
  }
  it should "fail with fail and fail" in {
    val p: m.Matcher[String, Int] = m.fail
    val q: m.Matcher[String, Int] = m.fail
    val r: m.Matcher[(String, String), (Int, Int)] = m.match2All(q, p)
    val tuple = ("", "1")
    r(tuple).successful shouldBe false
  }

  behavior of "match3All"
  it should "succeed with toInt and 0" in {
    val p: m.Matcher[String, Int] = m.Matcher(w => m.Match(w.toInt))
    val q: m.Matcher[String, Int] = m.success(0)
    val z: m.Matcher[String, Int] = m.success(1)
    val r: m.Matcher[(String, String, String), (Int, Int, Int)] = m.match3All(p, q, z)
    val tuple = ("1", "", "junk")
    r(tuple).successful shouldBe true
  }
  it should "fail with toInt and fail" in {
    val p: m.Matcher[String, Int] = m.Matcher(w => m.Match(w.toInt))
    val q: m.Matcher[String, Int] = m.fail
    val z: m.Matcher[String, Int] = m.success(1 / 0)
    val r: m.Matcher[(String, String, String), (Int, Int, Int)] = m.match3All(p, q, z)
    val tuple = ("1", "", "junk")
    r(tuple).successful shouldBe false
  }

  behavior of "opt"
  it should "succeed with a match" in {
    val p: m.Matcher[String, Int] = m.Matcher(w => m.Match(w.toInt))
    val q = m.opt(p)
    q("1") should matchPattern { case m.Match(Some(1)) => }
  }

  behavior of "?"
  it should "succeed with a match" in {
    val p: m.Matcher[String, Int] = m.Matcher(w => m.Match(w.toInt))
    val q: m.Matcher[String, Option[Int]] = p.?
    q("1") should matchPattern { case m.Match(Some(1)) => }
  }

  behavior of "trial"
  it should "succeed with a match" in {
    val p: m.Matcher[String, Int] = m.Matcher(w => m.Match(w.toInt))
    val q: m.Matcher[String, Try[Int]] = p.trial
    q("1") should matchPattern { case m.Match(Success(1)) => }
  }
}
