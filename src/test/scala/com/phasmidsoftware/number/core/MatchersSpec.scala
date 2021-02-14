package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Number.one
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.util.NoSuchElementException
import scala.util.{Success, Try}

class MatchersSpec extends AnyFlatSpec with should.Matchers {

  private val m = new Matchers {}

  behavior of "MatchResult"
  it should "implement apply(Boolean, T, R)" in {
    m.MatchResult(b = true, "1", 1) shouldBe m.Match(1)
    m.MatchResult(b = false, "1", 1) shouldBe m.Miss("1")
  }
  it should "implement apply(Either)" in {
    val e1: Either[String, Int] = Left("1")
    val e2: Either[String, Int] = Right(1)
    m.MatchResult(e2) shouldBe m.Match(1)
    m.MatchResult(e1) shouldBe m.Miss("1")
  }
  it should "implement apply((Q,R) => Boolean)" in {
    val v1: m.MatchResult[Int] = m.MatchResult.create[Int, String, Int](m.isEqual)(1, "1", 1)
    v1 shouldBe m.Match(1)
    m.MatchResult.create[Int, String, Int](m.isEqual)(1, "1", 2) shouldBe m.Miss("1")
  }
  it should "implement apply(T=>R, (Q,R) => Boolean))" in {
    val v1: m.MatchResult[Int] = m.MatchResult[Int, String, Int]({ s: String => s.toInt }, m.isEqual)(1, "1")
    v1 shouldBe m.Match(1)
    m.MatchResult.create[Int, String, Int](m.isEqual)(1, "1", 2) shouldBe m.Miss("1")
  }

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
  it should "support error" in {
    val target = m.error[Unit](new NoSuchElementException)
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

  behavior of "matches"
  it should "match 1" in {
    val f = m.matches(1)
    f(1).successful shouldBe true
  }

  behavior of "valve (1)"
  it should "match 1" in {
    val p: (Int, Int) => Boolean = {
      case (x, y) => x == y
    }
    val f: m.Matcher[(Int, Int), Int] = m.valve(p)
    f(1, 1).successful shouldBe true
  }
  it should "match parity" in {
    val p: (Int, Int) => Boolean = {
      case (x, y) => y % 2 == x
    }
    val f: m.Matcher[(Int, Int), Int] = m.valve(p)
    val odd = 1
    val even = 0
    f(odd, 3) shouldBe m.Match(3)
    f(even, 4) shouldBe m.Match(4)
    f(even, 3) shouldBe m.Miss(3)
  }

  behavior of "valve (2)"
  it should "match 1" in {
    val p: (Int, Int) => Boolean = {
      case (x, y) => x == y
    }
    val z: m.Matcher[(Int, String), Int] = m.valve(_.toInt, p)
    z(1, "1").successful shouldBe true
  }
  it should "match parity" in {
    val p: (Int, Int) => Boolean = {
      case (x, y) => y % 2 == x
    }
    val z: m.Matcher[(Int, String), Int] = m.valve(_.toInt, p)
    val odd = 1
    val even = 0
    z(odd, "3") shouldBe m.Match(3)
    z(even, "4") shouldBe m.Match(4)
    z(even, "3") shouldBe m.Miss("3")
  }

  behavior of "chain"
  it should "work for first form" in {
    val target = m.lift[String, Int](_.toInt)
    val p = m.valve[Int, Int] { case (q, r) => q == r }
    val z = target chain p
    z(1, "1").successful shouldBe true
  }
  it should "work for second form" in {
    val target = m.lift[String, Int](_.toInt)
    val p = m.matches[(String, Int)](("1", 1))
    val z = target chain p
    z("1", "1") shouldBe m.Match(("1", 1))
  }

  behavior of "|"
  it should "work with | 1 or 2" in {
    val f = m.matches(1)
    val g = f | m.matches(2)
    g(1).successful shouldBe true
    g(2).successful shouldBe true
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

  behavior of "swap"
  it should "work" in {
    val t = (1, "1")
    val u: m.MatchResult[(String, Int)] = m.swap(t)
    u shouldBe m.Match("1", 1)
  }

  behavior of "rotate3"
  it should "work" in {
    val t = (1, "1", 1.0)
    val u: m.MatchResult[(String, Double, Int)] = m.rotate3(t)
    u shouldBe m.Match("1", 1.0, 1)
  }

  behavior of "invert3"
  it should "work" in {
    val t = (1, "1", 1.0)
    val u: m.MatchResult[(Double, String, Int)] = m.invert3(t)
    u shouldBe m.Match(1.0, "1", 1)
  }

  behavior of "~"
  it should "match (1,2) and result in (1,2)" in {
    val p = m.matches(1)
    val q = m.matches(2)
    val z = p ~ q
    val result = z(1 -> 2)
    result.successful shouldBe true
    result.get shouldBe (1 -> 2)
  }
  it should "match (1,2) and result in 2" in {
    val p = m.matches(1)
    val q = m.matches(2)
    val z = p ~> q
    val result = z(1 -> 2)
    result.successful shouldBe true
    result.get shouldBe 2
  }
  it should "match (1,2) and result in 1" in {
    val p = m.matches(1)
    val q = m.matches(2)
    val z = p <~ q
    val result = z(1 -> 2)
    result.successful shouldBe true
    result.get shouldBe 1
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
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.success(0)
    val r: m.Matcher[StringPair, Int] = m.matchProduct2Any(p, q)(StringPair)
    val tuple = StringPair("1", "")
    r(tuple).successful shouldBe true
  }
  it should "succeed with toInt and fail" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.fail
    val r: m.Matcher[StringPair, Int] = m.matchProduct2Any(p, q)(StringPair)
    val tuple = StringPair("1", "")
    r(tuple).successful shouldBe true
  }
  it should "succeed with fail and toInt" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.fail
    val r: m.Matcher[StringPair, Int] = m.matchProduct2Any(q, p)(StringPair)
    val tuple = StringPair("", "1")
    r(tuple).successful shouldBe true
  }
  it should "fail with fail and fail" in {
    val p: m.Matcher[String, Int] = m.fail
    val q: m.Matcher[String, Int] = m.fail
    val r: m.Matcher[StringPair, Int] = m.matchProduct2Any(p, q)(StringPair)
    val tuple = StringPair("1", "")
    r(tuple).successful shouldBe false
  }

  behavior of "matchProduct2All"
  it should "succeed with toInt and 0" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.success(0)
    val r: m.Matcher[StringPair, (Int, Int)] = m.matchProduct2All(p, q)(StringPair)
    val tuple = StringPair("1", "")
    r(tuple).successful shouldBe true
  }

  behavior of "matchProduct3All"

  case class Triple(t1: String, t2: String, t3: Int)

  it should "succeed with toInt and 0 and identity" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.success(0)
    val z: m.Matcher[Int, Int] = m.lift(identity)
    val r: m.Matcher[Triple, (Int, Int, Int)] = m.matchProduct3All(p, q, z)(Triple.apply)
    val tuple = Triple("1", "", 0)
    r(tuple).successful shouldBe true
  }

  behavior of "flip"
  it should "work" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[Int, Double] = m.lift(_.toDouble)
    val z: m.Matcher[(String, Int), (Int, Double)] = p ~ q
    z("1", 1) should matchPattern { case m.Match((1, 1.0)) => }
    m.flip(z)(1, "1") should matchPattern { case m.Match((1, 1.0)) => }
  }

  behavior of "tuple2"
  it should "work" in {
    val p: m.Matcher[StringPair, (String, String)] = m.tuple2(StringPair)
    p(StringPair("x", "y")) should matchPattern { case m.Match(("x", "y")) => }
  }

  behavior of "tuple3"
  it should "work" in {
    val p: m.Matcher[Triple, (String, String, Int)] = m.tuple3(Triple)
    p(Triple("x", "y", 1)) should matchPattern { case m.Match(("x", "y", 1)) => }
  }

  behavior of "product2"
  it should "one-way" in {
    val p: m.Matcher[(String, String), StringPair] = m.product2(StringPair)
    p(("x", "y")) should matchPattern { case m.Match(StringPair("x", "y")) => }
  }
  it should "round-trip" in {
    val p: m.Matcher[StringPair, StringPair] = m.tuple2(StringPair) & m.product2(StringPair)
    p(StringPair("x", "y")) should matchPattern { case m.Match(StringPair("x", "y")) => }
  }

  behavior of "product3"
  it should "one-way" in {
    val p: m.Matcher[(String, String, Int), Triple] = m.product3(Triple)
    p(("x", "y", 1)) should matchPattern { case m.Match(Triple("x", "y", 1)) => }
  }
  it should "round-trip" in {
    val p: m.Matcher[Triple, Triple] = m.tuple3(Triple) & m.product3(Triple)
    p(Triple("x", "y", 1)) should matchPattern { case m.Match(Triple("x", "y", 1)) => }
  }

  behavior of "match2Any"
  it should "succeed with toInt and 0" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.success(0)
    val r: m.Matcher[(String, String), Int] = m.match2Any(p, q)
    val tuple = ("1", "")
    r(tuple).successful shouldBe true
  }
  it should "succeed with toInt and fail" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.fail
    val r: m.Matcher[(String, String), Int] = m.match2Any(p, q)
    val tuple = ("1", "")
    r(tuple).successful shouldBe true
  }
  it should "succeed with fail and toInt" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
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
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.success(0)
    val z: m.Matcher[String, Int] = m.success(1 / 0)
    val r: m.Matcher[(String, String, String), Int] = m.match3Any(p, q, z)
    val tuple = ("1", "", "junk")
    r(tuple).successful shouldBe true
  }
  it should "succeed with toInt and fail" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.fail
    val z: m.Matcher[String, Int] = m.success(1 / 0)
    val r: m.Matcher[(String, String, String), Int] = m.match3Any(p, q, z)
    val tuple = ("1", "", "junk")
    r(tuple).successful shouldBe true
  }
  it should "succeed with fail and toInt" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
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
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.success(0)
    val r: m.Matcher[(String, String), (Int, Int)] = m.match2All(p, q)
    val tuple = ("1", "")
    r(tuple).successful shouldBe true
  }
  it should "fail with toInt and fail" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.fail
    val r: m.Matcher[(String, String), (Int, Int)] = m.match2All(p, q)
    val tuple = ("1", "")
    r(tuple).successful shouldBe false
  }
  it should "fail with fail and toInt" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
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
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.success(0)
    val z: m.Matcher[String, Int] = m.success(1)
    val r: m.Matcher[(String, String, String), (Int, Int, Int)] = m.match3All(p, q, z)
    val tuple = ("1", "", "junk")
    r(tuple).successful shouldBe true
  }
  it should "fail with toInt and fail" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Int] = m.fail
    val z: m.Matcher[String, Int] = m.success(1 / 0)
    val r: m.Matcher[(String, String, String), (Int, Int, Int)] = m.match3All(p, q, z)
    val tuple = ("1", "", "junk")
    r(tuple).successful shouldBe false
  }

  behavior of "opt"
  it should "succeed with a match" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q = m.opt(p)
    q("1") should matchPattern { case m.Match(Some(1)) => }
  }

  behavior of "?"
  it should "succeed with a match" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Option[Int]] = p.?
    q("1") should matchPattern { case m.Match(Some(1)) => }
  }

  behavior of "trial"
  it should "succeed with a match" in {
    val p: m.Matcher[String, Int] = m.lift(_.toInt)
    val q: m.Matcher[String, Try[Int]] = p.trial
    q("1") should matchPattern { case m.Match(Success(1)) => }
  }
}
