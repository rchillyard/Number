package com.phasmidsoftware.number.expression.parse

import com.phasmidsoftware.number.core.inner.Rational
import org.scalatest.flatspec
import org.scalatest.matchers.should
import scala.util.*


/**
  * Specification for ArithmeticParser.
  *
  * CONSIDER move to parse package.
  *
  * @author scalaprof
  */
class ArithmeticParserSpec extends flatspec.AnyFlatSpec with should.Matchers {

  behavior of "DoubleArithmeticParser$.factor"
  it should "parse 3.1415927" in {
    val parser = DoubleArithmeticParser
    parser.parseAll(parser.factor, "3.1415927") should matchPattern { case parser.Success(_, _) => }
  }
  "DoubleArithmeticParser(1)" should "be 1.0" in {
    val parser = DoubleArithmeticParser
    parser.parseAll(parser.factor, "3.1415927") should matchPattern { case parser.Success(_, _) => }
  }
  it should "parse (1)" in {
    val parser = DoubleArithmeticParser
    parser.parseAll(parser.factor, "(1)") should matchPattern { case parser.Success(_, _) => }
  }
  it should "fail pi" in {
    val parser = DoubleArithmeticParser
    parser.parseAll(parser.factor, "pi") should matchPattern { case parser.Failure("factor", _) => }
  }

  behavior of "DoubleArithmeticParser.term"
  it should "parse 1*2" in {
    val parser = DoubleArithmeticParser
    parser.parseAll(parser.term, "1*2") should matchPattern { case parser.Success(_, _) => }
  }
  it should "parse 1/2" in {
    val parser = DoubleArithmeticParser
    parser.parseAll(parser.term, "1/2") should matchPattern { case parser.Success(_, _) => }
  }
  it should "parse 3.1415927" in {
    val parser = DoubleArithmeticParser
    parser.parseAll(parser.term, "3.1415927") should matchPattern { case parser.Success(_, _) => }
  }
  it should "parse (1)" in {
    val parser = DoubleArithmeticParser
    parser.parseAll(parser.term, "(1)") should matchPattern { case parser.Success(_, _) => }
  }
  it should "fail pi" in {
    val parser = DoubleArithmeticParser
    parser.parseAll(parser.term, "pi") should matchPattern { case parser.Failure("factor", _) => }
  }
  it should "fail 1+2" in {
    val parser = DoubleArithmeticParser
    parser.parseAll(parser.term, "1+2") should matchPattern { case parser.Failure(_, _) => }
  }

  behavior of "DoubleArithmeticParser.expr"
  it should "parse 1 as 1.0" in {
    val parser = DoubleArithmeticParser
    val r = parser.parseAll(parser.expr, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(1.0) => }
  }
  it should "parse 1+1 as 2.0" in {
    val parser = DoubleArithmeticParser
    val r = parser.parseAll(parser.expr, "1+1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(2.0) => }
  }
  it should "parse 1*2+1 as 3.0" in {
    val parser = DoubleArithmeticParser
    val r = parser.parseAll(parser.expr, "1*2+1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(3.0) => }
  }
  it should "parse 1*2+1-1.5 as 1.5" in {
    val parser = DoubleArithmeticParser
    val r = parser.parseAll(parser.expr, "1*2+1-1.5")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(1.5) => }
  }
  it should "parse 1/0 as infinite" in {
    val parser = DoubleArithmeticParser
    val r = parser.parseAll(parser.expr, "1/0")
    r.get.value should matchPattern { case Success(Double.PositiveInfinity) => }
  }
  it should "parse 1*2+1-3/2 as 1.5" in {
    val parser = DoubleArithmeticParser
    val r = parser.parseAll(parser.expr, "1*2+1-3/2")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(1.5) => }
  }
  it should "parse 10/3 as ..." in {
    val parser = DoubleArithmeticParser
    val r = parser.parseAll(parser.expr, "10/3")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(3.333333333333333) => }
  }
  it should "succeed for DoubleArithmeticParser(2+1-3/2)" in {
    val parser = DoubleArithmeticParser
    val r = parser.parseAll(parser.expr, "2+1-3/2")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(1.5) => }
  }
  it should "succeed DoubleArithmeticParser(5*(6+1-(3/2))/4)" in {
    val parser = DoubleArithmeticParser
    val r = parser.parseAll(parser.expr, "5*(6+1-(3/2))/4")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(6.875) => }
  }
  it should "succeed DoubleArithmeticParser(5*(6+1-3/2)/4) even without explicit parentheses" in {
    val parser = DoubleArithmeticParser
    val r: parser.ParseResult[parser.Expr] = parser.parseAll(parser.expr, "5*(6+1-3/2)/4")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(6.875) => }
  }
  it should "fail to parse 1*2+1-pi/2" in {
    val parser = DoubleArithmeticParser
    val r = parser.parseAll(parser.expr, "1*2+1-pi/2")
    r should matchPattern { case parser.Failure(_, _) => }
  }
  it should "fail to parse (1?2)" in {
    val parser = DoubleArithmeticParser
    val r = parser.parseAll(parser.expr, "(1?2)")
    r should matchPattern { case parser.Failure(_, _) => }
    r match {
      case parser.Failure(m, _) => m shouldBe "')' expected but '?' found"
      case _ => fail("should fail")
    }
  }
  "RationalArithmeticParser(1)" should "be 1" in {
    val parser = RationalArithmeticParser
    val r = parser.parseAll(parser.expr, "1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(Rational.one) => }
  }
  "RationalArithmeticParser(1+1)" should "be 2/1" in {
    val parser = RationalArithmeticParser
    val r = parser.parseAll(parser.expr, "1+1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(Rational(x, Rational.bigOne)) if x.toInt == 2 => }
  }
  it should "parse 1*2+1 as 3/1" in {
    val parser = RationalArithmeticParser
    val r = parser.parseAll(parser.expr, "1*2+1")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(Rational(x, Rational.bigOne)) if x.toInt == 3 => }
  }
  it should "parse 1*2+1-3/2 as 3/2" in {
    val parser = RationalArithmeticParser
    val r = parser.parseAll(parser.expr, "1*2+1-3/2")
    r should matchPattern { case parser.Success(_, _) => }
    r.get.value should matchPattern { case Success(Rational(x, y)) if x.toInt == 3 && y.toInt == 2 => }
  }
  "RationalArithmeticParser(1/0)" should "be infinite" in {
    val parser = RationalArithmeticParser
    val r = parser.parseAll(parser.expr, "1/0")
    r.get.value should matchPattern { case Success(Rational.infinity) => }
  }
  "(" should "fail" in {
    val parser = DoubleArithmeticParser
    val r = parser.parseAll(parser.expr, "(")
    r should matchPattern { case parser.Failure("factor", _) => }
  }
  it should "fail 1+2=2" in {
    val parser = DoubleArithmeticParser
    val r = parser.parseAll(parser.expr, "1+2=2")
    r should matchPattern { case parser.Failure(_, _) => }
    r match {
      case parser.Failure(m, _) => m shouldBe "'/' expected but '=' found"
      case _ => fail("should fail")
    }
  }
  "IntArithmeticParser(3/2)" should "fail" in {
    val parser = IntArithmeticParser
    val r = parser.parseAll(parser.expr, "3/2")
    an[IllegalArgumentException] should be thrownBy r.get.value
  }
}
