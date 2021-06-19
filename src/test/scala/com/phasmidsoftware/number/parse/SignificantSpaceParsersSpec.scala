package com.phasmidsoftware.number.parse

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Failure, Success}

class SignificantSpaceParsersSpec extends AnyFlatSpec with should.Matchers {

  behavior of "SignificantSpaceParsers"

  private val ssp: SignificantSpaceParsers = new SignificantSpaceParsers {}

  it should "stringParser" in {
    class Tester extends SignificantSpaceParsers {
      def y: Parser[String] = "y".r :| "y"
    }
    val p = new Tester()
    p.stringParser(p.y, "y") should matchPattern { case Success("y") => }
  }

  it should "match for RegexOps" in {
    val q: ssp.Parser[String] = ssp.RegexOps("Hello".r) :| "hello"
    ssp.stringParser(q, "Hello") should matchPattern { case Success(_) => }
  }

  it should "fail for RegexOps" in {
    val q: ssp.Parser[String] = ssp.RegexOps("hello".r) :| "hello"
    val triedString = ssp.stringParser(q, "Hello")
    triedString should matchPattern { case Failure(SignificantSpaceParserException("""hello did not match "Hello" at offset 0""")) => }
  }

  it should "ParserOps" in {
    class Tester extends SignificantSpaceParsers {
      def yy: Parser[String ~ String] = y ~ y :| "yy"

      def y: Parser[String] = "y".r :| "y"
    }
    val p = new Tester()
    p.parseAll(p.yy, "y") should matchPattern { case p.Failure("y", _) => }
  }

  it should "ParserOptionOps 1" in {
    class Tester extends SignificantSpaceParsers {
      def test1: Parser[String] = x ?? y :| "test1"

      def x: Parser[Option[String]] = opt("x") :| "x"

      def y: Parser[String] = "y".r :| "y"
    }
    val p = new Tester
    p.parseAll(p.test1, "y") should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.test1, "z") should matchPattern { case p.Failure(_, _) => }
  }

  it should "ParserOptionOps 2" in {
    class Tester extends SignificantSpaceParsers {
      def test1: Parser[Either[Int, String]] = x ?| y :| "test1"

      def x: Parser[Option[String]] = opt("x") :| "x"

      def y: Parser[Int] = """\d+""".r :| "y" ^^ (_.toInt)
    }
    val p = new Tester
    p.parseAll(p.test1, "1") should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.test1, "z") should matchPattern { case p.Failure(_, _) => }
  }

  it should "trim" in {
    class Tester extends SignificantSpaceParsers {
      def trimmedY: Parser[String] = trim(y) :| "trimmedY"

      def y: Parser[String] = "y".r :| "y"
    }
    val p = new Tester()
    p.stringParser(p.trimmedY,
      """y
        |""".stripMargin) should matchPattern { case Success("y") => }
    p.stringParser(p.y,
      """y
        |""".stripMargin) should matchPattern { case Failure(_) => }
  }

  it should "repSepSp" in {
    class Tester extends SignificantSpaceParsers {
      def ys: Parser[List[String]] = repSepSp(y) :| "ys"

      def y: Parser[String] = "y".r :| "y"
    }
    val p = new Tester()
    p.stringParser(p.ys, "y y".stripMargin) should matchPattern { case Success(List("y", "y")) => }
  }
}
