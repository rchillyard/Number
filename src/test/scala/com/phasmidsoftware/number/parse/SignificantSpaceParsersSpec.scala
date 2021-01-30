package com.phasmidsoftware.number.parse

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Failure, Success}

class SignificantSpaceParsersSpec extends AnyFlatSpec with should.Matchers {

  behavior of "SignificantSpaceParsers"

  private val p = new SignificantSpaceParsers {}

  it should "stringParser" in {
  }

  it should "match for RegexOps" in {
    val q: p.Parser[String] = p.RegexOps("Hello".r) :| "hello"
    p.stringParser(q, "Hello") should matchPattern { case Success(_) => }
  }

  it should "fail for RegexOps" in {
    val q: p.Parser[String] = p.RegexOps("hello".r) :| "hello"
    p.stringParser(q, "Hello") should matchPattern { case Failure(SignificantSpaceParserException("hello")) => }
  }

  it should "ParserOps" in {

  }

  it should "ParserOptionOps" in {

  }

  it should "trim" in {

  }

  it should "repSepSp" in {

  }

}
