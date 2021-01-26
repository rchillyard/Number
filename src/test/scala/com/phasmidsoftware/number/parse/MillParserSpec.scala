package com.phasmidsoftware.number.parse

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.Success

class MillParserSpec extends AnyFlatSpec with should.Matchers {

  behavior of "MillParser"

  val p = new MillParser

  //  it should "parse mill" in {
  //    val string = "42 37 + 2 *"
  //    p.parseAll(p.mill, string) should matchPattern { case p.Success(_, _) => }
  //  }

  //  it should "parse terms" in {
  //    val string = "42 37 + 2 *"
  //    p.parseAll(p.terms, string) should matchPattern { case p.Success(_, _) => }
  //  }

  it should "parse term" in {
    p.parseAll(p.term, "42") should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.term, "chs") should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.term, "CHS") should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.term, "+") should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.term, "*") should matchPattern { case p.Success(_, _) => }
  }

  it should "parse mill" in {
    p.parseAll(p.mill, "42") should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.mill, "7 chs") should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.mill, "42 37 +") should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.mill, "42 37 + 2 *") should matchPattern { case p.Success(_, _) => }
  }

  it should "parse number" in {
    p.parseAll(p.number, "42") should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.number, "-42") should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.number, "+") should matchPattern { case p.Failure(_, _) => }
  }

  //  it should "parse dyadicTerm" in {
  //    p.parseAll(p.dyadicTerm, "42 37 +") should matchPattern { case p.Success(_, _) => }
  //    p.parseAll(p.dyadicTerm, "42 37 *") should matchPattern { case p.Success(_, _) => }
  //    p.parseAll(p.dyadicTerm, "42 37 + 2 *") should matchPattern { case p.Success(_, _) => }
  //  }

  it should "parseMill" in {
    val string = "42 37 + 2 *"
    p.parseMill(string) should matchPattern { case Success(_) => }
  }

  //  it should "parse monadicTerm" in {
  //    p.parseAll(p.monadicTerm, "7 -") should matchPattern { case p.Success(_, _) => }
  //    p.parseAll(p.monadicTerm, "2 inv") should matchPattern { case p.Success(_, _) => }
  //  }

  //  it should "parse anadicTerm" in {
  //    p.parseAll(p.anadicTerm, "42") should matchPattern { case p.Success(_, _) => }
  //  }

}
