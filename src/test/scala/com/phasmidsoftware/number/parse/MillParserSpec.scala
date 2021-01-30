package com.phasmidsoftware.number.parse

import com.phasmidsoftware.number.core.{ExactNumber, Scalar}
import com.phasmidsoftware.number.mill.{Add, Swap}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Failure, Success}

class MillParserSpec extends AnyFlatSpec with should.Matchers {

  behavior of "MillParser"

  val p = new MillParser

  it should "parse monadicOperator" in {
    p.parseAll(p.monadicOperator, "chs") should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.monadicOperator, "CHS") should matchPattern { case p.Success(_, _) => }
  }

  it should "parse dyadicOperator" in {
    p.parseAll(p.dyadicOperator, "+") should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.dyadicOperator, "*") should matchPattern { case p.Success(_, _) => }
  }

  it should "parse number" in {
    p.parseAll(p.number, "42") should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.number, "-42") should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.number, "+") should matchPattern { case p.Failure(_, _) => }
  }

  it should "parse dyadicTerm" in {
    p.parseAll(p.dyadicTerm, "42 37 >< +".reverse) should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.dyadicTerm, "42 37 ots >< *".reverse) should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.dyadicTerm, "42 37 + 2 *".reverse) should matchPattern { case p.Success(_, _) => }
  }

  it should "parse monadicTerm (1)" in {
    p.parseAll(p.monadicTerm, "7 shc".reverse) should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.monadicTerm, "2 vni".reverse) should matchPattern { case p.Success(_, _) => }
  }

  it should "parse monadicTerm (2)" in {
    p.parseAll(p.monadicTerm, "2 rlc shc".reverse) should matchPattern { case p.Success(_, _) => }
  }

  it should "parse anadicTerm" in {
    p.parseAll(p.anadicTerm, "42") should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.anadicTerm, "rcl") should matchPattern { case p.Success(_, _) => }
  }

  it should "parse term" in {
    p.parseAll(p.term, "42") should matchPattern { case p.Success(p.AnadicTerm(Right(ExactNumber(Right(42), Scalar))), _) => }
    p.parseAll(p.term, "42 37 >< +".reverse) should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.term, "7 shc".reverse) should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.term, "2 vni".reverse) should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.term, "42 37 +".reverse) should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.term, "42 37 *".reverse) should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.term, "42 37 + 2 *".reverse) should matchPattern { case p.Success(_, _) => }
  }

  it should "parse mill" in {
    p.parseAll(p.mill, "42") should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.mill, "7 shc".reverse) should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.mill, "7 1 +".reverse) should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.mill, "1 7 + 2 *".reverse) should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.mill, "1 + 3 + 2 *".reverse) should matchPattern { case p.Failure(_, _) => }
  }

  it should "parseMill" in {
    p.parseMill("42 37 + 2 *") should matchPattern { case Success(_) => }
    p.parseMill("42") should matchPattern { case Success(_) => }
    p.parseMill("7 chs") should matchPattern { case Success(_) => }
    p.parseMill("7 1 +") should matchPattern { case Success(_) => }
    p.parseMill("1 7 + 2 *") should matchPattern { case Success(_) => }
    p.parseMill("1 + 3 + 2 *") should matchPattern { case Failure(_) => }
    val w =
      """12 34  *
        | 56 78  * +
        | 90  12  * chs """.stripMargin
    p.parseMill(w) should matchPattern { case Success(_) => }
  }
  it should "parse: 73 24 <> -" in {
    val result = p.parseMill("73 24 <> -")
    result should matchPattern { case Success(_) => }
    for (m <- result) yield {
      val (xo, n) = m.pop
      val (yo, _) = n.pop
      xo.get shouldBe Add
      yo.get shouldBe Swap
    }
  }
}
