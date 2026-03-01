/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.parse

import com.phasmidsoftware.number.algebra.eager.{Angle, RationalNumber, WholeNumber}
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.expression.expr.*
import com.phasmidsoftware.number.parse.ExpressionParser.puremath
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class LaTeXParserSpec extends AnyFlatSpec with should.Matchers {

  // Helper: assert the input parses successfully to the expected expression.
  private def assertParses(input: String, expected: Expression): Unit =
    LaTeXParser.parse(input) match {
      case LaTeXParser.Success(result, next) if next.atEnd => result shouldBe expected
      case LaTeXParser.Success(_, next) => fail(s"Did not consume all input, remainder: '${next.source.toString.drop(next.offset)}'")
      case f: LaTeXParser.NoSuccess => fail(s"Parse failed: ${f.msg}")
    }

  // Helper: assert the input does not parse successfully.
  private def assertFails(input: String): Unit =
    LaTeXParser.parse(input) match {
      case LaTeXParser.Success(_, next) if next.atEnd => fail(s"Expected failure but '$input' parsed successfully")
      case _ => // expected
    }

  behavior of "LaTeXParser"

  it should "apply number expressions" in {
    assertParses("1", One)
    assertParses("-1", UniFunction(One, Negate))
    assertParses("-(42)", UniFunction(Literal(WholeNumber(42), Some("42")), Negate))
    assertParses("1+2", BiFunction(One, Two, Sum))
    assertFails("1 2")
    assertParses("6*7", BiFunction(Literal(WholeNumber(6), Some("6")), Literal(WholeNumber(7), Some("7")), Product))
    assertParses("42/7", BiFunction(Literal(WholeNumber(42), Some("42")), UniFunction(Literal(WholeNumber(7), Some("7")), Reciprocal), Product))
    assertParses("6*(3+4)", BiFunction(Literal(WholeNumber(6), Some("6")), BiFunction(Literal(WholeNumber(3), Some("3")), Literal(WholeNumber(4), Some("4")), Sum), Product))
    assertParses("""\frac{42}{6}""", BiFunction(Literal(WholeNumber(42), Some("42")), UniFunction(Literal(WholeNumber(6), Some("6")), Reciprocal), Product))
    assertParses("3^2", BiFunction(Literal(WholeNumber(3), Some("3")), Literal(WholeNumber(2), Some("2")), Power))
    assertParses("-3^2", BiFunction(UniFunction(Literal(WholeNumber(3), Some("3")), Negate), Literal(WholeNumber(2), Some("2")), Power))
    assertParses("2+3*4", BiFunction(Two, BiFunction(Literal(WholeNumber(3), Some("3")), Literal(WholeNumber(4), Some("4")), Product), Sum))
    assertParses("3^2+4", BiFunction(BiFunction(Literal(WholeNumber(3), Some("3")), Literal(WholeNumber(2), Some("2")), Power), Literal(WholeNumber(4), Some("4")), Sum))
    assertParses("3^2+4*5", BiFunction(BiFunction(Literal(WholeNumber(3), Some("3")), Literal(WholeNumber(2), Some("2")), Power), BiFunction(Literal(WholeNumber(4), Some("4")), Literal(WholeNumber(5), Some("5")), Product), Sum))
  }

  it should "apply pi" in {
    assertParses("""\pi""", Pi)
    assertParses("""2\pi""", BiFunction(Two, Pi, Product))
    assertParses("""2*\pi""", BiFunction(Two, Pi, Product))
    assertParses("""\pi^2""", BiFunction(Pi, Two, Power))
    assertParses("""\pi+3-3""", BiFunction(BiFunction(Pi, Literal(WholeNumber(3), Some("3")), Sum), UniFunction(Literal(WholeNumber(3), Some("3")), Negate), Sum))
  }

  it should "apply e" in {
    assertParses("""\e""", E)
    assertParses("""\mathrm{e}""", E)
    assertParses("""\e^2""", BiFunction(E, Two, Power))
  }

  it should "apply functions" in {
    assertParses("""\sqrt{2}""", BiFunction(Two, Half, Power))
    assertParses("√2", BiFunction(Two, Half, Power))
    assertParses("""\sin(\pi)""", UniFunction(Pi, Sine))
    assertParses("""\cos(\pi)""", UniFunction(Pi, Cosine))
    assertParses("""\tan(\pi)""", BiFunction(UniFunction(Pi, Sine), UniFunction(UniFunction(Pi, Cosine), Reciprocal), Product))
    assertParses("""\ln(\e)""", UniFunction(E, Ln))
    assertParses("""\sin(\pi) * -1""", BiFunction(UniFunction(Pi, Sine), UniFunction(One, Negate), Product))
    assertParses("""\sin(\pi) + -1""", BiFunction(UniFunction(Pi, Sine), UniFunction(One, Negate), Sum))
    assertParses("""\exp(2)""", UniFunction(Two, Exp))
  }

  behavior of "parsed expressions"

  it should "apply number expressions" in {
    LaTeXParser.parse("2+3*4") match {
      case LaTeXParser.Success(result, next) if next.atEnd =>
        result.materialize shouldBe WholeNumber(14)
      case f: LaTeXParser.NoSuccess => fail(s"Parse failed: ${f.msg}")
      case _ => fail("Did not consume all input")
    }
  }

  behavior of "degrees and percentages"

  it should "parse percentages and degrees" in {
    assertParses("50%", Literal(RationalNumber(Rational.half, true)(), Some("50%")))
    assertParses("90°", Literal(Angle.degrees(90), Some("90°")))
  }

  // NOTE that the latex parser cannot understand uncertainty.
  behavior of "exponents"

  it should "parse 8.8541878128 \\times 10^{-12}" in {
    pending // TODO Issue #176
  }

  behavior of "old Expression.parse string"

  it should "parse and evaluate sqrt(3)" in {
    val e: Expression = puremath"3 ∧ ( 2 ∧ -1 )"
    e shouldBe Literal(3) ∧ (Literal(2) ∧ -1)
  }

  it should "parse and evaluate half" in {
    val e: Expression = puremath"2 ∧ -1"
    e shouldBe Literal(2) ∧ -1
  }
}