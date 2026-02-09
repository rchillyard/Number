/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.parse

import com.phasmidsoftware.number.algebra.eager.{Angle, RationalNumber, WholeNumber}
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.expression.expr.*
import com.phasmidsoftware.number.parse.LaTeXParser
import fastparse.Parsed
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class LaTeXParserSpec extends AnyFlatSpec with should.Matchers {

  behavior of "LaTeXParser"

  val p: String => Parsed[Expression] = LaTeXParser.parse

  it should "apply number expressions" in {
    p("1") shouldBe Parsed.Success(One, 1)
    p("-1") shouldBe Parsed.Success(UniFunction(One, Negate), 2)
    p("-(42)") shouldBe Parsed.Success(UniFunction(Literal(WholeNumber(42), Some("42")), Negate), 5)
    p("1+2") shouldBe Parsed.Success(BiFunction(One, Two, Sum), 3)
    p("1 2").isSuccess shouldBe false
    p("6*7") shouldBe Parsed.Success(BiFunction(Literal(WholeNumber(6), Some("6")), Literal(WholeNumber(7), Some("7")), Product), 3)
    p("42/7") shouldBe Parsed.Success(BiFunction(Literal(WholeNumber(42), Some("42")), UniFunction(Literal(WholeNumber(7), Some("7")), Reciprocal), Product), 4)
    p("6*(3+4)") shouldBe Parsed.Success(BiFunction(Literal(WholeNumber(6), Some("6")), BiFunction(Literal(WholeNumber(3), Some("3")), Literal(WholeNumber(4), Some("4")), Sum), Product), 7)
    p("""\frac{42}{6}""") shouldBe Parsed.Success(BiFunction(Literal(WholeNumber(42), Some("42")), UniFunction(Literal(WholeNumber(6), Some("6")), Reciprocal), Product), 12)
    p("3^2") shouldBe Parsed.Success(BiFunction(Literal(WholeNumber(3), Some("3")), Literal(WholeNumber(2), Some("2")), Power), 3)
    p("-3^2") shouldBe Parsed.Success(BiFunction(UniFunction(Literal(WholeNumber(3), Some("3")), Negate), Literal(WholeNumber(2), Some("2")), Power), 4)
    p("2+3*4") shouldBe Parsed.Success(BiFunction(Two, BiFunction(Literal(WholeNumber(3), Some("3")), Literal(WholeNumber(4), Some("4")), Product), Sum), 5)
    p("3^2+4") shouldBe Parsed.Success(BiFunction(BiFunction(Literal(WholeNumber(3), Some("3")), Literal(WholeNumber(2), Some("2")), Power), Literal(WholeNumber(4), Some("4")), Sum), 5)
    p("3^2+4*5") shouldBe Parsed.Success(BiFunction(BiFunction(Literal(WholeNumber(3), Some("3")), Literal(WholeNumber(2), Some("2")), Power), BiFunction(Literal(WholeNumber(4), Some("4")), Literal(WholeNumber(5), Some("5")), Product), Sum), 7)
  }
  it should "apply pi" in {
    p("""\pi""") shouldBe Parsed.Success(Pi, 3)
    p("""2\pi""") shouldBe Parsed.Success(BiFunction(Two, Pi, Product), 4)
    p("""2*\pi""") shouldBe Parsed.Success(BiFunction(Two, Pi, Product), 5)
    p("""\pi^2""") shouldBe Parsed.Success(BiFunction(Pi, Two, Power), 5)
    p("""\pi+3-3""") shouldBe Parsed.Success(BiFunction(BiFunction(Pi, Literal(WholeNumber(3), Some("3")), Sum), UniFunction(Literal(WholeNumber(3), Some("3")), Negate), Sum), 7)
  }
  it should "apply e" in {
    p("""\e""") shouldBe Parsed.Success(E, 2)
    p("""\mathrm{e}""") shouldBe Parsed.Success(E, 10)
    p("""\e^2""") shouldBe Parsed.Success(BiFunction(E, Two, Power), 4)
  }
  it should "apply functions" in {
    p("""\sqrt{2}""") shouldBe Parsed.Success(BiFunction(Two, Half, Power), 8)
    p("√2") shouldBe Parsed.Success(BiFunction(Two, Half, Power), 2)
    p("""\sin(\pi)""") shouldBe Parsed.Success(UniFunction(Pi, Sine), 9)
    p("""\cos(\pi)""") shouldBe Parsed.Success(UniFunction(Pi, Cosine), 9)
    p("""\tan(\pi)""") shouldBe Parsed.Success(BiFunction(UniFunction(Pi, Sine), UniFunction(UniFunction(Pi, Cosine), Reciprocal), Product), 9)
    p("""\ln(\e)""") shouldBe Parsed.Success(UniFunction(E, Ln), 7)
    p("""\sin(\pi) * -1""") shouldBe Parsed.Success(BiFunction(UniFunction(Pi, Sine), UniFunction(One, Negate), Product), 14)
    p("""\exp(2)""") shouldBe Parsed.Success(UniFunction(Two, Exp), 7)
  }

  behavior of "parsed expressions"
  it should "apply number expressions" in {
    val parsed = p("2+3*4")
    parsed.isSuccess shouldBe true
    parsed.get.value.evaluateAsIs shouldBe Some(WholeNumber(14))
  }

  behavior of "degrees and percentages"
  it should "parse percentages and degrees" in {
    p("50%") shouldBe Parsed.Success(Literal(RationalNumber(Rational.half,true)(), Some("50%")), 3)
    p("90°") shouldBe Parsed.Success(Literal(Angle.degrees(90), Some("90°")), 3) 
  }

  // NOTE that the latex parser cannot understand uncertainty.
  behavior of "exponents"
  it should "parse 8.8541878128 \\times 10^{-12}" in {
    //    p("8.8541878128 \\times 10^{-12}").isSuccess shouldBe true
    pending // TODO Issue #176
  }
}
