/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.parse

import com.phasmidsoftware.number.algebra.WholeNumber
import com.phasmidsoftware.number.expression.expr.*
import com.phasmidsoftware.number.parse.LaTeXParser
import com.phasmidsoftware.number.parse.LaTeXParser.MathExpr
import fastparse.Parsed
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class LaTeXParserSpec extends AnyFlatSpec with should.Matchers {

  behavior of "LaTeXParser"

  val p: String => Parsed[MathExpr] = LaTeXParser.parse

  it should "apply number expressions" in {
    p.apply("1") shouldBe Parsed.Success(Literal(WholeNumber(1), Some("1")), 1)
    p.apply("1+2") shouldBe Parsed.Success(BiFunction(Literal(WholeNumber(1), Some("1")), Literal(WholeNumber(2), Some("2")), Sum), 3)
    p.apply("6*7") shouldBe Parsed.Success(BiFunction(Literal(WholeNumber(6), Some("6")), Literal(WholeNumber(7), Some("7")), Product), 3)
    p.apply("42/7") shouldBe Parsed.Success(BiFunction(Literal(WholeNumber(42), Some("42")), UniFunction(Literal(WholeNumber(7), Some("7")), Reciprocal), Product), 4)
    p.apply("6*(3+4)") shouldBe Parsed.Success(BiFunction(Literal(WholeNumber(6), Some("6")), BiFunction(Literal(WholeNumber(3), Some("3")), Literal(WholeNumber(4), Some("4")), Sum), Product), 7)
    p.apply("""\frac{42}{6}""") shouldBe Parsed.Success(BiFunction(Literal(WholeNumber(42), Some("42")), UniFunction(Literal(WholeNumber(6), Some("6")), Reciprocal), Product), 12)
  }
  it should "apply pi" in {
    p.apply("""\pi""") shouldBe Parsed.Success(ConstPi, 3)
    p.apply("""2\pi""") shouldBe Parsed.Success(BiFunction(Literal(WholeNumber(2), Some("2")), ConstPi, Product), 4)
    p.apply("""2*\pi""") shouldBe Parsed.Success(BiFunction(Literal(WholeNumber(2), Some("2")), ConstPi, Product), 5)
  }
  it should "apply e" in {
    p.apply("""\e""") shouldBe Parsed.Success(ConstE, 2)
    p.apply("""\mathrm{e}""") shouldBe Parsed.Success(ConstE, 10)
  }
  it should "apply functions" in {
    p.apply("""\sin(\pi)""") shouldBe Parsed.Success(UniFunction(ConstPi, Sine), 9)
    p.apply("""\cos(\pi)""") shouldBe Parsed.Success(UniFunction(ConstPi, Cosine), 9)
    p.apply("""\tan(\pi)""") shouldBe Parsed.Success(BiFunction(UniFunction(ConstPi, Sine), UniFunction(UniFunction(ConstPi, Cosine), Reciprocal), Product), 9)
    p.apply("""\ln(\e)""") shouldBe Parsed.Success(UniFunction(ConstE, Ln), 7)
//    p.apply("""\{exp}(2)""") shouldBe Parsed.Success(UniFunction(One,Exp), 7)
  }
}
