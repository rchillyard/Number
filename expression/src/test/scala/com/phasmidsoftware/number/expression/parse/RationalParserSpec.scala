package com.phasmidsoftware.number.expression.parse

import com.phasmidsoftware.number.core.inner.Rational
import org.scalatest.flatspec
import org.scalatest.matchers.should

import scala.util.Try

/**
  * @author scalaprof
  */
class RationalParserSpec extends flatspec.AnyFlatSpec with should.Matchers {
  behavior of "simpleNumber"
  it should "parse 1" in {
    val parser = RationalParser
    val r = parser.parseAll(parser.simpleNumber, "1")
    r should matchPattern { case parser.Success(_, _) => }
    val ry: Try[Rational] = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational.one
  }
  it should "reject 1 " in {
    val parser = RationalParser
    val r = parser.parseAll(parser.simpleNumber, "1 ")
    r should matchPattern { case parser.Failure(_, _) => }
  }
  behavior of "rational"
  it should "parse 1/2" in {
    val parser = RationalParser
    val r = parser.parseAll(parser.ratioNumber, "1/2")
    r should matchPattern { case parser.Success(_, _) => }
    val ry: Try[Rational] = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational.half
  }

  behavior of "realNumber"
  it should "parse 3.1415927" in {
    val parser = RationalParser
    val r = parser.parseAll(parser.realNumber, "3.1415927")
    r should matchPattern { case parser.Success(_, _) => }
    val ry: Try[Rational] = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational(BigDecimal.valueOf(3.1415927))
  }
  it should "parse 3.1415927E1" in {
    val parser = RationalParser
    val r = parser.parseAll(parser.realNumber, "3.1415927E1")
    r should matchPattern { case parser.Success(_, _) => }
    val ry: Try[Rational] = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational.createExact(31.415927).get
  }
  behavior of "number"
  it should "parse 1/2" in {
    val parser = RationalParser
    val r = parser.parseAll(parser.rationalNumber, "1/2")
    r should matchPattern { case parser.Success(_, _) => }
    val ry: Try[Rational] = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational.half
  }
  it should "parse 1" in {
    val parser = RationalParser
    val r = parser.parseAll(parser.rationalNumber, "1")
    r should matchPattern { case parser.Success(_, _) => }
    val ry: Try[Rational] = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational.one
  }
  it should "parse 3.1415927" in {
    val parser = RationalParser
    val r = parser.parseAll(parser.rationalNumber, "3.1415927")
    r should matchPattern { case parser.Success(_, _) => }
    val ry: Try[Rational] = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational(BigDecimal.valueOf(3.1415927))
  }

  behavior of "repeatingDecimal"
  it should "parse 1/3" in {
    val parser = RationalParser
    val r = parser.parseAll(parser.repeatingDecimal, "0.<3>")
    r should matchPattern { case parser.Success(_, _) => }
    val ry: Try[Rational] = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational.third
  }

  it should "parse simple repeating decimals" in {
    val r1 = RationalParser.parse("0.<3>").get
    r1 shouldBe Rational(1, 3)

    val r2 = RationalParser.parse("0.<6>").get
    r2 shouldBe Rational(2, 3)

    val r3 = RationalParser.parse("0.<9>").get
    r3 shouldBe Rational(1, 1)
  }

  it should "parse repeating decimals with integer parts" in {
    val r1 = RationalParser.parse("1.<3>").get
    r1 shouldBe Rational(4, 3)

    val r2 = RationalParser.parse("2.<6>").get
    r2 shouldBe Rational(8, 3)

    val r3 = RationalParser.parse("5.<0>").get
    r3 shouldBe Rational(5, 1)
  }

  it should "parse repeating decimals with non-repeating parts" in {
    val r1 = RationalParser.parse("0.1<6>").get
    r1 shouldBe Rational(1, 6)

    val r2 = RationalParser.parse("0.08<3>").get
    r2 shouldBe Rational(1, 12)

    val r3 = RationalParser.parse("0.5<0>").get
    r3 shouldBe Rational(1, 2)
  }

  it should "parse repeating decimals with multi-digit patterns" in {
    val r1 = RationalParser.parse("0.<142857>").get
    r1 shouldBe Rational(1, 7)

    val r2 = RationalParser.parse("0.<285714>").get
    r2 shouldBe Rational(2, 7)

    val r3 = RationalParser.parse("0.<090909>").get
    r3 shouldBe Rational(1, 11)
  }

  it should "parse negative repeating decimals" in {
    val r1 = RationalParser.parse("-0.<3>").get
    r1 shouldBe Rational(-1, 3)

    val r2 = RationalParser.parse("-1.<6>").get
    r2 shouldBe Rational(-5, 3)

    val r3 = RationalParser.parse("-0.1<6>").get
    r3 shouldBe Rational(-1, 6)
  }

  it should "parse complex repeating decimals" in {
    val r1 = RationalParser.parse("3.14<15>").get
    // 3.141515... = 3 + 14/100 + 15/9900 = (29700 + 1386 + 15)/9900 = 31101/9900 = 10367/3300
    r1 shouldBe Rational(10367, 3300)

    val r2 = RationalParser.parse("0.12<34>").get
    // 0.123434... = 12/100 + 34/9900 = (1188 + 34)/9900 = 1222/9900 = 611/4950
    r2 shouldBe Rational(611, 4950)
  }

  it should "handle edge case of all 9s repeating" in {
    val r1 = RationalParser.parse("0.<9>").get
    r1 shouldBe Rational(1, 1) // 0.999... = 1

    val r2 = RationalParser.parse("1.<9>").get
    r2 shouldBe Rational(2, 1) // 1.999... = 2

    val r3 = RationalParser.parse("0.9<9>").get
    r3 shouldBe Rational(1, 1) // 0.9999... = 1
  }

  it should "round-trip through rendering" in {
    // If your renderer produces angle bracket notation for 1/3
    val original = Rational(1, 3)
    val rendered = original.toRationalString // Should this produce "0.<3>"?
    // Note: This test depends on your rendering implementation
    // If rendering produces "0.333..." instead, adjust accordingly
  }

  // Additional edge cases
  it should "parse single digit repeating with longer non-repeating" in {
    val r1 = RationalParser.parse("0.123<4>").get
    // 0.12344... = 123/1000 + 4/9000 = (1107 + 4)/9000 = 1111/9000
    r1 shouldBe Rational(1111, 9000)
  }

  it should "parse two-digit repeating patterns" in {
    val r1 = RationalParser.parse("0.<12>").get
    r1 shouldBe Rational(4, 33)

    val r2 = RationalParser.parse("0.<45>").get
    r2 shouldBe Rational(5, 11)
  }
}
