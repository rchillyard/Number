package com.phasmidsoftware.number.parse

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
}
