package com.phasmidsoftware.number.parse

import com.phasmidsoftware.number.core
import com.phasmidsoftware.number.core.{FuzzyNumber, Rational}
import org.scalatest.flatspec
import org.scalatest.matchers.should

import scala.util.Try

/**
  * @author scalaprof
  */
class NumberParserSpec extends flatspec.AnyFlatSpec with should.Matchers {
  val parser = new NumberParser

  behavior of "numberWithFuzziness"
  it should "parse 1" in {
    val r = parser.parseAll(parser.numberWithFuzziness, "1.0*")
    r should matchPattern { case parser.Success(_, _) => }
    val ry: Try[Rational] = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational.one
  }
  it should "parse 3.1415927" in {
    val r = parser.parseAll(parser.numberWithFuzziness, "3.1415927*")
    r should matchPattern { case parser.Success(_, _) => }
    val ry: Try[Rational] = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational(BigDecimal.valueOf(3.1415927))
  }
  it should "parse 3.1415927E1" in {
    val r = parser.parseAll(parser.numberWithFuzziness, "3.1415927*E1")
    r should matchPattern { case parser.Success(_, _) => }
    val ry: Try[Rational] = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational(31.415927)
  }
  behavior of "generalNumber"
  it should "parse 1" in {
    val r = parser.parseAll(parser.generalNumber, "1.0")
    r should matchPattern { case parser.Success(_, _) => }
    val ry: Try[Rational] = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational.one
  }
  it should "parse 3.1415927" in {
    val r = parser.parseAll(parser.generalNumber, "3.1415927")
    r should matchPattern { case parser.Success(_, _) => }
    val ry: Try[Rational] = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational(BigDecimal.valueOf(3.1415927))
  }
  it should "parse 3.1415927E1" in {
    val r = parser.parseAll(parser.generalNumber, "3.1415927E1")
    r should matchPattern { case parser.Success(_, _) => }
    val ry: Try[Rational] = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational(31.415927)
  }
  it should "parse 1/2" in {
    val r = parser.parseAll(parser.generalNumber, "1/2")
    r should matchPattern { case parser.Success(_, _) => }
    val ry = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational.half
  }

  behavior of "fuzz"
  it should "parse (17)" in {
    val r: parser.ParseResult[Option[Int]] = parser.parseAll(parser.fuzz, "(17)")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case Some(17) => }
  }
  it should "parse *" in {
    val r: parser.ParseResult[Option[Int]] = parser.parseAll(parser.fuzz, "*")
    r should matchPattern { case parser.Success(_, _) => }
    r.get should matchPattern { case Some(50) => }
  }
  behavior of "number"
  it should "parse 3.141592653589793*" in {
    val np: parser.ParseResult[core.Number] = parser.parseAll(parser.number, "3.141592653589793*")
    np should matchPattern { case parser.Success(_, _) => }
    val n = np.get
    n should matchPattern { case FuzzyNumber(_, _, _) => }
  }
}
