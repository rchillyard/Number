/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.parse

import com.phasmidsoftware.number.core.inner.*
import com.phasmidsoftware.number.core.numerical
import com.phasmidsoftware.number.core.numerical.*
import com.phasmidsoftware.number.core.parse.{NumberParser, ValuableNumber}
import org.scalatest.flatspec
import org.scalatest.matchers.should

import scala.util.{Left, Try}

/**
  * @author scalaprof
  */
class NumberParserSpec extends flatspec.AnyFlatSpec with should.Matchers {
  private val p = NumberParser

  behavior of "percentage"
  it should "parse 10%" in {
    val r: p.ParseResult[Number] = p.parseAll(p.number, "10%")
    r should matchPattern { case p.Success(_, _) => }
    r.get shouldBe ExactNumber(Right(10), Percent)
    r.get.render shouldBe "10%"
  }
  it should "parse 10.35%" in {
    val r: p.ParseResult[Number] = p.parseAll(p.number, "10.35%")
    r should matchPattern { case p.Success(_, _) => }
    r.get shouldBe ExactNumber(Value.fromRational(Rational(1035, 100)), Percent)
    r.get.render shouldBe "10.35%"
  }
  it should "parse 10.352%" in {
    val r: p.ParseResult[Number] = p.parseAll(p.number, "10.352%")
    r should matchPattern { case p.Success(_, _) => }
    val x: Number = r.get
    //    x shouldBe FuzzyNumber(Value.fromRational(Rational(10352, 1000)), Percent, Some(AbsoluteFuzz(0.0005, Box)))
    r.get.render shouldBe "10.352%"
  }
  behavior of "degree"
  it should "parse 180°" in {
    val r: p.ParseResult[Number] = p.parseAll(p.number, "180°")
    r.successful shouldBe true
    val x: Number = r.get
    x shouldBe ExactNumber(Right(180), Degree)
    x.render shouldBe "180°"
    val radians: Option[Value] = Degree.convert(x.nominalValue, Radian)
    radians shouldBe Some(Left(Left(Some(1))))
  }
  it should "parse -180°" in {
    val r: p.ParseResult[Number] = p.parseAll(p.number, "-180°")
    r.successful shouldBe true
    r.get shouldBe ExactNumber(Right(-180), Degree)
    r.get.render shouldBe "-180°"
  }
  it should "parse 0°" in {
    val r: p.ParseResult[Number] = p.parseAll(p.number, "0°")
    r.successful shouldBe true
    val x: Number = r.get
    x shouldBe ExactNumber(Right(0), Degree)
    x.render shouldBe "0°"
  }

  behavior of "numberWithFuzziness"
  it should "parse 1.0*" in {
    val r = p.parseAll(p.numberWithFuzziness, "1.0*")
    r should matchPattern { case p.Success(_, _) => }
    val ry: Try[Rational] = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational.one
  }
  it should "reject 1.0 *" in {
    val r = p.parseAll(p.numberWithFuzziness, "1.0 *")
    r should matchPattern { case p.Failure(_, _) => }
  }
  it should "parse 1.*" in {
    val r = p.parseAll(p.numberWithFuzziness, "1.*")
    r should matchPattern { case p.Success(_, _) => }
    val ry: Try[Rational] = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational.one
  }
  it should "reject 1.* " in {
    val r = p.parseAll(p.numberWithFuzziness, "1.* ")
    r should matchPattern { case p.Failure(_, _) => }
  }
  it should "parse 3.1415927*" in {
    val r = p.parseAll(p.numberWithFuzziness, "3.1415927*")
    r should matchPattern { case p.Success(_, _) => }
    val ry: Try[Rational] = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational(BigDecimal.valueOf(3.1415927))
  }
  it should "parse 3.1415927..." in {
    val r = p.parseAll(p.numberWithFuzziness, "3.1415927...")
    r should matchPattern { case p.Success(_, _) => }
    val ry: Try[Rational] = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational(BigDecimal.valueOf(3.1415927))
  }
  it should "parse 3.1415927*E1" in {
    val np: p.ParseResult[p.NumberWithFuzziness] = p.parseAll(p.numberWithFuzziness, "3.1415927*E1")
    np should matchPattern { case p.Success(_, _) => }
    val n: p.NumberWithFuzziness = np.get
    n should matchPattern { case p.NumberWithFuzziness(p.RealNumber(false, "3", Some("1415927"), None), None, Some("1")) => }
    n.value.get shouldBe Rational.createExact(31.415927).get
    n.fuzz should matchPattern { case Some(AbsoluteFuzz(_, Box)) => }
  }
  it should "reject 3.1415927* E1" in {
    val np: p.ParseResult[p.NumberWithFuzziness] = p.parseAll(p.numberWithFuzziness, "3.1415927* E1")
    np should matchPattern { case p.Failure(_, _) => }
  }
  it should "parse 1.00(5)" in {
    val r = p.parseAll(p.numberWithFuzziness, "1.00(5)")
    r should matchPattern { case p.Success(_, _) => }
    val ry: Try[Rational] = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational.one
  }
  it should "reject 1.00 (5)" in {
    val r = p.parseAll(p.numberWithFuzziness, "1.00 (5)")
    r should matchPattern { case p.Failure(_, _) => }
  }


  behavior of "generalNumber"
  it should "parse 1.0" in {
    val r = p.parseAll(p.generalNumber, "1.0")
    r should matchPattern { case p.Success(_, _) => }
    val ry: Try[Rational] = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational.one
  }
  it should "parse 1.*" in {
    val r = p.parseAll(p.generalNumber, "1.*")
    r should matchPattern { case p.Success(_, _) => }
    val ry: Try[Rational] = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational.one
  }
  it should "work for 3.14" in {
    val r: p.ParseResult[Number] = p.parseAll(p.number, "3.14")
    r should matchPattern { case p.Success(_, _) => }
    r.get shouldBe Number.create(Left(Right(Rational(314, 100))))
  }
  it should "work for 3.1415927" in {
    val r = p.parseAll(p.generalNumber, "3.1415927")
    r should matchPattern { case p.Success(_, _) => }
    val ry: Try[Rational] = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational(31415927, 10000000)
  }
  it should "parse 3.1415927*" in {
    val r = p.parseAll(p.generalNumber, "3.1415927*")
    r should matchPattern { case p.Success(_, _) => }
    val ry: Try[Rational] = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational(BigDecimal.valueOf(3.1415927))
  }
  it should "parse 2.9979245800E8" in {
    val r = p.parseAll(p.generalNumber, "2.9979245800E8")
    r should matchPattern { case p.Success(_, _) => }
    val ry: Try[Rational] = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational(BigDecimal.valueOf(299792458))
  }
  it should "parse 3.1415927*E1" in {
    val r = p.parseAll(p.generalNumber, "3.1415927*E1")
    r should matchPattern { case p.Success(_, _) => }
    val ry: Try[Rational] = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational.createExact(31.415927).get
  }
  it should "parse 1/2" in {
    val r = p.parseAll(p.generalNumber, "1/2")
    r should matchPattern { case p.Success(_, _) => }
    val ry = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational.half
  }
  it should "work for 1.00(5)" in {
    val np: p.ParseResult[ValuableNumber] = p.parseAll(p.generalNumber, "1.00(5)")
    np should matchPattern { case p.Success(_, _) => }
    val n = np.get
    val ry: Try[Rational] = n.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational.one
    val z: String = n match {
      case p.NumberWithFuzziness(_, fo, _) => fo.get
    }
    z shouldBe "(5)"
  }

  behavior of "fuzz"
  it should "parse (17)" in {
    val r: p.ParseResult[Option[String]] = p.parseAll(p.fuzz, "(17)")
    r should matchPattern { case p.Success(_, _) => }
    r.get should matchPattern { case Some("(17)") => }
  }
  it should "parse *" in {
    val r: p.ParseResult[Option[String]] = p.parseAll(p.fuzz, "*")
    r should matchPattern { case p.Success(_, _) => }
    r.get should matchPattern { case None => }
  }

  behavior of "number"
  it should "parse 1.*" in {
    val np: p.ParseResult[numerical.Number] = p.parseAll(p.number, "1.*")
    np should matchPattern { case p.Success(_, _) => }
    val n: numerical.Number = np.get
    n shouldBe FuzzyNumber(Right(1), PureNumber, Some(AbsoluteFuzz(0.5, Box)))
  }
  it should "work for 3.14" in {
    val np = p.parseAll(p.number, "3.14")
    np should matchPattern { case p.Success(_, _) => }
    val n: numerical.Number = np.get
    n shouldBe ExactNumber(Left(Right(Rational(314, 100))), PureNumber)
  }
  it should "work for 3.1415927*" in {
    val np = p.parseAll(p.number, "3.1415927*")
    np should matchPattern { case p.Success(_, _) => }
    val n: numerical.Number = np.get
    n shouldBe FuzzyNumber(Left(Right(Rational(31415927, 10000000))), PureNumber, Some(AbsoluteFuzz(0.00000005, Box)))
  }
  it should "parse 3.1415927*" in {
    val np = p.parseAll(p.number, "3.1415927*")
    np should matchPattern { case p.Success(_, _) => }
    val n: numerical.Number = np.get
    n shouldBe FuzzyNumber(Left(Right(Rational(31415927, 10000000))), PureNumber, Some(AbsoluteFuzz(0.00000005, Box)))
  }
  it should "parse 3.1415927*E1" in {
    val np: p.ParseResult[numerical.Number] = p.parseAll(p.number, "3.1415927*E1")
    np should matchPattern { case p.Success(_, _) => }
    val n: numerical.Number = np.get
    n shouldBe FuzzyNumber(Left(Right(Rational(31415927, 1000000))), PureNumber, Some(AbsoluteFuzz(0.0000005, Box)))
  }
  it should "parse 3.141592653589793*" in {
    val np: p.ParseResult[numerical.Number] = p.parseAll(p.number, "3.141592653589793*")
    np should matchPattern { case p.Success(_, _) => }
    val n = np.get
    n should matchPattern { case FuzzyNumber(_, _, _) => }
  }
  it should "parse \uD835\uDED1" in {
    val np: p.ParseResult[numerical.Number] = p.parseAll(p.number, "\uD835\uDED1")
    np should matchPattern { case p.Success(_, _) => }
    val n = np.get
    n should matchPattern { case ExactNumber(_, Radian) => }
    n.nominalValue shouldBe Right(1)
  }
  it should "parse 1\uD835\uDED1" in {
    val np: p.ParseResult[numerical.Number] = p.parseAll(p.number, "1\uD835\uDED1")
    np should matchPattern { case p.Success(_, _) => }
    val n = np.get
    n should matchPattern { case ExactNumber(_, Radian) => }
    n.nominalValue shouldBe Right(1)
  }
  it should "reject 1 \uD835\uDED1" in {
    val np: p.ParseResult[numerical.Number] = p.parseAll(p.number, "1 \uD835\uDED1")
    np should matchPattern { case p.Failure(_, _) => }
  }

  behavior of "realNumber"
  it should "parse 2.9979245800E8" in {
    val r = p.parseAll(p.realNumber, "2.9979245800E8")
    r should matchPattern { case p.Success(_, _) => }
    val ry: Try[Rational] = r.get.value
    ry should matchPattern { case scala.util.Success(_) => }
    ry.get shouldBe Rational(BigDecimal.valueOf(299792458))
  }

}
