package com.phasmidsoftware.number.parse

import com.phasmidsoftware.number.core
import com.phasmidsoftware.number.core._
import org.scalatest.flatspec
import org.scalatest.matchers.should
import scala.util.{Left, Try}

/**
  * @author scalaprof
  */
class NumberParserSpec extends flatspec.AnyFlatSpec with should.Matchers {
  val p = new NumberParser

  behavior of "numberWithFuzziness"
  it should "parse 1.0*" in {
      val r = p.parseAll(p.numberWithFuzziness, "1.0*")
      r should matchPattern { case p.Success(_, _) => }
      val ry: Try[Rational] = r.get.value
      ry should matchPattern { case scala.util.Success(_) => }
      ry.get shouldBe Rational.one
  }
  it should "parse 1.*" in {
      val r = p.parseAll(p.numberWithFuzziness, "1.*")
      r should matchPattern { case p.Success(_, _) => }
      val ry: Try[Rational] = r.get.value
      ry should matchPattern { case scala.util.Success(_) => }
      ry.get shouldBe Rational.one
  }
  it should "parse 3.1415927*" in {
      val r = p.parseAll(p.numberWithFuzziness, "3.1415927*")
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
        n.value.get shouldBe Rational(31.415927)
        n.fuzz should matchPattern { case Some(AbsoluteFuzz(_, Box)) => }
    }
    it should "parse 1.00(5)" in {
        val r = p.parseAll(p.numberWithFuzziness, "1.00(5)")
        r should matchPattern { case p.Success(_, _) => }
        val ry: Try[Rational] = r.get.value
        ry should matchPattern { case scala.util.Success(_) => }
        ry.get shouldBe Rational.one
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
        r.get shouldBe Number.create(Left(Left(Right(Rational(314, 100)))))
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
    it should "parse 3.1415927*E1" in {
        val r = p.parseAll(p.generalNumber, "3.1415927*E1")
        r should matchPattern { case p.Success(_, _) => }
        val ry: Try[Rational] = r.get.value
        ry should matchPattern { case scala.util.Success(_) => }
        ry.get shouldBe Rational(31.415927)
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
        z shouldBe "5"
    }

  behavior of "fuzz"
  it should "parse (17)" in {
      val r: p.ParseResult[Option[String]] = p.parseAll(p.fuzz, "(17)")
      r should matchPattern { case p.Success(_, _) => }
      r.get should matchPattern { case Some("17") => }
  }
    it should "parse *" in {
        val r: p.ParseResult[Option[String]] = p.parseAll(p.fuzz, "*")
        r should matchPattern { case p.Success(_, _) => }
        r.get should matchPattern { case None => }
    }

    behavior of "number"
    it should "parse 1.*" in {
        val np: p.ParseResult[core.Number] = p.parseAll(p.number, "1.*")
        np should matchPattern { case p.Success(_, _) => }
        val n: core.Number = np.get
        n shouldBe FuzzyNumber(Right(1), Scalar, Some(AbsoluteFuzz(0.5, Box)))
    }
    it should "work for 3.14" in {
        val np = p.parseAll(p.number, "3.14")
        np should matchPattern { case p.Success(_, _) => }
        val n: core.Number = np.get
        n shouldBe ExactNumber(Left(Left(Right(Rational(314, 100)))), Scalar)
    }
    it should "work for 3.1415927" in {
        val np = p.parseAll(p.number, "3.1415927")
        np should matchPattern { case p.Success(_, _) => }
        val n: core.Number = np.get
        n shouldBe FuzzyNumber(Left(Left(Right(Rational(31415927, 10000000)))), Scalar, Some(AbsoluteFuzz(0.00000005, Box)))
    }
    it should "parse 3.1415927*" in {
        val np = p.parseAll(p.number, "3.1415927*")
        np should matchPattern { case p.Success(_, _) => }
        val n: core.Number = np.get
        n shouldBe FuzzyNumber(Left(Left(Right(Rational(31415927, 10000000)))), Scalar, Some(AbsoluteFuzz(0.00000005, Box)))
    }
    it should "parse 3.1415927*E1" in {
        val np: p.ParseResult[core.Number] = p.parseAll(p.number, "3.1415927*E1")
        np should matchPattern { case p.Success(_, _) => }
        val n: core.Number = np.get
        n shouldBe FuzzyNumber(Left(Left(Right(Rational(31415927, 1000000)))), Scalar, Some(AbsoluteFuzz(0.0000005, Box)))
    }
    it should "parse 3.141592653589793*" in {
        val np: p.ParseResult[core.Number] = p.parseAll(p.number, "3.141592653589793*")
        np should matchPattern { case p.Success(_, _) => }
        val n = np.get
        n should matchPattern { case FuzzyNumber(_, _, _) => }
    }
}
