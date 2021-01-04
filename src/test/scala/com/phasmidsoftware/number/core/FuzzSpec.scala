package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.parse.NumberParser
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Left, Try}

class FuzzSpec extends AnyFlatSpec with should.Matchers {

  private val p = new NumberParser


  behavior of "generalNumber"
  it should "parse 1." in {
    val z = p.parseAll(p.generalNumber, "1.")
    z should matchPattern { case p.Success(p.RealNumber(false, "1", None, None), _) => }
  }
  it should "parse 1.0" in {
    val z = p.parseAll(p.generalNumber, "1.0")
    z should matchPattern { case p.Success(p.RealNumber(false, "1", Some("0"), None), _) => }
  }
  it should "parse 1.00" in {
    val z = p.parseAll(p.generalNumber, "1.00")
    z should matchPattern { case p.Success(p.RealNumber(false, "1", Some("00"), None), _) => }
  }
  it should "parse 1.000 as fuzzy" in {
    val z = p.parseAll(p.generalNumber, "1.000")
    z should matchPattern { case p.Success(p.RealNumber(false, "1", Some("000"), None), _) => }
  }

  behavior of "fuzz"
  it should "parse *" in {
    val z = p.parseAll(p.fuzz, "*")
    z should matchPattern { case p.Success(None, _) => }
  }
  it should "parse ..." in {
    val z = p.parseAll(p.fuzz, "...")
    z should matchPattern { case p.Success(None, _) => }
  }
  it should "parse (5)" in {
    val z = p.parseAll(p.fuzz, "(5)")
    z should matchPattern { case p.Success(Some("5"), _) => }
  }
  it should "parse (15)" in {
    val z = p.parseAll(p.fuzz, "(15)")
    z should matchPattern { case p.Success(Some("15"), _) => }
  }
  it should "parse (315)" in {
    val z = p.parseAll(p.fuzz, "(315)")
    z should matchPattern { case p.Success(Some("315"), _) => }
  }
  it should "fail to parse .." in {
    val z = p.parseAll(p.fuzz, "..")
    z should matchPattern { case p.Failure(_, _) => }
  }

  behavior of "exponent"
  it should "parse E-11" in {
    val z = p.parseAll(p.exponent, "E-11")
    z should matchPattern { case p.Success(_, _) => }
    z.get shouldBe "-11"
  }

  behavior of "numberWithFuzziness"
  it should "parse 1.0*" in {
    val z = p.parseAll(p.numberWithFuzziness, "1.0*")
    z should matchPattern { case p.Success(_, _) => }
    z.get shouldBe p.NumberWithFuzziness(p.RealNumber(sign = false, "1", Some("0"), None), None, None)
  }
  it should "parse 1.0..." in {
    val z: p.ParseResult[p.NumberWithFuzziness] = p.parseAll(p.numberWithFuzziness, "1.0...")
    z should matchPattern { case p.Success(_, _) => }
    z.get shouldBe p.NumberWithFuzziness(p.RealNumber(sign = false, "1", Some("0"), None), None, None)
  }
  it should "parse G" in {
    val G = "6.67430(15)E-11"
    val z: p.ParseResult[p.NumberWithFuzziness] = p.parseAll(p.numberWithFuzziness, G)
    z should matchPattern { case p.Success(_, _) => }
    z.get shouldBe p.NumberWithFuzziness(p.RealNumber(sign = false, "6", Some("67430"), None), Some("15"), Some("-11"))
  }
  it should "parse alpha" in {
    val alpha = "0.0072973525693(11)"
    val z: p.ParseResult[p.NumberWithFuzziness] = p.parseAll(p.numberWithFuzziness, alpha)
    z should matchPattern { case p.Success(_, _) => }
    z.get shouldBe p.NumberWithFuzziness(p.RealNumber(sign = false, "0", Some("0072973525693"), None), Some("11"), None)
  }

  behavior of "Fuzz.toString"
  it should "work for 1/0.5/Box" in {
    val target = AbsoluteFuzz(0.5, Box)
    target.toString(1) shouldBe "1.0[5]"
  }
  it should "work for 1/0.005/Box" in {
    val target = AbsoluteFuzz(0.005, Box)
    target.toString(1) shouldBe "1.000[5]"
  }
  it should "work for 1/0.5/Gaussian" in {
    val target = AbsoluteFuzz(0.5, Gaussian)
    target.toString(1) shouldBe "1.0(5)"
  }
  it should "work for 1/0.005/Gaussian" in {
    val target = AbsoluteFuzz(0.005, Gaussian)
    target.toString(1) shouldBe "1.000(5)"
  }
  it should "work for Planck" in {
    val target = AbsoluteFuzz(5E-41, Gaussian)
    target.toString(6.62607015E-34) shouldBe "6.6260701(5)E-34"
  }
  it should "work for Avagadro" in {
    val target = AbsoluteFuzz(5E16, Gaussian)
    target.toString(6.02214076E23) shouldBe "6.0221407(5)E+23"
  }
  it should "work for 3.1415927" in {
    val xy: Try[Number] = Number.parse("3.1415927")
    xy.get shouldBe FuzzyNumber(Left(Left(Right(Rational(31415927, 10000000)))), Scalar, Some(AbsoluteFuzz(0.00000005, Box)))
    val z: Number = xy.get
    val q: Option[String] = z.fuzz.map(f => f.toString(3.1415927))
    q should matchPattern { case Some("3.14159270[5]") => }
  }

  behavior of "parse"
  it should "work for 2.*" in {
    val xy = Number.parse("2.*")
    xy.get.fuzz should matchPattern { case Some(AbsoluteFuzz(0.5, Box)) => }
    xy.get.fuzz.get.normalizeShape should matchPattern { case AbsoluteFuzz(0.2886751345948129, Gaussian) => }
  }

  behavior of "render"
  private val z = implicitly[Valuable[Double]]
  it should "render Pi" in {
    z.render(3.1415927) shouldBe "3.141592700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
  }
  it should "render Planck" in {
    z.render(6.62607015E-34) shouldBe "6.62607015000000000000E-34"
  }
  it should "render Avagadro" in {
    z.render(6.02214076E23) shouldBe "6.02214076000000000000E+23"
  }
}
