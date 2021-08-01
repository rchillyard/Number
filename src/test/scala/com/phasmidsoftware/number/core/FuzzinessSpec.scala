package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Fuzziness.createFuzz
import com.phasmidsoftware.number.parse.NumberParser
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Left, Try}

class FuzzinessSpec extends AnyFlatSpec with should.Matchers {

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
    xy.get shouldBe FuzzyNumber(Left(Right(Rational(31415927, 10000000))), Scalar, Some(AbsoluteFuzz(0.00000005, Box)))
    val z: Number = xy.get
    val q: Option[String] = z.fuzz.map(f => f.toString(3.1415927))
    q should matchPattern { case Some("3.14159270[5]") => }
  }
  it should "work for 3.1416" in {
    val target = Number("3.1416")
    target.toString shouldBe "3.14160[5]"
  }

  behavior of "parse"
  it should "work for 2.*" in {
    val xy = Number.parse("2.*")
    xy.get.fuzz should matchPattern { case Some(AbsoluteFuzz(0.5, Box)) => }
    xy.get.fuzz.get.normalizeShape should matchPattern { case AbsoluteFuzz(0.2886751345948129, Gaussian) => }
  }

  behavior of "createFuzz"
  it should "work for 0" in {
    createFuzz(0) shouldBe RelativeFuzz(1.6E-16, Box)
  }
  it should "work for 1" in {
    createFuzz(1) shouldBe RelativeFuzz(3.2E-16, Box)
  }
  it should "work for 5" in {
    createFuzz(5) shouldBe RelativeFuzz(5.12E-15, Box)
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

  behavior of "Box.wiggle"
  it should "be likely for 1.251" in {
    val xy = Number.parse("1.251")
    xy.isSuccess shouldBe true
    val x: Number = xy.get
    val z: Option[Fuzziness[Double]] = x.fuzz
    z.isDefined shouldBe true
    val q: Fuzziness[Double] = z.get
    q.shape should matchPattern { case Box => }
    q.shape.wiggle(0.0005, 0.5) shouldBe 0.00025
    // NOTE that the probability is ignored for a box
    q.shape.wiggle(0.0005, 0.1) shouldBe 0.00025
    q.shape.wiggle(0.0005, 0.9) shouldBe 0.00025
  }

  behavior of "Fuzziness.wiggle"
  it should "be likely for 1.251" in {
    val xy = Number.parse("1.251")
    xy.isSuccess shouldBe true
    val x: Number = xy.get
    val z: Option[Fuzziness[Double]] = x.fuzz
    z.isDefined shouldBe true
    val q: Fuzziness[Double] = z.get
    q.wiggle(0.5) shouldBe 0.00025
    // NOTE that the probability is ignored for a box
    q.wiggle(0.1) shouldBe 0.00025
    q.wiggle(0.9) shouldBe 0.00025
  }

  behavior of "Gaussian.wiggle"
  it should "be likely for 5.0040" in {
    val xy: Option[Number] = for (a <- Number.parse("1.251").toOption; b <- Number.parse("4.00*").toOption; x <- (a * b).asNumber) yield x
    xy.isDefined shouldBe true
    val x: Number = xy.get
    val z: Option[Fuzziness[Double]] = x.fuzz
    z.isDefined shouldBe true
    val q: Fuzziness[Double] = z.get
    q should matchPattern { case RelativeFuzz(_, _) => }
    q.shape should matchPattern { case Gaussian => }
    q.style shouldBe true
    val r = q.asInstanceOf[RelativeFuzz[Double]].tolerance
    r shouldBe 8.605898416428996E-4 +- 0.0000000001
    q.shape.wiggle(r, 0.0) shouldBe Double.PositiveInfinity
    q.shape.wiggle(r, 0.1) shouldBe (r * 1.1630871536766743 / Gaussian.sigma) +- 0.00001
    q.shape.wiggle(r, 0.2) shouldBe (r * 0.9061938024368233 / Gaussian.sigma) +- 0.00001
    q.shape.wiggle(r, 0.3) shouldBe (r * 0.7328690779592166 / Gaussian.sigma) +- 0.00001
    q.shape.wiggle(r, 0.4) shouldBe (r * 0.5951160814499948 / Gaussian.sigma) +- 0.00001
    q.shape.wiggle(r, 0.5) shouldBe (r * 0.47693627620446977 / Gaussian.sigma) +- 0.00001
    q.shape.wiggle(r, 0.5) shouldBe 0.000580459 +- 0.000001
    q.shape.wiggle(r, 0.6) shouldBe (r * 0.37080715859355795 / Gaussian.sigma) +- 0.00001
    q.shape.wiggle(r, 0.7) shouldBe (r * 0.27246271472675443 / Gaussian.sigma) +- 0.00001
    q.shape.wiggle(r, 0.8) shouldBe (r * 0.1791434546212916 / Gaussian.sigma) +- 0.00001
    q.shape.wiggle(r, 0.9) shouldBe (r * 0.08885599049425764 / Gaussian.sigma) +- 0.00001
    q.shape.wiggle(r, 1) shouldBe 0
  }

  behavior of "map"
  it should "work" in {
    val fuzz = RelativeFuzz(1E-15, Box)
    val n: FuzzyNumber = FuzzyNumber(Value.fromInt(1), Scalar, None).addFuzz(fuzz).asInstanceOf[FuzzyNumber]
    val op = MonadicOperationExp
    val r: Option[Value] = Operations.doTransformValueMonadic(n.value)(op.functions)
    r.isDefined shouldBe true
    val q = n.make(r.get, Scalar)
    val x = q.toDouble
    val v = x.get
    println(v)
    val z: Option[Fuzziness[Double]] = Fuzziness.map[Double, Double, Double](1, v, !op.absolute, op.derivative, Some(fuzz))
    z.toString shouldBe "Some(RelativeFuzz(1.0E-15,Box))"
  }

}
