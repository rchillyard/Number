package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Constants.sBoltzmann
import com.phasmidsoftware.number.core.Field.convertToNumber
import com.phasmidsoftware.number.core.Number.{NumberIsOrdering, negate, one, root2, zero}
import com.phasmidsoftware.number.core.inner.Rational.RationalHelper
import com.phasmidsoftware.number.core.inner._
import com.phasmidsoftware.number.expression.Expression.{ExpressionOps, convertFieldToExpression}
import com.phasmidsoftware.number.expression.{Expression, Literal}
import org.scalactic.Equality
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Failure, Left, Success, Try}

class NumberSpec extends AnyFlatSpec with should.Matchers with FuzzyEquality {

  implicit object ExpressionEquality extends Equality[Expression] {
    def areEqual(a: Expression, b: Any): Boolean = b match {
      case n: Number => new ExpressionOps(a).compare(Literal(n)) == 0
      case n: Expression => a.compare(n) == 0
      case _ => false
    }
  }

  private val numberOne = Constants.one
  private val bigOne = BigInt(1)
  private val ratOne = Rational.one
  private val doubleOne = 1.0
  private val bigBigInt = BigInt(2147483648L)
  private val standardFuzz = AbsoluteFuzz[Double](1E-7, Gaussian)
  private val sAlpha = "0.0072973525693(11)"
  private val sPlanck = "6.6260701500E-34" // J Hz ^ -1
  private val sAvagadro = "6.0221407600E23" // mole ^ -1

  behavior of "create"
  it should "yield Right(1)" in {
    val target = Number.create(Right(1))
    target should matchPattern { case ExactNumber(_, _) => }
    target.nominalValue should matchPattern { case Right(_) => }
  }
  it should "yield Left(Right(bigBigInt))" in {
    val target = Number.create(Left(Right(bigBigInt)))
    target should matchPattern { case ExactNumber(_, _) => }
    target.nominalValue shouldBe Left(Right(Rational(bigBigInt)))
  }
  it should "yield Right(1, Fuzz)" in {
    val target = Number.create(Right(1), standardFuzz)
    target should matchPattern { case FuzzyNumber(_, _, _) => }
    target.nominalValue should matchPattern { case Right(_) => }
  }
  it should "yield Right(1, Radian)" in {
    val target = Number.pi
    target should matchPattern { case ExactNumber(_, _) => }
    target.nominalValue should matchPattern { case Right(_) => }
    target.factor shouldBe Radian
  }
  it should "yield Right(1, Radian, Fuzz)" in {
    val target = Number.create(Right(1), Radian, Some(standardFuzz))
    target should matchPattern { case FuzzyNumber(_, _, _) => }
    target.nominalValue should matchPattern { case Right(_) => }
    target.factor shouldBe Radian
  }

  behavior of "nominalValue"
  it should "yield Right(1)" in {
    val target = Number.create(Right(1))
    target.nominalValue shouldBe Right(1)
  }
  it should "yield Left(Right(bigBigInt))" in {
    val target = Number.create(Left(Right(bigBigInt)))
    target.nominalValue shouldBe Left(Right(Rational(bigBigInt)))
  }

  behavior of "toRational"
  it should "yield Some(1)" in {
    val target = Number.create(Right(1))
    target.toRational shouldBe Some(ratOne)
  }

  behavior of "toString"
  it should "yield 1" in {
    val target = Number.create(Right(1))
    target.toString shouldBe "1"
  }
  it should "work for Pi" in {
    Number.pi.toString shouldBe "\uD835\uDED1"
  }
  it should "work for Radian as scalar" in {
    val target = Number.pi.scale(PureNumber)
    // NOTE that:       Pi is 3.1415926535897932384626433...
    // NOTE that:  math.PI is 3.14159265358979323846
    // In IEEE 754 binary, pi is 400921fb54442d18, which is:
    //                        3.141592653589793
    target.toString shouldBe "3.141592653589793[5]"
  }
  it should "work for E" in {
    Number.e.toString shouldBe "\uD835\uDF00"
  }
  it should "work for E as scalar (rel fuzzy)" in {
    val target = Number.e.scale(PureNumber)
    val w = target.toString
    w should startWith("2.718281828459045")
    w should endWith("%")
  }
  it should "work for 1 scaled as Radian" in {
    Number.one.scale(Radian).toString shouldBe "0.3183098861837907[5]\uD835\uDED1"
  }
  it should "work for E^2" in {
    val target = Number.e doPower 2
    target.toString shouldBe "\uD835\uDF00\u00B2"
  }
  it should "work for E^3" in {
    val target = Number.e doPower 3
    target.toString shouldBe "\uD835\uDF00\u00B3"
  }
  it should "work for E^4" in {
    val target = Number.e doPower 4
    target.toString shouldBe "\uD835\uDF00\u2074"
  }
  it should "work for E^10" in {
    val target = Number.e doPower 10
    target.toString shouldBe "\uD835\uDF00^10"
  }
  it should "work for square root E" in {
    val target = Number.e.sqrt
    target.toString shouldBe "√\uD835\uDF00"
  }
  it should "work for i" in {
    val target = Number.i
    target.toString shouldBe "i"
  }

  behavior of "normalize"

  it should "work for E as scalar (abs fuzzy)" in {
    val target = Number.e.scale(PureNumber).normalize
    target.toString.substring(0, 17) shouldBe "2.718281828459045"
  }
  it should "work for E^2 as Real" in {
    val target = Number("2\uD835\uDF00").normalize.asInstanceOf[Real]
    target.x.scale(PureNumber).toString shouldBe "7.389056098930650(44)"
  }
  it should "work for E, SquareRoot" in {
    val target = Number(math.E * math.E)
    val result = target.sqrt.normalize.asInstanceOf[Real]
    result.render shouldBe "2.7182818284590450[86]"
  }
  // TODO fix this--it fails in CircleCI (fails here, too)
  // NOTE this is quite bizarre
  ignore should "work for NatLog, SquareRoot" in { //fixed
    val target = Number.e
    val expected = Number(math.E * math.E, SquareRoot)
    val result: Field = target.scale(SquareRoot).normalize
    result.render shouldBe "2.7182818284590455[98]"
    //result should ===(expected)
    //Literal(result) should ===(expected) Literal doesn't work here. I'll study this later.
    convertFieldToExpression(result) should ===(expected)
  }
  // NOTE same issues as previous test
  ignore should "work for NatLog, SquareRoot approx" in {
    val target = Number.e
    val expected = Number(math.E * math.E, SquareRoot)
    val normalized = target.scale(SquareRoot).normalize
    normalized match {
      case r: Real =>
        r.render shouldBe "2.7182818284590455[98]"
        r.render.substring(0, 17) shouldBe "2.718281828459045"
        r.x should ===(expected)
      case c: Complex =>
        fail(s"not expecting: $c")
    }
  }
  it should "multiply root2 and root2" in {
    (root2 multiply Constants.root2).normalize shouldBe Constants.two
  }
  // NOTE problem with handling roots
  ignore should "multiply sin by sin" in {
    val piBy4 = Number.pi doDivide 4
    val sinePiBy4 = piBy4.sin
    val oneHalf = sinePiBy4 doMultiply sinePiBy4
    oneHalf.normalize shouldBe Number.two.invert
  }
  it should "work for easy Rational" in {
    val z = Number(Rational(9, 4)).sqrt
    z.normalize shouldBe Real(Rational(3, 2))
  }

  behavior of "toDouble"
  it should "yield 1" in {
    val target = Number.create(Right(1))
    target.toNominalDouble shouldBe Some(doubleOne)
  }

  behavior of "isValid"
  it should "yield true for 1" in {
    val target = Number.create(Right(1))
    target.isValid shouldBe true
  }
  it should "yield false for None" in {
    val target = Number.create(Left(Left(None)))
    target.isValid shouldBe false
  }

  behavior of "parse"
  it should "work for 1" in {
    val xy: Try[Number] = Number.parse("1")
    xy.get shouldBe Number.create(Right(1))
  }
  it should "work for 3/2" in {
    val xy: Try[Number] = Number.parse("3/2")
    xy.get shouldBe Number.create(Left(Right(Rational(3, 2))))
  }
  it should "work for 3.1415927" in {
    val xy: Try[Number] = Number.parse("3.1415927")
    xy.get shouldBe FuzzyNumber(Left(Right(Rational(31415927, 10000000))), PureNumber, Some(AbsoluteFuzz(0.00000005, Box)))
  }
  it should "work for BigInt" in {
    val xy: Try[Number] = Number.parse("2147483648")
    xy.get shouldBe Number.create(Left(Right(bigBigInt)))
  }
  it should "work for Radian" in {
    val xy: Try[Number] = Number.parse("1" + Factor.sPi)
    xy.get shouldBe Number(Radian)
  }
  it should "work for pi" in {
    val xy: Try[Number] = Number.parse("1pi")
    xy.get shouldBe Number(Radian)
  }
  it should "work for e" in {
    val xy: Try[Number] = Number.parse("1" + Factor.sE)
    xy.get shouldBe Number(1, NatLog)
  }
  it should "fail with x" in {
    Number.parse("1x") should matchPattern { case Failure(_) => }
  }
  it should """fail for """"" in {
    Number.parse("") should matchPattern { case Failure(_) => }
  }

  behavior of "apply"
  it should """fail for """"" in {
    an[NumberExceptionWithCause] should be thrownBy Number("")
  }
  it should """work for "1"""" in {
    val target = Number("1")
    target.nominalValue shouldBe Right(1)
  }
  it should """work for "2147483648"""" in {
    val target = Number("2147483648")
    target.isExact shouldBe true
    target.nominalValue shouldBe Left(Right(Rational(bigBigInt)))
  }
  it should """work for "3.1415927"""" in {
    val target = Number("3.1415927")
    target.isExact shouldBe false
    target.nominalValue shouldBe Left(Right(Rational(31415927, 10000000)))
  }
  it should "work for 3.1416" in {
    val target = Number(3.1416)
    target.isExact shouldBe false
    target shouldEqual Number(Rational(3927, 1250))
  }
  it should """work for "𝛑" """ in {
    val target = Number("\uD835\uDED1")
    target.isExact shouldBe true
    target.nominalValue shouldBe Right(1)
    target.factor shouldBe Radian
  }
  it should "work for 1" in {
    val target = Number.one
    target.isExact shouldBe true
    target.nominalValue shouldBe Right(1)
  }
  it should "work for bigBigInt" in {
    val target = Number(bigBigInt)
    target.isExact shouldBe true
    target.nominalValue shouldBe Left(Right(Rational(bigBigInt)))
  }
  it should "work for Rational(1,2)" in {
    val target = Number(Rational(1, 2))
    target.isExact shouldBe true
    target.nominalValue shouldBe Left(Right(Rational(1, 2)))
  }
  it should "work for math.pi" in {
    val target = Number(Math.PI)
    target.isExact shouldBe false
    target shouldEqual Number(Rational(3141592653589793L, 1000000000000000L))
  }
  it should "work for nothing" in {
    val target = Number()
    target.nominalValue shouldBe Left(Left(None))
  }
  it should "work for BigDecimal(3.1415927)" in {
    val target = Number(BigDecimal(3.1415927))
    target.isExact shouldBe true
    target.nominalValue shouldBe Left(Right(Rational(31415927, 10000000)))
  }
  it should "work for 3.1415927" in {
    val target = Number(3.1415927)
    target.isExact shouldBe false
    target shouldEqual Number(Rational(31415927, 10000000))
  }
  it should """work for 3.1415926535897932384626433""" in {
    val target = Number(3.1415926535897932384626433)
    target.isExact shouldBe false
    target shouldEqual Number(Rational(3141592653589793L, 1000000000000000L))
  }
  it should "work for 0.5" in {
    val target = Number(0.5)
    target.isExact shouldBe true
    target.nominalValue shouldBe Left(Right(Rational(1, 2)))
  }
  it should "work for 1.23" in {
    val target = Number(1.23)
    target.isExact shouldBe true
    target.nominalValue shouldBe Left(Right(Rational(123, 100)))
  }
  it should "work for 1.234" in {
    val target = Number(1.234)
    target.isExact shouldBe false
    target.nominalValue shouldBe Left(Left(Some(1.234)))
    target.toString shouldBe "1.234[5]"
  }
  it should "work for 1.23400" in {
    val target = Number(1.23400)
    target.isExact shouldBe false
    target.nominalValue shouldBe Left(Left(Some(1.234)))
    target.toString shouldBe "1.234[5]"
  }
  it should "support exact strings" in {
    val target = Number("3.141592700")
    target.isExact shouldBe true
    target should matchPattern { case ExactNumber(_, _) => }
    target.nominalValue shouldBe Left(Right(Rational(31415927, 10000000)))
  }

  behavior of "Number.parse" // CONSIDER Moving these into NumberParserSpec
  it should "parse boltzmann" in {
    val zy = Number.parse(sBoltzmann)
    zy should matchPattern { case Success(_) => }
    zy.get.isExact shouldBe true
    zy.get.toNominalDouble shouldBe Some(1.380649E-23)
  }
  it should "fail to parse boltzmann with alternative minus" in {
    val zy = Number.parse("1.380649E−23")
    zy should matchPattern { case Failure(_) => }
  }
  it should "parse planck" in {
    val z = Number.parse(sPlanck)
    z should matchPattern { case Success(_) => }
    z.get.isExact shouldBe true
  }
  it should "parse c" in {
    val z = Number.parse("299792458") // m sec ^ -1
    z should matchPattern { case Success(_) => }
    z.get.isExact shouldBe true
  }
  it should "parse avagadro" in {
    val z = Number.parse(sAvagadro)
    z should matchPattern { case Success(_) => }
    z.get.isExact shouldBe true
  }

  behavior of "FuzzOps"
  it should "get mu" in {
    import Number.FuzzOps
    val x = 1836.15267343 ~ 11
    x.isExact shouldBe false
    x.toString shouldBe "1836.15267343(11)"
    Real(x) shouldEqual Constants.mu
  }
  it should "get G using FuzzStringOps" in {
    import Number.FuzzStringOps
    val xy = "6.67430E-11" ~ 15
    xy.isSuccess shouldBe true
    val x = xy.get
    x.isExact shouldBe false
    // TODO do this through pattern matching
    val fuzzyX = x.asInstanceOf[FuzzyNumber]
    fuzzyX.fuzz shouldBe Some(AbsoluteFuzz(1.5E-15, Gaussian))
    Real(x) shouldEqual Constants.G // "6.67430(15)E-11"
    x.toString shouldBe "6.67430(15)E-11"
  }
  it should "get alpha" in {
    import Number.FuzzOps
    val x = 0.0072973525693 ~ 11
    x.isExact shouldBe false
    x shouldEqual Number(sAlpha)
    x.toString shouldBe "0.0072973525693(11)"
  }

  behavior of "specialize"
  it should "work for 1" in {
    val target = Number.one
    target.specialize.nominalValue shouldBe Right(1)
  }
  it should "work for BigInt 1" in {
    val target = Number.create(Left(Right(Rational(bigOne))), PureNumber)
    target.specialize.nominalValue shouldBe Right(1)
  }
  it should "work for BigInt(1+Int.MaxValue)" in {
    val bigInt = BigInt(1L + Int.MaxValue)
    val target = Number(bigInt)
    target.specialize.nominalValue shouldBe Left(Right(Rational(bigInt)))
  }
  it should "work for Rational(1)" in {
    val target = Number.create(Left(Right(ratOne)), PureNumber)
    target.specialize.nominalValue shouldBe Right(1)
  }
  it should "work for Rational.half" in {
    val target = Number(Rational.half)
    target.specialize.nominalValue shouldBe Left(Right(Rational(1, 2)))
  }
  it should "work for 0.5" in {
    val target = Number(0.5)
    target.specialize.nominalValue shouldBe Left(Right(Rational(1, 2)))
  }
  it should "work for 1.0" in {
    val target = Number(doubleOne)
    target.specialize.nominalValue shouldBe Right(1)
  }
  it should "work for nothing" in {
    val target = Number()
    target.specialize.nominalValue shouldBe Left(Left(None))
  }

  behavior of "scale"
  it should "work for PureNumber, PureNumber" in {
    val target = Number.one
    target.scale(PureNumber) shouldBe Number.one
  }
  it should "work for Radian, Radian" in {
    val target = Number(1, Radian)
    target.scale(Radian) shouldBe target
  }
  it should "work for PureNumber, Radian" in {
    val target = Number.one
    target.scale(Radian) should ===(Number(1 / Math.PI, Radian))
  }
  it should "work for Radian, PureNumber" in {
    val target = Number(1, Radian)
    target.scale(PureNumber) should ===(Number(Math.PI))
  }
  it should "work for PureNumber, NatLog" in {
    val target = Number.one
    val result = target.scale(NatLog).simplify
    // NOTE that the simplify method brings this back to being just one.
    result shouldBe Number.one
  }
  it should "work for NatLog, PureNumber" in {
    val target = Number(1, NatLog)
    target.scale(PureNumber) should ===(Number(Math.E))
  }
  it should "work for 2E, PureNumber" in {
    val target = Number(2, NatLog)
    val actual: Number = target.scale(PureNumber)
    val expected: Number = Number(Math.E) doPower 2
    actual should ===(expected)
  }
  it should "work for 2E, PureNumber but comparing against NatLog * NatLog" in {
    val target = Number(2, NatLog)
    val actual: Number = target.scale(PureNumber)
    val expected: Number = Number(Math.E) doMultiply Number(Math.E)
    actual should ===(expected)
  }
  it should "work for PureNumber, 2E (same as before but with parameters to === reversed" in {
    val target = Number(2, NatLog)
    val actual: Number = target.scale(PureNumber)
    val expected: Number = Number(Math.E) doPower 2
    expected should ===(actual)
  }
  it should "work for PureNumber, 2E (same as before but using NatLog * NatLog and parameters to === reversed" in {
    val target = Number(2, NatLog)
    val actual: Number = target.scale(PureNumber)
    val expected: Number = Number(Math.E) doMultiply Number(Math.E)
    expected should ===(actual)
  }
  it should "work for NatLog, Radian" in {
    val target = Number(1, NatLog)
    val expected = Number(Math.E / Math.PI, Radian)
    val result = target.scale(Radian)
    result should ===(expected)
  }
  it should "work for Radian, NatLog" in {
    val target = Number(1, Radian)
    val expected = Number(math.log(Math.PI), NatLog)
    val result = target.scale(NatLog)
    result === expected shouldBe true
  }
  it should "work for Log2, PureNumber" in {
    val target = Number(Rational.half, Log2)
    target.render shouldBe "√2"
    val expected = Number(math.sqrt(2), PureNumber)
    val result = target.scale(PureNumber)
    result should ===(expected)
  }
  it should "work for Log10, PureNumber" in {
    val target = Number(Rational.half, Log10)
    target.render shouldBe "√10"
    val expected = Number(math.sqrt(10), PureNumber)
    val result = target.scale(PureNumber)
    result should ===(expected)
  }
  it should "work for Log2, NatLog" in {
    import Rational.RationalOps
    val target = Number(1 :/ 2, Log2)
    target.render shouldBe "√2"
    val expected = Number(math.log(math.sqrt(2)), NatLog)
    val result = target.scale(NatLog)
    result should ===(expected)
  }
  it should "work for SquareRoot, PureNumber" in {
    val target = Number(Rational.two, SquareRoot)
    target.render shouldBe "√2"
    val expected = Number(math.sqrt(2), PureNumber)
    val result = target.scale(PureNumber)
    result should ===(expected)
  }
  it should "work for Root2s" in {
    val target = Number(Rational.two, SquareRoot)
    target.render shouldBe "√2"
    val expected = Number(math.sqrt(2), PureNumber)
    val result = target.scale(PureNumber)
    result should ===(expected)
  }
  it should "work for CubeRoot, PureNumber" in {
    val target = Number(Rational.two, CubeRoot)
    target.render shouldBe "³√2"
    val expected = Number(math.pow(2, 1.0 / 3), PureNumber)
    val result = target.scale(PureNumber)
    result should ===(expected)
  }
  it should "work for SquareRoot, CubeRoot" in {
    val target = Number(4, SquareRoot)
    target.render shouldBe "√4"
    val expected = Number(8, CubeRoot)
    val result = target.scale(CubeRoot)
    result shouldBe expected
  }
  it should "work for PureNumber, SquareRoot" in {
    val target = Number(3)
    val expected = Number(3)
    val result = target.scale(SquareRoot).simplify
    result shouldBe expected
    result.toString shouldBe "3"
  }

  behavior of "alignFactors"
  it should "work for PureNumber, PureNumber" in {
    val target = Number.one
    target.asInstanceOf[GeneralNumber].alignFactors(Number(2)) shouldBe(Number.one, Number(2))
  }
  it should "work for PureNumber, Radian" in {
    val target = Number.one
    val (p, q) = target.asInstanceOf[GeneralNumber].alignFactors(Number(2, Radian))
    p shouldBe Number.one
    q shouldEqual Number(2 * Math.PI)
  }
  it should "work for Radian, PureNumber" in {
    val target = Number(2, Radian)
    val (f, x) = target.asInstanceOf[GeneralNumber].alignFactors(Number.one)
    f shouldEqual Number(2 * Math.PI)
    x shouldBe Number.one
  }
  it should "work for Radian, Radian" in {
    val target = Number(1, Radian)
    target.asInstanceOf[GeneralNumber].alignFactors(Number(2, Radian)) shouldBe(Number(1, Radian), Number(2, Radian))
  }

  behavior of "alignTypes"
  it should "work for Int,Int" in {
    val target = Number.one
    target.asInstanceOf[GeneralNumber].alignTypes(Number(2).asInstanceOf[GeneralNumber]) shouldBe(Number.one, Number(2))
  }
  it should "work for Int Int(Radian)" in {
    val target: GeneralNumber = Number.one.asInstanceOf[GeneralNumber]
    target.alignTypes(Number(1, Radian)) shouldBe(Number.one, Number(1, Radian))
  }
  it should "work for Int,BigInt" in {
    val target = Number.one.asInstanceOf[GeneralNumber]
    target.alignTypes(Number(bigBigInt)) shouldBe(Number(bigBigInt), Number(bigOne))
  }
  it should "work for BigInt,Int" in {
    val target = Number(bigOne).asInstanceOf[GeneralNumber]
    target.alignTypes(Number(2)) shouldBe(Number(bigOne), Number(BigInt(2)))
  }
  it should "work for Int,Rational" in {
    val target = Number.one.asInstanceOf[GeneralNumber]
    target.alignTypes(Number(Rational(2, 3))) shouldBe(Number(Rational(2, 3)), Number(Rational(1)))
  }
  it should "work for Rational,Int" in {
    val target = Number(Rational(1)).asInstanceOf[GeneralNumber]
    target.alignTypes(Number(2)) shouldBe(Number(Rational(1)), Number(Rational(2)))
  }
  it should "work for Int,Double" in {
    val target = Number.one.asInstanceOf[GeneralNumber]
    val (p, q) = target.alignTypes(Number(Math.PI))
    p should ===(Number(Math.PI))
    q should ===(Number(doubleOne))
  }
  it should "work for Double,Int" in {
    val target = Number(doubleOne).asInstanceOf[GeneralNumber]
    target.alignTypes(Number(2)) shouldBe(Number(doubleOne), Number(2.0))
  }
  it should "work for Int,None" in {
    val target = Number.one.asInstanceOf[GeneralNumber]
    target.alignTypes(Number()) shouldBe(Number(), Number())
  }
  it should "work for None,Int" in {
    val target = Number().asInstanceOf[GeneralNumber]
    target.alignTypes(Number(2)) shouldBe(Number(), Number())
  }
  it should "work for BigInt,BigInt" in {
    val target = Number(bigOne).asInstanceOf[GeneralNumber]
    target.alignTypes(Number(BigInt(2))) shouldBe(Number(bigOne), Number(BigInt(2)))
  }
  it should "work for BigInt,Rational" in {
    val target = Number(bigOne).asInstanceOf[GeneralNumber]
    target.alignTypes(Number(Rational(2, 3))) shouldBe(Number(Rational(2, 3)), Number(Rational(1)))
  }
  it should "work for Rational,BigInt" in {
    val target = Number(Rational(1)).asInstanceOf[GeneralNumber]
    target.alignTypes(Number(BigInt(2))) shouldBe(Number(Rational(1)), Number(Rational(2)))
  }
  it should "work for BigInt,Double" in {
    val target = Number(bigOne).asInstanceOf[GeneralNumber]
    val (p, q) = target.alignTypes(Number(Math.PI))
    p should ===(Number(Math.PI))
    q should ===(Number(doubleOne))
  }
  it should "work for Double,BigInt" in {
    val target = Number(doubleOne).asInstanceOf[GeneralNumber]
    target.alignTypes(Number(BigInt(2))) shouldBe(Number(doubleOne), Number(2.0))
  }
  it should "work for BigInt,None" in {
    val target = Number(bigOne).asInstanceOf[GeneralNumber]
    target.alignTypes(Number()) shouldBe(Number(), Number())
  }
  it should "work for None,BigInt" in {
    val target = Number().asInstanceOf[GeneralNumber]
    target.alignTypes(Number(BigInt(2))) shouldBe(Number(), Number())
  }
  it should "work for Rational,Rational" in {
    val target = Number(Rational(1)).asInstanceOf[GeneralNumber]
    target.alignTypes(Number(Rational(2))) shouldBe(Number(Rational(1)), Number(Rational(2)))
  }
  it should "work for Rational,Double" in {
    val target = Number(Rational(1)).asInstanceOf[GeneralNumber]
    val (p, q) = target.alignTypes(Number(Math.PI))
    p should ===(Number(Math.PI))
    q should ===(Number(doubleOne))
  }
  it should "work for Double,Rational" in {
    val target = Number(doubleOne).asInstanceOf[GeneralNumber]
    target.alignTypes(Number(Rational(2))) shouldBe(Number(doubleOne), Number(2.0))
  }
  it should "work for Rational,None" in {
    val target = Number(Rational(1)).asInstanceOf[GeneralNumber]
    target.alignTypes(Number()) shouldBe(Number(), Number())
  }
  it should "work for None,Rational" in {
    val target = Number().asInstanceOf[GeneralNumber]
    target.alignTypes(Number(Rational(2))) shouldBe(Number(), Number())
  }
  it should "work for Double,Double" in {
    val target = Number(doubleOne).asInstanceOf[GeneralNumber]
    target.alignTypes(Number(2.0)) shouldBe(Number(doubleOne), Number(2.0))
  }
  it should "work for Double,None" in {
    val target = Number(doubleOne).asInstanceOf[GeneralNumber]
    target.alignTypes(Number()) shouldBe(Number(), Number())
  }
  it should "work for None,Double" in {
    val target = Number().asInstanceOf[GeneralNumber]
    target.alignTypes(Number(2.0)) shouldBe(Number(), Number())
  }
  it should "work for None,None" in {
    val target = Number().asInstanceOf[GeneralNumber]
    target.alignTypes(Number()) shouldBe(Number(), Number())
  }

  behavior of "plus"
  it should "add 1 and 2" in {
    val x = numberOne
    val y = Constants.two
    (x add y) shouldBe Real(3)
  }
  it should "add BigInt 1 and 2" in {
    val x = Number(bigOne)
    val y = Constants.two
    (x add y) shouldBe Real(3)
  }
  it should "add Rational 1 and 2" in {
    val x = Number(ratOne)
    val y = Constants.two
    (x add y) shouldBe Real(3)
  }
  it should "add Double 1 and 2" in {
    val x = Number(doubleOne)
    val y = Constants.two
    (x add y) shouldBe Real(3)
  }
  it should "add Double 1 and Radian" in {
    val x = Number(doubleOne)
    val y = Real(Number(1, Radian))
    convertToNumber(x add y) should ===(Number(Math.PI + 1))
  }
  it should "add Radian and 2Pi" in {
    val x = Number(1, Radian)
    val y = Real(Number(2, Radian))
    convertToNumber(x add y) shouldBe Number(3, Radian)
  }
  it should "add 1 to pi" in {
    val x1 = Number.one
    val x2 = Constants.pi
    (x1 add x2).toString shouldBe "4.1415926535897930(41)"
  }
  it should "add 1 to e" in {
    val x1 = Number.one
    val x2 = Constants.e
    (x1 add x2) should ===(Real(3.7182818284590450))
  }
  it should "add 1 to √2" in {
    val x1 = Number.one
    val x2 = Constants.root2
    (x1 add x2) should ===(Real(2.414213562373095))
  }
  it should "add 1 to √3" in {
    val x1 = Number.one
    val x2 = Real(Number.root3)
    (x1 add x2) should ===(Real(2.732050807568878))
  }
  it should "add 1 to √5" in {
    val x1 = Number.one
    val x2 = Real(Number.root5)
    (x1 add x2) should ===(Real(3.23606797749979))
  }

  behavior of "minus"
  it should "negate 1" in {
    val x = numberOne
    -x shouldBe Constants.minusOne
  }
  it should "negate BigInt 1" in {
    val x = Number(bigOne)
    -x shouldBe Real(Number(BigInt(-1)))
  }
  it should "negate Rational 1" in {
    val x = Number(ratOne)
    -x shouldBe Real(Number(Rational(-1)))
  }
  it should "negate Double 1" in {
    val x = Number(doubleOne)
    -x shouldBe Real(-doubleOne)
  }

  behavior of "subtract"
  it should "subtract 1 from 2" in {
    val x = Constants.two
    val y = numberOne
    (x add -y) shouldBe numberOne
  }
  it should "subtract BigInt 1 from 2" in {
    val x = Number(BigInt(2))
    val y = numberOne
    (x add -y) shouldBe numberOne
  }
  it should "subtract Rational 1 from 2" in {
    val x = Number(Rational(2))
    val y = numberOne
    (x add -y) shouldBe numberOne
  }
  it should "subtract Double 1 from 2" in {
    val x = Number(2.0)
    val y = numberOne
    (x add -y) shouldBe numberOne
  }

  behavior of "multiply"
  it should "multiply 1 and 2" in {
    val x = numberOne
    val y = Constants.two
    (x multiply y) shouldBe Constants.two
  }
  it should "multiply BigInt 1 and 2" in {
    val x = Number(bigOne)
    val y = Constants.two
    (x multiply y) shouldBe Constants.two
  }
  it should "multiply Rational 1 and 2" in {
    val x = Number(Rational(1))
    val y = Constants.two
    (x multiply y) shouldBe Constants.two
  }
  it should "multiply Double 1 and 2" in {
    val x = Number(doubleOne)
    val y = Constants.two
    (x multiply y) shouldBe Constants.two
  }
  it should "multiply 2 and Radian" in {
    val x = Constants.two
    val y = Real(Number(1, Radian))
    (x multiply y).isSame(Real(Number.zeroR))
  }
  it should "multiply Radian and 2" in {
    val x = Number(1, Radian)
    val y = Constants.two
    (x multiply y) isSame Real(Number.zeroR)
  }
  it should "multiply rational by Int" in {
    val target = Number("3/5")
    val nineTenths = target.doMultiple(Rational("3/2"))
    nineTenths.toRational shouldBe Some(Rational("9/10"))
    nineTenths.doMultiple(10) shouldBe Number(9)
  }

  behavior of "invert"
  it should "invert 1" in {
    val x = numberOne
    x.invert shouldBe numberOne
  }
  it should "invert BigInt 1" in {
    val x = Number(bigOne)
    x.invert shouldBe numberOne
  }
  it should "invert 2" in {
    val x = Constants.two
    x.invert shouldBe Real(Rational.half)
  }
  it should "invert BigInt 2" in {
    val x = Number(BigInt(2))
    x.invert shouldBe Real(Rational.half)
  }
  it should "invert Rational 2" in {
    val x = Number(Rational.two)
    x.invert shouldBe Real(Rational.half)
  }
  it should "invert Double pi" in {
    val x = Number(Math.PI)
    x.invert should ===(Real(1 / Math.PI))
  }

  behavior of "division"
  it should "divide 1 by 2" in {
    val x = numberOne
    val y = Constants.two
    (x divide y) shouldBe Constants.half
  }
  it should "divide BigInt 1 by 2" in {
    val x = Number(bigOne)
    val y = Constants.two
    (x divide y) shouldBe Constants.half
  }
  it should "divide Rational 1 by 2" in {
    val x = Number(Rational(1))
    val y = Constants.two
    (x divide y) shouldBe Constants.half
  }
  it should "divide Double 1 by Double Radian" in {
    val x = Number(doubleOne)
    val y = Real(Number(Math.PI))
    convertToNumber(x divide y) should ===(Number(1 / Math.PI))
  }

  // XXX what is this ** operator?
  behavior of "**"
  it should "work for 2^2" in {
    val target = Constants.two
    (target power 2) shouldBe Real(4)
  }

  behavior of "power"
  it should "work for squaring PureNumber" in {
    val target = Number.two
    val number = target.power(2)
    number shouldBe Number(4)
  }
  it should "work for squaring SquareRoot" in {
    val target = Number.root2
    Real(target.power(2)) isSame Constants.two
  }
  it should "work for squaring CubeRoot" in {
    val target = Number.root3
    Real(target.power(2)) isSame Real(3)
  }
  it should "work for cubing cube-root2" in {
    val target = Number(2, CubeRoot)
    target.power(3) shouldBe Number.two
  }
  it should "work for squaring NatLog" in {
    val target = Number(Rational.half, NatLog)
    target.power(2) shouldBe Number.e
  }
  it should "work for squaring Log2" in {
    val target = Number(Rational.half, Log2) // square root of 2
    val result = target.power(2) // 2
    result shouldBe Number(1, Log2)
    // NOTE this should actually be equal to just plain old Number.two (need to simplify result).
    result should ===(Number.two)
  }

  behavior of "sqrt"
  it should "work for easy ints" in {
    Number(1).sqrt shouldBe Number(1)
    Number(4).sqrt shouldBe Number(2)
    Number(9).sqrt shouldBe Number(3)
  }
  it should "work for BigInt" in {
    Number(bigBigInt).sqrt should ===(Number(Rational(2317047500592079L, 50000000000L)))
  }
  it should "work for negative numbers" in {
    Number(-1).sqrt shouldBe Number.i
    Number(-4).sqrt shouldBe Number(-4, SquareRoot)
    Number(-4).power(Number(Rational.half)) shouldBe Number(-4, SquareRoot)
  }

  behavior of "sin"
  it should "be zero for pi" in {
    val target = Number.pi
    target.sin shouldBe Number(0, PureNumber)
  }
  it should "work for 0" in {
    val target = Number(0, Radian)
    target.sin shouldBe Number(0, PureNumber)
  }
  it should "be one for pi/2" in {
    val target = (Number.pi doDivide 2).sin
    target shouldBe Number.one
  }
  it should "work for Radian/2" in {
    val target = Number(Rational.half, Radian)
    val sin = target.sin
    sin shouldBe Number(1, PureNumber)
  }
  it should "work for Radian/6" in {
    val target = Number(Rational(6).invert, Radian)
    target.sin shouldBe Number(Rational(1, 2), PureNumber)
  }
  it should "work for Radian/3" in {
    val target = Number(Rational(1, 3), Radian)
    val sin = target.sin
    sin shouldBe Number(Rational(3, 4), SquareRoot)
  }
  it should "work for Radian/4" in {
    val target = Number(Rational(1, 4), Radian)
    val sin = target.sin
    sin shouldBe Number(Rational.half, SquareRoot)
  }
  it should "work for One" in {
    val target = Number.one
    val sin = target.sin
    import com.phasmidsoftware.number.core.Number.FuzzOps
    sin should ===(0.8414709848078965 ~ 21)
  }
  it should "work for 1/12" in {
    val target = Number(Rational(12).invert, Radian)
    val sin = target.sin
    import com.phasmidsoftware.number.core.Number.FuzzOps
    sin should ===(0.2588190451025207 ~ 10)
  }

  behavior of "cos"
  it should "be zero for pi" in {
    val target = Number.pi
    target.cos === Number(-1)
  }
  it should "work for 0" in {
    val target = Number(0, Radian)
    target.cos shouldBe Number(1)
  }
  it should "work for Pi/2" in {
    val target = Number(Rational.half, Radian)
    target.cos.isZero shouldBe true
  }
  it should "work for Pi/3" in {
    val target = Number.pi doDivide 3
    target.cos shouldBe Number(Rational(1, 2), PureNumber)
  }
  it should "work for Pi/6" in {
    val target = Number.pi doDivide 6
    val cos = target.cos
    val expected = Number(3).sqrt doDivide 2
    cos should ===(expected)
  }

  behavior of "tan"
  it should "be zero for 0" in {
    val target = Number(0, Radian)
    target.tan.isZero shouldBe true
  }
  it should "be zero for pi" in {
    val target = Number.pi
    target.tan.isZero shouldBe true
  }
  it should "work for Pi/2" in {
    val target = Number(Rational.half, Radian)
    target.tan.isInfinite shouldBe true
  }
  it should "work for Pi/3" in {
    // NOTE that at some point, the angle becomes non-exact (0.8333333...)
    val target = Number.pi doDivide 3
    target.tan shouldEqual Number(3).sqrt
  }
  it should "work for Pi/6" in {
    // NOTE that at some point, the angle becomes non-exact (0.1666666...)
    val target = Number.pi doDivide 6
    Real(target.tan) should ===(Number(3).sqrt.invert)
  }

  behavior of "atan"
  it should "be 0Pi for 0/1" in {
    val target = Number.one
    target.atan(Number.zero) shouldBe Number(0, Radian)
  }
  it should "be pi/4 for 1/1" in {
    val target = Number.one
    target.atan(Number.one) === (Number.pi doDivide 4)
  }
  it should "be 0Pi for 0/-1" in {
    val target = Number.negate(Number.one)
    target.atan(Number.zero) shouldBe Number(1, Radian)
  }
  it should "be Pi / 3 for root(3)" in {
    Number.one.atan(Number(Rational(3)).sqrt) shouldEqual Number(r"1/3", Radian)
  }
  it should "be 7 Pi / 6 for 1/-root(3)" in {
    // CONSIDER shouldn't this be 5 pi / 6?
    negate(Number(Rational(3)).sqrt).atan(Number.one) shouldEqual Number(r"-5/6", Radian)
  }
  it should "be 11 Pi / 6 for -1/2" in {
    val three = Number(Rational(3))
    val root3 = three.sqrt
    val actual = root3.atan(Number.negOne)
    val expected = Number(r"-1/6", Radian)
    actual shouldEqual expected
  }
  it should "be 3 pi / 4 for 1/-1" in {
    val adjacent = Number.negate(Number.one)
    val opposite = Number.one
    val actual: Number = adjacent.atan(opposite)
    val expected: Number = (Number.pi doMultiply 3) doDivide 4
    // CONSIDER revert this so that it reads actual ... expected
    //  XXX  actual should ===(expected)
    actual shouldBe expected
  }
  // CONSIDER need to operate appropriately on negZero.
  it should "evaluate atan of 1 over -0" in {
    val number = Number.negZero.atan(Number.one)
    number shouldBe Number(Rational(-1, 2), Radian)
  }
  it should "evaluate atan of -1 over 0" in {
    val number = Number.zero.atan(negate(Number.one))
    number shouldBe Number(Rational(-1, 2), Radian)
  }

  behavior of "exp"
  it should "be E for 1" in {
    val target = Number.one
    target.exp shouldBe Number.e
  }
  it should "be 1 for 0" in {
    val target = Number.zero
    target.exp shouldBe one
  }
  it should "be e^2 for 2" in {
    val target = Number.two
    target.exp should ===(Expression(Constants.e) * Constants.e)
  }

  behavior of "log"
  it should "be 1 for E" in {
    val target = Number.e
    target.log shouldBe one
  }
  it should "be 0 for 1" in {
    val target = Number.one
    val log = target.log
    log shouldBe zero
  }
  it should "be 2 for E^2" in {
    val target: Number = Expression(Constants.e) * Constants.e
    target.log should ===(Number.two)
  }

  behavior of "toInt"
  it should "work for 1" in {
    val target = Number.one
    target.toInt shouldBe Some(1)
  }
  it should "work for BigInt 1" in {
    val target = Number(bigOne)
    target.toInt shouldBe Some(1)
  }
  it should "work for Rational 1" in {
    val target = Number(ratOne)
    target.toInt shouldBe Some(1)
  }
  it should "work for too big" in {
    val target = Number(bigBigInt)
    val xo: Option[Int] = target.toInt
    xo shouldBe None
  }
  it should "work for 3.14..." in {
    val target = Number(3.1415927)
    target.toInt shouldBe None
  }
  it should "work for pi" in {
    val target = Number(1, Radian)
    target.toInt shouldBe Some(1)
  }

  // NOTE: Following are tests of Ordering[Number]
  private val no: Ordering[Number] = NumberIsOrdering

  behavior of "compare"
  it should "work for 1, 1" in {
    val x = Number.one
    val y = Number.one
    no.compare(x, y) shouldBe 0
  }
  it should "work for 1, 2" in {
    val x = Number.one
    val y = Number(2)
    no.compare(x, y) shouldBe -1
  }
  it should "work for 2, 1" in {
    val x = Number(2)
    val y = Number.one
    no.compare(x, y) shouldBe 1
  }
  it should "work for BigInt 1, 1" in {
    val x = Number(bigOne)
    val y = Number(bigOne)
    no.compare(x, y) shouldBe 0
  }
  it should "work for BigInt 1, 2" in {
    val x = Number(bigOne)
    val y = Number(BigInt(2))
    no.compare(x, y) shouldBe -1
  }
  it should "work for BigInt 2, 1" in {
    val x = Number(BigInt(2))
    val y = Number(bigOne)
    no.compare(x, y) shouldBe 1
  }
  it should "work for Rational 1, 1" in {
    val x = Number(ratOne)
    val y = Number(ratOne)
    no.compare(x, y) shouldBe 0
  }
  it should "work for Rational 1, 2" in {
    val x = Number(ratOne)
    val y = Number(Rational.two)
    no.compare(x, y) shouldBe -1
  }
  it should "work for Rational 2, 1" in {
    val x = Number(Rational.two)
    val y = Number(ratOne)
    no.compare(x, y) shouldBe 1
  }
  it should "work for Double 1, 1" in {
    val x = Number(doubleOne)
    val y = Number(doubleOne)
    no.compare(x, y) shouldBe 0
  }
  it should "work for Double 1, 2" in {
    val x = Number(doubleOne)
    val y = Number(2.0)
    no.compare(x, y) shouldBe -1
  }
  it should "work for Double 2, 1" in {
    val x = Number(2.0)
    val y = Number(doubleOne)
    no.compare(x, y) shouldBe 1
  }

  // NOTE: Following are tests of Numeric[Number]
  private val nn: Numeric[Number] = implicitly[Numeric[Number]]

  behavior of "Numeric toInt"
  it should "work for 1" in {
    val target = Number.one
    nn.toInt(target) shouldBe 1
  }
  it should "work for BigInt 1" in {
    val target = Number(bigOne)
    nn.toInt(target) shouldBe 1
  }
  it should "work for Rational 1" in {
    val target = Number(ratOne)
    nn.toInt(target) shouldBe 1
  }
  it should "work for 1.0" in {
    val target = Number(doubleOne)
    nn.toInt(target) shouldBe 1
  }

  behavior of "Numeric toLong"
  it should "work for 1" in {
    val target = Number.one
    nn.toLong(target) shouldBe 1L
  }
  it should "work for BigInt 1" in {
    val target = Number(bigOne)
    nn.toLong(target) shouldBe 1L
  }
  it should "work for Rational 1" in {
    val target = Number(ratOne)
    nn.toLong(target) shouldBe 1L
  }
  it should "work for 1.0" in {
    val target = Number(doubleOne)
    nn.toLong(target) shouldBe 1L
  }

  behavior of "Numeric toDouble"
  it should "work for 1" in {
    val target = Number.one
    nn.toDouble(target) shouldBe 1.0
  }
  it should "work for BigInt 1" in {
    val target = Number(bigOne)
    nn.toDouble(target) shouldBe 1.0
  }
  it should "work for Rational 1" in {
    val target = Number(ratOne)
    nn.toDouble(target) shouldBe 1.0
  }
  it should "work for 1.0" in {
    val target = Number(doubleOne)
    nn.toDouble(target) shouldBe 1.0
  }

  behavior of "Numeric toFloat"
  it should "work for 1" in {
    val target = Number.one
    nn.toFloat(target) shouldBe 1.0f
  }
  it should "work for BigInt 1" in {
    val target = Number(bigOne)
    nn.toFloat(target) shouldBe 1.0f
  }
  it should "work for Rational 1" in {
    val target = Number(ratOne)
    nn.toFloat(target) shouldBe 1.0f
  }
  it should "work for 1.0" in {
    val target = Number(doubleOne)
    nn.toFloat(target) shouldBe 1.0f
  }

  behavior of "Numeric plus"
  it should "work for 1" in {
    val target = Number.one
    nn.plus(target, Number.two) shouldBe Number(3)
  }
  it should "work for BigInt 1" in {
    val target = Number(bigOne)
    nn.plus(target, Number.one) shouldBe Number.two
  }
  it should "work for Rational 1" in {
    val target = Number(ratOne)
    nn.plus(target, Number.one) shouldBe Number.two
  }
  it should "work for 1.0" in {
    val target = Number(doubleOne)
    nn.plus(target, Number.one) shouldBe Number.two
  }

  behavior of "Numeric minus"
  it should "work for 1" in {
    val target = Number.one
    nn.minus(target, Number.two) shouldBe Number.negOne
  }
  it should "work for BigInt 1" in {
    val target = Number(bigOne)
    nn.minus(target, Number.one) shouldBe Number.zero
  }
  it should "work for Rational 1" in {
    val target = Number(ratOne)
    nn.minus(target, Number.one) shouldBe Number.zero
  }
  it should "work for 1.0" in {
    val target = Number(doubleOne)
    nn.minus(target, Number.one) shouldBe Number.zero
  }

  behavior of "Numeric fromInt"
  it should "work for -1" in {
    nn.fromInt(-1) shouldBe Number.negOne
  }
  it should "work for 0" in {
    nn.fromInt(0) shouldBe Number.zero
  }
  it should "work for 1" in {
    nn.fromInt(1) shouldBe Number.one
  }

  behavior of "Numeric parseString"
  it should "work for -1" in {
    nn.parseString("-1") shouldBe Some(Number.negOne)
  }
  it should "work for 0" in {
    nn.parseString("0") shouldBe Some(Number.zero)
  }
  it should "work for 1" in {
    nn.parseString("1") shouldBe Some(Number.one)
  }
  it should "work for 6.67430(15)E-11" in {
    nn.parseString("6.67430(15)E-11") shouldBe Constants.G.asNumber
  }

  // NOTE: Following are tests of Fractional[Number]
  private val nf: Fractional[Number] = implicitly[Fractional[Number]]

  behavior of "Numeric div"
  it should "work for 1/2" in {
    val target = Number.one
    nf.div(target, Number.two) shouldBe Number.half
  }
  it should "work for 2/2" in {
    val target = Number.two
    nf.div(target, Number.two) shouldBe Number.one
  }
  it should "work for 2/3" in {
    val target = Number.two
    import Rational.RationalOps
    nf.div(target, Number(3)) shouldBe Number(2 :/ 3)
  }

  behavior of "NumberOps"

  import com.phasmidsoftware.number.core.Number.NumberOps

  it should "work for 2 + Number(3)" in {
    val x: Number = 2 + Number(3)
    x shouldBe Number(5)
  }
  it should "work for 2 * Number(3)" in {
    val x: Number = 2 * Number(3)
    x shouldBe Number(6)
  }
  it should "work for 1 :/ 2" in {
    val x: Number = 1 :/ 2
    x shouldBe Number(Rational.half)
  }

  behavior of "isImaginary"
  it should "be true for i and its multiples" in {
    Number.i.isImaginary shouldBe true
    Number.i.multiply(Constants.two).asNumber map (_.isImaginary) shouldBe Some(true)
  }
  it should "be false for all ordinary numbers" in {
    Number.one.isImaginary shouldBe false
    Number.pi.isImaginary shouldBe false
    Constants.G.isImaginary shouldBe false
    Number.root2.isImaginary shouldBe false
  }

  behavior of "multiply"
  it should "work for pure numbers" in {
    Number.root2 multiply Constants.root2 shouldBe Constants.two
    Number.two multiply Constants.two shouldBe Real(4)
  }
  it should "work for complex numbers" in {
    (Number.two multiply ComplexCartesian(2, 3)).isSame(ComplexCartesian(4, 6)) shouldBe true
    ComplexCartesian(2, 3) multiply Constants.two shouldBe ComplexCartesian(4, 6)
  }
  it should "work for i" in {
    (Number.two multiply Constants.i).isSame(ComplexCartesian(0, 2)) shouldBe true
    (ComplexCartesian(2, 3) multiply Constants.i).isSame(ComplexCartesian(-3, 2)) shouldBe true
  }
  it should "multiply root2 and 3" in {
    root2 doMultiply Number(3) shouldBe ExactNumber(Value.fromInt(18), SquareRoot)
  }
  it should "multiply cube-root2 and 3" in {
    Number(2, CubeRoot) doMultiply Number(3) shouldBe ExactNumber(Value.fromInt(54), CubeRoot)
  }
  it should "multiply 3 and root2" in {
    Number(3) doMultiply root2 shouldBe ExactNumber(Value.fromInt(18), SquareRoot)
  }
  it should "multiply 3 and cube-root2" in {
    Number(3) doMultiply Number(2, CubeRoot) shouldBe ExactNumber(Value.fromInt(54), CubeRoot)
  }

  behavior of "field operations"
  it should "work with asReal" in {
    val target = Number.one
    target.asReal shouldBe Some(Real(1))
  }
}
