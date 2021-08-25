package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Constants.{sAvagadro, sBoltzmann, sC, sPlanck}
import com.phasmidsoftware.number.core.Expression.{ExpressionOps, one}
import com.phasmidsoftware.number.core.Field.convertToNumber
import com.phasmidsoftware.number.core.Number.{negate, root2}
import com.phasmidsoftware.number.core.Rational.RationalHelper
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

  private val numberOne = Number(1)
  private val bigOne = BigInt(1)
  private val ratOne = Rational.one
  private val doubleOne = 1.0
  private val bigBigInt = BigInt(2147483648L)
  private val standardFuzz = AbsoluteFuzz[Double](1E-7, Gaussian)

  behavior of "create"
  it should "yield Right(1)" in {
    val target = Number.create(Right(1))
    target should matchPattern { case ExactNumber(_, _) => }
    target.value should matchPattern { case Right(_) => }
  }
  it should "yield Left(Right(bigBigInt))" in {
    val target = Number.create(Left(Right(bigBigInt)))
    target should matchPattern { case ExactNumber(_, _) => }
    target.value shouldBe Left(Right(Rational(bigBigInt)))
  }
  it should "yield Right(1, Fuzz)" in {
    val target = Number.create(Right(1), standardFuzz)
    target should matchPattern { case FuzzyNumber(_, _, _) => }
    target.value should matchPattern { case Right(_) => }
  }
  it should "yield Right(1, Radian)" in {
    val target = Number.pi
    target should matchPattern { case ExactNumber(_, _) => }
    target.value should matchPattern { case Right(_) => }
    target.factor shouldBe Radian
  }
  it should "yield Right(1, Radian, Fuzz)" in {
    val target = Number.create(Right(1), Radian, Some(standardFuzz))
    target should matchPattern { case FuzzyNumber(_, _, _) => }
    target.value should matchPattern { case Right(_) => }
    target.factor shouldBe Radian
  }

  behavior of "value"
  it should "yield Right(1)" in {
    val target = Number.create(Right(1))
    target.value shouldBe Right(1)
  }
  it should "yield Left(Right(bigBigInt))" in {
    val target = Number.create(Left(Right(bigBigInt)))
    target.value shouldBe Left(Right(Rational(bigBigInt)))
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
  it should "work for Radian" in {
    Number.pi.toString shouldBe "1\uD835\uDED1"
  }
  it should "work for Radian as scalar" in {
    val target = Number.pi.scale(Scalar)
    // NOTE that:       Pi is 3.1415926535897932384626433...
    // NOTE that:  math.PI is 3.14159265358979323846
    // In IEEE 754 binary, pi is 400921fb54442d18, which is:
    //                        3.141592653589793
    //    target.toString shouldBe "3.14159265358979300(41)" // TODO this is how it should be (not any more)
    target.toString shouldBe "3.141592653589793[5]"
  }
  it should "work for E" in {
    Number.e.toString shouldBe "\uD835\uDF00"
  }
  it should "work for E as scalar" in {
    val target = Number.e.scale(Scalar)
    target.toString shouldBe "2.7182818284590450[85]"
  }
  it should "work for E^2 as Real" in {
    val target = Number("2\uD835\uDF00")
    target.scale(Scalar).toString shouldBe "7.389056098930650[59]"
  }
  it should "work for 1 scaled as Radian" in {
    numberOne.scale(Radian).toString shouldBe "0.3183098861837907[5]\uD835\uDED1"
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

  behavior of "toDouble"
  it should "yield 1" in {
    val target = Number.create(Right(1))
    target.toDouble shouldBe Some(doubleOne)
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
    xy.get shouldBe FuzzyNumber(Left(Right(Rational(31415927, 10000000))), Scalar, Some(AbsoluteFuzz(0.00000005, Box)))
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
    target.value shouldBe Right(1)
  }
  it should """work for "2147483648"""" in {
    val target = Number("2147483648")
    target.isExact(None) shouldBe true
      target.value shouldBe Left(Right(Rational(bigBigInt)))
  }
  it should """work for "3.1415927"""" in {
    val target = Number("3.1415927")
      target.isExact(None) shouldBe false
      target.value shouldBe Left(Right(Rational(31415927, 10000000)))
  }
  it should "work for 3.1416" in {
    val target = Number(3.1416)
      target.isExact(None) shouldBe false
      target shouldEqual Number(Rational(3927, 1250))
  }
  it should """work for "\uD835\uDED1""""" in {
    val target = Number("\uD835\uDED1")
      target.isExact(None) shouldBe true
    target.value shouldBe Right(1)
    target.factor shouldBe Radian
  }
  it should "work for 1" in {
    val target = numberOne
      target.isExact(None) shouldBe true
      target.value shouldBe Right(1)
  }
  it should "work for bigBigInt" in {
    val target = Number(bigBigInt)
      target.isExact(None) shouldBe true
      target.value shouldBe Left(Right(Rational(bigBigInt)))
  }
  it should "work for Rational(1,2)" in {
    val target = Number(Rational(1, 2))
      target.isExact(None) shouldBe true
      target.value shouldBe Left(Right(Rational(1, 2)))
  }
  it should "work for math.pi" in {
    val target = Number(Math.PI)
      target.isExact(None) shouldBe false
      target shouldEqual Number(Rational(3141592653589793L, 1000000000000000L))
  }
  it should "work for nothing" in {
    val target = Number()
    target.value shouldBe Left(Left(None))
  }
  it should "work for BigDecimal(3.1415927)" in {
    val target = Number(BigDecimal(3.1415927))
      target.isExact(None) shouldBe true
      target.value shouldBe Left(Right(Rational(31415927, 10000000)))
  }
  it should "work for 3.1415927" in {
    val target = Number(3.1415927)
      target.isExact(None) shouldBe false
      target shouldEqual Number(Rational(31415927, 10000000))
  }
  it should """work for 3.1415926535897932384626433""" in {
    val target = Number(3.1415926535897932384626433)
      target.isExact(None) shouldBe false
      target shouldEqual Number(Rational(3141592653589793L, 1000000000000000L))
  }
  it should "work for 0.5" in {
    val target = Number(0.5)
      target.isExact(None) shouldBe true
      target.value shouldBe Left(Right(Rational(1, 2)))
  }
  it should "work for 1.23" in {
    val target = Number(1.23)
      target.isExact(None) shouldBe true
      target.value shouldBe Left(Right(Rational(123, 100)))
  }
  it should "work for 1.234" in {
    val target = Number(1.234)
      target.isExact(None) shouldBe false
      target.value shouldBe Left(Left(Some(1.234)))
    target.toString shouldBe "1.234[5]"
  }
  it should "work for 1.23400" in {
    val target = Number(1.23400)
      target.isExact(None) shouldBe false
      target.value shouldBe Left(Left(Some(1.234)))
    target.toString shouldBe "1.234[5]"
  }
  it should "support exact strings" in {
    val target = Number("3.141592700")
      target.isExact(None) shouldBe true
      target should matchPattern { case ExactNumber(_, _) => }
    target.value shouldBe Left(Right(Rational(31415927, 10000000)))
  }

  behavior of "Number.parse" // CONSIDER Moving these into NumberParserSpec
  it should "parse boltzmann" in {
    val zy = Number.parse(sBoltzmann)
      zy should matchPattern { case Success(_) => }
      zy.get.isExact(None) shouldBe true
      zy.get.toDouble shouldBe Some(1.380649E-23)
  }
  it should "fail to parse boltzmann with alternative minus" in {
    val zy = Number.parse("1.380649E−23")
    zy should matchPattern { case Failure(_) => }
  }
  it should "parse planck" in {
    val z = Number.parse(sPlanck)
      z should matchPattern { case Success(_) => }
      z.get.isExact(None) shouldBe true
  }
  it should "parse c" in {
    val z = Number.parse(sC)
      z should matchPattern { case Success(_) => }
      z.get.isExact(None) shouldBe true
  }
  it should "parse avagadro" in {
    val z = Number.parse(sAvagadro)
      z should matchPattern { case Success(_) => }
      z.get.isExact(None) shouldBe true
  }

  behavior of "FuzzOps"
  it should "get mu" in {
    import Number.FuzzOps
    val x = 1836.15267343 ~ 11
      x.isExact(None) shouldBe false
      x.toString shouldBe "1836.15267343(11)"
    x shouldEqual Constants.mu
  }
  it should "get G" in {
    import Number.FuzzOps
    val x = 6.67430E-11 ~ 15
      x.isExact(None) shouldBe false
      x shouldEqual Constants.G
    // FIXME Issue #54
    //    x.toString shouldBe "6.67430(15)E-11"
  }

  behavior of "specialize"
  it should "work for 1" in {
    val target = numberOne
    target.specialize.value shouldBe Right(1)
  }
  it should "work for BigInt 1" in {
    val target = Number.create(Left(Right(Rational(bigOne))), Scalar)
    target.specialize.value shouldBe Right(1)
  }
  it should "work for BigInt(1+Int.MaxValue)" in {
    val bigInt = BigInt(1L + Int.MaxValue)
    val target = Number(bigInt)
    target.specialize.value shouldBe Left(Right(Rational(bigInt)))
  }
  it should "work for Rational(1)" in {
    val target = Number.create(Left(Right(ratOne)), Scalar)
    target.specialize.value shouldBe Right(1)
  }
  it should "work for Rational.half" in {
    val target = Number(Rational.half)
    target.specialize.value shouldBe Left(Right(Rational(1, 2)))
  }
  it should "work for 0.5" in {
    val target = Number(0.5)
    target.specialize.value shouldBe Left(Right(Rational(1, 2)))
  }
  it should "work for 1.0" in {
    val target = Number(doubleOne)
    target.specialize.value shouldBe Right(1)
  }
  it should "work for nothing" in {
    val target = Number()
    target.specialize.value shouldBe Left(Left(None))
  }

  behavior of "scale"
  it should "work for Scalar, Scalar" in {
    val target = numberOne
    target.scale(Scalar) shouldBe numberOne
  }
  it should "work for Radian, Radian" in {
    val target = Number(1, Radian)
    target.scale(Radian) shouldBe target
  }
  it should "work for Scalar, Radian" in {
    val target = numberOne
    target.scale(Radian) should ===(Number(1 / Math.PI, Radian))
  }
  it should "work for Radian, Scalar" in {
    val target = Number(1, Radian)
    target.scale(Scalar) should ===(Number(Math.PI))
  }
  it should "work for Scalar, NatLog" in {
    val target = numberOne
    val result = target.scale(NatLog)
    // NOTE that the simplify method brings this back to being just one.
    result shouldBe numberOne
  }
  it should "work for NatLog, Scalar" in {
    val target = Number(1, NatLog)
    target.scale(Scalar) should ===(Number(Math.E))
  }
  it should "work for 2E, Scalar" in {
    val target = Number(2, NatLog)
    val actual: Number = target.scale(Scalar)
    val expected: Number = convertToNumber(Number(Math.E) doPower 2)
    actual should ===(expected)
  }
  it should "work for 2E, Scalar but comparing against NatLog * NatLog" in {
    val target = Number(2, NatLog)
    val actual: Number = target.scale(Scalar)
    val expected: Number = convertToNumber(Number(Math.E) doMultiply Number(Math.E))
    actual should ===(expected)
  }
  it should "work for Scalar, 2E (same as before but with parameters to === reversed" in {
    val target = Number(2, NatLog)
    val actual: Number = target.scale(Scalar)
    val expected: Number = convertToNumber(Number(Math.E) doPower 2)
    expected should ===(actual)
  }
  it should "work for Scalar, 2E (same as before but using NatLog * NatLog and parameters to === reversed" in {
    val target = Number(2, NatLog)
    val actual: Number = target.scale(Scalar)
    val expected: Number = convertToNumber(Number(Math.E) doMultiply Number(Math.E))
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
  it should "work for Log2, Scalar" in {
    val target = Number(Rational.half, Log2)
    target.render shouldBe "√2"
    val expected = Number(math.sqrt(2), Scalar)
    val result = target.scale(Scalar)
    result should ===(expected)
  }
  it should "work for Log10, Scalar" in {
    val target = Number(Rational.half, Log10)
    target.render shouldBe "√10"
    val expected = Number(math.sqrt(10), Scalar)
    val result = target.scale(Scalar)
    result should ===(expected)
  }
  it should "work for Log2, NatLog" in {
    import com.phasmidsoftware.number.core.Rational.RationalOps
    val target = Number(1 :/ 2, Log2)
    target.render shouldBe "√2"
    val expected = Number(math.log(math.sqrt(2)), NatLog)
    val result = target.scale(NatLog)
    result should ===(expected)
  }
  it should "work for Root2, Scalar" in {
    val target = Number(Rational.two, Root2)
    target.render shouldBe "√2"
    val expected = Number(math.sqrt(2), Scalar)
    val result = target.scale(Scalar)
    result should ===(expected)
  }
  it should "work for Root3, Scalar" in {
    val target = Number(Rational.two, Root3)
    target.render shouldBe "³√2"
    val expected = Number(math.pow(2, 1.0 / 3), Scalar)
    val result = target.scale(Scalar)
    result should ===(expected)
  }
  it should "work for Root2, Root3" in {
    val target = Number(4, Root2)
    target.render shouldBe "√4"
    val expected = Number(8, Root3)
    val result = target.scale(Root3)
    result shouldBe expected
  }
  it should "work for Scalar, Root2" in {
    val target = Number(3)
    val expected = Number(3)
    val result = target.scale(Root2)
    result shouldBe expected
    result.toString shouldBe "3"
  }
  it should "work for NatLog, Root2" in {
    val target = Number.e
    val expected = Number(math.E * math.E, Root2)
    val result: Number = target.scale(Root2)
    result.render shouldBe "√7.38905609893064[2]"
    result should ===(expected)
  }

  behavior of "alignFactors"
  it should "work for Scalar, Scalar" in {
    val target = numberOne
    target.asInstanceOf[GeneralNumber].alignFactors(Number(2)) shouldBe(numberOne, Number(2))
  }
  it should "work for Scalar, Radian" in {
    val target = numberOne
    val (p, q) = target.asInstanceOf[GeneralNumber].alignFactors(Number(2, Radian))
    p shouldBe numberOne
    q shouldEqual Number(2 * Math.PI)
  }
  it should "work for Radian, Scalar" in {
    val target = Number(2, Radian)
    val (f, x) = target.asInstanceOf[GeneralNumber].alignFactors(numberOne)
    f shouldEqual Number(2 * Math.PI)
    x shouldBe numberOne
  }
  it should "work for Radian, Radian" in {
    val target = Number(1, Radian)
    target.asInstanceOf[GeneralNumber].alignFactors(Number(2, Radian)) shouldBe(Number(1, Radian), Number(2, Radian))
  }

  behavior of "alignTypes"
  it should "work for Int,Int" in {
    val target = numberOne
    target.asInstanceOf[GeneralNumber].alignTypes(Number(2).asInstanceOf[GeneralNumber]) shouldBe(numberOne, Number(2))
  }
  it should "work for Int Int(Radian)" in {
    val target: GeneralNumber = numberOne.asInstanceOf[GeneralNumber]
    target.alignTypes(Number(1, Radian)) shouldBe(numberOne, Number(1, Radian))
  }
  it should "work for Int,BigInt" in {
    val target = numberOne.asInstanceOf[GeneralNumber]
    target.alignTypes(Number(bigBigInt)) shouldBe(Number(bigBigInt), Number(bigOne))
  }
  it should "work for BigInt,Int" in {
    val target = Number(bigOne).asInstanceOf[GeneralNumber]
    target.alignTypes(Number(2)) shouldBe(Number(bigOne), Number(BigInt(2)))
  }
  it should "work for Int,Rational" in {
    val target = numberOne.asInstanceOf[GeneralNumber]
    target.alignTypes(Number(Rational(2, 3))) shouldBe(Number(Rational(2, 3)), Number(Rational(1)))
  }
  it should "work for Rational,Int" in {
    val target = Number(Rational(1)).asInstanceOf[GeneralNumber]
    target.alignTypes(Number(2)) shouldBe(Number(Rational(1)), Number(Rational(2)))
  }
  it should "work for Int,Double" in {
    val target = numberOne.asInstanceOf[GeneralNumber]
    val (p, q) = target.alignTypes(Number(Math.PI))
    p should ===(Number(Math.PI))
    q should ===(Number(doubleOne))
  }
  it should "work for Double,Int" in {
    val target = Number(doubleOne).asInstanceOf[GeneralNumber]
    target.alignTypes(Number(2)) shouldBe(Number(doubleOne), Number(2.0))
  }
  it should "work for Int,None" in {
    val target = numberOne.asInstanceOf[GeneralNumber]
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
    val y = Number(2)
    (x add y) shouldBe Number(3)
  }
  it should "add BigInt 1 and 2" in {
    val x = Number(bigOne)
    val y = Number(2)
    (x add y) shouldBe Number(3)
  }
  it should "add Rational 1 and 2" in {
    val x = Number(ratOne)
    val y = Number(2)
    (x add y) shouldBe Number(3)
  }
  it should "add Double 1 and 2" in {
    val x = Number(doubleOne)
    val y = Number(2)
    (x add y) shouldBe Number(3)
  }
  it should "add Double 1 and Radian" in {
    val x = Number(doubleOne)
    val y = Number(1, Radian)
    convertToNumber(x add y) should ===(Number(Math.PI + 1))
  }
  it should "add Radian and 2Pi" in {
    val x = Number(1, Radian)
    val y = Number(2, Radian)
    convertToNumber(x add y) shouldBe Number(3, Radian)
  }
  it should "add 1 to pi" in {
    val x1 = Number.one
    val x2 = Number.pi
    (x1 add x2).toString shouldBe "4.1415926535897930(41)"
  }
  it should "add 1 to e" in {
    val x1 = Number.one
    val x2 = Number.e
    (x1 add x2) should ===(Number(3.7182818284590450))
  }
  it should "add 1 to √2" in {
    val x1 = Number.one
    val x2 = Number.root2
    (x1 add x2) should ===(Number(2.414213562373095))
  }
  it should "add 1 to √3" in {
    val x1 = Number.one
    val x2 = Number.root3
    (x1 add x2) should ===(Number(2.732050807568878))
  }
  it should "add 1 to √5" in {
    val x1 = Number.one
    val x2 = Number.root5
    (x1 add x2) should ===(Number(3.23606797749979))
  }

  behavior of "minus"
  it should "negate 1" in {
    val x = numberOne
    -x shouldBe Number(-1)
  }
  it should "negate BigInt 1" in {
    val x = Number(bigOne)
    -x shouldBe Number(BigInt(-1))
  }
  it should "negate Rational 1" in {
    val x = Number(ratOne)
    -x shouldBe Number(Rational(-1))
  }
  it should "negate Double 1" in {
    val x = Number(doubleOne)
    -x shouldBe Number(-doubleOne)
  }

  behavior of "subtract"
  it should "subtract 1 from 2" in {
    val x = Number(2)
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
    val y = Number(2)
    (x multiply y) shouldBe Number(2)
  }
  it should "multiply BigInt 1 and 2" in {
    val x = Number(bigOne)
    val y = Number(2)
    (x multiply y) shouldBe Number(2)
  }
  it should "multiply Rational 1 and 2" in {
    val x = Number(Rational(1))
    val y = Number(2)
    (x multiply y) shouldBe Number(2)
  }
  it should "multiply Double 1 and 2" in {
    val x = Number(doubleOne)
    val y = Number(2)
    (x multiply y) shouldBe Number(2)
  }
  it should "multiply 2 and Radian" in {
    val x = Number(2)
    val y = Number(1, Radian)
    (x multiply y) shouldBe Number(2, Radian)
  }
  it should "multiply Radian and 2" in {
    val x = Number(1, Radian)
    val y = Number(2)
    (x multiply y) shouldBe Number(2, Radian)
  }
  it should "multiply root2 and root2" in {
    (root2 multiply root2).normalize shouldBe Number(2, Scalar)
  }
  it should "multiply sin by sin" in {
    val piBy4 = Number.pi doDivide 4
    val sinePiBy4 = piBy4.sin
    val oneHalf = sinePiBy4 doMultiply sinePiBy4
    oneHalf.normalize shouldBe Number.two.invert
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
    val x = Number(2)
    x.invert shouldBe Number(Rational.half)
  }
  it should "invert BigInt 2" in {
    val x = Number(BigInt(2))
    x.invert shouldBe Number(Rational.half)
  }
  it should "invert Rational 2" in {
    val x = Number(Rational.two)
    x.invert shouldBe Number(Rational.half)
  }
  it should "invert Double pi" in {
    val x = Number(Math.PI)
    x.invert should ===(Number(1 / Math.PI))
  }

  behavior of "division"
  it should "divide 1 by 2" in {
    val x = numberOne
    val y = Number(2)
    (x divide y) shouldBe Number(Rational.half)
  }
  it should "divide BigInt 1 by 2" in {
    val x = Number(bigOne)
    val y = Number(2)
    (x divide y) shouldBe Number(Rational.half)
  }
  it should "divide Rational 1 by 2" in {
    val x = Number(Rational(1))
    val y = Number(2)
    (x divide y) shouldBe Number(Rational.half)
  }
  it should "divide Double 1 by Double Radian" in {
    val x = Number(doubleOne)
    val y = Number(Math.PI)
    convertToNumber(x divide y) should ===(Number(1 / Math.PI))
  }

  // XXX what is this ** operator?
  behavior of "**"
  it should "work for 2^2" in {
    val target = Number(2)
    (target doPower 2) shouldBe Number(4)
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
  it should "work for easy Rational" in {
    Number(Rational(9, 4)).sqrt shouldBe Number(Rational(3, 2))
  }

  behavior of "sin"
  it should "be zero for pi" in {
    val target = Number.pi
    target.sin shouldBe Number(0, Scalar)
  }
  it should "work for 0" in {
    val target = Number(0, Radian)
    target.sin shouldBe Number(0, Scalar)
  }
  it should "be one for pi/2" in {
    val target = (Number.pi doDivide 2).sin
    target shouldBe Number.one
  }
  it should "work for Radian/2" in {
    val target = Number(Rational.half, Radian)
    val sin = target.sin
    sin shouldBe Number(1, Scalar)
  }
  it should "work for Radian/6" in {
    val target = Number(Rational(6).invert, Radian)
    target.sin shouldBe Number(Rational(1, 2), Scalar)
  }
  it should "work for Radian/3" in {
    val target = Number(Rational(1, 3), Radian)
    val sin = target.sin
    sin shouldBe Number(Rational(3, 4), Root2)
  }
  it should "work for Radian/4" in {
    val target = Number(Rational(1, 4), Radian)
    val sin = target.sin
    sin shouldBe Number(Rational.half, Root2)
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
    target.cos shouldBe Number(Rational(1, 2), Scalar)
  }
  it should "work for Pi/6" in {
    val target = Number.pi doDivide 6
    target.cos should ===(Number(3).sqrt doDivide 2)
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
    val target = Number.pi doDivide 3
    target.tan shouldEqual Number(3).sqrt
  }
  it should "work for Pi/6" in {
    val target = Number.pi doDivide 6
    target.tan should ===(Number(3).sqrt.invert)
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
    negate(Number(Rational(3)).sqrt).atan(Number.one) shouldEqual Number(r"7/6", Radian)
  }
  it should "be 11 Pi / 6 for -1/2" in {
    Number(Rational(3)).sqrt.atan(negate(Number.one)) shouldEqual Number(r"11/6", Radian)
  }
  it should "be 3 pi / 4 for 1/-1" in {
    val adjacent = Number.negate(Number.one)
    val opposite = Number.one
    val actual: Number = adjacent.atan(opposite)
    val expected: Number = (Number.pi doMultiply 3) doDivide 4
    // TODO revert this so that it reads actual ... expected
    //  XXX  actual should ===(expected)
    actual shouldBe expected
  }
  // TODO need to operate appropriately on negZero.
  it should "evaluate atan of 1 over -0" in {
    val number = Number.negZero.atan(Number.one)
    number shouldBe Number(Rational(3, 2), Radian)
  }
  it should "evaluate atan of -1 over 0" in {
    val number = Number.zero.atan(negate(Number.one))
    number shouldBe Number(Rational(3, 2), Radian)
  }

  // NOTE: Following are the tests of Ordering[Number]

  behavior of "compare"
  it should "work for 1, 1" in {
    val x = numberOne
    val y = numberOne
    implicitly[Numeric[Number]].compare(x, y) shouldBe 0
  }
  it should "work for 1, 2" in {
    val x = numberOne
    val y = Number(2)
    implicitly[Numeric[Number]].compare(x, y) shouldBe -1
  }
  it should "work for 2, 1" in {
    val x = Number(2)
    val y = numberOne
    implicitly[Numeric[Number]].compare(x, y) shouldBe 1
  }
  it should "work for BigInt 1, 1" in {
    val x = Number(bigOne)
    val y = Number(bigOne)
    implicitly[Numeric[Number]].compare(x, y) shouldBe 0
  }
  it should "work for BigInt 1, 2" in {
    val x = Number(bigOne)
    val y = Number(BigInt(2))
    implicitly[Numeric[Number]].compare(x, y) shouldBe -1
  }
  it should "work for BigInt 2, 1" in {
    val x = Number(BigInt(2))
    val y = Number(bigOne)
    implicitly[Numeric[Number]].compare(x, y) shouldBe 1
  }
  it should "work for Rational 1, 1" in {
    val x = Number(ratOne)
    val y = Number(ratOne)
    implicitly[Numeric[Number]].compare(x, y) shouldBe 0
  }
  it should "work for Rational 1, 2" in {
    val x = Number(ratOne)
    val y = Number(Rational.two)
    implicitly[Numeric[Number]].compare(x, y) shouldBe -1
  }
  it should "work for Rational 2, 1" in {
    val x = Number(Rational.two)
    val y = Number(ratOne)
    implicitly[Numeric[Number]].compare(x, y) shouldBe 1
  }
  it should "work for Double 1, 1" in {
    val x = Number(doubleOne)
    val y = Number(doubleOne)
    implicitly[Numeric[Number]].compare(x, y) shouldBe 0
  }
  it should "work for Double 1, 2" in {
    val x = Number(doubleOne)
    val y = Number(2.0)
    implicitly[Numeric[Number]].compare(x, y) shouldBe -1
  }
  it should "work for Double 2, 1" in {
    val x = Number(2.0)
    val y = Number(doubleOne)
    implicitly[Numeric[Number]].compare(x, y) shouldBe 1
  }

  // NOTE: Following are the tests of Numeric[Number]

  behavior of "toInt"
  it should "work for 1" in {
    val target = numberOne
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

  // XXX Following are the tests of Numeric[Number]

  behavior of "Numeric toInt"
  it should "work for 1" in {
    val target = numberOne
    implicitly[Numeric[Number]].toInt(target) shouldBe 1
  }
  it should "work for BigInt 1" in {
    val target = Number(bigOne)
    implicitly[Numeric[Number]].toInt(target) shouldBe 1
  }
  it should "work for Rational 1" in {
    val target = Number(ratOne)
    implicitly[Numeric[Number]].toInt(target) shouldBe 1
  }
  it should "work for 1.0" in {
    val target = Number(doubleOne)
    implicitly[Numeric[Number]].toInt(target) shouldBe 1
  }

  behavior of "Numeric toLong"
  it should "work for 1" in {
    val target = numberOne
    implicitly[Numeric[Number]].toLong(target) shouldBe 1L
  }
  it should "work for BigInt 1" in {
    val target = Number(bigOne)
    implicitly[Numeric[Number]].toLong(target) shouldBe 1L
  }
  it should "work for Rational 1" in {
    val target = Number(ratOne)
    implicitly[Numeric[Number]].toLong(target) shouldBe 1L
  }
  it should "work for 1.0" in {
    val target = Number(doubleOne)
    implicitly[Numeric[Number]].toLong(target) shouldBe 1L
  }

  behavior of "Numeric toDouble"
  it should "work for 1" in {
    val target = numberOne
    implicitly[Numeric[Number]].toDouble(target) shouldBe 1.0
  }
  it should "work for BigInt 1" in {
    val target = Number(bigOne)
    implicitly[Numeric[Number]].toDouble(target) shouldBe 1.0
  }
  it should "work for Rational 1" in {
    val target = Number(ratOne)
    implicitly[Numeric[Number]].toDouble(target) shouldBe 1.0
  }
  it should "work for 1.0" in {
    val target = Number(doubleOne)
    implicitly[Numeric[Number]].toDouble(target) shouldBe 1.0
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

  behavior of "pre-defined"
  it should "have root5" in {
    val root5 = Constants.root5
    println(root5)
    val value = root5.normalize
    println(value)
    value should ===(Number(math.sqrt(5)))
  }

  behavior of "constants"
  it should "have phi" in {
    val phi = Constants.phi
    val z = one + Constants.root5
    println(Constants.root5.scale(Scalar))
    println(z.asNumber)
    val goldenRatio = Expression.phi
    println(goldenRatio)
    println(Expression.em.simplifier(goldenRatio))
    val maybeNumber: Option[Number] = goldenRatio.asNumber
    maybeNumber.isDefined shouldBe true
    println(maybeNumber)
    maybeNumber.get should ===(phi)
  }
  it should "have root2" in {
    val root2: Number = Constants.root2
    val expected = Number(Rational.half, Log2)
    root2 should ===(expected)
  }
}
