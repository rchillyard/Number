package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Constants.{sAvagadro, sBoltzmann, sC, sPlanck}
import com.phasmidsoftware.number.core.Expression.ExpressionOps
import com.phasmidsoftware.number.core.Field.convertToNumber
import com.phasmidsoftware.number.core.Number.negate
import com.phasmidsoftware.number.core.Rational.RationalHelper
import org.scalactic.Equality
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Failure, Left, Success, Try}

class NumberSpec extends AnyFlatSpec with should.Matchers {

  implicit object NumberEquality extends Equality[Number] {
    def areEqual(a: Number, b: Any): Boolean = b match {
      case n: Number => a.compare(n) == 0
      case n: Expression => a.compare(n) == 0
      case _ => false
    }
  }

  implicit object FieldEquality extends Equality[Field] {
    def areEqual(a: Field, b: Any): Boolean = b match {
      case n: Field => a.compare(n) == 0
      case _ => false
    }
  }

  implicit object ExpressionEquality extends Equality[Expression] {
    def areEqual(a: Expression, b: Any): Boolean = b match {
      case n: Number => new ExpressionOps(a).compare(n) == 0
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
  it should "yield Right(1, Pi)" in {
    val target = Number.pi
    target should matchPattern { case ExactNumber(_, _) => }
    target.value should matchPattern { case Right(_) => }
    target.factor shouldBe Pi
  }
  it should "yield Right(1, Pi, Fuzz)" in {
    val target = Number.create(Right(1), Pi, Some(standardFuzz))
    target should matchPattern { case FuzzyNumber(_, _, _) => }
    target.value should matchPattern { case Right(_) => }
    target.factor shouldBe Pi
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
  it should "work for Pi" in {
    Number.pi.toString shouldBe "1\uD835\uDED1"
  }
  it should "work for Pi as scalar" in {
    val target = Number.pi.scale(Scalar)
    // NOTE that:       Pi is 3.1415926535897932384626433...
    // NOTE that:  math.PI is 3.14159265358979323846
    // In IEEE 754 binary, pi is 400921fb54442d18, which is:
    //                        3.141592653589793
    //    target.toString shouldBe "3.14159265358979300(41)" // TODO this is how it should be (not any more)
    target.toString shouldBe "3.141592653589793..."
  }
  it should "work for E" in {
    Number.e.toString shouldBe "\uD835\uDF00"
  }
  it should "work for E as scalar" in {
    val target = Number.e.scale(Scalar)
    target.toString shouldBe "2.7182818284590450[35]"
  }
  it should "work for E^2 as Real" in {
    val target = Number("2\uD835\uDF00")
    target.scale(Scalar).toString shouldBe "7.3890560989306500[95]"
  }
  it should "work for 1 scaled as Pi" in {
    numberOne.scale(Pi).toString shouldBe "0.3183098861837907...\uD835\uDED1"
    //    numberOne.scale(Pi).toString shouldBe "0.318309886183790700(42)\uD835\uDED1" // this is how it should be
  }
  it should "work for E^2" in {
    val target = Number.e ^ 2
    target.materialize.toString shouldBe "\uD835\uDF00\u00B2"
  }
  it should "work for E^3" in {
    val target = Number.e ^ 3
    target.materialize.toString shouldBe "\uD835\uDF00\u00B3"
  }
  it should "work for E^4" in {
    val target = Number.e ^ 4
    target.materialize.toString shouldBe "\uD835\uDF00\u2074"
  }
  it should "work for E^10" in {
    val target = Number.e ^ 10
    target.materialize.toString shouldBe "\uD835\uDF00^10"
  }
  it should "work for square root E" in {
    val target = Number.e.sqrt
    target.materialize.toString shouldBe "√\uD835\uDF00"
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
  it should "work for Pi" in {
    val xy: Try[Number] = Number.parse("1" + Factor.sPi)
    xy.get shouldBe Number(Pi)
  }
  it should "work for pi" in {
    val xy: Try[Number] = Number.parse("1pi")
    xy.get shouldBe Number(Pi)
  }
  it should "work for e" in {
    val xy: Try[Number] = Number.parse("1" + Factor.sE)
    xy.get shouldBe Number(1, E)
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
    target.isExact shouldBe true
    target.value shouldBe Right(1)
  }
  it should """work for "2147483648"""" in {
    val target = Number("2147483648")
    target.isExact shouldBe true
    target.value shouldBe Left(Right(Rational(bigBigInt)))
  }
  it should """work for "3.1415927"""" in {
    val target = Number("3.1415927")
    target.isExact shouldBe false
    target.value shouldBe Left(Right(Rational(31415927, 10000000)))
  }
  it should "work for 3.1416" in {
    val target = Number(3.1416)
    target.isExact shouldBe false
    target shouldEqual Number(Rational(3927, 1250))
  }
  it should """work for "\uD835\uDED1""""" in {
    val target = Number("\uD835\uDED1")
    target.isExact shouldBe true
    target.value shouldBe Right(1)
    target.factor shouldBe Pi
  }
  it should "work for 1" in {
    val target = numberOne
    target.isExact shouldBe true
    target.value shouldBe Right(1)
  }
  it should "work for bigBigInt" in {
    val target = Number(bigBigInt)
    target.isExact shouldBe true
    target.value shouldBe Left(Right(Rational(bigBigInt)))
  }
  it should "work for Rational(1,2)" in {
    val target = Number(Rational(1, 2))
    target.isExact shouldBe true
    target.value shouldBe Left(Right(Rational(1, 2)))
  }
  it should "work for math.pi" in {
    val target = Number(Math.PI)
    target.isExact shouldBe false
    target shouldEqual Number(Rational(3141592653589793L, 1000000000000000L))
  }
  it should "work for nothing" in {
    val target = Number()
    target.value shouldBe Left(Left(None))
  }
  it should "work for BigDecimal(3.1415927)" in {
    val target = Number(BigDecimal(3.1415927))
    target.isExact shouldBe true
    target.value shouldBe Left(Right(Rational(31415927, 10000000)))
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
    target.value shouldBe Left(Right(Rational(1, 2)))
  }
  it should "work for 1.23" in {
    val target = Number(1.23)
    target.isExact shouldBe true
    target.value shouldBe Left(Right(Rational(123, 100)))
  }
  it should "work for 1.234" in {
    val target = Number(1.234)
    target.isExact shouldBe false
    target.value shouldBe Left(Left(Some(1.234)))
    target.toString shouldBe "1.234[5]"
  }
  it should "work for 1.23400" in {
    val target = Number(1.23400)
    target.isExact shouldBe false
    target.value shouldBe Left(Left(Some(1.234)))
    target.toString shouldBe "1.234[5]"
  }
  it should "support exact strings" in {
    val target = Number("3.141592700")
    target.isExact shouldBe true
    target should matchPattern { case ExactNumber(_, _) => }
    target.value shouldBe Left(Right(Rational(31415927, 10000000)))
  }

  behavior of "Number.parse" // CONSIDER Moving these into NumberParserSpec
  it should "parse boltzmann" in {
    val zy = Number.parse(sBoltzmann)
    zy should matchPattern { case Success(_) => }
    zy.get.isExact shouldBe true
    zy.get.toDouble shouldBe Some(1.380649E-23)
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
    val z = Number.parse(sC)
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
    x shouldEqual Constants.mu
  }
  it should "get G" in {
    import Number.FuzzOps
    val x = 6.67430E-11 ~ 15
    x.isExact shouldBe false
    x shouldEqual Constants.G
    // FIXME Issue #54
    //    x.toString shouldBe "6.67430(15)E-11"
  }


  behavior of "normalize"
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
  it should "work for Pi, Pi" in {
    val target = Number(1, Pi)
    target.scale(Pi) shouldBe target
  }
  it should "work for Scalar, Pi" in {
    val target = numberOne
    target.scale(Pi) should ===(Number(1 / Math.PI, Pi))
  }
  it should "work for Pi, Scalar" in {
    val target = Number(1, Pi)
    target.scale(Scalar) should ===(Number(Math.PI))
  }
  it should "work for Scalar, E" in {
    val target = numberOne
    target.scale(E) should ===(Number(math.log(1), E))
  }
  it should "work for E, Scalar" in {
    val target = Number(1, E)
    target.scale(Scalar) should ===(Number(Math.E))
  }
  it should "work for 2E, Scalar" in {
    val target = Number(2, E)
    val actual: Number = target.scale(Scalar)
    val expected: Number = convertToNumber((Number(Math.E) ^ 2).materialize)
    actual should ===(expected)
  }
  it should "work for 2E, Scalar but comparing against E * E" in {
    val target = Number(2, E)
    val actual: Number = target.scale(Scalar)
    val expected: Number = convertToNumber((Number(Math.E) * Number(Math.E)).materialize)
    actual should ===(expected)
  }
  it should "work for Scalar, 2E (same as before but with parameters to === reversed" in {
    val target = Number(2, E)
    val actual: Number = target.scale(Scalar)
    val expected: Number = convertToNumber((Number(Math.E) ^ 2).materialize)
    expected should ===(actual)
  }
  it should "work for Scalar, 2E (same as before but using E * E and parameters to === reversed" in {
    val target = Number(2, E)
    val actual: Number = target.scale(Scalar)
    val expected: Number = convertToNumber((Number(Math.E) * Number(Math.E)).materialize)
    expected should ===(actual)
  }
  it should "work for E, Pi" in {
    val target = Number(1, E)
    val expected = Number(Math.E / Math.PI, Pi)
    target.scale(Pi) should ===(expected)
  }
  it should "not work for Pi, E (because E numbers are not linear)" in {
    val target = Number(1, Pi)
    val expected = Number(Math.PI / Math.E, E)
    val result = target.scale(E)
    result === expected shouldBe false
  }

  behavior of "alignFactors"
  it should "work for Scalar, Scalar" in {
    val target = numberOne
    target.asInstanceOf[GeneralNumber].alignFactors(Number(2)) shouldBe(numberOne, Number(2))
  }
  it should "work for Scalar, Pi" in {
    val target = numberOne
    val (p, q) = target.asInstanceOf[GeneralNumber].alignFactors(Number(2, Pi))
    p shouldBe numberOne
    q shouldEqual Number(2 * Math.PI)
  }
  it should "work for Pi, Scalar" in {
    val target = Number(2, Pi)
    val (f, x) = target.asInstanceOf[GeneralNumber].alignFactors(numberOne)
    f shouldEqual Number(2 * Math.PI)
    x shouldBe numberOne
  }
  it should "work for Pi, Pi" in {
    val target = Number(1, Pi)
    target.asInstanceOf[GeneralNumber].alignFactors(Number(2, Pi)) shouldBe(Number(1, Pi), Number(2, Pi))
  }

  behavior of "alignTypes"
  it should "work for Int,Int" in {
    val target = numberOne
    target.asInstanceOf[GeneralNumber].alignTypes(Number(2).asInstanceOf[GeneralNumber]) shouldBe(numberOne, Number(2))
  }
  it should "work for Int Int(Pi)" in {
    val target: GeneralNumber = numberOne.asInstanceOf[GeneralNumber]
    target.alignTypes(Number(1, Pi)) shouldBe(numberOne, Number(1, Pi))
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
  it should "add Double 1 and Pi" in {
    val x = Number(doubleOne)
    val y = Number(1, Pi)
    convertToNumber(x add y) should ===(Number(Math.PI + 1))
  }
  it should "add Pi and 2Pi" in {
    val x = Number(1, Pi)
    val y = Number(2, Pi)
    convertToNumber(x add y) shouldBe Number(3, Pi)
  }
  it should "add 1 to pi" in {
    val x1 = Number.one
    val x2 = Number.pi
    (x1 add x2).toString shouldBe "4.141592653589793[5]"
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

  behavior of "times"
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
  it should "multiply 2 and Pi" in {
    val x = Number(2)
    val y = Number(1, Pi)
    (x multiply y) shouldBe Number(2, Pi)
  }
  it should "multiply Pi and 2" in {
    val x = Number(1, Pi)
    val y = Number(2)
    (x multiply y) shouldBe Number(2, Pi)
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
  it should "divide Double 1 by Double Pi" in {
    val x = Number(doubleOne)
    val y = Number(Math.PI)
    convertToNumber(x divide y) should ===(Number(1 / Math.PI))
  }

  // XXX what is this ** operator?
  behavior of "**"
  it should "work for 2^2" in {
    val target = Number(2)
    (target ^ 2).materialize shouldBe Number(4)
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
    val target = Number(0, Pi)
    target.sin shouldBe Number(0, Scalar)
  }
  it should "be one for pi/2" in {
    val target = (Number.pi / 2).sin
    target.materialize shouldBe Number.one
  }
  it should "work for Pi/2" in {
    val target = Number(Rational.half, Pi)
    val sin = target.sin
    sin shouldBe Number(1, Scalar)
  }
  it should "work for Pi/6" in {
    val target = Number(Rational(6).invert, Pi)
    target.sin shouldBe Number(Rational(1, 2), Scalar)
  }
  it should "work for Pi/3" in {
    val target = Number(Rational(1, 3), Pi)
    val sin = target.sin
    sin should ===(Number(3).sqrt / 2)
  }

  behavior of "cos"
  it should "be zero for pi" in {
    val target = Number.pi
    target.cos === Number(-1)
  }
  it should "work for 0" in {
    val target = Number(0, Pi)
    target.cos shouldBe Number(1)
  }
  it should "work for Pi/2" in {
    val target = Number(Rational.half, Pi)
    target.cos.isZero shouldBe true
  }
  it should "work for Pi/3" in {
    val target = Number.pi / 3
    target.cos.materialize shouldBe Number(Rational(1, 2), Scalar)
  }
  it should "work for Pi/6" in {
    val target: Expression = Number.pi / 6
    target.cos should ===(Number(3).sqrt / 2)
  }

  behavior of "tan"
  it should "be zero for 0" in {
    val target: Expression = Number(0, Pi)
    target.tan.materialize.isZero shouldBe true
  }
  it should "be zero for pi" in {
    val target: Expression = Number.pi
    target.tan.materialize.isZero shouldBe true
  }
  it should "work for Pi/2" in {
    val target: Expression = Number(Rational.half, Pi)
    target.tan.materialize.isInfinite shouldBe true
  }
  it should "work for Pi/3" in {
    val target: Expression = Number.pi / 3
    target.tan shouldEqual Number(3).sqrt
  }
  it should "work for Pi/6" in {
    val target = Number.pi / 6
    target.tan should ===(Number(3).sqrt.invert)
  }

  behavior of "atan"
  it should "be 0Pi for 0/1" in {
    val target = Number.one
    target.atan(Number.zero) shouldBe Number(0, Pi)
  }
  it should "be pi/4 for 1/1" in {
    val target = Number.one
    target.atan(Number.one) === (Number.pi / 4)
  }
  it should "be 0Pi for 0/-1" in {
    val target = Number.negate(Number.one)
    target.atan(Number.zero) shouldBe Number(1, Pi)
  }
  it should "be Pi / 3 for root(3)" in {
    Number.one.atan(Number(Rational(3)).sqrt) shouldEqual Number(r"1/3", Pi)
  }
  it should "be 7 Pi / 6 for 1/-root(3)" in {
    // CONSIDER shouldn't this be 5 Pi / 6?
    negate(Number(Rational(3)).sqrt).atan(Number.one) shouldEqual Number(r"7/6", Pi)
  }
  it should "be 11 Pi / 6 for -1/2" in {
    Number(Rational(3)).sqrt.atan(negate(Number.one)) shouldEqual Number(r"11/6", Pi)
  }
  it should "be 3 pi / 4 for 1/-1" in {
    val adjacent = Number.negate(Number.one)
    val opposite = Number.one
    val actual: Number = adjacent.atan(opposite)
    import Expression.ExpressionOps
    val expected: Number = (Number.pi * 3 / 4).asNumber.get
    // TODO revert this so that it reads actual ... expected
    //  XXX  actual should ===(expected)
    actual shouldBe expected
  }
  // TODO need to operate appropriately on negZero.
  it should "evaluate atan of 1 over -0" in {
    val number = Number.negZero.atan(Number.one)
    number shouldBe Number(Rational(3, 2), Pi)
  }
  it should "evaluate atan of -1 over 0" in {
    val number = Number.zero.atan(negate(Number.one))
    number shouldBe Number(Rational(3, 2), Pi)
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
    val target = Number(1, Pi)
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
}
