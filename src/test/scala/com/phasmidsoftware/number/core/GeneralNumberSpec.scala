package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Expression.ExpressionOps
import com.phasmidsoftware.number.core.Field.convertToNumber
import com.phasmidsoftware.number.core.GeneralNumber.{negate, pi}
import org.scalactic.Equality
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Failure, Left, Try}

class GeneralNumberSpec extends AnyFlatSpec with should.Matchers {

  implicit object NumberEquality extends Equality[GeneralNumber] {
    def areEqual(a: GeneralNumber, b: Any): Boolean = b match {
      case n: GeneralNumber => a.compare(n) == 0
      case n: Expression => a.compare(n) == 0
      case _ => false
    }
  }

  implicit object ExpressionEquality extends Equality[Expression] {
    def areEqual(a: Expression, b: Any): Boolean = b match {
      case n: GeneralNumber => new ExpressionOps(a).compare(n) == 0
      case n: Expression => a.compare(n) == 0
      case _ => false
    }
  }

  private val numberOne = GeneralNumber(1)
  private val bigOne = BigInt(1)
  private val ratOne = Rational.one
  private val doubleOne = 1.0
  private val bigBigInt = BigInt(2147483648L)
  private val standardFuzz = AbsoluteFuzz[Double](1E-7, Gaussian)

  behavior of "create"
  it should "yield Right(1)" in {
    val target = GeneralNumber.create(Right(1))
    target should matchPattern { case ExactNumber(_, _) => }
    target.value should matchPattern { case Right(_) => }
  }
  it should "yield Left(Right(bigBigInt))" in {
    val target = GeneralNumber.create(Left(Right(bigBigInt)))
    target should matchPattern { case ExactNumber(_, _) => }
    target.value shouldBe Left(Right(Rational(bigBigInt)))
  }
  it should "yield Right(1, Fuzz)" in {
    val target = GeneralNumber.create(Right(1), standardFuzz)
    target should matchPattern { case FuzzyNumber(_, _, _) => }
    target.value should matchPattern { case Right(_) => }
  }
  it should "yield Right(1, Pi)" in {
    val target = pi
    target should matchPattern { case ExactNumber(_, _) => }
    target.value should matchPattern { case Right(_) => }
    target.factor shouldBe Pi
  }
  it should "yield Right(1, Pi, Fuzz)" in {
    val target = GeneralNumber.create(Right(1), Pi, Some(standardFuzz))
    target should matchPattern { case FuzzyNumber(_, _, _) => }
    target.value should matchPattern { case Right(_) => }
    target.factor shouldBe Pi
  }

  behavior of "value"
  it should "yield Right(1)" in {
    val target = GeneralNumber.create(Right(1))
    target.value shouldBe Right(1)
  }
  it should "yield Left(Right(bigBigInt))" in {
    val target = GeneralNumber.create(Left(Right(bigBigInt)))
    target.value shouldBe Left(Right(Rational(bigBigInt)))
  }

  behavior of "toRational"
  it should "yield Some(1)" in {
    val target = GeneralNumber.create(Right(1))
    target.toRational shouldBe Some(ratOne)
  }

  behavior of "toString"
  it should "yield 1" in {
    val target = GeneralNumber.create(Right(1))
    target.toString shouldBe "1"
  }
  it should "work for Pi" in {
    GeneralNumber.pi.toString shouldBe "1\uD835\uDED1"
  }
  it should "work for Pi as scalar" in {
    val target = GeneralNumber.pi.scale(Scalar)
    // NOTE that:       Pi is 3.1415926535897932384626433...
    // NOTE that:  math.PI is 3.14159265358979323846
    // In IEEE 754 binary, pi is 400921fb54442d18, which is:
    //                        3.141592653589793
    //    target.toString shouldBe "3.14159265358979300(41)" // TODO this is how it should be
    target.toString shouldBe "3.1415926535897930[5]"
  }
  it should "work for E" in {
    GeneralNumber.e.toString shouldBe "\uD835\uDF00"
  }
  it should "work for E as scalar" in {
    val target = GeneralNumber.e.scale(Scalar)
    target.toString shouldBe "2.71828182845904500[43]"
  }
  it should "work for E^2 as Real" in {
    val target = GeneralNumber("2\uD835\uDF00")
    target.scale(Scalar).toString shouldBe "7.3890560989306500[12]"
  }
  it should "work for 1 scaled as Pi" in {
    numberOne.scale(Pi).toString shouldBe "0.318309886183790700[51]\uD835\uDED1"
    //    numberOne.scale(Pi).toString shouldBe "0.318309886183790700(42)\uD835\uDED1" // this is how it should be
  }
  it should "work for E^2" in {
    val target = GeneralNumber.e ^ 2
    target.materialize.toString shouldBe "\uD835\uDF00\u00B2"
  }
  it should "work for E^3" in {
    val target = GeneralNumber.e ^ 3
    target.materialize.toString shouldBe "\uD835\uDF00\u00B3"
  }
  it should "work for E^4" in {
    val target = GeneralNumber.e ^ 4
    target.materialize.toString shouldBe "\uD835\uDF00\u2074"
  }
  it should "work for E^10" in {
    val target = GeneralNumber.e ^ 10
    target.materialize.toString shouldBe "\uD835\uDF00^10"
  }
  it should "work for square root E" in {
    val target = GeneralNumber.e.sqrt
    target.materialize.toString shouldBe "√\uD835\uDF00"
  }

  behavior of "toDouble"
  it should "yield 1" in {
    val target = GeneralNumber.create(Right(1))
    target.toDouble shouldBe Some(doubleOne)
  }

  behavior of "isValid"
  it should "yield true for 1" in {
    val target = GeneralNumber.create(Right(1))
    target.isValid shouldBe true
  }
  it should "yield false for None" in {
    val target = GeneralNumber.create(Left(Left(None)))
    target.isValid shouldBe false
  }

  behavior of "parse"
  it should "work for 1" in {
    val xy: Try[GeneralNumber] = GeneralNumber.parse("1")
    xy.get shouldBe GeneralNumber.create(Right(1))
  }
  it should "work for 3/2" in {
    val xy: Try[GeneralNumber] = GeneralNumber.parse("3/2")
    xy.get shouldBe GeneralNumber.create(Left(Right(Rational(3, 2))))
  }
  it should "work for 3.1415927" in {
    val xy: Try[GeneralNumber] = GeneralNumber.parse("3.1415927")
    xy.get shouldBe FuzzyNumber(Left(Right(Rational(31415927, 10000000))), Scalar, Some(AbsoluteFuzz(0.00000005, Box)))
  }
  it should "work for BigInt" in {
    val xy: Try[GeneralNumber] = GeneralNumber.parse("2147483648")
    xy.get shouldBe GeneralNumber.create(Left(Right(bigBigInt)))
  }
  it should "work for Pi" in {
    val xy: Try[GeneralNumber] = GeneralNumber.parse("1" + Factor.sPi)
    xy.get shouldBe GeneralNumber(Pi)
  }
  it should "work for pi" in {
    val xy: Try[GeneralNumber] = GeneralNumber.parse("1pi")
    xy.get shouldBe GeneralNumber(Pi)
  }
  it should "work for e" in {
    val xy: Try[GeneralNumber] = GeneralNumber.parse("1" + Factor.sE)
    xy.get shouldBe GeneralNumber(1, E)
  }
  it should "fail with x" in {
    GeneralNumber.parse("1x") should matchPattern { case Failure(_) => }
  }
  it should """fail for """"" in {
    GeneralNumber.parse("") should matchPattern { case Failure(_) => }
  }

  behavior of "apply"
  it should """fail for """"" in {
    an[NumberExceptionWithCause] should be thrownBy GeneralNumber("")
  }
  it should """work for "1"""" in {
    val target = GeneralNumber("1")
    target.value shouldBe Right(1)
  }
  it should """work for "2147483648"""" in {
    val target = GeneralNumber("2147483648")
    target.value shouldBe Left(Right(Rational(bigBigInt)))
  }
  it should """work for "3.1415927"""" in {
    val target = GeneralNumber("3.1415927")
    target.value shouldBe Left(Right(Rational(31415927, 10000000)))
  }
  it should "work for 3.1416" in {
    val target = GeneralNumber(3.1416)
    target shouldEqual GeneralNumber(Rational(3927, 1250))
  }
  it should """work for "\uD835\uDED1""""" in {
    val target = GeneralNumber("\uD835\uDED1")
    target.value shouldBe Right(1)
    target.factor shouldBe Pi
  }
  it should "work for 1" in {
    val target = numberOne
    target.value shouldBe Right(1)
  }
  it should "work for bigBigInt" in {
    val target = GeneralNumber(bigBigInt)
    target.value shouldBe Left(Right(Rational(bigBigInt)))
  }
  it should "work for Rational(1,2)" in {
    val target = GeneralNumber(Rational(1, 2))
    target.value shouldBe Left(Right(Rational(1, 2)))
  }
  it should "work for math.pi" in {
    val target = GeneralNumber(Math.PI)
    target shouldEqual GeneralNumber(Rational(3141592653589793L, 1000000000000000L))
  }
  it should "work for nothing" in {
    val target = GeneralNumber()
    target.value shouldBe Left(Left(None))
  }
  it should "work for BigDecimal(3.1415927)" in {
    val target = GeneralNumber(BigDecimal(3.1415927))
    target.value shouldBe Left(Right(Rational(31415927, 10000000)))
  }
  it should "work for 3.1415927" in {
    val target = GeneralNumber(3.1415927)
    target shouldEqual GeneralNumber(Rational(31415927, 10000000))
  }
  it should """work for 3.1415926535897932384626433""" in {
    val target = GeneralNumber(3.1415926535897932384626433)
    target shouldEqual GeneralNumber(Rational(3141592653589793L, 1000000000000000L))
  }
  it should "work for 0.5" in {
    val target = GeneralNumber(0.5)
    target.value shouldBe Left(Right(Rational(1, 2)))
  }
  it should "support exact strings" in {
    val target = GeneralNumber("3.141592700")
    target should matchPattern { case ExactNumber(_, _) => }
    target.value shouldBe Left(Right(Rational(31415927, 10000000)))
  }

  behavior of "normalize"
  it should "work for 1" in {
    val target = numberOne
    target.specialize.value shouldBe Right(1)
  }
  it should "work for BigInt 1" in {
    val target = GeneralNumber.create(Left(Right(Rational(bigOne))), Scalar)
    target.specialize.value shouldBe Right(1)
  }
  it should "work for BigInt(1+Int.MaxValue)" in {
    val bigInt = BigInt(1L + Int.MaxValue)
    val target = GeneralNumber(bigInt)
    target.specialize.value shouldBe Left(Right(Rational(bigInt)))
  }
  it should "work for Rational(1)" in {
    val target = GeneralNumber.create(Left(Right(ratOne)), Scalar)
    target.specialize.value shouldBe Right(1)
  }
  it should "work for Rational.half" in {
    val target = GeneralNumber(Rational.half)
    target.specialize.value shouldBe Left(Right(Rational(1, 2)))
  }
  it should "work for 0.5" in {
    val target = GeneralNumber(0.5)
    target.specialize.value shouldBe Left(Right(Rational(1, 2)))
  }
  it should "work for 1.0" in {
    val target = GeneralNumber(doubleOne)
    target.specialize.value shouldBe Right(1)
  }
  it should "work for nothing" in {
    val target = GeneralNumber()
    target.specialize.value shouldBe Left(Left(None))
  }

  behavior of "scale"
  it should "work for Scalar, Scalar" in {
    val target = numberOne
    target.scale(Scalar) shouldBe numberOne
  }
  it should "work for Pi, Pi" in {
    val target = GeneralNumber(1, Pi)
    target.scale(Pi) shouldBe target
  }
  it should "work for Scalar, Pi" in {
    val target = numberOne
    target.scale(Pi) should ===(GeneralNumber(1 / Math.PI, Pi))
  }
  it should "work for Pi, Scalar" in {
    val target = GeneralNumber(1, Pi)
    target.scale(Scalar) should ===(GeneralNumber(Math.PI))
  }
  it should "work for Scalar, E" in {
    val target = numberOne
    target.scale(E) should ===(GeneralNumber(math.log(1), E))
  }
  it should "work for E, Scalar" in {
    val target = GeneralNumber(1, E)
    target.scale(Scalar) should ===(GeneralNumber(Math.E))
  }
  it should "work for 2E, Scalar" in {
    val target = GeneralNumber(2, E)
    val actual: GeneralNumber = target.scale(Scalar)
    val expected: GeneralNumber = convertToNumber((GeneralNumber(Math.E) ^ 2).materialize)
    actual should ===(expected)
  }
  it should "work for E, Pi" in {
    val target = GeneralNumber(1, E)
    val expected = GeneralNumber(Math.E / Math.PI, Pi)
    target.scale(Pi) should ===(expected)
  }
  it should "not work for Pi, E (because E numbers are not linear)" in {
    val target = GeneralNumber(1, Pi)
    val expected = GeneralNumber(Math.PI / Math.E, E)
    val result = target.scale(E) === expected
    result shouldBe false
  }

  behavior of "alignFactors"
  it should "work for Scalar, Scalar" in {
    val target = numberOne
    target.alignFactors(GeneralNumber(2)) shouldBe(numberOne, GeneralNumber(2))
  }
  it should "work for Scalar, Pi" in {
    val target = numberOne
    val (p, q) = target.alignFactors(GeneralNumber(2, Pi))
    p shouldBe numberOne
    q shouldEqual GeneralNumber(2 * Math.PI)
  }
  it should "work for Pi, Scalar" in {
    val target = GeneralNumber(2, Pi)
    val (f, x) = target.alignFactors(numberOne)
    f shouldEqual GeneralNumber(2 * Math.PI)
    x shouldBe numberOne
  }
  it should "work for Pi, Pi" in {
    val target = GeneralNumber(1, Pi)
    target.alignFactors(GeneralNumber(2, Pi)) shouldBe(GeneralNumber(1, Pi), GeneralNumber(2, Pi))
  }

  behavior of "alignTypes"
  it should "work for Int,Int" in {
    val target = numberOne
    target.alignTypes(GeneralNumber(2)) shouldBe(numberOne, GeneralNumber(2))
  }
  it should "work for Int Int(Pi)" in {
    val target = numberOne
    target.alignTypes(GeneralNumber(1, Pi)) shouldBe(numberOne, GeneralNumber(1, Pi))
  }
  it should "work for Int,BigInt" in {
    val target = numberOne
    target.alignTypes(GeneralNumber(bigBigInt)) shouldBe(GeneralNumber(bigBigInt), GeneralNumber(bigOne))
  }
  it should "work for BigInt,Int" in {
    val target = GeneralNumber(bigOne)
    target.alignTypes(GeneralNumber(2)) shouldBe(GeneralNumber(bigOne), GeneralNumber(BigInt(2)))
  }
  it should "work for Int,Rational" in {
    val target = numberOne
    target.alignTypes(GeneralNumber(Rational(2, 3))) shouldBe(GeneralNumber(Rational(2, 3)), GeneralNumber(Rational(1)))
  }
  it should "work for Rational,Int" in {
    val target = GeneralNumber(Rational(1))
    target.alignTypes(GeneralNumber(2)) shouldBe(GeneralNumber(Rational(1)), GeneralNumber(Rational(2)))
  }
  it should "work for Int,Double" in {
    val target = numberOne
    val (p, q) = target.alignTypes(GeneralNumber(Math.PI))
    p should ===(GeneralNumber(Math.PI))
    q should ===(GeneralNumber(doubleOne))
  }
  it should "work for Double,Int" in {
    val target = GeneralNumber(doubleOne)
    target.alignTypes(GeneralNumber(2)) shouldBe(GeneralNumber(doubleOne), GeneralNumber(2.0))
  }
  it should "work for Int,None" in {
    val target = numberOne
    target.alignTypes(GeneralNumber()) shouldBe(GeneralNumber(), GeneralNumber())
  }
  it should "work for None,Int" in {
    val target = GeneralNumber()
    target.alignTypes(GeneralNumber(2)) shouldBe(GeneralNumber(), GeneralNumber())
  }

  it should "work for BigInt,BigInt" in {
    val target = GeneralNumber(bigOne)
    target.alignTypes(GeneralNumber(BigInt(2))) shouldBe(GeneralNumber(bigOne), GeneralNumber(BigInt(2)))
  }
  it should "work for BigInt,Rational" in {
    val target = GeneralNumber(bigOne)
    target.alignTypes(GeneralNumber(Rational(2, 3))) shouldBe(GeneralNumber(Rational(2, 3)), GeneralNumber(Rational(1)))
  }
  it should "work for Rational,BigInt" in {
    val target = GeneralNumber(Rational(1))
    target.alignTypes(GeneralNumber(BigInt(2))) shouldBe(GeneralNumber(Rational(1)), GeneralNumber(Rational(2)))
  }
  it should "work for BigInt,Double" in {
    val target = GeneralNumber(bigOne)
    val (p, q) = target.alignTypes(GeneralNumber(Math.PI))
    p should ===(GeneralNumber(Math.PI))
    q should ===(GeneralNumber(doubleOne))
  }
  it should "work for Double,BigInt" in {
    val target = GeneralNumber(doubleOne)
    target.alignTypes(GeneralNumber(BigInt(2))) shouldBe(GeneralNumber(doubleOne), GeneralNumber(2.0))
  }
  it should "work for BigInt,None" in {
    val target = GeneralNumber(bigOne)
    target.alignTypes(GeneralNumber()) shouldBe(GeneralNumber(), GeneralNumber())
  }
  it should "work for None,BigInt" in {
    val target = GeneralNumber()
    target.alignTypes(GeneralNumber(BigInt(2))) shouldBe(GeneralNumber(), GeneralNumber())
  }

  it should "work for Rational,Rational" in {
    val target = GeneralNumber(Rational(1))
    target.alignTypes(GeneralNumber(Rational(2))) shouldBe(GeneralNumber(Rational(1)), GeneralNumber(Rational(2)))
  }
  it should "work for Rational,Double" in {
    val target = GeneralNumber(Rational(1))
    val (p, q) = target.alignTypes(GeneralNumber(Math.PI))
    p should ===(GeneralNumber(Math.PI))
    q should ===(GeneralNumber(doubleOne))
  }
  it should "work for Double,Rational" in {
    val target = GeneralNumber(doubleOne)
    target.alignTypes(GeneralNumber(Rational(2))) shouldBe(GeneralNumber(doubleOne), GeneralNumber(2.0))
  }
  it should "work for Rational,None" in {
    val target = GeneralNumber(Rational(1))
    target.alignTypes(GeneralNumber()) shouldBe(GeneralNumber(), GeneralNumber())
  }
  it should "work for None,Rational" in {
    val target = GeneralNumber()
    target.alignTypes(GeneralNumber(Rational(2))) shouldBe(GeneralNumber(), GeneralNumber())
  }

  it should "work for Double,Double" in {
    val target = GeneralNumber(doubleOne)
    target.alignTypes(GeneralNumber(2.0)) shouldBe(GeneralNumber(doubleOne), GeneralNumber(2.0))
  }
  it should "work for Double,None" in {
    val target = GeneralNumber(doubleOne)
    target.alignTypes(GeneralNumber()) shouldBe(GeneralNumber(), GeneralNumber())
  }
  it should "work for None,Double" in {
    val target = GeneralNumber()
    target.alignTypes(GeneralNumber(2.0)) shouldBe(GeneralNumber(), GeneralNumber())
  }

  it should "work for None,None" in {
    val target = GeneralNumber()
    target.alignTypes(GeneralNumber()) shouldBe(GeneralNumber(), GeneralNumber())
  }

  behavior of "plus"
  it should "add 1 and 2" in {
    val x = numberOne
    val y = GeneralNumber(2)
    (x add y) shouldBe GeneralNumber(3)
  }
  it should "add BigInt 1 and 2" in {
    val x = GeneralNumber(bigOne)
    val y = GeneralNumber(2)
    (x add y) shouldBe GeneralNumber(3)
  }
  it should "add Rational 1 and 2" in {
    val x = GeneralNumber(ratOne)
    val y = GeneralNumber(2)
    (x add y) shouldBe GeneralNumber(3)
  }
  it should "add Double 1 and 2" in {
    val x = GeneralNumber(doubleOne)
    val y = GeneralNumber(2)
    (x add y) shouldBe GeneralNumber(3)
  }
  it should "add Double 1 and Pi" in {
    val x = GeneralNumber(doubleOne)
    val y = GeneralNumber(1, Pi)
    convertToNumber(x add y) should ===(GeneralNumber(Math.PI + 1))
  }
  it should "add Pi and 2Pi" in {
    val x = GeneralNumber(1, Pi)
    val y = GeneralNumber(2, Pi)
    convertToNumber(x add y) shouldBe GeneralNumber(3, Pi)
  }
  it should "add 1 to pi" in {
    val x1 = GeneralNumber.one
    val x2 = GeneralNumber.pi
    (x1 add x2).toString shouldBe "4.1415926535897930(29)"
  }


  behavior of "minus"
  it should "negate 1" in {
    val x = numberOne
    -x shouldBe GeneralNumber(-1)
  }
  it should "negate BigInt 1" in {
    val x = GeneralNumber(bigOne)
    -x shouldBe GeneralNumber(BigInt(-1))
  }
  it should "negate Rational 1" in {
    val x = GeneralNumber(ratOne)
    -x shouldBe GeneralNumber(Rational(-1))
  }
  it should "negate Double 1" in {
    val x = GeneralNumber(doubleOne)
    -x shouldBe GeneralNumber(-doubleOne)
  }

  behavior of "subtract"
  it should "subtract 1 from 2" in {
    val x = GeneralNumber(2)
    val y = numberOne
    (x add -y) shouldBe numberOne
  }
  it should "subtract BigInt 1 from 2" in {
    val x = GeneralNumber(BigInt(2))
    val y = numberOne
    (x add -y) shouldBe numberOne
  }
  it should "subtract Rational 1 from 2" in {
    val x = GeneralNumber(Rational(2))
    val y = numberOne
    (x add -y) shouldBe numberOne
  }
  it should "subtract Double 1 from 2" in {
    val x = GeneralNumber(2.0)
    val y = numberOne
    (x add -y) shouldBe numberOne
  }

  behavior of "times"
  it should "multiply 1 and 2" in {
    val x = numberOne
    val y = GeneralNumber(2)
    (x multiply y) shouldBe GeneralNumber(2)
  }
  it should "multiply BigInt 1 and 2" in {
    val x = GeneralNumber(bigOne)
    val y = GeneralNumber(2)
    (x multiply y) shouldBe GeneralNumber(2)
  }
  it should "multiply Rational 1 and 2" in {
    val x = GeneralNumber(Rational(1))
    val y = GeneralNumber(2)
    (x multiply y) shouldBe GeneralNumber(2)
  }
  it should "multiply Double 1 and 2" in {
    val x = GeneralNumber(doubleOne)
    val y = GeneralNumber(2)
    (x multiply y) shouldBe GeneralNumber(2)
  }
  it should "multiply 2 and Pi" in {
    val x = GeneralNumber(2)
    val y = GeneralNumber(1, Pi)
    (x multiply y) shouldBe GeneralNumber(2, Pi)
  }
  it should "multiply Pi and 2" in {
    val x = GeneralNumber(1, Pi)
    val y = GeneralNumber(2)
    (x multiply y) shouldBe GeneralNumber(2, Pi)
  }

  behavior of "invert"
  it should "invert 1" in {
    val x = numberOne
    x.invert shouldBe numberOne
  }
  it should "invert BigInt 1" in {
    val x = GeneralNumber(bigOne)
    x.invert shouldBe numberOne
  }
  it should "invert 2" in {
    val x = GeneralNumber(2)
    x.invert shouldBe GeneralNumber(Rational.half)
  }
  it should "invert BigInt 2" in {
    val x = GeneralNumber(BigInt(2))
    x.invert shouldBe GeneralNumber(Rational.half)
  }
  it should "invert Rational 2" in {
    val x = GeneralNumber(Rational.two)
    x.invert shouldBe GeneralNumber(Rational.half)
  }
  it should "invert Double pi" in {
    val x = GeneralNumber(Math.PI)
    x.invert should ===(GeneralNumber(1 / Math.PI))
  }

  behavior of "division"
  it should "divide 1 by 2" in {
    val x = numberOne
    val y = GeneralNumber(2)
    (x divide y) shouldBe GeneralNumber(Rational.half)
  }
  it should "divide BigInt 1 by 2" in {
    val x = GeneralNumber(bigOne)
    val y = GeneralNumber(2)
    (x divide y) shouldBe GeneralNumber(Rational.half)
  }
  it should "divide Rational 1 by 2" in {
    val x = GeneralNumber(Rational(1))
    val y = GeneralNumber(2)
    (x divide y) shouldBe GeneralNumber(Rational.half)
  }
  it should "divide Double 1 by Double Pi" in {
    val x = GeneralNumber(doubleOne)
    val y = GeneralNumber(Math.PI)
    convertToNumber(x divide y) should ===(GeneralNumber(1 / Math.PI))
  }

  // XXX what is this ** operator?
  behavior of "**"
  it should "work for 2^2" in {
    val target = GeneralNumber(2)
    (target ^ 2).materialize shouldBe GeneralNumber(4)
  }

  behavior of "sqrt"
  it should "work for easy ints" in {
    GeneralNumber(1).sqrt shouldBe GeneralNumber(1)
    GeneralNumber(4).sqrt shouldBe GeneralNumber(2)
    GeneralNumber(9).sqrt shouldBe GeneralNumber(3)
  }
  it should "work for BigInt" in {
    GeneralNumber(bigBigInt).sqrt should ===(GeneralNumber(Rational(2317047500592079L, 50000000000L)))
  }
  it should "work for easy Rational" in {
    GeneralNumber(Rational(9, 4)).sqrt shouldBe GeneralNumber(Rational(3, 2))
  }

  behavior of "sin"
  it should "be zero for pi" in {
    val target = GeneralNumber.pi
    target.sin shouldBe GeneralNumber(0, Scalar)
  }
  it should "work for 0" in {
    val target = GeneralNumber(0, Pi)
    target.sin shouldBe GeneralNumber(0, Scalar)
  }
  it should "be one for pi/2" in {
    val target = (GeneralNumber.pi / 2).sin
    target.materialize shouldBe GeneralNumber.one
  }
  it should "work for Pi/2" in {
    val target = GeneralNumber(Rational.half, Pi)
    val sin = target.sin
    sin shouldBe GeneralNumber(1, Scalar)
  }
  it should "work for Pi/6" in {
    val target = GeneralNumber(Rational(6).invert, Pi)
    target.sin shouldBe GeneralNumber(Rational(1, 2), Scalar)
  }
  it should "work for Pi/3" in {
    val target = GeneralNumber(Rational(1, 3), Pi)
    val sin = target.sin
    sin should ===(GeneralNumber(3).sqrt / 2)
  }

  behavior of "cos"
  it should "be zero for pi" in {
    val target = GeneralNumber.pi
    target.cos === GeneralNumber(-1)
  }
  it should "work for 0" in {
    val target = GeneralNumber(0, Pi)
    target.cos shouldBe GeneralNumber(1)
  }
  it should "work for Pi/2" in {
    val target = GeneralNumber(Rational.half, Pi)
    target.cos.isZero shouldBe true
  }
  it should "work for Pi/3" in {
    val target = GeneralNumber.pi / 3
    target.cos.materialize shouldBe GeneralNumber(Rational(1, 2), Scalar)
  }
  it should "work for Pi/6" in {
    val target: Expression = GeneralNumber.pi / 6
    target.cos should ===(GeneralNumber(3).sqrt / 2)
  }

  behavior of "tan"
  it should "be zero for 0" in {
    val target: Expression = GeneralNumber(0, Pi)
    target.tan.materialize.isZero shouldBe true
  }
  it should "be zero for pi" in {
    val target: Expression = GeneralNumber.pi
    target.tan.materialize.isZero shouldBe true
  }
  it should "work for Pi/2" in {
    val target: Expression = GeneralNumber(Rational.half, Pi)
    target.tan.materialize.isInfinite shouldBe true
  }
  it should "work for Pi/3" in {
    val target: Expression = GeneralNumber.pi / 3
    target.tan shouldEqual GeneralNumber(3).sqrt
  }
  it should "work for Pi/6" in {
    val target = GeneralNumber.pi / 6
    target.tan should ===(GeneralNumber(3).sqrt.invert)
  }

  behavior of "atan"
  it should "be 0Pi for 0/1" in {
    val target = GeneralNumber.one
    target.atan(GeneralNumber.zero) shouldBe GeneralNumber(0, Pi)
  }
  it should "be pi/4 for 1/1" in {
    val target = GeneralNumber.one
    target.atan(GeneralNumber.one) === (GeneralNumber.pi / 4)
  }
  it should "be 0Pi for 0/-1" in {
    val target = negate(GeneralNumber.one)
    target.atan(GeneralNumber.zero) shouldBe GeneralNumber(1, Pi)
  }
  it should "be pi/4 for 1/-1" in {
    val target = negate(GeneralNumber.one)
    val actual = target.atan(GeneralNumber.one)
    import Expression.ExpressionOps
    val expected: Expression = GeneralNumber.pi * 7 / 4
    // TODO revert this so that it reads actual ... expected
    //  XXX  actual should ===(expected)
    expected should ===(actual)
  }
  // TODO need to operate appropriately on negZero.
  ignore should "evaluate atan" in {
    GeneralNumber.one.atan(GeneralNumber.negZero) shouldBe GeneralNumber(3, Pi)
  }


  // NOTE: Following are the tests of Ordering[GeneralNumber]

  behavior of "compare"
  it should "work for 1, 1" in {
    val x = numberOne
    val y = numberOne
    implicitly[Numeric[GeneralNumber]].compare(x, y) shouldBe 0
  }
  it should "work for 1, 2" in {
    val x = numberOne
    val y = GeneralNumber(2)
    implicitly[Numeric[GeneralNumber]].compare(x, y) shouldBe -1
  }
  it should "work for 2, 1" in {
    val x = GeneralNumber(2)
    val y = numberOne
    implicitly[Numeric[GeneralNumber]].compare(x, y) shouldBe 1
  }
  it should "work for BigInt 1, 1" in {
    val x = GeneralNumber(bigOne)
    val y = GeneralNumber(bigOne)
    implicitly[Numeric[GeneralNumber]].compare(x, y) shouldBe 0
  }
  it should "work for BigInt 1, 2" in {
    val x = GeneralNumber(bigOne)
    val y = GeneralNumber(BigInt(2))
    implicitly[Numeric[GeneralNumber]].compare(x, y) shouldBe -1
  }
  it should "work for BigInt 2, 1" in {
    val x = GeneralNumber(BigInt(2))
    val y = GeneralNumber(bigOne)
    implicitly[Numeric[GeneralNumber]].compare(x, y) shouldBe 1
  }
  it should "work for Rational 1, 1" in {
    val x = GeneralNumber(ratOne)
    val y = GeneralNumber(ratOne)
    implicitly[Numeric[GeneralNumber]].compare(x, y) shouldBe 0
  }
  it should "work for Rational 1, 2" in {
    val x = GeneralNumber(ratOne)
    val y = GeneralNumber(Rational.two)
    implicitly[Numeric[GeneralNumber]].compare(x, y) shouldBe -1
  }
  it should "work for Rational 2, 1" in {
    val x = GeneralNumber(Rational.two)
    val y = GeneralNumber(ratOne)
    implicitly[Numeric[GeneralNumber]].compare(x, y) shouldBe 1
  }
  it should "work for Double 1, 1" in {
    val x = GeneralNumber(doubleOne)
    val y = GeneralNumber(doubleOne)
    implicitly[Numeric[GeneralNumber]].compare(x, y) shouldBe 0
  }
  it should "work for Double 1, 2" in {
    val x = GeneralNumber(doubleOne)
    val y = GeneralNumber(2.0)
    implicitly[Numeric[GeneralNumber]].compare(x, y) shouldBe -1
  }
  it should "work for Double 2, 1" in {
    val x = GeneralNumber(2.0)
    val y = GeneralNumber(doubleOne)
    implicitly[Numeric[GeneralNumber]].compare(x, y) shouldBe 1
  }

  // NOTE: Following are the tests of Numeric[GeneralNumber]

  behavior of "toInt"
  it should "work for 1" in {
    val target = numberOne
    target.toInt shouldBe Some(1)
  }
  it should "work for BigInt 1" in {
    val target = GeneralNumber(bigOne)
    target.toInt shouldBe Some(1)
  }
  it should "work for Rational 1" in {
    val target = GeneralNumber(ratOne)
    target.toInt shouldBe Some(1)
  }
  it should "work for too big" in {
    val target = GeneralNumber(bigBigInt)
    val xo: Option[Int] = target.toInt
    xo shouldBe None
  }
  it should "work for 3.14..." in {
    val target = GeneralNumber(3.1415927)
    target.toInt shouldBe None
  }
  it should "work for pi" in {
    val target = GeneralNumber(1, Pi)
    target.toInt shouldBe Some(1)
  }

  // XXX Following are the tests of Numeric[GeneralNumber]

  behavior of "Numeric toInt"
  it should "work for 1" in {
    val target = numberOne
    implicitly[Numeric[GeneralNumber]].toInt(target) shouldBe 1
  }
  it should "work for BigInt 1" in {
    val target = GeneralNumber(bigOne)
    implicitly[Numeric[GeneralNumber]].toInt(target) shouldBe 1
  }
  it should "work for Rational 1" in {
    val target = GeneralNumber(ratOne)
    implicitly[Numeric[GeneralNumber]].toInt(target) shouldBe 1
  }
  it should "work for 1.0" in {
    val target = GeneralNumber(doubleOne)
    implicitly[Numeric[GeneralNumber]].toInt(target) shouldBe 1
  }

  behavior of "Numeric toLong"
  it should "work for 1" in {
    val target = numberOne
    implicitly[Numeric[GeneralNumber]].toLong(target) shouldBe 1L
  }
  it should "work for BigInt 1" in {
    val target = GeneralNumber(bigOne)
    implicitly[Numeric[GeneralNumber]].toLong(target) shouldBe 1L
  }
  it should "work for Rational 1" in {
    val target = GeneralNumber(ratOne)
    implicitly[Numeric[GeneralNumber]].toLong(target) shouldBe 1L
  }
  it should "work for 1.0" in {
    val target = GeneralNumber(doubleOne)
    implicitly[Numeric[GeneralNumber]].toLong(target) shouldBe 1L
  }

  behavior of "Numeric toDouble"
  it should "work for 1" in {
    val target = numberOne
    implicitly[Numeric[GeneralNumber]].toDouble(target) shouldBe 1.0
  }
  it should "work for BigInt 1" in {
    val target = GeneralNumber(bigOne)
    implicitly[Numeric[GeneralNumber]].toDouble(target) shouldBe 1.0
  }
  it should "work for Rational 1" in {
    val target = GeneralNumber(ratOne)
    implicitly[Numeric[GeneralNumber]].toDouble(target) shouldBe 1.0
  }
  it should "work for 1.0" in {
    val target = GeneralNumber(doubleOne)
    implicitly[Numeric[GeneralNumber]].toDouble(target) shouldBe 1.0
  }

  behavior of "NumberOps"

  import com.phasmidsoftware.number.core.GeneralNumber.NumberOps

  it should "work for 2 + GeneralNumber(3)" in {
    val x: GeneralNumber = 2 + GeneralNumber(3)
    x shouldBe GeneralNumber(5)
  }
  it should "work for 2 * GeneralNumber(3)" in {
    val x: GeneralNumber = 2 * GeneralNumber(3)
    x shouldBe GeneralNumber(6)
  }
  it should "work for 1 :/ 2" in {
    val x: GeneralNumber = 1 :/ 2
    x shouldBe GeneralNumber(Rational.half)
  }
}
