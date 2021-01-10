package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Expression.ExpressionOps
import com.phasmidsoftware.number.core.Number.{negate, pi}
import org.scalactic.Equality
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Failure, Left, Try}

class NumberSpec extends AnyFlatSpec with should.Matchers {

  implicit object NumberEquality extends Equality[Number] {
    def areEqual(a: Number, b: Any): Boolean = b match {
      case n: Number => a.compare(n) == 0
      case n: Expression => a.compare(n) == 0
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
    val target = pi
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
    target.toString shouldBe "3.1415926535897930[31]"
  }
  it should "work for E" in {
    Number.e.toString shouldBe "1\uD835\uDF00"
  }
  it should "work for E as scalar" in {
    val target = Number.e.scale(Scalar)
    target.toString shouldBe "2.7182818284590450[27]"
  }
  it should "work for 1 scaled as Pi" in {
    numberOne.scale(Pi).toString shouldBe "0.31830988618379070[32]\uD835\uDED1"
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
    xy.get shouldBe Number(1, Pi)
  }
  it should "work for pi" in {
    val xy: Try[Number] = Number.parse("1pi")
    xy.get shouldBe Number(1, Pi)
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
    target.value shouldBe Right(1)
  }
  it should """work for "2147483648"""" in {
    val target = Number("2147483648")
    target.value shouldBe Left(Right(Rational(bigBigInt)))
  }
  it should """work for "3.1415927"""" in {
    val target = Number("3.1415927")
    target.value shouldBe Left(Right(Rational(31415927, 10000000)))
  }
  it should "work for 3.1416" in {
    val target = Number(3.1416)
    target shouldEqual Number(Rational(3927, 1250))
  }
  it should """work for "\uD835\uDED1""""" in {
    val target = Number("\uD835\uDED1")
    target.value shouldBe Right(1)
    target.factor shouldBe Pi
  }
  it should "work for 1" in {
    val target = numberOne
    target.value shouldBe Right(1)
  }
  it should "work for bigBigInt" in {
    val target = Number(bigBigInt)
    target.value shouldBe Left(Right(Rational(bigBigInt)))
  }
  it should "work for Rational(1,2)" in {
    val target = Number(Rational(1, 2))
    target.value shouldBe Left(Right(Rational(1, 2)))
  }
  it should "work for math.pi" in {
    val target = Number(Math.PI)
    target shouldEqual Number(Rational(3141592653589793L, 1000000000000000L))
  }
  it should "work for nothing" in {
    val target = Number()
    target.value shouldBe Left(Left(None))
  }
  it should "work for BigDecimal(3.1415927)" in {
    val target = Number(BigDecimal(3.1415927))
    target.value shouldBe Left(Right(Rational(31415927, 10000000)))
  }
  it should "work for 3.1415927" in {
    val target = Number(3.1415927)
    target shouldEqual Number(Rational(31415927, 10000000))
  }
  it should """work for 3.1415926535897932384626433""" in {
    val target = Number(3.1415926535897932384626433)
    target shouldEqual Number(Rational(3141592653589793L, 1000000000000000L))
  }
  it should "work for 0.5" in {
    val target = Number(0.5)
    target.value shouldBe Left(Right(Rational(1, 2)))
  }
  it should "support exact strings" in {
    val target = Number("3.141592700")
    target should matchPattern { case ExactNumber(_, _) => }
    target.value shouldBe Left(Right(Rational(31415927, 10000000)))
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
    target.scale(E) should ===(Number(1 / Math.E, E))
  }
  it should "work for E, Scalar" in {
    val target = Number(1, E)
    target.scale(Scalar) should ===(Number(Math.E))
  }
  it should "work for Pi, E" in {
    val target = Number(1, Pi)
    target.scale(E) should ===(Number(Math.PI / Math.E, E))
  }
  it should "work for E, Pi" in {
    val target = Number(1, E)
    target.scale(Pi) should ===(Number(Math.E / Math.PI, Pi))
  }

  behavior of "alignFactors"
  it should "work for Scalar, Scalar" in {
    val target = numberOne
    target.alignFactors(Number(2)) shouldBe(numberOne, Number(2))
  }
  it should "work for Scalar, Pi" in {
    val target = numberOne
    val (p, q) = target.alignFactors(Number(2, Pi))
    p shouldBe numberOne
    q shouldEqual Number(2 * Math.PI)
  }
  ignore should "work for Pi, Scalar" in {
    val target = Number(2, Pi)
    target.alignFactors(numberOne) shouldBe(Number(2 * Math.PI), numberOne)
  }
  it should "work for Pi, Pi" in {
    val target = Number(1, Pi)
    target.alignFactors(Number(2, Pi)) shouldBe(Number(1, Pi), Number(2, Pi))
  }

  behavior of "alignTypes"
  it should "work for Int,Int" in {
    val target = numberOne
    target.alignTypes(Number(2)) shouldBe(numberOne, Number(2))
  }
  it should "work for Int Int(Pi)" in {
    val target = numberOne
    target.alignTypes(Number(1, Pi)) shouldBe(numberOne, Number(1, Pi))
  }
  it should "work for Int,BigInt" in {
    val target = numberOne
    target.alignTypes(Number(bigBigInt)) shouldBe(Number(bigBigInt), Number(bigOne))
  }
  it should "work for BigInt,Int" in {
    val target = Number(bigOne)
    target.alignTypes(Number(2)) shouldBe(Number(bigOne), Number(BigInt(2)))
  }
  it should "work for Int,Rational" in {
    val target = numberOne
    target.alignTypes(Number(Rational(2, 3))) shouldBe(Number(Rational(2, 3)), Number(Rational(1)))
  }
  it should "work for Rational,Int" in {
    val target = Number(Rational(1))
    target.alignTypes(Number(2)) shouldBe(Number(Rational(1)), Number(Rational(2)))
  }
  it should "work for Int,Double" in {
    val target = numberOne
    val (p, q) = target.alignTypes(Number(Math.PI))
    p should ===(Number(Math.PI))
    q should ===(Number(doubleOne))
  }
  it should "work for Double,Int" in {
    val target = Number(doubleOne)
    target.alignTypes(Number(2)) shouldBe(Number(doubleOne), Number(2.0))
  }
  it should "work for Int,None" in {
    val target = numberOne
    target.alignTypes(Number()) shouldBe(Number(), Number())
  }
  it should "work for None,Int" in {
    val target = Number()
    target.alignTypes(Number(2)) shouldBe(Number(), Number())
  }

  it should "work for BigInt,BigInt" in {
    val target = Number(bigOne)
    target.alignTypes(Number(BigInt(2))) shouldBe(Number(bigOne), Number(BigInt(2)))
  }
  it should "work for BigInt,Rational" in {
    val target = Number(bigOne)
    target.alignTypes(Number(Rational(2, 3))) shouldBe(Number(Rational(2, 3)), Number(Rational(1)))
  }
  it should "work for Rational,BigInt" in {
    val target = Number(Rational(1))
    target.alignTypes(Number(BigInt(2))) shouldBe(Number(Rational(1)), Number(Rational(2)))
  }
  it should "work for BigInt,Double" in {
    val target = Number(bigOne)
    val (p, q) = target.alignTypes(Number(Math.PI))
    p should ===(Number(Math.PI))
    q should ===(Number(doubleOne))
  }
  it should "work for Double,BigInt" in {
    val target = Number(doubleOne)
    target.alignTypes(Number(BigInt(2))) shouldBe(Number(doubleOne), Number(2.0))
  }
  it should "work for BigInt,None" in {
    val target = Number(bigOne)
    target.alignTypes(Number()) shouldBe(Number(), Number())
  }
  it should "work for None,BigInt" in {
    val target = Number()
    target.alignTypes(Number(BigInt(2))) shouldBe(Number(), Number())
  }

  it should "work for Rational,Rational" in {
    val target = Number(Rational(1))
    target.alignTypes(Number(Rational(2))) shouldBe(Number(Rational(1)), Number(Rational(2)))
  }
  it should "work for Rational,Double" in {
    val target = Number(Rational(1))
    val (p, q) = target.alignTypes(Number(Math.PI))
    p should ===(Number(Math.PI))
    q should ===(Number(doubleOne))
  }
  it should "work for Double,Rational" in {
    val target = Number(doubleOne)
    target.alignTypes(Number(Rational(2))) shouldBe(Number(doubleOne), Number(2.0))
  }
  it should "work for Rational,None" in {
    val target = Number(Rational(1))
    target.alignTypes(Number()) shouldBe(Number(), Number())
  }
  it should "work for None,Rational" in {
    val target = Number()
    target.alignTypes(Number(Rational(2))) shouldBe(Number(), Number())
  }

  it should "work for Double,Double" in {
    val target = Number(doubleOne)
    target.alignTypes(Number(2.0)) shouldBe(Number(doubleOne), Number(2.0))
  }
  it should "work for Double,None" in {
    val target = Number(doubleOne)
    target.alignTypes(Number()) shouldBe(Number(), Number())
  }
  it should "work for None,Double" in {
    val target = Number()
    target.alignTypes(Number(2.0)) shouldBe(Number(), Number())
  }

  it should "work for None,None" in {
    val target = Number()
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
    (x add y) should ===(Number(Math.PI + 1))
  }
  it should "add Pi and 2Pi" in {
    val x = Number(1, Pi)
    val y = Number(2, Pi)
    (x add y) shouldBe Number(3, Pi)
  }
  it should "add 1 to pi" in {
    val x1 = Number.one
    val x2 = Number.pi
    (x1 add x2).toString shouldBe "4.1415926535897930(61)"
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
    (x subtract y) shouldBe numberOne
  }
  it should "subtract BigInt 1 from 2" in {
    val x = Number(BigInt(2))
    val y = numberOne
    (x subtract y) shouldBe numberOne
  }
  it should "subtract Rational 1 from 2" in {
    val x = Number(Rational(2))
    val y = numberOne
    (x subtract y) shouldBe numberOne
  }
  it should "subtract Double 1 from 2" in {
    val x = Number(2.0)
    val y = numberOne
    (x subtract y) shouldBe numberOne
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
    x divide y should ===(Number(1 / Math.PI))
  }

  behavior of "**"
  it should "work for 2**2" in {
    val target = Number(2)
    target power 2 shouldBe Number(4)
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
    val target = (Number.pi divide 2).sin
    target shouldBe Number.one
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
    sin should ===(Number(3).sqrt divide 2)
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
    val target = Number.pi divide 3
    target.cos shouldBe Number(Rational(1, 2), Scalar)
  }
  it should "work for Pi/6" in {
    val target = Number.pi divide 6
    target.cos should ===(Number(3).sqrt divide 2)
  }

  behavior of "tan"
  it should "be zero for 0" in {
    val target = Number(0, Pi)
    target.tan.isZero shouldBe true
  }
  it should "be zero for pi" in {
    val target = Number.pi
    target.tan.isZero shouldBe true
  }
  it should "work for Pi/2" in {
    val target = Number(Rational.half, Pi)
    target.tan.isInfinite shouldBe true
  }
  it should "work for Pi/3" in {
    val target = Number.pi divide 3
    target.tan shouldEqual Number(3).sqrt
  }
  it should "work for Pi/6" in {
    val target = Number.pi divide 6
    target.tan should ===(Number(3).sqrt.invert)
  }

  behavior of "atan"
  it should "be 0Pi for 0/1" in {
    val target = Number.one
    target.atan(Number.zero) shouldBe Number(0, Pi)
  }
  it should "be pi/4 for 1/1" in {
    val target = Number.one
    target.atan(Number.one) === (Number.pi divide 4)
  }
  it should "be 0Pi for 0/-1" in {
    val target = negate(Number.one)
    target.atan(Number.zero) shouldBe Number(1, Pi)
  }
  it should "be pi/4 for 1/-1" in {
    val target = negate(Number.one)
    val actual = target.atan(Number.one)
    import Expression.ExpressionOps
    val expected: Expression = Number.pi * 7 / 4
    // TODO revert this so that it reads actual ... expected
    //    actual should ===(expected)
    expected should ===(actual)
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

  // Following are the tests of Numeric[Number]

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
