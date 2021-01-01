package com.phasmidsoftware.number.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Success, Try}

class FuzzyNumberSpec extends AnyFlatSpec with should.Matchers {

  private val numberOne = Number(1)
  private val bigOne = BigInt(1)
  private val ratOne = Rational.one
  private val doubleOne = 1.0

  behavior of "create"
  it should "yield 1 with absolute fuzz" in {
    val target = Number.create(Right(1), AbsoluteFuzz(0.01, Box))
    target should matchPattern { case FuzzyNumber(_, _, _) => }
    target.value should matchPattern { case Right(_) => }
    target.fuzz shouldBe Some(AbsoluteFuzz(0.01, Box))
  }
  it should "yield 1 with relative fuzz" in {
    val target = Number.create(Right(1), RelativeFuzz(0.01, Box))
    target should matchPattern { case FuzzyNumber(_, _, _) => }
    target.value should matchPattern { case Right(_) => }
    target.fuzz shouldBe Some(RelativeFuzz(0.01, Box))
  }

  behavior of "Fuzz.toString"
  it should "work for 1/0.5/Box" in {
    val target = AbsoluteFuzz(0.5, Box)
    target.toString(1) shouldBe "1.00[50]"
  }
  it should "work for 1/0.005/Box" in {
    val target = AbsoluteFuzz(0.005, Box)
    target.toString(1) shouldBe "1.0000[50]"
  }
  it should "work for 1/0.5/Gaussian" in {
    val target = AbsoluteFuzz(0.5, Gaussian)
    target.toString(1) shouldBe "1.00(50)"
  }
  it should "work for 1/0.005/Gaussian" in {
    val target = AbsoluteFuzz(0.005, Gaussian)
    target.toString(1) shouldBe "1.0000(50)"
  }

  behavior of "parse"
  it should "work for 1.*" in {
    val xy: Try[Number] = Number.parse("1.*")
    xy.get shouldBe Number.create(Right(1), AbsoluteFuzz(0.5, Box))
  }
  it should "work for 1.00(5)" in {
    val xy: Try[Number] = Number.parse("1.00(5)")
    xy.get shouldBe Number.create(Right(1), AbsoluteFuzz(0.05, Gaussian))
  }
  it should "work for 1.000(50)" in {
    val xy: Try[Number] = Number.parse("1.000(50)")
    xy.get shouldBe Number.create(Right(1), AbsoluteFuzz(0.05, Gaussian))
  }
  it should "work for 1.00(05)" in {
    val xy: Try[Number] = Number.parse("1.00(05)")
    xy.get shouldBe Number.create(Right(1), AbsoluteFuzz(0.05, Gaussian))
  }

  behavior of "plus"
  it should "add 1 and 2" in {
    val x = FuzzyNumber(Value.fromInt(1), Scalar, None)
    val y = Number(2)
    val z = x + y
    z.value shouldBe Right(3)
    z.factor shouldBe Scalar
    z.fuzz should matchPattern { case None => }
  }
  it should "add 1.* and 2" in {
    val xy: Try[Number] = Number.parse("1.*")
    val yy = Success(Number(2))
    val zy = for (x <- xy; y <- yy) yield x + y
    zy should matchPattern { case Success(_) => }
    zy.get.value shouldBe Right(3)
    zy.get.factor shouldBe Scalar
    zy.get.fuzz should matchPattern { case Some(AbsoluteFuzz(0.5, Box)) => }
  }
  it should "add 1 and 2.*" in {
    val xy = Number.parse("2.*")
    val yy = Success(Number(1))
    val zy = for (x <- xy; y <- yy) yield y + x
    zy should matchPattern { case Success(_) => }
    zy.get.value shouldBe Right(3)
    zy.get.factor shouldBe Scalar
    zy.get.fuzz should matchPattern { case Some(AbsoluteFuzz(0.5, Box)) => }
  }
  it should "add 1.* and 2.*" in {
    val xy = Number.parse("1.*")
    val yy = Number.parse("2.*")
    val zy = for (x <- xy; y <- yy) yield x + y
    zy should matchPattern { case Success(_) => }
    zy.get.value shouldBe Right(3)
    zy.get.factor shouldBe Scalar
    zy.get.fuzz should matchPattern { case Some(AbsoluteFuzz(0.5773502691896258, Gaussian)) => }
    zy.get.toString shouldBe "3.00(58)"
  }


  behavior of "times"
  it should "multiply 1 and 2" in {
    val x = FuzzyNumber(Value.fromInt(1), Scalar, None)
    val y = Number(2)
    val z = x * y
    z.value shouldBe Right(2)
    z.factor shouldBe Scalar
    z.fuzz should matchPattern { case None => }
  }
  it should "multiply 1.* and 2" in {
    val xy: Try[Number] = Number.parse("1.*")
    val yy = Success(Number(2))
    val zy = for (x <- xy; y <- yy) yield x * y
    zy should matchPattern { case Success(_) => }
    zy.get.value shouldBe Right(2)
    zy.get.factor shouldBe Scalar
    zy.get.fuzz should matchPattern { case Some(RelativeFuzz(0.25, Box)) => }
  }
  it should "multiply 1 and 2.*" in {
    val xy = Number.parse("2.*")
    val yy = Success(Number(1))
    val zy = for (x <- xy; y <- yy) yield x * y
    zy should matchPattern { case Success(_) => }
    zy.get.value shouldBe Right(2)
    zy.get.factor shouldBe Scalar
    zy.get.fuzz should matchPattern { case Some(RelativeFuzz(0.25, Box)) => }
  }
  it should "multiply 1.* and 2.*" in {
    val xy: Try[Number] = Number.parse("1.*")
    val yy = Number.parse("2.*")
    val zy = for (x <- xy; y <- yy) yield x * y
    zy should matchPattern { case Success(_) => }
    zy.get.value shouldBe Right(2)
    zy.get.factor shouldBe Scalar
    zy.get.fuzz should matchPattern { case Some(RelativeFuzz(0.20412414523193154, Gaussian)) => }
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
  it should "invert Double 2" in {
    val x = Number(Math.PI)
    x.invert shouldBe Number(1 / Math.PI)
  }

  behavior of "division"
  it should "divide 1 by 2" in {
    val x = numberOne
    val y = Number(2)
    (x / y) shouldBe Number(Rational.half)
  }
  it should "divide BigInt 1 by 2" in {
    val x = Number(bigOne)
    val y = Number(2)
    (x / y) shouldBe Number(Rational.half)
  }
  it should "divide Rational 1 by 2" in {
    val x = Number(Rational(1))
    val y = Number(2)
    (x / y) shouldBe Number(Rational.half)
  }
  it should "divide Double 1 by 2" in {
    val x = Number(doubleOne)
    val y = Number(Math.PI)
    (x / y) shouldBe Number(1 / Math.PI)
  }

  // Following are the tests of Ordering[Number]

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

  behavior of "sin"
  it should "work for 0" in {
    val target = Number(0, Pi)
    target.sin shouldBe Number(0, Scalar)
  }
  it should "work for 1/2" in {
    val target = Number(Rational.half, Pi)
    val sin = target.sin
    sin shouldBe Number(1, Scalar)
  }
  it should "work for 1/6" in {
    val target = Number(Rational(6).invert, Pi)
    target.sin shouldBe Number(Rational(1, 2), Scalar)
  }
}
