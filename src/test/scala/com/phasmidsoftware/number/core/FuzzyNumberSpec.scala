package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Expression.ExpressionOps
import org.scalactic.Equality
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Success, Try}

class FuzzyNumberSpec extends AnyFlatSpec with should.Matchers {

  implicit object FuzzyNumberEquality extends Equality[FuzzyNumber] {
    def areEqual(a: FuzzyNumber, b: Any): Boolean = b match {
      case n: Number => a.compare(n) == 0
      case _ => false
    }
  }

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
    val z = x add y
    z.value shouldBe Right(3)
    z.factor shouldBe Scalar
    z.fuzz should matchPattern { case None => }
  }
  it should "add 1.* and 2" in {
    val xy: Try[Number] = Number.parse("1.*")
    val yy = Success(Number(2))
    val zy = for (x <- xy; y <- yy) yield x add y
    zy should matchPattern { case Success(_) => }
    zy.get.value shouldBe Right(3)
    zy.get.factor shouldBe Scalar
    zy.get.fuzz should matchPattern { case Some(AbsoluteFuzz(0.5, Box)) => }
  }
  it should "add 1 and 2.*" in {
    val xy = Number.parse("2.*")
    val yy = Success(Number(1))
    val zy = for (x <- xy; y <- yy) yield y add x
    zy should matchPattern { case Success(_) => }
    zy.get.value shouldBe Right(3)
    zy.get.factor shouldBe Scalar
    zy.get.fuzz should matchPattern { case Some(AbsoluteFuzz(0.5, Box)) => }
  }
  it should "add 1.* and 2.*" in {
    val xy = Number.parse("1.*")
    xy.get.fuzz should matchPattern { case Some(AbsoluteFuzz(0.5, Box)) => }
    xy.get.fuzz.get.normalizeShape should matchPattern { case AbsoluteFuzz(0.2886751345948129, Gaussian) => }
    val yy = Number.parse("2.*")
    yy.get.fuzz should matchPattern { case Some(AbsoluteFuzz(0.5, Box)) => }
    val zy = for (x <- xy; y <- yy) yield x add y
    zy should matchPattern { case Success(_) => }
    zy.get.value shouldBe Right(3)
    zy.get.factor shouldBe Scalar
    zy.get.fuzz should matchPattern { case Some(AbsoluteFuzz(0.4082482904638631, Gaussian)) => }
    zy.get.toString shouldBe "3.00(41)"
  }

  behavior of "times"
  ignore should "multiply 1 and 2" in {
    val x = FuzzyNumber(Value.fromInt(1), Scalar, None)
    val y = Number(2)
    val z: Number = (x * y).materialize
    z.value shouldBe Right(2)
    z.factor shouldBe Scalar
    z.fuzz should matchPattern { case None => }
  }
  ignore should "multiply 1.* and 2" in {
    val xy: Try[Number] = Number.parse("1.*")
    val yy = Success(Number(2))
    val zy = for (x <- xy; y <- yy) yield x * y
    zy should matchPattern { case Success(_) => }
    val result = zy.get.materialize
    result.value shouldBe Right(2)
    result.factor shouldBe Scalar
    result.fuzz should matchPattern { case Some(RelativeFuzz(0.5, Box)) => }
  }
  ignore should "multiply 1 and 2.*" in {
    val xy = Number.parse("2.*")
    val yy = Success(Number(1))
    val zy: Try[Expression] = for (x <- xy; y <- yy) yield x * y
    zy should matchPattern { case Success(_) => }
    val result = zy.get.materialize
    result.value shouldBe Right(2)
    result.factor shouldBe Scalar
    result.fuzz should matchPattern { case Some(RelativeFuzz(0.25, Box)) => }
  }
  ignore should "multiply 1.* and 2.*" in {
    val xy: Try[Number] = Number.parse("1.*")
    xy.get.fuzz.get.normalizeShape.normalize(1, relative = true) should matchPattern { case Some(RelativeFuzz(0.2886751345948129, Gaussian)) => }
    val yy = Number.parse("2.*")
    yy.get.fuzz.get.normalizeShape.normalize(2, relative = true) should matchPattern { case Some(RelativeFuzz(0.14433756729740646, Gaussian)) => }
    val zy = for (x <- xy; y <- yy) yield x * y
    zy should matchPattern { case Success(_) => }
    val result = zy.get.materialize
    result.value shouldBe Right(2)
    result.factor shouldBe Scalar
    result.fuzz should matchPattern { case Some(RelativeFuzz(0.3818813079129867, Gaussian)) => }
  }

  behavior of "-"
  it should "work for 1.*" in {
    val xy: Try[Number] = Number.parse("1.*")
    val zy = for (x <- xy) yield -x
    zy should matchPattern { case Success(_) => }
    zy.get.value shouldBe Right(-1)
    zy.get.factor shouldBe Scalar
    zy.get.fuzz should matchPattern { case Some(AbsoluteFuzz(0.5, Box)) => }
  }

  behavior of "negate"
  it should "work for 1.*" in {
    val xy: Try[Number] = Number.parse("1.*")
    val zy = for (x <- xy) yield Number.negate(x)
    zy should matchPattern { case Success(_) => }
    zy.get.value shouldBe Right(-1)
    zy.get.factor shouldBe Scalar
    zy.get.fuzz should matchPattern { case Some(AbsoluteFuzz(0.5, Box)) => }
  }

  behavior of "power"
  ignore should "work for (fuzzy 3)^2 (i.e. an constant Int power)" in {
    val x: FuzzyNumber = FuzzyNumber(Value.fromInt(3), Scalar, Some(RelativeFuzz(0.1, Gaussian)))
    val z: Number = (x ^ 2).materialize
    z.value shouldBe Right(9)
    z.factor shouldBe Scalar
    z.fuzz should matchPattern { case Some(RelativeFuzz(_, Gaussian)) => }
    z.fuzz.get match {
      case RelativeFuzz(m, Gaussian) => m shouldBe 0.06666666666666667
    }
  }
  ignore should "work for 2**2 (i.e. an constant Int power)" in {
    val xy: Try[Number] = Number.parse("2.0*")
    xy.get.fuzz should matchPattern { case Some(AbsoluteFuzz(0.05, Box)) => }
    xy.get.fuzz.get.normalizeShape.normalize(1, relative = true) should matchPattern { case Some(RelativeFuzz(0.028867513459481294, Gaussian)) => }
    val zy = for (x <- xy) yield (x ^ 2).materialize
    zy should matchPattern { case Success(_) => }
    val result: Number = zy.get
    result.value shouldBe Right(4)
    result.factor shouldBe Scalar
    result.fuzz.isDefined shouldBe true
    result.fuzz.get match {
      case RelativeFuzz(m, Box) => m shouldBe 0.025
    }
  }
  it should "work for 2**2 (i.e. a fuzzy Number power)" in {
    val xy: Try[Number] = Number.parse("2.0*")
    xy.get.fuzz should matchPattern { case Some(AbsoluteFuzz(0.05, Box)) => }
    xy.get.fuzz.get.normalizeShape.normalize(1, relative = true) should matchPattern { case Some(RelativeFuzz(0.028867513459481294, Gaussian)) => }
    val exponent = xy.get
    val zy = for (x <- xy) yield x power exponent
    zy should matchPattern { case Success(_) => }
    zy.get.value shouldBe Right(4)
    zy.get.factor shouldBe Scalar
    zy.get.fuzz should matchPattern { case Some(RelativeFuzz(_, Gaussian)) => }
    zy.get.fuzz.get match {
      case RelativeFuzz(m, Gaussian) => m shouldBe 0.02443847451184851 +- 0.00000000000000001
    }
  }

  behavior of "compare"
  it should "find 1.* equivalent to 1.*" in {
    val xy: Try[Number] = Number.parse("1.*")
    val yy: Try[Number] = Number.parse("1.05*")
    val zy = for (x <- xy; y <- yy) yield Number.doCompare(x, y)
    zy should matchPattern { case Success(0) => }
  }
  it should "find 1 smaller than 2" in {
    val xy: Try[Number] = Number.parse("1.*")
    val yy: Try[Number] = Number.parse("2.*")
    val zy = for (x <- xy; y <- yy) yield Number.doCompare(x, y)
    zy should matchPattern { case Success(-1) => }
  }

  behavior of "sin"
  it should "work for 0" in {
    val target = Number(0, Pi)
    target.sin shouldBe Number(0, Scalar)
  }
  it should "work for 3.141592653589793" in {
    val target = Number("3.1415926535897932384626433")
    val result = target.sin
    result.fuzzyCompare(Number.zero, 0.1) shouldBe 0
  }
  it should "work for 3.141592653589793 backwards" in {
    val target = Number("3.1415926535897932384626433")
    val result = target.sin
    Number.zero.fuzzyCompare(result, 0.1) shouldBe 0
  }

  // Following are the tests of Ordering[Number]

  behavior of "compare"
  it should "work for 1, 1" in {
    val x = Number.one
    val y = Number.one
    implicitly[Numeric[Number]].compare(x, y) shouldBe 0
  }
}
