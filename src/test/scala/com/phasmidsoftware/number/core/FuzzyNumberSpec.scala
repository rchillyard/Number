package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Constants.{sGamma, sPhi}
import com.phasmidsoftware.number.core.Field.convertToNumber
import com.phasmidsoftware.number.core.Fuzziness.showPercentage
import com.phasmidsoftware.number.core.Number.{negate, twoPi}
import com.phasmidsoftware.number.core.Rational.RationalHelper
import com.phasmidsoftware.number.expression.Expression.ExpressionOps
import com.phasmidsoftware.number.expression.{ConstPi, Expression, Literal}
import org.scalactic.Equality
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Success, Try}

class FuzzyNumberSpec extends AnyFlatSpec with should.Matchers {

  implicit object FuzzyNumberEquality extends Equality[FuzzyNumber] {
    def areEqual(a: FuzzyNumber, b: Any): Boolean = b match {
      case n: GeneralNumber => a.compare(n) == 0
      case _ => false
    }
  }

  private val sMu = "1836.15267343(11)" // (dimensionless)

  behavior of "create"
  it should "yield 1 with absolute fuzz" in {
    val target = Number.create(Right(1), AbsoluteFuzz(0.01, Box))
    target should matchPattern { case FuzzyNumber(_, _, _) => }
    target.nominalValue should matchPattern { case Right(_) => }
    target.fuzz shouldBe Some(AbsoluteFuzz(0.01, Box))
  }
  it should "yield 1 with relative fuzz" in {
    val target = Number.create(Right(1), RelativeFuzz(0.01, Box))
    target should matchPattern { case FuzzyNumber(_, _, _) => }
    target.nominalValue should matchPattern { case Right(_) => }
    target.fuzz shouldBe Some(RelativeFuzz(0.01, Box))
  }

  behavior of "Fuzz.toString"
  it should "work for 1/0.5/Box" in {
    val target = AbsoluteFuzz(0.5, Box)
    target.toString(1) shouldBe(true, "1.0[5]")
  }
  it should "work for 1/0.005/Box" in {
    val target = AbsoluteFuzz(0.005, Box)
    target.toString(1) shouldBe(true, "1.000[5]")
  }
  it should "work for 1/0.5/Gaussian" in {
    val target = AbsoluteFuzz(0.5, Gaussian)
    target.toString(1) shouldBe(true, "1.0(5)")
  }
  it should "work for 1/0.005/Gaussian" in {
    val target = AbsoluteFuzz(0.005, Gaussian)
    target.toString(1) shouldBe(true, "1.000(5)")
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
  it should "parse gamma" in {
    val zy = Number.parse(sGamma)
    zy should matchPattern { case Success(_) => }
    val z = zy.get
    z.isExact shouldBe false
    z.nominalValue should matchPattern { case Left(Right(Rational(_, _))) => }
    z.fuzz.get shouldBe AbsoluteFuzz(5.0E-51, Box)
    z.toNominalDouble.get shouldBe 0.5772156649015329 +- 1E-14
    // NOTE that if you had to force z to be a Number based on a Double nominalValue,
    // then of course, we would lose most of the 50 places of decimals.
  }
  it should "parse phi" in {
    val zy = Number.parse(sPhi)
    zy should matchPattern { case Success(_) => }
    val z = zy.get
    z.isExact shouldBe false
    z.fuzz.get shouldBe AbsoluteFuzz(5.0E-105, Box)
    z.toNominalDouble.get shouldBe 1.618033988749894 +- 1E-13
  }
  it should "parse G" in {
    val z = Number.parse("6.67430(15)E-11")
    z should matchPattern { case Success(_) => }
    z.get.isExact shouldBe false
    z.get.fuzz.get shouldBe AbsoluteFuzz(1.5E-15, Gaussian)
  }
  val sAlpha = "0.0072973525693(11)"
  it should "parse alpha" in {
    val z = Number.parse(sAlpha)
    z should matchPattern { case Success(_) => }
    z.get.isExact shouldBe false
    z.get.fuzz.get shouldBe AbsoluteFuzz(1.1E-12, Gaussian)
  }
  it should "parse mu" in {
    val z = Number.parse(sMu)
    z should matchPattern { case Success(_) => }
    z.get.isExact shouldBe false
    z.get.fuzz.get shouldBe AbsoluteFuzz(1.1E-7, Gaussian)
  }


  behavior of "plus"

  it should "add 1 and 2" in {
    val x = FuzzyNumber(Value.fromInt(1), PureNumber, None)
    val z = convertToNumber(x add Constants.two)
    z.nominalValue shouldBe Right(3)
    z.factor shouldBe PureNumber
    z.fuzz should matchPattern { case None => }
  }
  it should "add 1.* and 2" in {
    val xy: Try[Number] = Number.parse("1.*")
    val yy = Success(Constants.two)
    val zy = for (x <- xy; y <- yy) yield convertToNumber(x add y)
    zy should matchPattern { case Success(_) => }
    zy.get.nominalValue shouldBe Right(3)
    zy.get.factor shouldBe PureNumber
    zy.get.fuzz should matchPattern { case Some(AbsoluteFuzz(0.5, Box)) => }
  }
  it should "add 1 and 2.*" in {
    val xy = Real.parse("2.*")
    val yy = Success(Number(1))
    val zy = for (x <- xy; y <- yy) yield convertToNumber(y add x)
    zy should matchPattern { case Success(_) => }
    zy.get.nominalValue shouldBe Right(3)
    zy.get.factor shouldBe PureNumber
    zy.get.fuzz should matchPattern { case Some(AbsoluteFuzz(0.5, Box)) => }
  }
  it should "add 1.* and 2.*" in {
    val xy = Number.parse("1.*")
    xy.get.fuzz should matchPattern { case Some(AbsoluteFuzz(0.5, Box)) => }
    xy.get.fuzz.get.normalizeShape should matchPattern { case AbsoluteFuzz(0.2886751345948129, Gaussian) => }
    val yy = Number.parse("2.*")
    yy.get.fuzz should matchPattern { case Some(AbsoluteFuzz(0.5, Box)) => }
    val zy: Try[Number] = for (x <- xy; y <- yy) yield convertToNumber(x add Real(y))
    zy should matchPattern { case Success(_) => }
    zy.get.nominalValue shouldBe Right(3)
    zy.get.factor shouldBe PureNumber
    zy.get.fuzz should matchPattern { case Some(AbsoluteFuzz(0.4082482904638631, Gaussian)) => }
    zy.get.toString shouldBe "3.00(41)"
  }

  behavior of "times"
  it should "multiply 1 and 2" in {
    val x = FuzzyNumber(Value.fromInt(1), PureNumber, None)
    val y = Constants.two
    val z: Number = convertToNumber((Literal(x) * y).materialize)
    z.nominalValue shouldBe Right(2)
    z.factor shouldBe PureNumber
    z.fuzz should matchPattern { case None => }
  }
  it should "multiply 1.* and 2" in {
    val xy: Try[Number] = Number.parse("1.*")
    val yy = Success(Constants.two)
    val zy = for (x <- xy; y <- yy) yield Literal(x) * y
    zy should matchPattern { case Success(_) => }
    val result = convertToNumber(zy.get.materialize)
    result.nominalValue shouldBe Right(2)
    result.factor shouldBe PureNumber
    result.fuzz should matchPattern { case Some(AbsoluteFuzz(1.0, Box)) => } // was 0.5
  }
  it should "multiply 1 and 2.*" in {
    val xy = Number.parse("2.*")
    val yy = Success(Constants.one)
    val zy: Try[Expression] = for (x <- xy; y <- yy) yield Literal(x) * y
    zy should matchPattern { case Success(_) => }
    val result = convertToNumber(zy.get.materialize)
    result.nominalValue shouldBe Right(2)
    result.factor shouldBe PureNumber
    result.fuzz should matchPattern { case Some(AbsoluteFuzz(0.5, Box)) => }
  }
  it should "multiply 1.* and 2.*" in {
    val xy: Try[Number] = Number.parse("1.*")
    val yy = Real.parse("2.*")
    val zy: Try[Field] = for (x <- xy; y <- yy) yield x.multiply(y)
    zy should matchPattern { case Success(_) => }
    val result = convertToNumber(zy.get)
    result.nominalValue shouldBe Right(2)
    result.factor shouldBe PureNumber
    result.fuzz should matchPattern { case Some(AbsoluteFuzz(1.5, Box)) => } // NOTE was 0.75
  }
  it should "multiply 1.* and 2.* with normalization of fuzz" in {
    val xy: Try[Number] = Number.parse("1.*")
    xy.get.fuzz.get.normalizeShape.normalize(1, relative = true) should matchPattern { case Some(RelativeFuzz(0.2886751345948129, Gaussian)) => }
    val yy = Number.parse("2.*")
    yy.get.fuzz.get.normalizeShape.normalize(2, relative = true) should matchPattern { case Some(RelativeFuzz(0.14433756729740646, Gaussian)) => }
    val zy = for (x <- xy; y <- yy) yield Literal(x) * Real(y)
    zy should matchPattern { case Success(_) => }
    val result = convertToNumber(zy.get.materialize)
    result.nominalValue shouldBe Right(2)
    result.factor shouldBe PureNumber
    result.fuzz should matchPattern { case Some(AbsoluteFuzz(1.5, Box)) => }  // NOTE this was formerly 0.75 // CHECK it
  }
  it should "work for (fuzzy 3)^2 (i.e. an constant Int power (Box))" in {
    val x: FuzzyNumber = FuzzyNumber(Value.fromInt(3), PureNumber, Some(RelativeFuzz(0.1, Box)))
    val z: Number = x.doMultiply(x)
    z.nominalValue shouldBe Right(9)
    z.factor shouldBe PureNumber
    //    z.fuzz should matchPattern { case Some(RelativeFuzz(0.11547005383792518, Gaussian)) => }
    z.fuzz.get match {
      case RelativeFuzz(m, Box) => m shouldBe 0.2
    }
  }

  behavior of "-"
  it should "work for 1.*" in {
    val xy: Try[Number] = Number.parse("1.*")
    val zy = for (x <- xy) yield convertToNumber(-x)
    zy should matchPattern { case Success(_) => }
    zy.get.nominalValue shouldBe Right(-1)
    zy.get.factor shouldBe PureNumber
    zy.get.fuzz should matchPattern { case Some(AbsoluteFuzz(0.5, Box)) => }
//    zy.get.fuzz should matchPattern { case Some(RelativeFuzz(0.5, Box)) => }
  }

  behavior of "negate"
  it should "work for 1.*" in {
    val xy: Try[Number] = Number.parse("1.*")
    val zy = for (x <- xy) yield Number.negate(x)
    zy should matchPattern { case Success(_) => }
    zy.get.nominalValue shouldBe Right(-1)
    zy.get.factor shouldBe PureNumber
    zy.get.fuzz should matchPattern { case Some(AbsoluteFuzz(0.5, Box)) => }
  }

  behavior of "power"
  it should "work for (fuzzy 3)^2 (i.e. an constant Int power (Box))" in {
    val x: FuzzyNumber = FuzzyNumber(Value.fromInt(3), PureNumber, Some(RelativeFuzz(0.1, Box)))
    val z: Number = convertToNumber((Literal(x) ^ 2).materialize)
    z.nominalValue shouldBe Right(9)
    z.factor shouldBe PureNumber
    z.fuzz should matchPattern { case Some(RelativeFuzz(_, Box)) => }
    z.fuzz.get match {
      case RelativeFuzz(m, Box) => m shouldBe 0.2 +- 0.00001
    }
  }
  it should "work for (fuzzy 3)^2 (i.e. an constant Int power (Gaussian))" in {
    val x: FuzzyNumber = FuzzyNumber(Value.fromInt(3), PureNumber, Some(RelativeFuzz(0.1, Gaussian)))
    val z: Number = convertToNumber((Literal(x) ^ 2).materialize)
    z.nominalValue shouldBe Right(9)
    z.factor shouldBe PureNumber
    z.fuzz should matchPattern { case Some(RelativeFuzz(_, Gaussian)) => }
    z.fuzz.get match {
      case RelativeFuzz(m, Gaussian) => m shouldBe 0.2 +- 0.00001
    }
  }
  it should "work for 2**2 (i.e. an constant Int power)" in {
    val xy: Try[Number] = Number.parse("2.0*")
    xy.get.fuzz should matchPattern { case Some(AbsoluteFuzz(0.05, Box)) => }
    xy.get.fuzz.get.normalizeShape.normalize(1, relative = true) should matchPattern { case Some(RelativeFuzz(0.028867513459481294, Gaussian)) => }
    val zy = for (x <- xy) yield (Literal(x) ^ 2).materialize
    zy should matchPattern { case Success(_) => }
    val result: Number = convertToNumber(zy.get)
    result.nominalValue shouldBe Right(4)
    result.factor shouldBe PureNumber
    result.fuzz.isDefined shouldBe true
    result.fuzz.get match {
      case RelativeFuzz(m, Box) => m shouldBe 0.05
    }
  }
  it should "work for 2**2 (i.e. a fuzzy Number power)" in {
    val xy: Try[Number] = Number.parse("2.0*")
    xy.get.fuzz should matchPattern { case Some(AbsoluteFuzz(0.05, Box)) => }
    xy.get.fuzz.get.normalizeShape.normalize(1, relative = true) should matchPattern { case Some(RelativeFuzz(0.028867513459481294, Gaussian)) => }
    val exponent = xy.get
    val zy = for (x <- xy) yield x doPower exponent
    zy should matchPattern { case Success(_) => }
    zy.get.nominalValue shouldBe Right(4)
    zy.get.factor shouldBe PureNumber
    zy.get.fuzz should matchPattern { case Some(RelativeFuzz(_, Box)) => }
    zy.get.fuzz.get match {
      case RelativeFuzz(m, Box) => m shouldBe 0.08465735902799726 +- 0.00000000000000001
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
    val target = Number(0, Radian)
    target.sin shouldBe Number(0, PureNumber)
  }
  it should "work for pi" in {
    val target = Number.pi
    target.sin shouldBe Number(0, PureNumber)
  }
  it should "work for 2pi" in {
    val target = Number.twoPi
    target.sin shouldBe Number(0, PureNumber)
  }
  it should "work for pi/2" in {
    val target = Number.piBy2
    target.sin shouldBe Number(1, PureNumber)
  }
  it should "work for 3pi/2" in {
    val target: Option[Number] = (ConstPi / 2 * 3).materialize.asNumber
    target.map(_.sin) shouldBe Some(Number(-1, PureNumber))
  }
  it should "work for 3.141592653589793" in {
    val target = Number("3.1415926535897932384626433")
    val result = target.sin
    // NOTE: this is rather a low probability value (normally, we use 0.5)
    result.asInstanceOf[GeneralNumber].fuzzyCompare(Number.zero, 0.1) shouldBe 0
  }
  it should "work for 3.141592653589793 backwards" in {
    val target = Number("3.1415926535897932384626433")
    val result = target.sin
    // NOTE: this is rather a low probability value (normally, we use 0.5)
    Number.zero.asInstanceOf[GeneralNumber].fuzzyCompare(result, 0.1) shouldBe 0
  }

  behavior of "cos"
  it should "work for 0" in {
    val target = Number(0, Radian)
    target.cos shouldBe Number(1, PureNumber)
  }
  it should "work for pi" in {
    val target = Number.pi
    target.cos shouldBe Number(-1, PureNumber)
  }
  it should "work for 2pi" in {
    // NOTE this involves evaluating sin of 2.5 pi
    val target = Number.twoPi
    target.cos shouldBe Number(1, PureNumber)
  }
  it should "work for pi/2" in {
    val target = Number.piBy2
    target.cos shouldBe Number(0, PureNumber)
  }
  it should "work for 3pi/2" in {
    val target = (Number.piBy2 doMultiply Number(3)).asNumber
    target.map(_.cos) shouldBe Some(Number(0, PureNumber))
  }
  it should "work for 3.141592653589793" in {
    val target = Number("3.1415926535897932384626433")
    val result = target.cos
    // NOTE: this is rather a low probability value (normally, we use 0.5)
    result.asInstanceOf[GeneralNumber].fuzzyCompare(negate(Number.one), 0.1) shouldBe 0
  }
  it should "work for 3.141592653589793 backwards" in {
    val target = Number("3.1415926535897932384626433")
    val result = target.cos
    // NOTE: this is rather a low probability value (normally, we use 0.5)
    negate(Number.one).asInstanceOf[GeneralNumber].fuzzyCompare(result, 0.1) shouldBe 0
  }

  behavior of "tan"
  it should "work for 0" in {
    val target = Number(0, Radian)
    target.tan shouldBe Number(0, PureNumber)
  }
  it should "work for pi" in {
    val target = Number.pi
    target.tan shouldBe Number(0, PureNumber)
  }
  it should "work for 2pi" in {
    val target = Number.twoPi
    target.tan shouldBe Number(0, PureNumber)
  }
  it should "work for pi/2" in {
    val target = Number.piBy2
    target.tan shouldBe Number(Rational.infinity, PureNumber)
  }
  it should "work for 3pi/2" in {
    val target = Number.piBy2 doMultiply Number(3)
    target.tan shouldBe Number(Rational.negInfinity, PureNumber)
  }
  it should "work for 3.141592653589793" in {
    val target = Number("3.1415926535897932384626433")
    val result = target.tan
    // NOTE: this is rather a low probability value (normally, we use 0.5)
    result.asInstanceOf[GeneralNumber].fuzzyCompare(Number.zero, 0.1) shouldBe 0
  }
  it should "work for 3.141592653589793 backwards" in {
    val target = Number("3.1415926535897932384626433")
    val result = target.tan
    // NOTE: this is rather a low probability value (normally, we use 0.5)
    Number.zero.asInstanceOf[GeneralNumber].fuzzyCompare(result, 0.1) shouldBe 0
  }

  behavior of "exp"
  it should "work for non-exact 1" in {
    val x = Number("1.00000000000*", NatLog)
    x.normalize.render shouldBe "2.718281828459[14]"
  }

  // Following are the tests of Ordering[Number]

  behavior of "compare"
  it should "work for 1, 1" in {
    val x = Number.one
    val y = Number.one
    implicitly[Numeric[Number]].compare(x, y) shouldBe 0
  }

  behavior of "same"

  import FuzzyNumber._

  it should "think 1 and 1 are the same" in {
    val x = Number.one
    val y = Number.one
    implicitly[Fuzzy[Number]].same(0.95)(x, y) shouldBe true
  }
  it should "think pi and pi are the same" in {
    val x = Number.parse("3.142").get
    val y = Number.pi
    // NOTE we check that these two numbers are the same with a confidence of 80%
    implicitly[Fuzzy[Number]].same(0.8)(x, y) shouldBe true
    // NOTE we check that these two numbers are not the same with a confidence of 90%
    implicitly[Fuzzy[Number]].same(0.9)(x, y) shouldBe false
  }

  behavior of "isProbablyZero"
  it should "think pi and pi are the same (1)" in {
    val x: Number = Number.parse("3.142").get
    val y: Number = Number.pi
    val zo: Option[Number] = (x doAdd negate(y)).asNumber
    zo.isDefined shouldBe true
    val z = zo.get
    z.isProbablyZero(0) shouldBe true
    z.isProbablyZero(1) shouldBe false
    z.isProbablyZero() shouldBe true
    z.isProbablyZero(0.895) shouldBe false
  }
  it should "think pi and pi are the same (2)" in {
    val x = Number.parse("3.1415927").get
    val y = Number.pi
    val zo = (x doSubtract y).asNumber
    zo.isDefined shouldBe true
    val z = zo.get
    z.isProbablyZero(0) shouldBe true
    z.isProbablyZero(1) shouldBe false
    z.isProbablyZero() shouldBe true
    z.isProbablyZero(0.875) shouldBe false
  }

  behavior of "make(Fuzziness)"
  it should "work" in {
    val fuzz = RelativeFuzz(1E-15, Box)
    // TODO do this through pattern matching
    val n: FuzzyNumber = FuzzyNumber(Value.fromInt(1), PureNumber, None).addFuzz(fuzz).asInstanceOf[FuzzyNumber]
    val op = MonadicOperationExp
    val r: Option[Value] = Operations.doTransformValueMonadic(n.nominalValue)(op.functions)
    r.isDefined shouldBe true
    val q: Number = n.make(r.get, PureNumber)
    val xo = q.toNominalDouble
    xo.isDefined shouldBe true
    val x = xo.get
    val normalized: Field = q.make(Fuzziness.map[Double, Double, Double](1, x, relative = true, op.relativeFuzz, Some(fuzz))).normalize
    val z = normalized.asNumber
    val (_, w) = z.get.fuzz.get.toString(x)
    w.substring(0, 17) + w.substring(18, 22) shouldBe "2.718281828459045[27]"
  }

  it should "implement asComparedWith" in {
    val n: Number = Number(r"22/7")
    showPercentage(n.asComparedWith(Number.pi)) shouldBe "0.020%"
    showPercentage(Number.pi.asComparedWith(n)) shouldBe "0.020%"
  }

  behavior of "foucault"
  it should "do part of the calculation" in {
    val t = Number("16.5*")
    //    println(s"y fuzz: ${t.fuzz.map(_.normalize(16.5, relative = true))}")
    val square: Option[Number] = (t doPower 2).asNumber
//    println(s"square fuzz: ${square.get.fuzz}")
    square.get.fuzz should matchPattern { case Some(RelativeFuzz(_, Box)) => }
    square.get.fuzz.get.asInstanceOf[RelativeFuzz[Double]].tolerance shouldBe 0.006 +- 0.001
  }

  it should "calculate length of Foucault's pendulum but with relative box fuzziness" in {
    // NOTE this is here to support Foucault1.sc
    val g = Number("9.81*")
    //        println(s"g fuzz: ${g.fuzz.map(_.normalize(9.81, relative = true))}")
    val t = Number("16.5*")
    //          println(s"t fuzz: ${t.fuzz.map(_.normalize(16.5, relative = true))}")
    val tScaled: Option[Number] = (t doDivide twoPi).asNumber
    //        println(s"tScaled fuzz: ${tScaled.get.fuzz}")
    val square: Option[Number] = tScaled.flatMap(x => (x doPower 2).asNumber)
//    println(s"square fuzz: ${square.get.fuzz}")

    // NOTE Relative (Box) fuzz should be approximately 0.006 (twice the value shown above for t)
    square.get.fuzz should matchPattern { case Some(RelativeFuzz(_, Box)) => }
    square.get.fuzz.get.asInstanceOf[RelativeFuzz[Double]].tolerance shouldBe 0.006 +- 0.001
    val length: Option[Number] = square flatMap (x => (x doMultiply g).asNumber)
    length.get.fuzz shouldBe Some(RelativeFuzz(0.00657029005653496, Box))
//    println(length.get)
  }
  it should "calculate length of Foucault's pendulum" in {
    // NOTE this is here to support Foucault2.sc
    val g = Number("9.81*")
    //    println(s"g fuzz: ${g.fuzz.map(_.normalize(9.81, relative = true))}")
    val t = Number("16.487(41)")
//      println(s"y fuzz: ${t.fuzz.map(_.normalize(16.5, relative = true))}")

    val tScaled: Option[Number] = (t doDivide twoPi).asNumber
    //    println(s"tScaled fuzz: ${tScaled.get.fuzz}")
    val square: Option[Number] = tScaled.flatMap(x => (x doPower 2).asNumber)
//    println(s"square fuzz: ${square.get.fuzz}")

    square.get.fuzz should matchPattern { case Some(RelativeFuzz(_, Gaussian)) => }
    square.get.fuzz.get.asInstanceOf[RelativeFuzz[Double]].tolerance shouldBe 0.005 +- 0.0002

    val length: Option[Number] = square flatMap (x => (x doMultiply g).asNumber)
    length.get.fuzz shouldBe Some(RelativeFuzz(0.005139383648325369, Gaussian))

//    println(length.get)
  }
}
