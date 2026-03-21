/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.inner

import com.phasmidsoftware.number.core.numerical.{AbsoluteFuzz, FuzzyNumber, *}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.Success

class OperationsSpec extends AnyFlatSpec with should.Matchers {

  behavior of "MonadicOperationModulate (numbers)"
  it should "work" in {
    val z = MonadicOperationModulate(-1, 1, inclusive = true, false)
    val (fInt, fRational, fDouble) = z.functions
    fInt(3) shouldBe Success(-1)
    fRational(Rational(3)) shouldBe Success(Rational.negOne)
    fDouble(3) shouldBe Success(-1.0)
  }
  it should "work with inclusive true" in {
    val z = MonadicOperationModulate(-1, 1, inclusive = true, false)
    val (fInt, _, _) = z.functions
    fInt(-1) shouldBe Success(-1)
  }
  it should "work with inclusive false" in {
    val z = MonadicOperationModulate(-1, 1, inclusive = false, false)
    val (fInt, _, _) = z.functions
    fInt(-1) shouldBe Success(-1)
  }

  behavior of "MonadicOperationModulate (angles)"
  it should "work" in {
    val z = MonadicOperationModulate(-1, 1, inclusive = true, true)
    val (fInt, fRational, fDouble) = z.functions
    fInt(3) shouldBe Success(1)
    fRational(Rational(3)) shouldBe Success(Rational.one)
    fDouble(3) shouldBe Success(1.0)
  }
  it should "work with inclusive true" in {
    val z = MonadicOperationModulate(-1, 1, inclusive = true, true)
    val (fInt, _, _) = z.functions
    fInt(-1) shouldBe Success(-1)
  }
  it should "work with inclusive false" in {
    val z = MonadicOperationModulate(-1, 1, inclusive = false, true)
    val (fInt, _, _) = z.functions
    fInt(-1) shouldBe Success(1)
  }

  behavior of "MonadicOperationNegate"
  it should "work for Int" in {
    val (fInt, _, _) = MonadicOperationNegate.functions
    fInt(3) shouldBe Success(-3)
    fInt(0) shouldBe Success(0)
    fInt(-3) shouldBe Success(3)
  }
  it should "work for Rational" in {
    val (_, fRational, _) = MonadicOperationNegate.functions
    fRational(Rational(3, 2)) shouldBe Success(Rational(-3, 2))
    fRational(Rational.zero) shouldBe Success(Rational.zero)
  }
  it should "work for Double" in {
    val (_, _, fDouble) = MonadicOperationNegate.functions
    fDouble(3.0) shouldBe Success(-3.0)
    fDouble(-3.0) shouldBe Success(3.0)
  }
  it should "have relativeFuzz of 1" in {
    MonadicOperationNegate.relativeFuzz(0.01) shouldBe 1.0
    MonadicOperationNegate.relativeFuzz(0.5) shouldBe 1.0
  }
  it should "have no precision loss" in {
    MonadicOperationNegate.fuzz shouldBe None
  }

  behavior of "MonadicOperationInvert"
  it should "work for Int" in {
    val (fInt, _, _) = MonadicOperationInvert.functions
    fInt(1) shouldBe Success(1)
    fInt(2).isFailure shouldBe true
  }
  it should "work for Rational" in {
    val (_, fRational, _) = MonadicOperationInvert.functions
    fRational(Rational(2, 3)) shouldBe Success(Rational(3, 2))
    fRational(Rational(4)) shouldBe Success(Rational(1, 4))
  }
  it should "work for Double" in {
    val (_, _, fDouble) = MonadicOperationInvert.functions
    fDouble(2.0) shouldBe Success(0.5)
    fDouble(4.0) shouldBe Success(0.25)
  }
  it should "have relativeFuzz of -1" in {
    MonadicOperationInvert.relativeFuzz(0.01) shouldBe -1.0
    MonadicOperationInvert.relativeFuzz(2.0) shouldBe -1.0
  }
  it should "have precision loss of 0" in {
    MonadicOperationInvert.fuzz shouldBe Some(0)
  }

  behavior of "MonadicOperationExp"
  it should "work for Int" in {
    val (fInt, _, _) = MonadicOperationExp.functions
    fInt(0) shouldBe Success(1)
    fInt(1).isFailure shouldBe true
  }
  it should "work for Double" in {
    val (_, _, fDouble) = MonadicOperationExp.functions
    fDouble(0.0) shouldBe Success(1.0)
    fDouble(1.0).get shouldBe math.E +- 1E-10
    fDouble(-1.0).get shouldBe 1.0 / math.E +- 1E-10
  }
  it should "have relativeFuzz equal to input" in {
    MonadicOperationExp.relativeFuzz(2.0) shouldBe 2.0
    MonadicOperationExp.relativeFuzz(0.5) shouldBe 0.5
  }
  it should "have precision loss of 3" in {
    MonadicOperationExp.fuzz shouldBe Some(3)
  }

  behavior of "MonadicOperationLog"
  it should "work for Int" in {
    val (fInt, _, _) = MonadicOperationLog.functions
    fInt(1) shouldBe Success(0)
    fInt(2).isFailure shouldBe true
  }
  it should "work for Double" in {
    val (_, _, fDouble) = MonadicOperationLog.functions
    fDouble(1.0) shouldBe Success(0.0)
    fDouble(math.E).get shouldBe 1.0 +- 1E-10
  }
  it should "have correct relativeFuzz" in {
    // relativeFuzz = 1/ln(x)
    MonadicOperationLog.relativeFuzz(math.E) shouldBe 1.0 +- 1E-10
    MonadicOperationLog.relativeFuzz(math.E * math.E) shouldBe 0.5 +- 1E-10
  }
  it should "have precision loss of 3" in {
    MonadicOperationLog.fuzz shouldBe Some(3)
  }

  behavior of "MonadicOperationSin"
  it should "work for Int" in {
    val (fInt, _, _) = MonadicOperationSin.functions
    fInt(0) shouldBe Success(0)
    fInt(1) shouldBe Success(0) // sin(n*pi) = 0 for integer n
  }
  it should "work for exact Rational values" in {
    val (_, fRational, _) = MonadicOperationSin.functions
    fRational(Rational(1, 2)) shouldBe Success(Rational.one) // sin(pi/2) = 1
    fRational(Rational(3, 2)) shouldBe Success(Rational.negOne) // sin(3pi/2) = -1
    fRational(Rational(1, 6)) shouldBe Success(Rational.half) // sin(pi/6) = 1/2
  }
  it should "work for Double" in {
    val (_, _, fDouble) = MonadicOperationSin.functions
    fDouble(0.0).get shouldBe 0.0 +- 1E-10
    fDouble(0.5).get shouldBe 1.0 +- 1E-10 // sin(pi/2) = 1
    fDouble(1.0 / 6).get shouldBe 0.5 +- 1E-10 // sin(pi/6) = 0.5
  }
  it should "have precision loss of 3" in {
    MonadicOperationSin.fuzz shouldBe Some(3)
  }

  behavior of "MonadicOperationSinh/Cosh/Tanh"
  it should "compute sinh correctly" in {
    val (_, _, fDouble) = MonadicOperationSinh.functions
    fDouble(0.0).get shouldBe 0.0 +- 1E-10
    fDouble(1.0).get shouldBe math.sinh(1.0) +- 1E-10
  }
  it should "compute cosh correctly" in {
    val (_, _, fDouble) = MonadicOperationCosh.functions
    fDouble(0.0).get shouldBe 1.0 +- 1E-10
    fDouble(1.0).get shouldBe math.cosh(1.0) +- 1E-10
  }
  it should "compute tanh correctly" in {
    val (_, _, fDouble) = MonadicOperationTanh.functions
    fDouble(0.0).get shouldBe 0.0 +- 1E-10
    fDouble(1.0).get shouldBe math.tanh(1.0) +- 1E-10
  }

  behavior of "FuzzyNumber with monadic operators"

  // The key invariant: fuzz should be preserved (possibly transformed) through monadic ops

  it should "preserve fuzz through negate" in {
    val x = FuzzyNumber(Value.fromDouble(Some(3.0)), PureNumber, Some(AbsoluteFuzz(0.005, Box)))
    val result = Number.negate(x)
    result.fuzz.isDefined shouldBe true
    result.toNominalDouble shouldBe Some(-3.0)
    result.fuzz shouldBe Some(AbsoluteFuzz(0.005, Box)) // fuzz unchanged
  }

  it should "preserve relative fuzz through negate" in {
    val x = FuzzyNumber(Value.fromDouble(Some(3.0)), PureNumber, Some(RelativeFuzz(0.01, Gaussian)))
    val result = Number.negate(x)
    result.fuzz shouldBe Some(AbsoluteFuzz(0.03, Gaussian))
    result.toNominalDouble shouldBe Some(-3.0)
  }

  it should "preserve fuzz through invert" in {
    val x = FuzzyNumber(Value.fromDouble(Some(2.0)), PureNumber, Some(RelativeFuzz(0.01, Gaussian)))
    val result: Number = x.invert.asNumber.get
    result.fuzz.isDefined shouldBe true
    result.toNominalDouble.get shouldBe 0.5 +- 1E-10
    // relative fuzz is preserved for inversion (multiplier = -1, magnitude unchanged)
    result.fuzz.get.asInstanceOf[RelativeFuzz[Double]].tolerance shouldBe 0.01 +- 1E-10
  }

  //  it should "transform relative fuzz correctly through exp" in {
  //    val x = FuzzyNumber(Value.fromDouble(Some(1.0)), PureNumber, Some(RelativeFuzz(0.01, Gaussian)))
  //    val result = x.doExp
  //    result.fuzz.isDefined shouldBe true
  //    result.toDouble.get shouldBe math.E +- 1E-10
  //    // relativeFuzz for exp: x => x, so relative fuzz = input * 1.0 = 0.01
  //    result.fuzz.get.asInstanceOf[RelativeFuzz[Double]].tolerance shouldBe 0.01 +- 1E-6
  //  }

  //  it should "transform relative fuzz correctly through log" in {
  //    val x = FuzzyNumber(Value.fromDouble(Some(math.E)), PureNumber, Some(RelativeFuzz(0.01, Gaussian)))
  //    val result = x.doLog
  //    result.fuzz.isDefined shouldBe true
  //    result.toDouble.get shouldBe 1.0 +- 1E-10
  //    // relativeFuzz for log: x => 1/ln(x), so at x=e, multiplier = 1/ln(e) = 1
  //    result.fuzz.get.asInstanceOf[RelativeFuzz[Double]].tolerance shouldBe 0.01 +- 1E-6
  //  }

  it should "be symmetric for fuzzyCompare with exact number" in {
    val fuzzy = Number("3.1415927*") // fuzzy
    val exact = Number.pi // exact
    val c = 0.05
    // both directions should agree
    val r1 = fuzzy.asInstanceOf[GeneralNumber].fuzzyCompare(exact, c)
    val r2 = exact.asInstanceOf[GeneralNumber].fuzzyCompare(fuzzy, c)
    r1 shouldBe r2
  }

  it should "be symmetric for isSame with exact number" in {
    val fuzzy = Number("3.00*")
    val exact = Number(Rational(24577, 8192)) // exact rational ≈ 3.000122
    fuzzy.isSame(exact, 0.03) shouldBe exact.isSame(fuzzy, 0.03)
  }
}
