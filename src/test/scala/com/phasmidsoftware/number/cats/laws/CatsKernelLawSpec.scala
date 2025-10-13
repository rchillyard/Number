package com.phasmidsoftware.number.cats.laws

import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import cats.kernel.laws.discipline.{EqTests, OrderTests, PartialOrderTests}

import com.phasmidsoftware.number.core.{AbsoluteFuzz, Box, ExactNumber, FuzzyNumber, Gaussian, GeneralNumber, Number, Real, Field, Complex, ComplexCartesian, ComplexPolar}
import com.phasmidsoftware.number.core.inner.Radian
import com.phasmidsoftware.number.core.inner.{PureNumber, Rational, Value}
import com.phasmidsoftware.number.cats.CatsKernel._

/**
  * Discipline-based law tests for Cats Kernel instances defined in `instances.catsKernel`.
  *
  * Covered instances and laws:
  * - Rational: Order laws (total ordering via `Rational.compare`).
  * - ExactNumber: Order laws (delegates to `Number.doCompare` while remaining exact).
  * - Number: Eq laws (structural equality) and PartialOrder laws (to reflect fuzz/NaN comparability limits).
  *
  * Generator strategy:
  * - Rational: reasonably distributed finite rationals with positive denominators to avoid degenerate cases.
  * - ExactNumber: wraps a generated Rational into `Value` with the `PureNumber` factor.
  * - Number: mix of exact values, fuzzy values (Box/Gaussian shapes with width in [0, 1.0]), and a small portion of NaN.
  *   This ensures we exercise both Eq and PartialOrder semantics across typical and edge scenarios.
  *
  * Test runner:
  * - Uses `FunSuiteDiscipline` to integrate Cats laws into ScalaTest.
  * - `Checkers` mixed in for default ScalaCheck configuration; adjust if you need larger sample sizes.
  */
class CatsKernelLawSpec
    extends AnyFunSuite
    with Checkers
    with FunSuiteDiscipline {

  // Arbitrary[Rational]: generate normalized rationals with positive denominators.
  implicit val arbitraryRational: Arbitrary[Rational] = Arbitrary {
    val genNumerator = Arbitrary.arbitrary[Long].map(BigInt(_))
    val genDenominator = Gen.chooseNum[Long](1L, Long.MaxValue).map(BigInt(_))

    for {
      n <- genNumerator
      d <- genDenominator
    } yield Rational(n, d)
  }

  // Cogen[Rational]: derive from a Double view for stable seed perturbation.
  implicit val cogenRational: Cogen[Rational] =
    Cogen[(BigInt, BigInt)].contramap(r => (r.n, r.d))

  // Arbitrary[ExactNumber]: lift a Rational into an exact Value with PureNumber factor.
  implicit val arbitraryExactNumber: Arbitrary[ExactNumber] = Arbitrary {
    for {
      value <- arbitraryRational.arbitrary
    } yield ExactNumber(Value.fromRational(value), PureNumber)
  }

  // Cogen[ExactNumber]: conservative fallback based on string rendering.
  implicit val cogenExactNumber: Cogen[ExactNumber] =
    Cogen[(Option[Int], Option[(BigInt, BigInt)], Option[Double], String)].contramap { e =>
      val nominal = e.nominalValue
      val asInt = Value.maybeInt(nominal)
      val asRational = Value.maybeRational(nominal).map(r => (r.n, r.d))
      val asDouble = Value.maybeDouble(nominal)
      val factorId = e.factor.toString
      (asInt, asRational, asDouble, factorId)
    }

  // Arbitrary[Number]: mix of exact, fuzzy (Box/Gaussian), and NaN values.
  implicit val arbitraryNumber: Arbitrary[Number] = Arbitrary {
    val genExact: Gen[Number] = arbitraryExactNumber.arbitrary

    val genFuzzy: Gen[Number] = for {
      base <- arbitraryExactNumber.arbitrary
      fuzzWidth <- Gen.chooseNum(0.0, 1.0)
      fuzzType <- Gen.oneOf(Box, Gaussian)
    } yield FuzzyNumber(base.nominalValue, base.factor, Some(AbsoluteFuzz[Double](fuzzWidth, fuzzType)))

    val genNaN = Gen.const(Number.NaN)
    
    //About 70% generate ExactNumber, 25% generate FuzzyNumber, and 5% generate Number.NaN
    Gen.frequency(
      70 -> genExact,
      25 -> genFuzzy,
      5  -> genNaN
    )
  }

  // Cogen[Number]: conservative fallback across shapes via string rendering.
  implicit val cogenNumber: Cogen[Number] =
    Cogen[(Option[Double], Option[(BigInt, BigInt)], String, Option[(Double, String, Boolean)])].contramap { n =>
      val nominalDouble = n.toNominalDouble
      val nominalRational = n.toNominalRational.map(r => (r.n, r.d))
      val factorId = n.factor.toString
      val fuzzInfo = n match {
        case g: GeneralNumber =>
          g.fuzz.map { f =>
            val magnitude = f match {
              case AbsoluteFuzz(m: Double, _) => m
              case other => other.wiggle(0.5)
            }
            (magnitude, f.shape.toString, f.style)
          }
        case _ => None
      }
      (nominalDouble, nominalRational, factorId, fuzzInfo)
    }

  // Arbitrary[Real] and Cogen[Real]: delegate to underlying Number generators
  implicit val arbitraryReal: Arbitrary[Real] =
    Arbitrary(arbitraryNumber.arbitrary.map(n => Real(n)))

  implicit val cogenReal: Cogen[Real] =
    cogenNumber.contramap(_.x)

  // Arbitrary/Cogen[Field]: combine Real and simple Complex constructors
  implicit val arbitraryField: Arbitrary[Field] = Arbitrary {
    val genReal: Gen[Field] = arbitraryReal.arbitrary
    val genCartesian: Gen[Field] = for {
      x <- arbitraryNumber.arbitrary
      y <- arbitraryNumber.arbitrary
    } yield ComplexCartesian(x, y)
    // Restrict theta to valid Radian-typed angles to satisfy ComplexPolar.apply
    val genThetaRadian: Gen[Number] = Gen.oneOf(Number.zeroR, Number.pi, Number.minusPi, Number.piBy2)
    val genPolar: Gen[Field] = for {
      r <- arbitraryNumber.arbitrary
      theta <- genThetaRadian
    } yield ComplexPolar(r, theta, 1)
    Gen.frequency(
      6 -> genReal,
      3 -> genCartesian,
      1 -> genPolar
    )
  }

  implicit val cogenField: Cogen[Field] = Cogen[(Int, Option[Double], Option[(BigInt, BigInt)], String, Option[(Double, String, Boolean)], Option[Double], Option[(BigInt, BigInt)], Int)].contramap {
    case Real(n) =>
      (0, n.toNominalDouble, n.toNominalRational.map(r => (r.n, r.d)), n.factor.toString, None, None, None, 0)
    case ComplexCartesian(x, y) =>
      (1, x.toNominalDouble, x.toNominalRational.map(r => (r.n, r.d)), x.factor.toString, None, y.toNominalDouble, y.toNominalRational.map(r => (r.n, r.d)), 0)
    case ComplexPolar(r, t, n) =>
      (2, r.toNominalDouble, r.toNominalRational.map(r0 => (r0.n, r0.d)), r.factor.toString, None, t.toNominalDouble, t.toNominalRational.map(t0 => (t0.n, t0.d)), n)
  }

  // Arbitrary/Cogen[Complex]: restrict theta to valid radians for polar
  implicit val arbitraryComplex: Arbitrary[Complex] = Arbitrary {
    val genCartesian: Gen[Complex] = for {
      x <- arbitraryNumber.arbitrary
      y <- arbitraryNumber.arbitrary
    } yield ComplexCartesian(x, y)
    val genThetaRadian: Gen[Number] = Gen.oneOf(Number.zeroR, Number.pi, Number.minusPi, Number.piBy2)
    val genPolar: Gen[Complex] = for {
      r <- arbitraryNumber.arbitrary
      theta <- genThetaRadian
    } yield ComplexPolar(r, theta, 1)
    Gen.frequency(
      3 -> genCartesian,
      1 -> genPolar
    )
  }

  implicit val cogenComplex: Cogen[Complex] = Cogen[(Int, Option[Double], Option[(BigInt, BigInt)], String, Option[Double], Option[(BigInt, BigInt)], Int)].contramap {
    case ComplexCartesian(x, y) =>
      (1, x.toNominalDouble, x.toNominalRational.map(r => (r.n, r.d)), x.factor.toString, y.toNominalDouble, y.toNominalRational.map(r => (r.n, r.d)), 0)
    case ComplexPolar(r, t, n) =>
      (2, r.toNominalDouble, r.toNominalRational.map(r0 => (r0.n, r0.d)), r.factor.toString, t.toNominalDouble, t.toNominalRational.map(t0 => (t0.n, t0.d)), n)
  }


  // Law checks
  checkAll("Rational", OrderTests[Rational].order)
  checkAll("ExactNumber", OrderTests[ExactNumber].order)
  checkAll("Number", EqTests[Number].eqv)
  checkAll("Number", PartialOrderTests[Number].partialOrder)
  checkAll("Real", EqTests[Real].eqv)
  checkAll("Real", PartialOrderTests[Real].partialOrder)
  // Now that Eq[Field] is provided globally, we can test Eq and PartialOrder
  checkAll("Field", EqTests[Field].eqv)
  checkAll("Field", PartialOrderTests[Field].partialOrder)
  checkAll("Complex", EqTests[Complex].eqv)
}

