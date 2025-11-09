package com.phasmidsoftware.number.cats.laws

import algebra.laws.RingLaws
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import com.phasmidsoftware.number.core.{Complex, ComplexCartesian, Number}
import com.phasmidsoftware.number.core.Number.FuzzOps
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.cats.{ComplexAlgebraicInstances => CI}

/**
  * Discipline-based algebra-laws testing for Complex CommutativeRing.
  *
  * We restrict generators to Cartesian form to avoid Polar multiplication mismatches.
  */
class ComplexAlgebraicInstancesLawSpec
  extends AnyFunSuite
  with Checkers
  with FunSuiteDiscipline {

  private object instances extends CI
  implicit val algebraCommutativeRing: algebra.ring.CommutativeRing[Complex] = instances.complexCommutativeRing

  implicit val arbitraryComplex: Arbitrary[Complex] = Arbitrary {
    // Integers: exact, fast, broad coverage
    val genCartesianInt: Gen[Complex] = for {
      re <- Gen.chooseNum(-1000000, 1000000)
      im <- Gen.chooseNum(-1000000, 1000000)
    } yield ComplexCartesian(re, im)

    // Rationals: exact non-integer coverage (small denominators for performance)
    val genCartesianRational: Gen[Complex] = for {
      n1 <- Gen.chooseNum[Long](-1000000, 1000000).map(BigInt(_))
      d1 <- Gen.chooseNum[Long](1L, 1000000L).map(BigInt(_))
      n2 <- Gen.chooseNum[Long](-1000000, 1000000).map(BigInt(_))
      d2 <- Gen.chooseNum[Long](1L, 1000000L).map(BigInt(_))
    } yield ComplexCartesian(Number(Rational(n1, d1)), Number(Rational(n2, d2)))

    // Fuzzy Numbers (Cartesian) with rational centers: reduce Double-origin rounding
    val genCartesianFuzzy: Gen[Complex] = for {
      n1 <- Gen.chooseNum[Long](-100000L, 100000L).map(BigInt(_))
      d1 <- Gen.chooseNum[Long](1L, 100000L).map(BigInt(_))
      n2 <- Gen.chooseNum[Long](-100000L, 100000L).map(BigInt(_))
      d2 <- Gen.chooseNum[Long](1L, 100000L).map(BigInt(_))
      sd1 <- Gen.chooseNum(10, 20)
      sd2 <- Gen.chooseNum(10, 20)
    } yield {
      val reCenter = (BigDecimal(n1) / BigDecimal(d1)).toDouble
      val imCenter = (BigDecimal(n2) / BigDecimal(d2)).toDouble
      ComplexCartesian(reCenter ~ sd1, imCenter ~ sd2)
    }

    // Axis-aligned values: target edge-cases (pure real / pure imaginary)
    val genRealAxis: Gen[Complex] = Gen.chooseNum(-1000000, 1000000).map(re => ComplexCartesian(re, 0))
    val genImagAxis: Gen[Complex] = Gen.chooseNum(-1000000, 1000000).map(im => ComplexCartesian(0, im))

    Gen.frequency(
      4 -> genCartesianInt,
      3 -> genCartesianRational,
      2 -> genCartesianFuzzy,
      1 -> genRealAxis,
      1 -> genImagAxis
    )
  }

  import com.phasmidsoftware.number.cats.CatsKernel.complexEq
  checkAll("Complex", RingLaws[Complex].commutativeRing)
}



