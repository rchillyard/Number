package com.phasmidsoftware.number.cats.laws

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import algebra.laws.RingLaws
import cats.kernel.Eq

import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.cats.{RationalAlgebraicInstances => RI}
import com.phasmidsoftware.number.cats.CatsKernel._

/**
  * Discipline-based algebra-laws testing for Rational.
  *
  * We verify CommutativeRing laws using the instances defined in RationalAlgebraicInstances.
  */
class RationalAlgebraicInstancesLawSpec
  extends AnyFunSuite
  with Checkers
  with FunSuiteDiscipline {

  private object instances extends RI
  implicit val algebraCommutativeRing: algebra.ring.CommutativeRing[Rational] = instances.rationalCommutativeRing

  implicit val arbitraryRational: Arbitrary[Rational] = Arbitrary {
    val genSmallInt: Gen[Rational] = Gen.chooseNum(-1000000, 1000000).map(i => Rational(i))

    val genSmallRational: Gen[Rational] = for {
      n <- Gen.chooseNum[Long](-1000000L, 1000000L).map(BigInt(_))
      d <- Gen.chooseNum[Long](1L, 1000000L).map(BigInt(_))
    } yield Rational(n, d)

    Gen.frequency(
      7 -> genSmallInt,
      3 -> genSmallRational
    )
  }

  //implicit val eqRational: Eq[Rational] = rationalEq

  checkAll("Rational", RingLaws[Rational].commutativeRing)
}



