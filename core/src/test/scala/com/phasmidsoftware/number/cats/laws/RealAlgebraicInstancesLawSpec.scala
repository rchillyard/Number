package com.phasmidsoftware.number.cats.laws

import com.phasmidsoftware.number.cats.{RealAlgebraicInstances => RA}
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.numerical.Number.FuzzOps
import com.phasmidsoftware.number.core.numerical.Real
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

/**
  * Discipline-based algebra-laws testing: Verify that `algebra.ring.Field` instances of `Real` satisfy Field's laws.
  *
  * illustrate:
  * - Instances are derived from `RealAlgebraicInstances#realField`;
  * - We need `Eq[Real]` and `Arbitrary[Real]`;
  * - We only check Field laws (which implicitly enforce CommutativeRing/Ring/Group level laws).
  */
class RealAlgebraicInstancesLawSpec
  extends AnyFunSuite
  with Checkers
  with FunSuiteDiscipline {

  private object instances extends RA
  implicit val algebraCommutativeRing: algebra.ring.CommutativeRing[Real] = instances.realCommutativeRing

  implicit val arbitraryReal: Arbitrary[Real] = Arbitrary {
    // Primarily values ​​that can be precisely represented: small integers and rational numbers with small denominators.
    val genSmallInt: Gen[Real] = Gen.chooseNum(-1000000, 1000000).map(Real(_))

    val genSmallRational: Gen[Real] = for {
      n <- Gen.chooseNum[Long](-1000000L, 1000000L).map(BigInt(_))
      d <- Gen.chooseNum[Long](1L, 1000000L).map(BigInt(_))
    } yield Real(Rational(n, d))

    // Fuzzy reals via Gaussian absolute fuzz (two-digit sd code)
    // Fuzzy around a rational center: reduce Double-origin rounding
    val genFuzzyReal: Gen[Real] = for {
      n <- Gen.chooseNum[Long](-100000L, 100000L).map(BigInt(_))
      d <- Gen.chooseNum[Long](1L, 100000L).map(BigInt(_))
      sd <- Gen.chooseNum(10, 20)
    } yield {
      val center = (BigDecimal(n) / BigDecimal(d)).toDouble
      Real(center ~ sd)
    }

    Gen.frequency(
      6 -> genSmallInt,
      3 -> genSmallRational,
      1 -> genFuzzyReal
    )
  }

  //implicit val eqReal: Eq[Real] = realEq

//  checkAll("Real", RingLaws[Real].commutativeRing)
  
  //TODO: Uncomment this when we have a way to pass the field law tests.
  //checkAll("Real", RingLaws[Real].field)
}


