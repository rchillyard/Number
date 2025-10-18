package com.phasmidsoftware.number.cats.laws

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import cats.kernel.Eq
import cats.kernel.laws.discipline.CommutativeMonoidTests

import com.phasmidsoftware.number.cats.ErrorCommutativeMonoid._

/**
  * Cats laws for error metric wrappers defined in ErrorCommutativeMonoid.
  *
  * We verify CommutativeMonoid laws for:
  * - AbsSigma: absolute standard deviation aggregation (independent, additive context).
  *
  * Floating-point rounding can cause tiny deviations, so we use a relative-tolerance Eq.
  */
class ErrorCommutativeMonoidLawSpec
    extends AnyFunSuite
    with Checkers
    with FunSuiteDiscipline {

  private def approxEq(a: Double, b: Double, eps: Double = 1e-10): Boolean =
    (a == b) || math.abs(a - b) <= eps * (1.0 + math.max(math.abs(a), math.abs(b)))

  implicit val eqAbsSigma: Eq[AbsSigma] = (x, y) => approxEq(x.value, y.value)

  // Keep ranges moderate to reduce catastrophic cancellation in nested sqrt
  implicit val arbAbsSigma: Arbitrary[AbsSigma] =
    Arbitrary(Gen.chooseNum(0.0, 1e6).map(AbsSigma(_)))

  // Law checks
  checkAll("AbsSigma", CommutativeMonoidTests[AbsSigma].commutativeMonoid)
}


