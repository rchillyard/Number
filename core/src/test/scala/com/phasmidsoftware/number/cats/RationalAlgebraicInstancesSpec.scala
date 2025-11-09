package com.phasmidsoftware.number.cats

import com.phasmidsoftware.number.core.inner.Rational
import algebra.ring.{TruncatedDivision => AlgebraTruncatedDivision}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RationalAlgebraicInstancesSpec extends AnyFlatSpec with Matchers with RationalAlgebraicInstances {
  behavior of "rationalTruncatedDivisionCRing (tquot/tmod/tquotmod + order)"

  it should "truncate quotient toward zero and compute remainder" in {
    val TD = rationalTruncatedDivisionCRing

    // 7 / 3 => q = 2, r = 1
    TD.tquot(Rational(7), Rational(3)) shouldBe Rational(2)
    TD.tmod(Rational(7), Rational(3)) shouldBe Rational(1)
    TD.tquotmod(Rational(7), Rational(3)) shouldBe (Rational(2), Rational(1))

    // -7 / 3 => q = -2 (ceil toward zero), r = -1
    TD.tquot(Rational(-7), Rational(3)) shouldBe Rational(-2)
    TD.tmod(Rational(-7), Rational(3)) shouldBe Rational(-1)

    // 7 / -3 => q = -2, r = 1 (since r = x - y*q = 7 - (-3 * -2) = 1)
    TD.tquot(Rational(7), Rational(-3)) shouldBe Rational(-2)
    TD.tmod(Rational(7), Rational(-3)) shouldBe Rational(1)
  }

  it should "expose an order consistent with Rational.RationalIsOrdering" in {
    val TD = rationalTruncatedDivisionCRing
    TD.order.compare(Rational(1), Rational(2)) should be < 0
    TD.order.compare(Rational(2), Rational(1)) should be > 0
    TD.order.compare(Rational(2), Rational(2)) shouldBe 0
  }
}


