package com.phasmidsoftware.number.cats

import com.phasmidsoftware.number.core.{Constants, Real}
import algebra.ring.{Field => AlgebraField}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RealAlgebraicInstancesSpec extends AnyFlatSpec with Matchers with RealAlgebraicInstances {
  /**
  behavior of "realField (algebra.Field[Real])"

  it should "provide zero/one/fromInt/plus/times/negate/div/reciprocal" in {
    val F: AlgebraField[Real] = realField

    // zero/one
    F.zero shouldBe Constants.zero
    F.one shouldBe Constants.one

    // fromInt
    F.fromInt(2) shouldBe Real(2)

    // plus/times/negate
    F.plus(Real(1), Real(2)) shouldBe Real(3)
    F.times(Real(3), Real(2)) shouldBe Real(6)
    F.negate(Real(3)) shouldBe Real(-3)

    // div/reciprocal (compare via toDouble for simplicity)
    F.div(Real(6), Real(4)).toDouble shouldBe 1.5 +- 1e-12
    F.reciprocal(Real(2)).toDouble shouldBe 0.5 +- 1e-12
  }
  */
  behavior of "realTruncatedDivisionCRing (tquot/tmod/tquotMod + order)"

  it should "truncate quotient toward zero and compute remainder" in {
    val TD = realTruncatedDivisionCRing

    // 7 / 3 => q = 2, r = 1
    TD.tquot(Real(7), Real(3)) shouldBe Real(2)
    TD.tmod(Real(7), Real(3)) shouldBe Real(1)
    TD.tquotmod(Real(7), Real(3)) shouldBe (Real(2), Real(1))

    // -7 / 3 => q = -2 (ceil toward zero), r = -1
    TD.tquot(Real(-7), Real(3)) shouldBe Real(-2)
    TD.tmod(Real(-7), Real(3)) shouldBe Real(-1)

    // 7 / -3 => q = -2, r = 1 (since r = x - y*q = 7 - (-3 * -2) = 1)
    TD.tquot(Real(7), Real(-3)) shouldBe Real(-2)
    TD.tmod(Real(7), Real(-3)) shouldBe Real(1)
  }

  it should "expose an order consistent with Real.RealIsOrdering" in {
    val TD = realTruncatedDivisionCRing
    TD.order.compare(Real(1), Real(2)) should be < 0
    TD.order.compare(Real(2), Real(1)) should be > 0
    TD.order.compare(Real(2), Real(2)) shouldBe 0
  }
}


