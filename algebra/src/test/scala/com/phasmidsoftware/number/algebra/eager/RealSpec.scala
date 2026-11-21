/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.eager

import algebra.ring.Ring
import com.phasmidsoftware.number.algebra.eager.Real.realIsRing
import com.phasmidsoftware.number.algebra.eager.{Angle, RationalNumber, Real}
import com.phasmidsoftware.number.core.numerical.{AbsoluteFuzz, Box, RelativeFuzz}
import com.phasmidsoftware.number.core.numerical.Number as CoreNumber
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RealSpec extends AnyFlatSpec with Matchers {

  private val rr: Ring[Real] = implicitly[Ring[Real]]

  behavior of "Real"

  // NOTE the policy lives in Fuzziness.fuzzForDouble and is shared with core's GeneralNumber
  // (#618): a value whose exact Rational has a BigDecimal scale of 0, 1 or 2 is exact, and beyond
  // that it gets half of its last place. The E-notation cases are here because counting the
  // characters after the '.' used to count the exponent's digits as decimal places (#617).
  private def fuzzOf(x: Double): Option[Double] =
    Real(x).fuzz collect { case AbsoluteFuzz(m, _) => m }

  it should "treat a value of scale 0, 1 or 2 as exact" in {
    Real(4.0).isExact shouldBe true
    Real(1.5).isExact shouldBe true
    Real(1.25).isExact shouldBe true
  }

  it should "give half the last place beyond scale 2" in {
    fuzzOf(1.125) shouldBe Some(5.0E-4)
    fuzzOf(1.0 / 3) shouldBe Some(5.0E-17)
  }

  it should "derive fuzz correctly for a value in E-notation" in {
    // Before #617 this was 5.0E-6, which is larger than the value itself.
    fuzzOf(1.23E-7).get should be < 1.23E-7
    fuzzOf(1.23E-7) shouldBe Some(5.0E-10)
  }

  it should "keep the previous relative fuzz for a non-finite value" in {
    Real(Double.PositiveInfinity).fuzz should matchPattern { case Some(RelativeFuzz(_, _)) => }
    Real(Double.NaN).fuzz should matchPattern { case Some(RelativeFuzz(_, _)) => }
  }

  // NOTE this is the regression guard for #618: the two paths from a Double must not drift apart
  // again. Before the fix they disagreed on every value here.
  it should "agree with core about which Doubles are exact" in {
    for (x <- Seq(4.0, 1.5, 1.25, 1.125, 1.0 / 3, 1.0E20, 1.23E-7, 1.23456789E8))
      withClue(s"for $x: ")(Real(x).isExact shouldBe CoreNumber(x).isExact)
  }

  it should "apply(String)" in {
    val (x, y, z) = (Real("1.5"), Real("2"), Real("3.1415927(31)"))
    x shouldBe Real(1.5, None)
    y shouldBe Real(2, None)
    z.toDouble === 3.1415927 shouldBe true
  }

  // Basic arithmetic operations
  it should "perform addition correctly" in {
    val x = Real(1)
    val y = Real(2)
    rr.plus(x, y) compareTo RationalNumber(3) shouldBe 0
  }

  it should "perform subtraction correctly" in {
    val x = Real(5)
    val y = Real(3)
    rr.plus(x, realIsRing.negate(y)) should matchPattern { case Real(2, _) => }
  }

  it should "perform multiplication correctly" in {
    val x = Real(2)
    val y = Real(3)
    rr.times(x, y) should matchPattern { case Real(6, _) => }
  }

  it should "perform division correctly" in {
    val x = Real(6)
    val y = Real(2)
    realIsRing.div(x, y) should matchPattern { case Real(3, _) => }
  }

  // Comparison operations
  it should "compare numbers correctly A" in {
    val x = Real(1)
    val y = Real(2)
    x.compare(y) shouldBe -1
    y.compare(x) shouldBe 1
    x.compare(x) shouldBe 0
  }
  it should "compare numbers correctly B" in {
    val x = Real(1.5)
    val y = Real(2)
    x.compare(y) shouldBe -1
    y.compare(x) shouldBe 1
    x.compare(x) shouldBe 0
  }

  // Conversion operations
  it should "convert to different number types" in {
    val x = Real(5)
    x.convert(Real.zero) shouldBe Some(x)
    x.convert(Angle.zero) shouldBe None
  }

  // Edge cases and special values
  it should "handle zero correctly" in {
    val x = Real(0)
    x.isZero shouldBe true
  }

  behavior of "Ring[Real]"

  it should "plus" in {
    rr.plus(Real(3), Angle.pi.approximate) shouldBe Real(6.141592653589793, Some(AbsoluteFuzz(5.02654824574367E-16, Box)))
  }
  it should "negate" in {
    rr.negate(Real(3)) shouldBe Real(-3)
  }

}
