/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.eager

import algebra.ring.Ring
import com.phasmidsoftware.number.algebra.eager.Real.realIsRing
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.numerical.{AbsoluteFuzz, Box}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Assertion, Inside}

class RealPowerSpec extends AnyFlatSpec with Matchers with Inside {

  // ─── helpers ────────────────────────────────────────────────────────────────

  /** Tolerance for fuzzy-double comparisons in tests that examine raw Double values. */
  private val eps = 1e-10

  private val rr: Ring[Real] = implicitly[Ring[Real]]

  private def rational(n: Int, d: Int): Rational = Rational(n, d)

  private def rational(n: Int): Rational = Rational(n)

  // Checks that two Doubles are close enough given the tolerance.
  private def closeTo(a: Double, b: Double, tol: Double = eps): Assertion =
    (math.abs(a - b) < tol) shouldBe true

  // ─── power(r: Rational) – exact Real ────────────────────────────────────────

  behavior of "Real.power(Rational) with exact Real"

  it should "return 1 when exponent is 0" in {
    val x = Real(4, None)
    x.power(rational(0)).flatMap(_.maybeDouble) shouldBe Some(1.0)
  }

  it should "return itself when exponent is 1" in {
    val x = Real(7, None)
    x.power(rational(1)).flatMap(_.maybeDouble) shouldBe Some(7.0)
  }

  it should "compute integer square correctly" in {
    val x = Real(3, None)
    x.power(rational(2)).flatMap(_.maybeDouble) shouldBe Some(9.0)
  }

  it should "compute integer cube correctly" in {
    val x = Real(2, None)
    x.power(rational(3)).flatMap(_.maybeDouble) shouldBe Some(8.0)
  }

  it should "compute square root (1/2) correctly" in {
    val x = Real(9, None)
    val root = x.power(rational(1, 2))
    root.isDefined shouldBe true
    closeTo(root.get.value, 3.0)
  }

  it should "compute cube root (1/3) correctly" in {
    val x = Real(27, None)
    val root = x.power(rational(1, 3))
    root.isDefined shouldBe true
    closeTo(root.get.value, 3.0)
  }

  it should "compute 2^(3/2) = 2√2 correctly" in {
    val x = Real(2, None)
    val result = x.power(rational(3, 2))
    result.isDefined shouldBe true
    closeTo(result.get.value, math.pow(2, 1.5))
  }

  it should "compute 4^(−1/2) = 0.5 correctly" in {
    val x = Real(4, None)
    val result = x.power(rational(-1, 2))
    result.isDefined shouldBe true
    closeTo(result.get.value, 0.5)
  }

  it should "compute negative integer exponent correctly" in {
    val x = Real(2, None)
    val result = x.power(rational(-3))
    result.isDefined shouldBe true
    closeTo(result.get.value, 0.125)
  }

  it should "return 0 when base is 0 and exponent is positive" in {
    val x = Real(0, None)
    val result = x.power(rational(5))
    result.isDefined shouldBe true
    result.get.value shouldBe 0.0
  }

  it should "return 1 when base is 1 for any rational exponent" in {
    val x = Real(1, None)
    for (r <- Seq(rational(0), rational(1), rational(2), rational(-1), rational(1, 2))) {
      x.power(r).flatMap(_.maybeDouble) shouldBe Some(1.0)
    }
  }

  it should "return Some (never None) for positive base" in {
    val x = Real(5, None)
    x.power(rational(2)) shouldBe defined
    x.power(rational(1, 3)) shouldBe defined
    x.power(rational(-1)) shouldBe defined
  }

  // ─── power(r: Rational) – fuzz propagation ──────────────────────────────────

  behavior of "Real.power(Rational) fuzz propagation"

  it should "carry fuzz through squaring" in {
    // fuzz scales by |n| * x^(n-1) relatively
    val fuzzVal = 0.01
    val x = Real(4.0, Some(AbsoluteFuzz(fuzzVal, Box)))
    val result = x.power(rational(2))
    result.isDefined shouldBe true
    // Result should be fuzzy (fuzz present)
    result.get.fuzz shouldBe defined
  }

  it should "carry fuzz through square root" in {
    val x = Real(9.0, Some(AbsoluteFuzz(0.1, Box)))
    val result = x.power(rational(1, 2))
    result.isDefined shouldBe true
    result.get.fuzz shouldBe defined
    closeTo(result.get.value, 3.0, 1e-9)
  }

  it should "produce an exact result when base is exact" in {
    val x = Real(4, None)
    val result = x.power(rational(2))
    // exact base, exact exponent -> result should be exact (fuzz.isEmpty)
    result.get.fuzz shouldBe empty
  }

  // ─── power(ExactNumber) ───────────────────────────────────────────────────────

  behavior of "Real.power(ExactNumber)"

  // ExactNumber is typically WholeNumber; adjust if your project exposes a different factory.
  // We use WholeNumber(n) as the canonical ExactNumber here.

  it should "compute x^2 via power" in {
    val x = Real(5, None)
    val r: Option[Real] = x `power` Rational(2)
    r.isDefined shouldBe true
    closeTo(r.get.value, 25.0)
  }

  it should "compute x^0 = 1 via power" in {
    val x = Real(99, None)
    val r = x `power` Rational(0)
    r.isDefined shouldBe true
    closeTo(r.get.asDouble, 1.0)
  }

  it should "compute x^1 = x via power" in {
    val x = Real(42, None)
    val r = x `power` Rational(1)
    r.isDefined shouldBe true
    closeTo(r.get.asDouble, 42.0)
  }

  it should "compute x^(−1) = 1/x via power" in {
    val x = Real(4, None)
    val r = x `power` Rational(-1)
    r.isDefined shouldBe true
    closeTo(r.get.asDouble, 0.25)
  }

  it should "propagate fuzz through power" in {
    val x = Real(3.0, Some(AbsoluteFuzz(0.05, Box)))
    val r = x `power` Rational(3)
    r.isDefined shouldBe true
    r.get.fuzz shouldBe defined
    closeTo(r.get.asDouble, 27.0, 1e-9)
  }
  // ─── power on negative Real ──────────────────────────────────────────────────

  behavior of "Real.power(Rational) with negative base"

  it should "compute (-2)^2 = 4 correctly" in {
    val x = Real(-2, None)
    val result = x.power(rational(2))
    result.isDefined shouldBe true
    closeTo(result.get.value, 4.0)
  }

  it should "compute (-2)^3 = -8 correctly" in {
    val x = Real(-2, None)
    val result = x.power(rational(3))
    result.isDefined shouldBe true
    closeTo(result.get.value, -8.0)
  }

  it should "return None for negative base with fractional exponent (1/2)" in {
    // math.pow(-4, 0.5) = NaN — real square root of negative number is undefined
    val x = Real(-4, None)
    val result: Option[Real] = x.power(rational(1, 2))
    result.isEmpty shouldBe true
  }

  it should "return None for negative base with non-integer rational exponent (2/3)" in {
    // math.pow delegates to IEEE 754 — NaN for non-integer exponents on negative base
    val x = Real(-8, None)
    val result: Option[Real] = x.power(rational(2, 3))
    result.isEmpty shouldBe true
  }

  // ─── Ring operations not well covered ───────────────────────────────────────

  behavior of "Ring[Real] – additional coverage"

  it should "multiply fuzzy numbers and combine fuzz" in {
    val x = Real(2.0, Some(AbsoluteFuzz(0.02, Box)))
    val y = Real(3.0, Some(AbsoluteFuzz(0.03, Box)))
    val z = rr.times(x, y)
    closeTo(z.value, 6.0)
    z.fuzz shouldBe defined
  }

  it should "add fuzzy numbers and combine fuzz" in {
    val x = Real(1.0, Some(AbsoluteFuzz(0.1, Box)))
    val y = Real(2.0, Some(AbsoluteFuzz(0.2, Box)))
    val z = rr.plus(x, y)
    closeTo(z.value, 3.0)
    z.fuzz shouldBe defined
  }

  it should "fromInt produces exact Real" in {
    val r = rr.fromInt(7)
    r.value shouldBe 7.0
    r.fuzz shouldBe empty
  }

  it should "inverse of 4 gives 0.25" in {
    val x = Real(4, None)
    val i = realIsRing.inverse(x)
    i.isDefined shouldBe true
    closeTo(i.get.value, 0.25)
  }

  it should "inverse of 0 returns None" in {
    realIsRing.inverse(Real(0, None)) shouldBe None
  }

  it should "div by exact 3 propagates correctly" in {
    val x = Real(9, None)
    val y = Real(3, None)
    val z = realIsRing.div(x, y)
    closeTo(z.value, 3.0)
  }

  it should "div by zero returns Infinity" in {
    val x = Real(1, None)
    val y = Real(0, None)
    val z = realIsRing.div(x, y)
    z.value shouldBe Double.PositiveInfinity
  }

  // ─── Unary minus and subtraction ────────────────────────────────────────────

  behavior of "Real unary minus / subtraction"

  it should "negate preserves magnitude" in {
    val x = Real(3.14, None)
    val n = -x
    closeTo(n.value, -3.14)
    n.fuzz shouldBe empty
  }

  it should "negate of negation is identity" in {
    val x = Real(2, None)
    (-(-x)).value shouldBe x.value
  }

  it should "subtract gives correct result" in {
    val x = Real(10, None)
    val y = Real(4, None)
    import com.phasmidsoftware.number.algebra.eager.Real.realIsRing
    val z = x - y
    closeTo(z.value, 6.0)
  }

  // ─── isUnity / isZero / isExact ─────────────────────────────────────────────

  behavior of "Real predicates"

  it should "report isUnity correctly" in {
    Real(1, None).isUnity shouldBe true
    Real(2, None).isUnity shouldBe false
    // fuzzy 1.0 is NOT unity (not exact)
    Real(1.0).isUnity shouldBe false
  }

  it should "report isZero correctly" in {
    Real(0, None).isZero shouldBe true
    Real(1, None).isZero shouldBe false
  }

  it should "report isExact correctly" in {
    Real(5, None).isExact shouldBe true
    Real(5.0).isExact shouldBe false
  }

  // ─── Ordering ────────────────────────────────────────────────────────────────

  behavior of "Ring[Real] ordering"

  it should "compare correctly with fuzzy numbers straddling boundary" in {
    // Both fuzzy, overlapping: compare should return 0 (indistinguishable)
    val x = Real(1.0, Some(AbsoluteFuzz(0.5, Box)))
    val y = Real(1.1, Some(AbsoluteFuzz(0.5, Box)))
    realIsRing.compare(x, y) shouldBe 0
  }

  it should "compare correctly with well-separated fuzzy numbers" in {
    val x = Real(1.0, Some(AbsoluteFuzz(0.01, Box)))
    val y = Real(2.0, Some(AbsoluteFuzz(0.01, Box)))
    realIsRing.compare(x, y) should be < 0
    realIsRing.compare(y, x) should be > 0
  }

  // ─── convert ─────────────────────────────────────────────────────────────────

  behavior of "Real.convert"

  it should "convert Real to Real (identity)" in {
    val x = Real(3, None)
    x.convert(Real.zero) shouldBe Some(x)
  }

  it should "convert Real to RationalNumber when exact integer" in {
    val x = Real(4, None)
    x.convert(RationalNumber(Rational(0))) shouldBe defined
  }

  it should "return None when converting to incompatible type" in {
    val x = Real(3, None)
    x.convert(Angle.zero) shouldBe None
  }

  // ─── maybeDouble ─────────────────────────────────────────────────────────────

  behavior of "Real.maybeDouble"

  it should "return Some(v) for an exact Real" in {
    Real(7, None).maybeDouble shouldBe Some(7.0)
  }

  it should "return None for a fuzzy Real" in {
    Real(7.0).maybeDouble shouldBe None
  }

  // ─── scaleByPi ───────────────────────────────────────────────────────────────

  behavior of "Real.scaleByPi"

  it should "scale 2 by pi" in {
    val x = Real(2, None)
    closeTo(x.scaleByPi.value, 2 * math.Pi, 1e-9)
  }

  it should "scale 0 by pi to give 0" in {
    val x = Real(0, None)
    x.scaleByPi.value shouldBe 0.0
  }

  // ─── signum ──────────────────────────────────────────────────────────────────

  behavior of "Real.signum"

  it should "return 1 for positive" in {
    Real(5, None).signum shouldBe 1
  }
  it should "return -1 for negative" in {
    Real(-3, None).signum shouldBe -1
  }
  it should "return 0 for zero" in {
    Real(0, None).signum shouldBe 0
  }

  // ─── round-trip power identity ───────────────────────────────────────────────

  behavior of "Real.power round-trip"

  it should "satisfy x^(1/n)^n ≈ x for n=2" in {
    val x = Real(16, None)
    val root = x.power(rational(1, 2)).get
    val back = root.power(rational(2)).get
    closeTo(back.value, 16.0, 1e-9)
  }

  it should "satisfy x^(1/n)^n ≈ x for n=3" in {
    val x = Real(27, None)
    val root = x.power(rational(1, 3)).get
    val back = root.power(rational(3)).get
    closeTo(back.value, 27.0, 1e-9)
  }

  it should "satisfy x^n * x^m = x^(n+m) for whole exponents" in {
    val x = Real(2, None)
    val p2 = x.power(rational(2)).get
    val p3 = x.power(rational(3)).get
    val p5 = x.power(rational(5)).get
    closeTo(rr.times(p2.asInstanceOf[Real], p3.asInstanceOf[Real]).value, p5.value, 1e-9)
  }
}