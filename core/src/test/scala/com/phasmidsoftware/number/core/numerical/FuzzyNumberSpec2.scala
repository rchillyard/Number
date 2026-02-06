package com.phasmidsoftware.number.core.numerical

import com.phasmidsoftware.number.core.inner.*
import com.phasmidsoftware.number.core.numerical.Number
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FuzzyNumberSpec2 extends AnyFlatSpec with Matchers {

  behavior of "FuzzyNumber construction"

  it should "create a fuzzy number with absolute fuzz" in {
    val value = Value.fromDouble(Some(100.0))
    val fuzz = Some(AbsoluteFuzz[Double](1.0, Gaussian))
    val fn = FuzzyNumber(value, PureNumber, fuzz)

    fn.isExact shouldBe false
    fn.fuzz should be(defined)
  }

  it should "create a fuzzy number with relative fuzz" in {
    val value = Value.fromDouble(Some(100.0))
    val fuzz = Some(RelativeFuzz[Double](0.01, Gaussian))
    val fn = FuzzyNumber(value, PureNumber, fuzz)

    fn.isExact shouldBe false
    fn.fuzz should be(defined)
  }

  it should "treat None fuzz as exact" in {
    val value = Value.fromDouble(Some(100.0))
    val fn = FuzzyNumber(value, PureNumber, None)

    fn.isExact shouldBe true
    fn.fuzz shouldBe None
  }

  behavior of "FuzzyNumber.normalizeFuzz"

  it should "convert relative fuzz to absolute" in {
    val value = Value.fromDouble(Some(100.0))
    val fuzz = Some(RelativeFuzz[Double](0.01, Gaussian))
    val fn = FuzzyNumber(value, PureNumber, fuzz)

    val normalized = fn.normalizeFuzz
    normalized shouldBe a[FuzzyNumber]
  }

  it should "preserve absolute fuzz" in {
    val value = Value.fromDouble(Some(100.0))
    val fuzz = Some(AbsoluteFuzz[Double](1.0, Gaussian))
    val fn = FuzzyNumber(value, PureNumber, fuzz)

    val normalized = fn.normalizeFuzz
    normalized shouldBe a[FuzzyNumber]
  }

  it should "handle zero nominal value without throwing" in {
    val value = Value.fromDouble(Some(0.0))
    val fuzz = Some(AbsoluteFuzz[Double](1.0, Gaussian))
    val fn = FuzzyNumber(value, PureNumber, fuzz)

    val normalized = fn.normalizeFuzz
    // Should not throw, should preserve absolute fuzz
    normalized.fuzz should be(defined)
  }

  behavior of "FuzzyNumber addition"

  it should "add two fuzzy numbers with absolute fuzz" in {
    val fn1 = FuzzyNumber(Value.fromDouble(Some(100.0)), PureNumber, Some(AbsoluteFuzz[Double](1.0, Gaussian)))
    val fn2 = FuzzyNumber(Value.fromDouble(Some(50.0)), PureNumber, Some(AbsoluteFuzz[Double](0.5, Gaussian)))

    val result = fn1.doAdd(fn2)
    result shouldBe a[Number]
    result.fuzz should be(defined)
  }

  it should "add two fuzzy numbers with relative fuzz" in {
    val fn1 = FuzzyNumber(Value.fromDouble(Some(100.0)), PureNumber, Some(RelativeFuzz[Double](0.01, Gaussian)))
    val fn2 = FuzzyNumber(Value.fromDouble(Some(50.0)), PureNumber, Some(RelativeFuzz[Double](0.02, Gaussian)))

    val result = fn1.doAdd(fn2)
    result shouldBe a[Number]
    result.fuzz should be(defined)
  }

  it should "add fuzzy and exact numbers" in {
    val fn1 = FuzzyNumber(Value.fromDouble(Some(100.0)), PureNumber, Some(RelativeFuzz[Double](0.01, Gaussian)))
    val fn2 = FuzzyNumber(Value.fromDouble(Some(50.0)), PureNumber, None)

    val result = fn1.doAdd(fn2)
    result shouldBe a[Number]
    // Result should have fuzz from fn1
    result.fuzz should be(defined)
  }

  behavior of "FuzzyNumber multiplication"

  it should "multiply two fuzzy numbers with relative fuzz (independent)" in {
    val fn1 = FuzzyNumber(Value.fromDouble(Some(100.0)), PureNumber, Some(RelativeFuzz[Double](0.01, Gaussian)))
    val fn2 = FuzzyNumber(Value.fromDouble(Some(2.0)), PureNumber, Some(RelativeFuzz[Double](0.02, Gaussian)))

    val result = fn1.doMultiply(fn2)
    result shouldBe a[Number]
    result.fuzz should be(defined)

    // Error should be combined (independent case)
    val resultFuzz = result.fuzz.get.asInstanceOf[RelativeFuzz[Double]]
    // sqrt(0.01^2 + 0.02^2 + 0.01^2*0.02^2) ≈ 0.0224
    resultFuzz.tolerance should be > 0.02
  }

  it should "multiply fuzzy number by itself (non-independent)" in {
    val fn = FuzzyNumber(Value.fromDouble(Some(10.0)), PureNumber, Some(RelativeFuzz[Double](0.01, Gaussian)))

    val result = fn.doMultiply(fn)
    result shouldBe a[Number]
    result.fuzz should be(defined)

    // Error should be doubled for squaring (non-independent)
    val resultFuzz = result.fuzz.get.asInstanceOf[RelativeFuzz[Double]]
    resultFuzz.tolerance shouldBe 0.02 +- 0.001
  }

  it should "multiply fuzzy and exact numbers" in {
    val fn1 = FuzzyNumber(Value.fromDouble(Some(100.0)), PureNumber, Some(RelativeFuzz[Double](0.01, Gaussian)))
    val fn2 = FuzzyNumber(Value.fromDouble(Some(2.0)), PureNumber, None)

    val result = fn1.doMultiply(fn2)
    result shouldBe a[Number]
    // Result should have fuzz from fn1
    result.fuzz should be(defined)
  }

  behavior of "FuzzyNumber power"

  it should "raise fuzzy number to integer power" in {
    val fn = FuzzyNumber(Value.fromDouble(Some(10.0)), PureNumber, Some(RelativeFuzz[Double](0.01, Gaussian)))
    val power = FuzzyNumber(Value.fromInt(2), PureNumber, None)

    val result = fn.doPower(power)
    result shouldBe a[Number]
    result.fuzz should be(defined)

    // For x^2, relative error is 2 * original
    val resultFuzz = result.fuzz.get.asInstanceOf[RelativeFuzz[Double]]
    resultFuzz.tolerance shouldBe 0.02 +- 0.001
  }

  it should "raise fuzzy number to fractional power" in {
    val fn = FuzzyNumber(Value.fromDouble(Some(100.0)), PureNumber, Some(RelativeFuzz[Double](0.01, Gaussian)))
    val power = FuzzyNumber(Value.fromRational(Rational(1, 2)), PureNumber, None) // Square root

    val result = fn.doPower(power)
    result shouldBe a[Number]
    result.fuzz should be(defined)

    // For sqrt(x), relative error is 0.5 * original
    val resultFuzz = result.fuzz.get.asInstanceOf[RelativeFuzz[Double]]
    resultFuzz.tolerance shouldBe 0.005 +- 0.001
  }

  behavior of "FuzzyNumber comparison"

  it should "compare fuzzy numbers with overlapping error bounds as equal" in {
    val fn1 = FuzzyNumber(Value.fromDouble(Some(100.0)), PureNumber, Some(AbsoluteFuzz[Double](2.0, Gaussian)))
    val fn2 = FuzzyNumber(Value.fromDouble(Some(101.0)), PureNumber, Some(AbsoluteFuzz[Double](2.0, Gaussian)))

    val comparison = fn1.compare(fn2)
    // With overlapping error bounds at 50% confidence, should be equal
    comparison shouldBe 0
  }

  it should "compare fuzzy numbers with non-overlapping error bounds correctly" in {
    val fn1 = FuzzyNumber(Value.fromDouble(Some(100.0)), PureNumber, Some(AbsoluteFuzz[Double](0.1, Gaussian)))
    val fn2 = FuzzyNumber(Value.fromDouble(Some(200.0)), PureNumber, Some(AbsoluteFuzz[Double](0.1, Gaussian)))

    val comparison = fn1.compare(fn2)
    comparison should be < 0 // fn1 < fn2
  }

  it should "treat exact numbers with same value as equal" in {
    val fn1 = FuzzyNumber(Value.fromDouble(Some(100.0)), PureNumber, None)
    val fn2 = FuzzyNumber(Value.fromDouble(Some(100.0)), PureNumber, None)

    val comparison = fn1.compare(fn2)
    comparison shouldBe 0
  }

  behavior of "FuzzyNumber.isProbablyZero"

  it should "detect zero with exact number" in {
    val fn = FuzzyNumber(Value.fromInt(0), PureNumber, None)
    fn.isProbablyZero() shouldBe true
  }

  it should "detect probably zero with large fuzz" in {
    val fn = FuzzyNumber(Value.fromDouble(Some(0.1)), PureNumber, Some(AbsoluteFuzz[Double](1.0, Gaussian)))
    fn.isProbablyZero(0.5) shouldBe true
  }

  it should "detect not zero with small fuzz" in {
    val fn = FuzzyNumber(Value.fromDouble(Some(10.0)), PureNumber, Some(AbsoluteFuzz[Double](0.1, Gaussian)))
    fn.isProbablyZero(0.5) shouldBe false
  }

  it should "handle different confidence levels" in {
    val fn = FuzzyNumber(Value.fromDouble(Some(1.0)), PureNumber, Some(AbsoluteFuzz[Double](1.0, Gaussian)))

    // At low confidence, more likely to be considered zero
    fn.isProbablyZero(0.1) shouldBe true

    // At high confidence, less likely to be considered zero
    fn.isProbablyZero(0.9) shouldBe false
  }

  behavior of "FuzzyNumber.signum"

  it should "return 0 for probably zero" in {
    val fn = FuzzyNumber(Value.fromDouble(Some(0.1)), PureNumber, Some(AbsoluteFuzz[Double](1.0, Gaussian)))
    fn.signum shouldBe 0
  }

  it should "return 1 for positive number" in {
    val fn = FuzzyNumber(Value.fromDouble(Some(10.0)), PureNumber, Some(AbsoluteFuzz[Double](0.1, Gaussian)))
    fn.signum shouldBe 1
  }

  it should "return -1 for negative number" in {
    val fn = FuzzyNumber(Value.fromDouble(Some(-10.0)), PureNumber, Some(AbsoluteFuzz[Double](0.1, Gaussian)))
    fn.signum shouldBe -1
  }

  it should "respect confidence parameter" in {
    val fn = FuzzyNumber(Value.fromDouble(Some(1.0)), PureNumber, Some(AbsoluteFuzz[Double](1.0, Gaussian)))

    fn.signum(0.1) shouldBe 0 // Low confidence -> probably zero
    fn.signum(0.9) shouldBe 1 // High confidence -> positive
  }

  behavior of "FuzzyNumber.isZero"

  it should "delegate to isProbablyZero with 50% confidence" in {
    val fn1 = FuzzyNumber(Value.fromInt(0), PureNumber, None)
    fn1.isZero shouldBe true

    val fn2 = FuzzyNumber(Value.fromDouble(Some(10.0)), PureNumber, Some(AbsoluteFuzz[Double](0.1, Gaussian)))
    fn2.isZero shouldBe false
  }

  behavior of "FuzzyNumber.isSame"

  it should "compare fuzzy numbers using fuzzy equality" in {
    val fn1 = FuzzyNumber(Value.fromDouble(Some(100.0)), PureNumber, Some(AbsoluteFuzz[Double](2.0, Gaussian)))
    val fn2 = FuzzyNumber(Value.fromDouble(Some(101.0)), PureNumber, Some(AbsoluteFuzz[Double](2.0, Gaussian)))

    fn1.isSame(fn2) shouldBe true // Within error bounds
  }

  it should "distinguish clearly different numbers" in {
    val fn1 = FuzzyNumber(Value.fromDouble(Some(100.0)), PureNumber, Some(AbsoluteFuzz[Double](0.1, Gaussian)))
    val fn2 = FuzzyNumber(Value.fromDouble(Some(200.0)), PureNumber, Some(AbsoluteFuzz[Double](0.1, Gaussian)))

    fn1.isSame(fn2) shouldBe false
  }

  behavior of "FuzzyNumber.simplify"

  it should "convert fuzzy number to exact number when fuzz is None" in {
    val fn = FuzzyNumber(Value.fromDouble(Some(100.0)), PureNumber, None)
    val simplified = fn.simplify

    simplified shouldBe an[ExactNumber]
  }

  it should "scale NthRoot factor to PureNumber for fuzzy numbers" in {
    val fn = FuzzyNumber(Value.fromDouble(Some(2.0)), SquareRoot, Some(RelativeFuzz[Double](0.01, Gaussian)))
    val simplified = fn.simplify

    simplified.factor shouldBe PureNumber
  }

  it should "preserve non-root factors" in {
    val fn = FuzzyNumber(Value.fromDouble(Some(2.0)), Radian, Some(RelativeFuzz[Double](0.01, Gaussian)))
    val simplified = fn.simplify

    simplified.factor shouldBe Radian
  }

  behavior of "FuzzyNumber.toString"

  it should "render fuzzy number with Gaussian fuzz" in {
    val fn = FuzzyNumber(Value.fromDouble(Some(100.0)), PureNumber, Some(AbsoluteFuzz[Double](0.5, Gaussian)))
    val str = fn.toString

    str should include("(")
    str should include(")")
  }

  it should "render fuzzy number with Box fuzz" in {
    val fn = FuzzyNumber(Value.fromDouble(Some(100.0)), PureNumber, Some(AbsoluteFuzz[Double](0.5, Box)))
    val str = fn.render

    str should include("*")
  }

  it should "render fuzzy number with relative fuzz as percentage" in {
    val fn = FuzzyNumber(Value.fromDouble(Some(100.0)), PureNumber, Some(RelativeFuzz[Double](0.01, Gaussian)))
    val str = fn.render

    str should include("%")
  }

  it should "handle very small fuzz by showing asterisk" in {
    val fn = FuzzyNumber(Value.fromDouble(Some(100.0)), PureNumber, Some(AbsoluteFuzz[Double](1e-20, Gaussian)))
    val str = fn.render

    str should include("*")
  }

  behavior of "FuzzyNumber with different factors"

  it should "handle Radian factor" in {
    val fn = FuzzyNumber(Value.fromDouble(Some(1.5)), Radian, Some(RelativeFuzz[Double](0.01, Gaussian)))
    fn.factor shouldBe Radian
  }

  it should "handle NatLog factor" in {
    val fn = FuzzyNumber(Value.fromDouble(Some(2.0)), NatLog, Some(RelativeFuzz[Double](0.01, Gaussian)))
    fn.factor shouldBe NatLog
  }

  it should "handle SquareRoot factor" in {
    val fn = FuzzyNumber(Value.fromDouble(Some(2.0)), SquareRoot, Some(RelativeFuzz[Double](0.01, Gaussian)))
    fn.factor shouldBe SquareRoot
  }

  behavior of "FuzzyNumber edge cases"

  it should "handle very large values" in {
    val fn = FuzzyNumber(Value.fromDouble(Some(1e100)), PureNumber, Some(RelativeFuzz[Double](0.01, Gaussian)))
    fn.toNominalDouble should be(defined)
  }

  it should "handle very small values" in {
    val fn = FuzzyNumber(Value.fromDouble(Some(1e-100)), PureNumber, Some(RelativeFuzz[Double](0.01, Gaussian)))
    fn.toNominalDouble should be(defined)
  }

  it should "handle negative values" in {
    val fn = FuzzyNumber(Value.fromDouble(Some(-100.0)), PureNumber, Some(RelativeFuzz[Double](0.01, Gaussian)))
    fn.signum shouldBe -1
  }

  it should "handle zero with fuzz" in {
    val fn = FuzzyNumber(Value.fromDouble(Some(0.0)), PureNumber, Some(AbsoluteFuzz[Double](1.0, Gaussian)))
    fn.isProbablyZero() shouldBe true
  }

  behavior of "FuzzyNumber.addFuzz"

  it should "add fuzz to a fuzzy number" in {
    val fn = FuzzyNumber(Value.fromDouble(Some(100.0)), PureNumber, Some(RelativeFuzz[Double](0.01, Gaussian)))
    val additionalFuzz = AbsoluteFuzz[Double](1.0, Gaussian)

    val result = FuzzyNumber.addFuzz(fn, additionalFuzz)
    result.fuzz should be(defined)
  }

  it should "add fuzz to an exact number" in {
    val fn = FuzzyNumber(Value.fromDouble(Some(100.0)), PureNumber, None)
    val additionalFuzz = RelativeFuzz[Double](0.01, Gaussian)

    val result = FuzzyNumber.addFuzz(fn, additionalFuzz)
    result.fuzz should be(defined)
  }

  behavior of "FuzzyNumber.fuzzyCompare"

  it should "compare numbers with 50% confidence by default" in {
    val fn1 = FuzzyNumber(Value.fromDouble(Some(100.0)), PureNumber, Some(AbsoluteFuzz[Double](2.0, Gaussian)))
    val fn2 = FuzzyNumber(Value.fromDouble(Some(101.0)), PureNumber, Some(AbsoluteFuzz[Double](2.0, Gaussian)))

    val result = FuzzyNumber.fuzzyCompare(fn1, fn2, 0.5)
    result shouldBe 0 // Should be considered equal
  }

  it should "distinguish numbers at higher confidence" in {
    val fn1 = FuzzyNumber(Value.fromDouble(Some(100.0)), PureNumber, Some(AbsoluteFuzz[Double](0.5, Gaussian)))
    val fn2 = FuzzyNumber(Value.fromDouble(Some(101.0)), PureNumber, Some(AbsoluteFuzz[Double](0.5, Gaussian)))

    val result = FuzzyNumber.fuzzyCompare(fn1, fn2, 0.9)
    result should be < 0 // fn1 < fn2 at high confidence
  }

  behavior of "FuzzyNumber integration tests"

  it should "handle complex expression: (x + y) * z" in {
    val x = FuzzyNumber(Value.fromDouble(Some(10.0)), PureNumber, Some(RelativeFuzz[Double](0.01, Gaussian)))
    val y = FuzzyNumber(Value.fromDouble(Some(5.0)), PureNumber, Some(RelativeFuzz[Double](0.02, Gaussian)))
    val z = FuzzyNumber(Value.fromDouble(Some(2.0)), PureNumber, Some(RelativeFuzz[Double](0.01, Gaussian)))

    val sum = x.doAdd(y)
    val result = sum.doMultiply(z)

    result shouldBe a[Number]
    result.fuzz should be(defined)
  }

  it should "handle repeated operations: x * x * x" in {
    val x = FuzzyNumber(Value.fromDouble(Some(2.0)), PureNumber, Some(RelativeFuzz[Double](0.01, Gaussian)))

    val x2 = x.doMultiply(x)
    val x3 = x2.doMultiply(x)

    x3 shouldBe a[Number]
    x3.fuzz should be(defined)

    // Error should accumulate
    val resultFuzz = x3.fuzz.get.asInstanceOf[RelativeFuzz[Double]]
    resultFuzz.tolerance should be > 0.02 // At least 2 * original error
  }

  it should "handle division through multiplication by inverse" in {
    val x = FuzzyNumber(Value.fromDouble(Some(100.0)), PureNumber, Some(RelativeFuzz[Double](0.01, Gaussian)))
    val y = FuzzyNumber(Value.fromDouble(Some(2.0)), PureNumber, Some(RelativeFuzz[Double](0.02, Gaussian)))

    // Division is typically x * (1/y)
    val yInv = FuzzyNumber(Value.fromDouble(Some(0.5)), PureNumber, y.fuzz) // Simplified
    val result = x.doMultiply(yInv)

    result shouldBe a[Number]
    result.fuzz should be(defined)
  }
}
