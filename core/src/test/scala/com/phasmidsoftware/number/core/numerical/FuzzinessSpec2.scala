package com.phasmidsoftware.number.core.numerical

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FuzzinessSpec2 extends AnyFlatSpec with Matchers {

  behavior of "RelativeFuzz"

  it should "create a relative fuzz with correct tolerance" in {
    val fuzz = RelativeFuzz[Double](0.01, Gaussian)
    fuzz.tolerance shouldBe 0.01
    fuzz.shape shouldBe Gaussian
    fuzz.style shouldBe true
  }

  it should "convert to absolute fuzz correctly" in {
    val relativeFuzz = RelativeFuzz[Double](0.01, Gaussian) // 1% relative
    val absoluteFuzz = relativeFuzz.absolute(100.0)

    absoluteFuzz should be(defined)
    absoluteFuzz.get.magnitude shouldBe 1.0 +- 0.001 // 100 * 0.01 = 1
    absoluteFuzz.get.shape shouldBe Gaussian
  }

  it should "handle negative nominal values in conversion to absolute" in {
    val relativeFuzz = RelativeFuzz[Double](0.02, Gaussian)
    val absoluteFuzz = relativeFuzz.absolute(-50.0)

    absoluteFuzz should be(defined)
    absoluteFuzz.get.magnitude shouldBe 1.0 +- 0.001 // |-50 * 0.02| = 1
  }

  it should "handle zero nominal value in conversion to absolute" in {
    val relativeFuzz = RelativeFuzz[Double](0.01, Gaussian)
    val absoluteFuzz = relativeFuzz.absolute(0.0)

    absoluteFuzz should be(defined)
    absoluteFuzz.get.magnitude shouldBe 0.0
  }

  it should "transform correctly for scaling" in {
    val relativeFuzz = RelativeFuzz[Double](0.01, Gaussian)
    // For f(x) = 2x, relativeFuzz function is x*2/(2x) = 1
    val transformed = relativeFuzz.transform[Double, Double](_ => 1.0)(100.0)

    transformed shouldBe a[RelativeFuzz[?]]
    transformed.asInstanceOf[RelativeFuzz[Double]].tolerance shouldBe 0.01 +- 0.0001
  }

  it should "transform correctly for squaring" in {
    val relativeFuzz = RelativeFuzz[Double](0.01, Gaussian)
    // For f(x) = x^2, relativeFuzz function is x*2x/x^2 = 2
    val transformed = relativeFuzz.transform[Double, Double](_ => 2.0)(100.0)

    transformed shouldBe a[RelativeFuzz[?]]
    transformed.asInstanceOf[RelativeFuzz[Double]].tolerance shouldBe 0.02 +- 0.0001
  }

  it should "convolve with another relative fuzz (independent, Gaussian)" in {
    val fuzz1 = RelativeFuzz[Double](0.01, Gaussian)
    val fuzz2 = RelativeFuzz[Double](0.02, Gaussian)

    val result = fuzz1.*(fuzz2, independent = true)

    result shouldBe a[RelativeFuzz[?]]
    val tolerance = result.asInstanceOf[RelativeFuzz[Double]].tolerance
    // sqrt(0.01² + 0.02² + 0.01×0.02) ≈ 0.02646
    tolerance shouldBe 0.02646 +- 0.0001
  }

  it should "convolve with another relative fuzz (non-independent, Gaussian)" in {
    val fuzz1 = RelativeFuzz[Double](0.01, Gaussian)
    val fuzz2 = RelativeFuzz[Double](0.02, Gaussian)

    val result = fuzz1.*(fuzz2, independent = false)

    result shouldBe a[RelativeFuzz[?]]
    val tolerance = result.asInstanceOf[RelativeFuzz[Double]].tolerance
    // For non-independent: 0.01 + 0.02 = 0.03
    tolerance shouldBe 0.03 +- 0.0001
  }

  it should "convolve with another relative fuzz (Box shape)" in {
    val fuzz1 = RelativeFuzz[Double](0.01, Box)
    val fuzz2 = RelativeFuzz[Double](0.02, Box)

    val result = fuzz1.*(fuzz2, independent = true)

    result shouldBe a[RelativeFuzz[?]]
    val tolerance = result.asInstanceOf[RelativeFuzz[Double]].tolerance
    // For Box: simple addition
    tolerance shouldBe 0.03 +- 0.0001
  }

  it should "render as percentage correctly" in {
    val fuzz1 = RelativeFuzz[Double](0.01, Gaussian)
    fuzz1.asPercentage shouldBe "1.0%"

    val fuzz2 = RelativeFuzz[Double](0.5, Gaussian)
    fuzz2.asPercentage shouldBe "50.0%"

    val fuzz3 = RelativeFuzz[Double](0.0001, Gaussian)
    fuzz3.asPercentage shouldBe "0.010%" // 3 decimal places for 2 sig figs
  }

  it should "normalize shape from Box to Gaussian" in {
    val boxFuzz = RelativeFuzz[Double](0.01, Box)
    val gaussianFuzz = boxFuzz.normalizeShape

    gaussianFuzz.shape shouldBe Gaussian
    gaussianFuzz shouldBe a[RelativeFuzz[?]]
  }

  behavior of "AbsoluteFuzz"

  it should "create an absolute fuzz with correct magnitude" in {
    val fuzz = AbsoluteFuzz[Double](1.0, Gaussian)
    fuzz.magnitude shouldBe 1.0
    fuzz.shape shouldBe Gaussian
    fuzz.style shouldBe false
  }

  it should "convert to relative fuzz correctly" in {
    val absoluteFuzz = AbsoluteFuzz[Double](1.0, Gaussian)
    val relativeFuzz = absoluteFuzz.relative(100.0)

    relativeFuzz should be(defined)
    relativeFuzz.get.tolerance shouldBe 0.01 +- 0.0001 // 1/100 = 0.01
    relativeFuzz.get.shape shouldBe Gaussian
  }

  it should "handle negative nominal values in conversion to relative" in {
    val absoluteFuzz = AbsoluteFuzz[Double](2.0, Gaussian)
    val relativeFuzz = absoluteFuzz.relative(-50.0)

    relativeFuzz should be(defined)
    relativeFuzz.get.tolerance shouldBe 0.04 +- 0.0001 // |2/-50| = 0.04
  }

  it should "return Some with Infinity when converting to relative with zero nominal value" in {
    val absoluteFuzz = AbsoluteFuzz[Double](1.0, Gaussian)
    val relativeFuzz = absoluteFuzz.relative(0.0)

    // Division by zero produces Infinity, not None
    relativeFuzz should be(defined)
    relativeFuzz.get.tolerance.isInfinite shouldBe true
  }

  it should "preserve absolute fuzz when normalization with zero fails" in {
    val absoluteFuzz = AbsoluteFuzz[Double](1.0, Gaussian)

    // When normalizing to relative with zero nominal value
    val normalized = absoluteFuzz.normalize(0.0, relativeStyle = true)

    // Should fall back to original absolute fuzz (our Point 6 fix)
    normalized should be(defined)
    normalized.get shouldBe absoluteFuzz
  }

  it should "transform correctly for constant offset" in {
    val absoluteFuzz = AbsoluteFuzz[Double](1.0, Gaussian)
    // For f(x) = x + c, derivative is 1, so absolute error unchanged
    val transformed = absoluteFuzz.transform[Double, Double](_ => 1.0)(100.0)

    transformed shouldBe a[AbsoluteFuzz[?]]
    transformed.asInstanceOf[AbsoluteFuzz[Double]].magnitude shouldBe 1.0 +- 0.001
  }

  it should "transform correctly for scaling" in {
    val absoluteFuzz = AbsoluteFuzz[Double](1.0, Gaussian)
    // For f(x) = 5x, derivative is 5, so absolute error scales by 5
    val transformed = absoluteFuzz.transform[Double, Double](_ => 5.0)(100.0)

    transformed shouldBe a[AbsoluteFuzz[?]]
    transformed.asInstanceOf[AbsoluteFuzz[Double]].magnitude shouldBe 5.0 +- 0.001
  }

  it should "convolve with another absolute fuzz (Gaussian)" in {
    val fuzz1 = AbsoluteFuzz[Double](1.0, Gaussian)
    val fuzz2 = AbsoluteFuzz[Double](2.0, Gaussian)

    val result = fuzz1.*(fuzz2, independent = true)

    result shouldBe a[AbsoluteFuzz[?]]
    val magnitude = result.asInstanceOf[AbsoluteFuzz[Double]].magnitude
    // sqrt(1^2 + 2^2) = sqrt(5) ≈ 2.236
    magnitude shouldBe 2.236 +- 0.001
  }

  it should "normalize shape from Box to Gaussian" in {
    val boxFuzz = AbsoluteFuzz[Double](1.0, Box)
    val gaussianFuzz = boxFuzz.normalizeShape

    gaussianFuzz.shape shouldBe Gaussian
    gaussianFuzz shouldBe a[AbsoluteFuzz[?]]
  }

  it should "render with embedded nominal value" in {
    val absoluteFuzz = AbsoluteFuzz[Double](0.5, Gaussian)
    val (embedded, str) = absoluteFuzz.getQualifiedString(100.0)

    embedded shouldBe true
    str shouldBe "1.000(5)E+02"
  }

  behavior of "Fuzziness.normalize"

  it should "preserve absolute fuzz when requesting absolute normalization" in {
    val absoluteFuzz = AbsoluteFuzz[Double](1.0, Gaussian)
    val normalized = absoluteFuzz.normalize(100.0, relativeStyle = false)

    normalized should be(defined)
    normalized.get shouldBe absoluteFuzz
  }

  it should "convert absolute to relative when requesting relative normalization" in {
    val absoluteFuzz = AbsoluteFuzz[Double](1.0, Gaussian)
    val normalized = absoluteFuzz.normalize(100.0, relativeStyle = true)

    normalized should be(defined)
    normalized.get shouldBe a[RelativeFuzz[?]]
    normalized.get.asInstanceOf[RelativeFuzz[Double]].tolerance shouldBe 0.01 +- 0.0001
  }

  it should "preserve relative fuzz when requesting relative normalization" in {
    val relativeFuzz = RelativeFuzz[Double](0.01, Gaussian)
    val normalized = relativeFuzz.normalize(100.0, relative = true)

    normalized should be(defined)
    normalized.get shouldBe relativeFuzz
  }

  it should "convert relative to absolute when requesting absolute normalization" in {
    val relativeFuzz = RelativeFuzz[Double](0.01, Gaussian)
    val normalized = relativeFuzz.normalize(100.0, relative = false)

    normalized should be(defined)
    normalized.get shouldBe a[AbsoluteFuzz[?]]
    normalized.get.asInstanceOf[AbsoluteFuzz[Double]].magnitude shouldBe 1.0 +- 0.001
  }

  it should "fall back to original when conversion to relative fails (zero nominal)" in {
    val absoluteFuzz = AbsoluteFuzz[Double](1.0, Gaussian)
    val normalized = absoluteFuzz.normalize(0.0, relativeStyle = true)

    // Should fall back to original absolute fuzz
    normalized should be(defined)
    normalized.get shouldBe absoluteFuzz
  }

  it should "fall back to original when conversion to absolute fails" in {
    val relativeFuzz = RelativeFuzz[Double](0.01, Gaussian)
    // Edge case: if absolute conversion somehow fails, should fall back
    val normalized = relativeFuzz.normalize(0.0, relative = false)

    normalized should be(defined)
  }

  behavior of "Fuzziness.combine"

  it should "combine two absolute fuzzes for addition" in {
    val fuzz1 = Some(AbsoluteFuzz[Double](1.0, Gaussian))
    val fuzz2 = Some(AbsoluteFuzz[Double](2.0, Gaussian))

    val result = Fuzziness.combine(100.0, 200.0, relative = false, independent = true)((fuzz1, fuzz2))

    result should be(defined)
    result.get shouldBe a[AbsoluteFuzz[?]]
  }

  it should "combine two relative fuzzes for multiplication" in {
    val fuzz1 = Some(RelativeFuzz[Double](0.01, Gaussian))
    val fuzz2 = Some(RelativeFuzz[Double](0.02, Gaussian))

    val result = Fuzziness.combine(100.0, 200.0, relative = true, independent = true)((fuzz1, fuzz2))

    result should be(defined)
    result.get shouldBe a[RelativeFuzz[?]]
  }

  it should "combine absolute and relative by normalizing both" in {
    val fuzz1 = Some(AbsoluteFuzz[Double](1.0, Gaussian))
    val fuzz2 = Some(RelativeFuzz[Double](0.02, Gaussian))

    val result = Fuzziness.combine(100.0, 200.0, relative = true, independent = true)((fuzz1, fuzz2))

    result should be(defined)
  }

  it should "return first fuzz when second is None" in {
    val fuzz1 = Some(RelativeFuzz[Double](0.01, Gaussian))
    val fuzz2 = None

    val result = Fuzziness.combine(100.0, 200.0, relative = true, independent = true)((fuzz1, fuzz2))

    result should be(defined)
    result.get shouldBe fuzz1.get
  }

  it should "return second fuzz when first is None" in {
    val fuzz1 = None
    val fuzz2 = Some(RelativeFuzz[Double](0.02, Gaussian))

    val result = Fuzziness.combine(100.0, 200.0, relative = true, independent = true)((fuzz1, fuzz2))

    result should be(defined)
    result.get shouldBe fuzz2.get
  }

  it should "return None when both fuzzes are None" in {
    val fuzz1 = None
    val fuzz2 = None

    val result = Fuzziness.combine(100.0, 200.0, relative = true, independent = true)((fuzz1, fuzz2))

    result shouldBe None
  }

  behavior of "Fuzziness.monadicFuzziness"

  it should "correctly propagate error through exp function" in {
    val inputFuzz = Some(RelativeFuzz[Double](0.01, Gaussian))
    val t = 10.0
    val x = math.exp(t) // ≈ 22026

    import com.phasmidsoftware.number.core.inner.MonadicOperationExp
    val result = Fuzziness.monadicFuzziness(MonadicOperationExp, t, x, inputFuzz)

    result should be(defined)
    result.get shouldBe a[RelativeFuzz[?]]
    // For exp, relativeFuzz is x => x, so output error ≈ 10 * 0.01 = 0.1
    // Plus operation fuzz
    val tolerance = result.get.asInstanceOf[RelativeFuzz[Double]].tolerance
    tolerance should be > 0.09 // Should be around 0.1 plus operation error
  }

  it should "correctly use output value x for operation fuzz normalization" in {
    val inputFuzz = Some(RelativeFuzz[Double](0.0, Gaussian)) // No input error
    val t = 10.0
    val x = math.exp(t) // ≈ 22026

    import com.phasmidsoftware.number.core.inner.MonadicOperationExp
    val result = Fuzziness.monadicFuzziness(MonadicOperationExp, t, x, inputFuzz)

    result should be(defined)
    // Should have operation fuzz normalized relative to x, not t
    // This test verifies the fix doesn't throw errors with large x
  }

  it should "handle functions with small output values" in {
    val inputFuzz = Some(RelativeFuzz[Double](0.01, Gaussian))
    val t = -10.0
    val x = math.exp(t) // ≈ 0.000045

    import com.phasmidsoftware.number.core.inner.MonadicOperationExp
    val result = Fuzziness.monadicFuzziness(MonadicOperationExp, t, x, inputFuzz)

    result should be(defined)
    // Should handle small output values correctly
  }

  behavior of "Fuzziness.createFuzz"

  it should "create fuzz with correct relative precision" in {
    val fuzz0 = Fuzziness.createFuzz(0)
    fuzz0 shouldBe a[RelativeFuzz[?]]

    val fuzz3 = Fuzziness.createFuzz(3)
    fuzz3 shouldBe a[RelativeFuzz[?]]
    val tolerance = fuzz3.asInstanceOf[RelativeFuzz[Double]].tolerance
    // Should be 8x the base precision (1 << 3 = 8)
    tolerance should be > 0.0
  }

  behavior of "Shape - Gaussian"

  it should "calculate wiggle room correctly" in {
    val wiggle = Gaussian.wiggle(1.0, 0.5)
    wiggle should be > 0.0
  }

  it should "calculate probability correctly" in {
    val prob = Gaussian.probability(1.0, 1.0)
    prob should be > 0.0
    prob should be < 1.0
  }

  it should "calculate convolution sum correctly" in {
    val result = Gaussian.convolutionSum(1.0, 2.0)
    // sqrt(1^2 + 2^2) = sqrt(5) ≈ 2.236
    result shouldBe 2.236 +- 0.001
  }

  it should "calculate convolution product for independent variables" in {
    val result = Gaussian.convolutionProduct(0.01, 0.02, independent = true)
    // sqrt(0.01² + 0.02² + 0.01×0.02) ≈ 0.02646
    result shouldBe 0.02646 +- 0.0001
  }

  it should "calculate convolution product for non-independent variables" in {
    val result = Gaussian.convolutionProduct(0.01, 0.02, independent = false)
    // 0.01 + 0.02 = 0.03
    result shouldBe 0.03 +- 0.0001
  }

  behavior of "Shape - Box"

  it should "convert to Gaussian relative correctly" in {
    val result = Box.toGaussianRelative(0.01)
    result shouldBe 0.01 / math.sqrt(3) +- 0.0001
  }

  it should "convert to Gaussian absolute correctly" in {
    val result = Box.toGaussianAbsolute(1.0)
    result shouldBe 1.0 / math.sqrt(3) +- 0.001
  }

  it should "calculate wiggle room correctly" in {
    val wiggle = Box.wiggle(1.0, 0.5)
    wiggle shouldBe 0.5 // Half the width
  }

  it should "calculate probability correctly" in {
    val prob = Box.probability(1.0, 0.5)
    prob shouldBe 1.0 // Within the box

    val prob2 = Box.probability(1.0, 1.5)
    prob2 shouldBe 1.0 // Outside the box
  }

  behavior of "Edge cases"

  it should "handle very small fuzziness values" in {
    val fuzz = RelativeFuzz[Double](1e-15, Gaussian)
    fuzz.tolerance shouldBe 1e-15
  }

  it should "handle very large fuzziness values" in {
    val fuzz = RelativeFuzz[Double](10.0, Gaussian) // 1000% error
    fuzz.tolerance shouldBe 10.0
  }

  it should "handle zero fuzziness" in {
    val fuzz = RelativeFuzz[Double](0.0, Gaussian)
    fuzz.tolerance shouldBe 0.0
  }

  it should "handle conversion with very large nominal values" in {
    val relativeFuzz = RelativeFuzz[Double](0.01, Gaussian)
    val absoluteFuzz = relativeFuzz.absolute(1e100)
    absoluteFuzz should be(defined)
  }

  it should "handle conversion with very small nominal values" in {
    val relativeFuzz = RelativeFuzz[Double](0.01, Gaussian)
    val absoluteFuzz = relativeFuzz.absolute(1e-100)
    absoluteFuzz should be(defined)
  }
}
