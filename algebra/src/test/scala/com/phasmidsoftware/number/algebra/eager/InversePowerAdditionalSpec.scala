/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.eager

import com.phasmidsoftware.number.algebra.core.AnyContext
import com.phasmidsoftware.number.core.inner.{CubeRoot, PureNumber, Rational, SquareRoot}
import com.phasmidsoftware.number.core.numerical.{Box, RelativeFuzz}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

/**
  * Additional comprehensive tests for InversePower beyond those in InversePowerSpec.
  */
class InversePowerAdditionalSpec extends AnyFlatSpec with should.Matchers {

  behavior of "InversePower construction"

  it should "require positive n" in {
    an[IllegalArgumentException] should be thrownBy InversePower(0, WholeNumber(2))
    an[IllegalArgumentException] should be thrownBy InversePower(-1, WholeNumber(2))
  }

  it should "require non-zero number" in {
    an[IllegalArgumentException] should be thrownBy InversePower(2, WholeNumber.zero)
  }

  it should "construct with named values" in {
    val root = InversePower(2, WholeNumber(2))(Some("√2"))
    root.maybeName shouldBe Some("√2")
    root.render shouldBe "√2"
  }

  it should "construct via apply methods" in {
    val root1 = InversePower(2, WholeNumber(4))
    root1.n shouldBe 2
    root1.number shouldBe WholeNumber(4)

    val root2 = InversePower(3, 8)
    root2.n shouldBe 3
    root2.number shouldBe WholeNumber(8)
  }

  behavior of "InversePower.normalizeIntegralRoot"

  it should "extract perfect square roots" in {
    InversePower.normalizeIntegralRoot(4, 2) shouldBe(2, 1)
    InversePower.normalizeIntegralRoot(9, 2) shouldBe(3, 1)
    InversePower.normalizeIntegralRoot(16, 2) shouldBe(4, 1)
    InversePower.normalizeIntegralRoot(25, 2) shouldBe(5, 1)
    InversePower.normalizeIntegralRoot(100, 2) shouldBe(10, 1)
  }

  it should "extract perfect cube roots" in {
    InversePower.normalizeIntegralRoot(8, 3) shouldBe(2, 1)
    InversePower.normalizeIntegralRoot(27, 3) shouldBe(3, 1)
    InversePower.normalizeIntegralRoot(64, 3) shouldBe(4, 1)
    InversePower.normalizeIntegralRoot(125, 3) shouldBe(5, 1)
  }

  it should "extract perfect fourth roots" in {
    InversePower.normalizeIntegralRoot(16, 4) shouldBe(2, 1)
    InversePower.normalizeIntegralRoot(81, 4) shouldBe(3, 1)
    InversePower.normalizeIntegralRoot(256, 4) shouldBe(4, 1)
  }

  it should "partially extract imperfect roots" in {
    // √8 = 2√2
    InversePower.normalizeIntegralRoot(8, 2) shouldBe(2, 2)

    // √12 = 2√3
    InversePower.normalizeIntegralRoot(12, 2) shouldBe(2, 3)

    // √18 = 3√2
    InversePower.normalizeIntegralRoot(18, 2) shouldBe(3, 2)

    // √50 = 5√2
    InversePower.normalizeIntegralRoot(50, 2) shouldBe(5, 2)

    // √72 = 6√2
    InversePower.normalizeIntegralRoot(72, 2) shouldBe(6, 2)
  }

  it should "handle cube roots with partial extraction" in {
    // ³√16 = 2 ³√2
    InversePower.normalizeIntegralRoot(16, 3) shouldBe(2, 2)

    // ³√54 = 3 ³√2
    InversePower.normalizeIntegralRoot(54, 3) shouldBe(3, 2)
  }

  it should "handle negative radicands with odd roots" in {
    InversePower.normalizeIntegralRoot(-8, 3) shouldBe(-2, 1)
    InversePower.normalizeIntegralRoot(-27, 3) shouldBe(-3, 1)
  }

  it should "handle roots of 1" in {
    InversePower.normalizeIntegralRoot(1, 2) shouldBe(1, 1)
    InversePower.normalizeIntegralRoot(1, 3) shouldBe(1, 1)
    InversePower.normalizeIntegralRoot(1, 100) shouldBe(1, 1)
  }

  it should "handle large perfect powers" in {
    // √10000 = 100
    InversePower.normalizeIntegralRoot(10000, 2) shouldBe(100, 1)

    // ³√1000 = 10
    InversePower.normalizeIntegralRoot(1000, 3) shouldBe(10, 1)
  }

  behavior of "InversePower normalization with rational bases"

  it should "normalize rational perfect squares" in {
    // √(4/9) = 2/3
    InversePower(2, RationalNumber(4, 9)).normalize shouldBe RationalNumber(2, 3)

    // √(1/4) = 1/2
    InversePower(2, RationalNumber(1, 4)).normalize shouldBe RationalNumber(1, 2)

    // √(9/16) = 3/4
    InversePower(2, RationalNumber(9, 16)).normalize shouldBe RationalNumber(3, 4)
  }

  it should "normalize rational perfect cubes" in {
    // ³√(8/27) = 2/3
    InversePower(3, RationalNumber(8, 27)).normalize shouldBe RationalNumber(2, 3)

    // ³√(1/8) = 1/2
    InversePower(3, RationalNumber(1, 8)).normalize shouldBe RationalNumber(1, 2)
  }

  it should "keep imperfect rational roots as InversePower" in {
    val root = InversePower(2, RationalNumber(2, 3))
    root.normalize should matchPattern { case InversePower(2, RationalNumber(Rational(2, 3), _)) => }
  }

  it should "handle negative rational bases with odd roots" in {
    // ³√(-8/27) = -2/3
    InversePower(3, RationalNumber(-8, 27)).normalize shouldBe RationalNumber(-2, 3)
  }

  behavior of "InversePower edge cases for normalize"

  it should "normalize when n=1 (identity)" in {
    InversePower(1, WholeNumber(5)).normalize shouldBe WholeNumber(5)
    InversePower(1, RationalNumber(3, 4)).normalize shouldBe RationalNumber(3, 4)
    InversePower(1, WholeNumber(100)).normalize shouldBe WholeNumber(100)
  }

  it should "handle complex normalizations" in {
    // √(16/4) = √4 = 2
    InversePower(2, RationalNumber(16, 4)).normalize shouldBe WholeNumber(2)

    // ³√(64/8) = ³√8 = 2
    InversePower(3, RationalNumber(64, 8)).normalize shouldBe WholeNumber(2)
  }

  it should "normalize nested simplifications" in {
    // Create a RationalNumber that will normalize to WholeNumber first
    InversePower(2, RationalNumber(16, 1)).normalize shouldBe WholeNumber(4)
  }

  behavior of "InversePower comparison operations"

  it should "compare equal roots" in {
    val root1 = InversePower(2, 4)
    val root2 = InversePower(2, 4)

    root1.compareTo(root2) shouldBe 0
  }

  it should "compare different roots with same degree" in {
    val root1 = InversePower(2, 2) // √2
    val root2 = InversePower(2, 3) // √3

    root1.compareTo(root2) should be < 0
    root2.compareTo(root1) should be > 0
  }

  it should "compare equivalent roots with different representations" in {
    val root1 = InversePower(2, 4) // √4 = 2
    val root2 = InversePower(2, 16) // √16 = 4

    root1.compareTo(root2) should be < 0
  }

  it should "handle Ordered trait properly" in {
    val root1 = InversePower(2, 2)
    val root2 = InversePower(2, 3)
    val root3 = InversePower(2, 2)

    (root1 < root2) shouldBe true
    (root2 > root1) shouldBe true
    (root1 <= root3) shouldBe true
    (root1 >= root3) shouldBe true
  }

  behavior of "InversePower conversion operations"

  it should "convert to RationalNumber for perfect roots" in {
    InversePower(2, 9).convert[RationalNumber](RationalNumber.zero) shouldBe Some(RationalNumber(3, 1))
    InversePower(3, 8).convert[RationalNumber](RationalNumber.zero) shouldBe Some(RationalNumber(2, 1))
  }

  it should "convert to Real for imperfect roots" in {
    val root = InversePower(2, 2)
    val real = root.convert[Real](Real.zero)

    real.isDefined shouldBe true
    real.get.toDouble shouldBe math.sqrt(2.0) +- 1e-10
  }

  it should "convert to same type returns self" in {
    val root = InversePower(2, 2)
    val converted = root.convert[InversePower](InversePower(2, 3))

    converted shouldBe Some(root)
  }

  it should "return None for unsupported conversions" in {
    val root = InversePower(2, 2)
    root.convert[WholeNumber](WholeNumber.zero) shouldBe None
  }

  behavior of "InversePower transformation"

  it should "transform to Real" in {
    val root = InversePower(2, 2)
    val real = root.transformation[Real]

    real.isDefined shouldBe true
    real.get.toDouble shouldBe math.sqrt(2.0) +- 1e-10
  }

  it should "transform perfect roots to exact values" in {
    val root = InversePower(2, 4)
    val real = root.transformation[Real]

    real.isDefined shouldBe true
    real.get shouldBe Real(2.0, None)
  }

  behavior of "InversePower power operations"

  it should "handle pow operation with exact numbers" in {
    val root = InversePower(2, 4) // √4 = 2
    val result = root pow RationalNumber(2, 1)

    result shouldBe Some(WholeNumber(4))
  }

  it should "handle pow with fractional exponents" in {
    val root = InversePower(2, 16) // √16 = 4
    val result = root pow RationalNumber(1, 2)

    result.isDefined shouldBe true
  }

  it should "return None for pow on unsupported base" in {
    println(Real(2.0).fuzz)
    println(Real(2.0).pow(RationalNumber(1, 1)).map(_.fuzz))
    val two = Real(2.0, Some(RelativeFuzz(0.01, Box)))
    println(s"${two.maybeFuzz}")
    val root: InversePower = InversePower(2, two)
    root.scaleFunction(2.0) shouldBe 1.4142135623730951
    root.derivativeFunction(2.0) shouldBe 0.3535533905932738
    val fuzz = root.maybeFuzz
    println(s"fuzz = $fuzz")
    val result = root pow RationalNumber(2, 1)

    result shouldBe Some(two)
  }

  behavior of "InversePower scale operations"

  it should "scale by rational numbers" in {
    val root = InversePower(2, 4)
    val scaled = root * Rational.two

    scaled shouldBe InversePower(2, 16)
  }

  it should "scale cube roots" in {
    val root = InversePower(3, 8)
    val scaled = root * Rational.two

    scaled shouldBe InversePower(3, 64)
  }

  it should "scale preserves root degree" in {
    val root = InversePower(5, 32)
    val scaled = root * Rational(3, 1)

    scaled.n shouldBe 5
  }

  behavior of "InversePower exactness and properties"

  it should "be exact for whole number bases" in {
    InversePower(2, 2).isExact shouldBe true
    InversePower(3, 3).isExact shouldBe true
    InversePower(2, 100).isExact shouldBe true
  }

  it should "be exact for rational bases" in {
    InversePower(2, RationalNumber(1, 2)).isExact shouldBe true
    InversePower(3, RationalNumber(8, 27)).isExact shouldBe true
  }

  it should "not be exact for fuzzy real bases" in {
    InversePower(2, Real(2.0)).isExact shouldBe false
  }

  it should "never be zero" in {
    InversePower(2, 2).isZero shouldBe false
    InversePower(3, 1000).isZero shouldBe false
    InversePower(2, RationalNumber(1, 100)).isZero shouldBe false
  }

  it should "determine signum correctly" in {
    InversePower(2, 4).signum shouldBe 1
    InversePower(3, -8).signum shouldBe -1
    InversePower(5, -32).signum shouldBe -1
    InversePower(2, RationalNumber(1, 4)).signum shouldBe 1
  }

  behavior of "InversePower maybeFactor"

  it should "return SquareRoot factor for n=2" in {
    InversePower(2, 4).maybeFactor(AnyContext) shouldBe Some(SquareRoot)
    InversePower(2, 2).maybeFactor(AnyContext) shouldBe Some(SquareRoot)
  }

  it should "return CubeRoot factor for n=3" in {
    InversePower(3, 8).maybeFactor(AnyContext) shouldBe Some(CubeRoot)
    InversePower(3, 27).maybeFactor(AnyContext) shouldBe Some(CubeRoot)
  }

  ignore should "return PureNumber for n=1" in {
    InversePower(1, 5).maybeFactor(AnyContext) shouldBe Some(PureNumber)
  }

  behavior of "InversePower toRational"

  it should "convert perfect square roots to Rational" in {
    InversePower(2, 4).toRational shouldBe Some(Rational(2))
    InversePower(2, 9).toRational shouldBe Some(Rational(3))
    InversePower(2, 25).toRational shouldBe Some(Rational(5))
  }

  it should "convert perfect cube roots to Rational" in {
    InversePower(3, 8).toRational shouldBe Some(Rational(2))
    InversePower(3, 27).toRational shouldBe Some(Rational(3))
  }

  ignore should "convert rational perfect roots" in {
    InversePower(2, RationalNumber(4, 9)).toRational shouldBe Some(Rational(2, 3))
    InversePower(3, RationalNumber(8, 27)).toRational shouldBe Some(Rational(2, 3))
  }

  it should "return None for imperfect roots" in {
    InversePower(2, 2).toRational shouldBe None
    InversePower(3, 10).toRational shouldBe None
  }

  behavior of "InversePower rendering and display"

  it should "render with Unicode root symbols" in {
    Eager.root2.render should include("√")
    Eager.root3.render should include("√")
  }

  it should "render cube roots with superscript" in {
    InversePower(3, 2).render should include("³√")
    InversePower(3, 8).render should include("³√")
  }

  ignore should "render fourth and higher roots" in {
    val root4 = InversePower(4, 16)
    root4.render should include("√")
  }

  it should "use maybeName when present" in {
    val root = new InversePower(2, 2)(Some("sqrt2"))
    root.render shouldBe "sqrt2"
  }

  behavior of "InversePower equality and fuzzy equality"

  it should "be equal to itself" in {
    val root = InversePower(2, 2)
    (root == root) shouldBe true
  }

  it should "be equal to equivalent roots" in {
    val root1 = InversePower(2, 4)
    val root2 = InversePower(2, 4)
    (root1 == root2) shouldBe true
  }

  it should "not be equal to different roots" in {
    val root1 = InversePower(2, 2)
    val root2 = InversePower(2, 3)
    (root1 == root2) shouldBe false
  }

  it should "handle fuzzy equality" in {
    val root1 = InversePower(2, 2)
    val root2 = InversePower(2, 2)

    root1.fuzzyEqv(0.95)(root2).get shouldBe true
  }

  behavior of "InversePower approximation"

  it should "approximate perfect roots exactly" in {
    val root = InversePower(2, 4)
    val approx = root.approximation

    approx shouldBe Some(Real(2.0, None))
  }

  it should "approximate imperfect roots with uncertainty" in {
    val root = InversePower(2, 2)
    val approx = root.approximation

    approx.isDefined shouldBe true
    approx.get.toDouble shouldBe math.sqrt(2.0) +- 1e-10
  }

  it should "approximate cube roots" in {
    val root = InversePower(3, 2)
    val approx = root.approximation

    approx.isDefined shouldBe true
    approx.get.toDouble shouldBe math.pow(2.0, 1.0 / 3.0) +- 1e-10
  }

  behavior of "InversePower maybeDouble"

  it should "return exact doubles for perfect roots" in {
    InversePower(2, 4).maybeDouble shouldBe Some(2.0)
    InversePower(3, 8).maybeDouble shouldBe Some(2.0)
    InversePower(2, 9).maybeDouble shouldBe Some(3.0)
  }

  it should "return doubles for imperfect but exact roots" in {
    val sqrt2 = InversePower(2, 2).maybeDouble
    sqrt2.isDefined shouldBe true
    sqrt2.get shouldBe math.sqrt(2.0) +- 1e-10
  }

  behavior of "InversePower asJavaNumber"

  it should "convert perfect roots to Java numbers" in {
    val javaNum = InversePower(2, 4).asJavaNumber
    javaNum.isDefined shouldBe true
    javaNum.get.doubleValue() shouldBe 2.0
  }

  it should "convert imperfect roots to Java doubles" in {
    val javaNum = InversePower(2, 2).asJavaNumber
    javaNum.isDefined shouldBe true
    javaNum.get.doubleValue() shouldBe math.sqrt(2.0) +- 1e-10
  }

  behavior of "InversePower scaleFunction and derivativeFunction"

  it should "have correct scaleFunction for square roots" in {
    val root = InversePower(2, 4)
    root.scaleFunction(4.0) shouldBe 2.0
    root.scaleFunction(9.0) shouldBe 3.0
    root.scaleFunction(16.0) shouldBe 4.0
  }

  it should "have correct scaleFunction for cube roots" in {
    val root = InversePower(3, 8)
    root.scaleFunction(8.0) shouldBe 2.0
    root.scaleFunction(27.0) shouldBe 3.0
  }

  it should "have correct derivativeFunction for square roots" in {
    val root = InversePower(2, 4)
    // d/dx(√x) = 1/(2√x)
    val expected = 1.0 / (2.0 * 2.0) // at x=4, √4=2
    root.derivativeFunction(4.0) shouldBe expected +- 1e-10
  }

  it should "have correct derivativeFunction for cube roots" in {
    val root = InversePower(3, 8)
    // d/dx(³√x) = 1/(3 * x^(2/3))
    val expected = 1.0 / (3.0 * math.pow(8.0, 2.0 / 3.0))
    root.derivativeFunction(8.0) shouldBe expected +- 1e-10
  }

  behavior of "InversePower companion object constants"

  it should "provide one constant" in {
    InversePower.one.n shouldBe 1
    InversePower.one.number shouldBe WholeNumber.one
    InversePower.one.maybeName shouldBe Some("1")
    InversePower.one.isUnity shouldBe true
  }

  behavior of "InversePower with complex normalization scenarios"

  it should "simplify √(x²) to x" in {
    // This tests that consecutive operations work correctly
    InversePower(2, WholeNumber(4)).normalize shouldBe WholeNumber(2)
    InversePower(2, WholeNumber(36)).normalize shouldBe WholeNumber(6)
  }

  it should "handle mixed operations" in {
    // √8 should simplify to 2√2, but since we can't represent that as a single
    // WholeNumber, it should remain as InversePower
    val root = InversePower(2, WholeNumber(8))
    root.normalize should matchPattern { case InversePower(2, WholeNumber(8)) => }
  }

  it should "normalize chains properly" in {
    // If we have a rational that normalizes, the root should follow
    val root = InversePower(2, RationalNumber(4, 1))
    root.normalize shouldBe WholeNumber(2)
  }
}