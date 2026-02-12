package com.phasmidsoftware.number.algebra.eager


import com.phasmidsoftware.number.algebra.eager.InversePower.normalizeIntegralRoot
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NormalizeRootSpec extends AnyFlatSpec with Matchers {

  behavior of "normalizeIntegralRoot"

  it should "simplify ⁴√16 to (2, 1)" in {
    val (extracted, remaining) = normalizeIntegralRoot(16, 4)
    extracted shouldBe 2
    remaining shouldBe 1
  }

  it should "simplify ⁴√32 to (2, 2)" in {
    val (extracted, remaining) = normalizeIntegralRoot(32, 4)
    extracted shouldBe 2
    remaining shouldBe 2
  }

  it should "simplify √4 to (2, 1)" in {
    val (extracted, remaining) = normalizeIntegralRoot(4, 2)
    extracted shouldBe 2
    remaining shouldBe 1
  }

  it should "simplify ³√8 to (2, 1)" in {
    val (extracted, remaining) = normalizeIntegralRoot(8, 3)
    extracted shouldBe 2
    remaining shouldBe 1
  }

  it should "simplify ⁵√32 to (2, 1)" in {
    val (extracted, remaining) = normalizeIntegralRoot(32, 5)
    extracted shouldBe 2
    remaining shouldBe 1
  }

  it should "simplify ⁶√64 to (2, 1)" in {
    val (extracted, remaining) = normalizeIntegralRoot(64, 6)
    extracted shouldBe 2
    remaining shouldBe 1
  }

  it should "partially simplify √8 to (2, 2)" in {
    val (extracted, remaining) = normalizeIntegralRoot(8, 2)
    extracted shouldBe 2
    remaining shouldBe 2
  }

  it should "partially simplify √18 to (3, 2)" in {
    // √18 = √(9·2) = 3√2
    val (extracted, remaining) = normalizeIntegralRoot(18, 2)
    extracted shouldBe 3
    remaining shouldBe 2
  }

  it should "partially simplify √50 to (5, 2)" in {
    // √50 = √(25·2) = 5√2
    val (extracted, remaining) = normalizeIntegralRoot(50, 2)
    extracted shouldBe 5
    remaining shouldBe 2
  }

  it should "not simplify √3 (prime)" in {
    val (extracted, remaining) = normalizeIntegralRoot(3, 2)
    extracted shouldBe 1
    remaining shouldBe 3
  }

  it should "not simplify ³√7 (prime)" in {
    val (extracted, remaining) = normalizeIntegralRoot(7, 3)
    extracted shouldBe 1
    remaining shouldBe 7
  }

  it should "simplify √100 to (10, 1)" in {
    // √100 = √(4·25) = √(2²·5²) = 2·5 = 10
    val (extracted, remaining) = normalizeIntegralRoot(100, 2)
    extracted shouldBe 10
    remaining shouldBe 1
  }

  it should "simplify ³√27 to (3, 1)" in {
    val (extracted, remaining) = normalizeIntegralRoot(27, 3)
    extracted shouldBe 3
    remaining shouldBe 1
  }

  it should "simplify ⁴√81 to (3, 1)" in {
    // ⁴√81 = ⁴√(3⁴) = 3
    val (extracted, remaining) = normalizeIntegralRoot(81, 4)
    extracted shouldBe 3
    remaining shouldBe 1
  }

  it should "partially simplify ⁴√48 to (2, 3)" in {
    // ⁴√48 = ⁴√(16·3) = ⁴√(2⁴·3) = 2·⁴√3
    val (extracted, remaining) = normalizeIntegralRoot(48, 4)
    extracted shouldBe 2
    remaining shouldBe 3
  }

  it should "handle 1 as radicand" in {
    val (extracted, remaining) = normalizeIntegralRoot(1, 5)
    extracted shouldBe 1
    remaining shouldBe 1
  }

  it should "simplify √128 to (8, 2)" in {
    // √128 = √(64·2) = 8√2
    val (extracted, remaining) = normalizeIntegralRoot(128, 2)
    extracted shouldBe 8
    remaining shouldBe 2
  }

  it should "simplify ³√216 to (6, 1)" in {
    // ³√216 = ³√(6³) = 6
    val (extracted, remaining) = normalizeIntegralRoot(216, 3)
    extracted shouldBe 6
    remaining shouldBe 1
  }

  it should "partially simplify ³√24 to (2, 3)" in {
    // ³√24 = ³√(8·3) = 2·³√3
    val (extracted, remaining) = normalizeIntegralRoot(24, 3)
    extracted shouldBe 2
    remaining shouldBe 3
  }

  it should "simplify √144 to (12, 1)" in {
    val (extracted, remaining) = normalizeIntegralRoot(144, 2)
    extracted shouldBe 12
    remaining shouldBe 1
  }

  it should "handle composite numbers with multiple prime factors" in {
    // √72 = √(36·2) = 6√2
    val (extracted, remaining) = normalizeIntegralRoot(72, 2)
    extracted shouldBe 6
    remaining shouldBe 2
  }
}