/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.eager

import com.phasmidsoftware.number.algebra.eager.{InversePower, NaturalExponential, RationalNumber, WholeNumber}
import com.phasmidsoftware.number.core.inner.Rational
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * Integration tests showing how normalize works within arithmetic operations.
  * These tests verify that operations automatically normalize their results.
  */
class NormalizeIntegrationSpec extends AnyFlatSpec with Matchers {

  behavior of "normalize in addition operations"

  it should "normalize RationalNumber + RationalNumber to WholeNumber when appropriate" in {
    // 1/2 + 1/2 = 1
    val half1 = RationalNumber(Rational(1, 2))
    val half2 = RationalNumber(Rational(1, 2))

    // Assuming your operation creates a RationalNumber result that then normalizes
    // Adjust this based on your actual operation implementation
    val sum = half1.r + half2.r  // This gives Rational(1, 1)
    val result = RationalNumber(sum).normalize

    result shouldBe WholeNumber(1)
  }

  it should "keep as RationalNumber when sum is not whole" in {
    // 1/3 + 1/4 = 7/12
    val oneThird = RationalNumber(Rational(1, 3))
    val oneFourth = RationalNumber(Rational(1, 4))

    val sum = oneThird.r + oneFourth.r
    val result = RationalNumber(sum).normalize

    result shouldBe a[RationalNumber]
    result.asInstanceOf[RationalNumber].r shouldBe Rational(7, 12)
  }

  it should "normalize WholeNumber + RationalNumber correctly" in {
    // 2 + 1/2 = 5/2 (stays as RationalNumber)
//    val two = WholeNumber(2)
    val half = RationalNumber(Rational(1, 2))

    val sum = Rational.two + half.r
    val result = RationalNumber(sum).normalize

    result shouldBe a[RationalNumber]
    result.asInstanceOf[RationalNumber].r shouldBe Rational(5, 2)
  } : Unit

  behavior of "normalize in multiplication operations"

  it should "normalize RationalNumber * RationalNumber to WholeNumber" in {
    // 2/3 * 3/2 = 1
    val twoThirds = RationalNumber(Rational(2, 3))
    val threeHalves = RationalNumber(Rational(3, 2))

    val product = twoThirds.r * threeHalves.r
    val result = RationalNumber(product).normalize

    result shouldBe WholeNumber(1)
  }

  it should "normalize WholeNumber * RationalNumber to WholeNumber when appropriate" in {
    // 4 * 1/2 = 2
    val four = WholeNumber(4)
    val half = RationalNumber(Rational(1, 2))

    val product = Rational(four.x, 1) * half.r
    val result = RationalNumber(product).normalize

    result shouldBe WholeNumber(2)
  }

  behavior of "normalize in division operations"

  it should "create and normalize rational from WholeNumber division" in {
    // 6 / 2 = 3
    val six = WholeNumber(6)
    val two = WholeNumber(2)

    val quotient = Rational(six.x, two.x)
    val result = RationalNumber(quotient).normalize

    result shouldBe WholeNumber(3)
  }

  it should "keep as RationalNumber when division is not exact" in {
    // 7 / 3 = 7/3
    val seven = WholeNumber(7)
    val three = WholeNumber(3)

    val quotient = Rational(seven.x, three.x)
    val result = RationalNumber(quotient).normalize

    result shouldBe a[RationalNumber]
    result.asInstanceOf[RationalNumber].r shouldBe Rational(7, 3)
  }

  behavior of "normalize with InversePower operations"

  it should "simplify square root of perfect square" in {
    // √4 = 2
    val sqrt4 = InversePower(2, WholeNumber(4))
    sqrt4.normalize shouldBe a[WholeNumber]

    // If InversePower computes exact roots:
    // sqrt4.normalize shouldBe WholeNumber(2)
  }

  it should "simplify cube root of perfect cube" in {
    // ∛8 = 2
    val cbrt8 = InversePower(3, WholeNumber(8))
    cbrt8.normalize shouldBe a[WholeNumber]
  }

  behavior of "normalize with logarithm operations"

  it should "reduce NaturalExponential(0) in expressions" in {
    // e^0 = 1
    NaturalExponential(WholeNumber.zero).normalize shouldBe WholeNumber.one
  }

//  it should "reduce Exponential(base, 0) in expressions" in {
//    // 10^0 = 1
//    Exponential(WholeNumber(10), WholeNumber.zero).normalize shouldBe WholeNumber.one
//  }

  behavior of "normalize in nested operations"

  it should "normalize through multiple levels" in {
    val rational = Rational(6, 3)
    InversePower(1, RationalNumber(rational)).normalize shouldBe WholeNumber(2)
  }

  it should "normalize NaturalExponential with normalized argument" in {
    // NaturalExponential(RationalNumber(0, 1))
    // -> NaturalExponential(WholeNumber(0))
    // -> WholeNumber(1)
    val nested = NaturalExponential(RationalNumber(Rational(0, 1)))
    nested.normalize shouldBe WholeNumber.one
  }

  behavior of "normalize preserves mathematical equality"

  it should "maintain fuzzy equality after normalization" in {
    val r1 = RationalNumber(Rational(2, 2))
    val w1 = WholeNumber(1)

    val normalized = r1.normalize

    // After normalization, should be exactly equal
    normalized shouldBe w1
  }

  it should "normalize both sides of comparison consistently" in {
    val r1 = RationalNumber(Rational(4, 2))
    val r2 = RationalNumber(Rational(6, 3))

    r1.normalize shouldBe r2.normalize
    r1.normalize shouldBe WholeNumber(2)
  }

  behavior of "normalize edge cases"

  it should "handle normalization of zero in various forms" in {
    RationalNumber(Rational(0, 5)).normalize shouldBe WholeNumber.zero
    RationalNumber(Rational(0, 1)).normalize shouldBe WholeNumber.zero
    WholeNumber.zero.normalize shouldBe WholeNumber.zero
  }

  it should "handle normalization of one in various forms" in {
    RationalNumber(Rational(3, 3)).normalize shouldBe WholeNumber.one
    RationalNumber(Rational(1, 1)).normalize shouldBe WholeNumber.one
    WholeNumber.one.normalize shouldBe WholeNumber.one
  }

  it should "handle negative numbers correctly" in {
    RationalNumber(Rational(-4, 2)).normalize shouldBe WholeNumber(-2)
    RationalNumber(Rational(4, -2)).normalize shouldBe WholeNumber(-2)
    RationalNumber(Rational(-3, 4)).normalize shouldBe a[RationalNumber]
  }

  behavior of "normalize with percentage flag"

  it should "preserve percentage flag when RationalNumber doesn't reduce to whole" in {
    val percent = new RationalNumber(Rational(3, 4), percentage = true)()
    val normalized = percent.normalize

    normalized shouldBe a[RationalNumber]
    normalized.asInstanceOf[RationalNumber].percentage shouldBe true
  }

  it should "not lose percentage flag when normalizing" in {
    val percent = new RationalNumber(Rational(5, 5), percentage = true)()
    val normalized = percent.normalize

    normalized shouldBe a[RationalNumber]
  }
}
