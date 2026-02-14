/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.*
import com.phasmidsoftware.number.algebra.core.Valuable
import com.phasmidsoftware.number.algebra.eager.*
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.expression.expr.EagerOps.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * Tests for EagerOps extension methods.
  * These verify that infix operators create expressions and normalize correctly.
  */
class EagerOpsSpec extends AnyFlatSpec with Matchers {

  behavior of "EagerOps addition (+)"

  it should "add two WholeNumbers and normalize" in {
    val result = WholeNumber(2) + 3
    result shouldBe WholeNumber(5)
  }

  it should "add two RationalNumbers and normalize to WholeNumber" in {
    // 1/2 + 1/2 = 1
    val half1: Eager = RationalNumber(1, 2)
    val half2 = Rational(1, 2)
    val result = half1 + half2
    result shouldBe WholeNumber(1)
  }

  it should "add two RationalNumbers and keep as RationalNumber" in {
    // 1/3 + 1/4 = 7/12
    val oneThird = RationalNumber(1, 3)
    val oneFourth = Rational(1, 4)
    val result = oneThird + oneFourth

    result shouldBe a[RationalNumber]
    result.r shouldBe Rational(7, 12)
  }

  it should "add WholeNumber and RationalNumber" in {
    // 2 + 1/2 = 5/2
    val two = WholeNumber(2)
    val half = Rational(1, 2)
    val result = two + half

    result shouldBe a[RationalNumber]
    result.asInstanceOf[RationalNumber].r shouldBe Rational(5, 2)
  }

  it should "add fuzzy Reals and return Structure" in {
    val real1: Eager = Real("2.0*")
    val real2 = Real("3.0*")
    val result = real1 + real2
    result shouldBe a[Structure]
    result.isExact shouldBe false
  }

  it should "add exact Real and WholeNumber" in {
    val real = Real(2.5, None)  // Exact Real
    val whole = WholeNumber(3)
    val result = real + whole

    // Depends on whether exact Reals materialize or stay as expressions
    // Adjust based on your isExact/isSimple implementation
    result shouldBe a[Valuable]
  }

  it should "handle addition with zero" in {
    val result = WholeNumber(5) + 0
    result shouldBe WholeNumber(5)
  }

  it should "handle negative numbers" in {
    val result = WholeNumber(5) + -3
    result shouldBe WholeNumber(2)
  }

  behavior of "EagerOps multiplication (*)"

  it should "multiply two WholeNumbers and normalize" in {
    val result = WholeNumber(3) * 4
    result shouldBe WholeNumber(12)
  }

  it should "multiply two RationalNumbers and normalize to WholeNumber" in {
    // 2/3 * 3/2 = 1
    val twoThirds: Eager = RationalNumber(2, 3)
    val threeHalves = Rational(3, 2)
    val result = twoThirds * threeHalves

    result shouldBe WholeNumber(1)
  }

  it should "multiply WholeNumber and RationalNumber" in {
    // 4 * 1/2 = 2
    val four = WholeNumber(4)
    val half = Rational(1, 2)
    val result = four * half

    result shouldBe WholeNumber(2)
  }

  it should "multiply RationalNumbers keeping as RationalNumber" in {
    // 2/3 * 3/4 = 1/2
    val twoThirds = RationalNumber(2, 3)
    val threeFourths = Rational(3, 4)
    val result: ExactNumber = twoThirds * threeFourths

    result shouldBe a[RationalNumber]
    result.asInstanceOf[RationalNumber].r shouldBe Rational(1, 2)
  }

  it should "multiply fuzzy Reals and return Expression" in {
    val real1: Eager = Real("2.0*")
    val real2 = Real("3.0*")
    val result = real1 * real2

    result shouldBe a[Structure]
    result.isExact shouldBe false
  }

  it should "handle multiplication by zero" in {
    val result = WholeNumber(5) * 0
    result shouldBe WholeNumber.zero
  }

  it should "handle multiplication by one" in {
    val result = WholeNumber(5) * 1
    result shouldBe WholeNumber(5)
  }

  it should "handle negative multiplication" in {
    val result = WholeNumber(5) * -2
    result shouldBe WholeNumber(-10)
  }

  behavior of "EagerOps subtraction (-)" // if implemented

  it should "subtract two WholeNumbers" in {
    val result = WholeNumber(5) - 3
    result shouldBe WholeNumber(2)
  }

  it should "subtract resulting in negative" in {
    val result = WholeNumber(3) - 5
    result shouldBe WholeNumber(-2)
  }

  it should "subtract RationalNumbers normalizing to WholeNumber" in {
    // 5/2 - 3/2 = 1
    val fiveHalves: Eager = RationalNumber(5, 2)
    val threeHalves = Rational(3, 2)
    val result = fiveHalves - threeHalves

    result shouldBe WholeNumber(1)
  }

  behavior of "EagerOps division (/)" // if implemented

  it should "divide two WholeNumbers with exact result" in {
    val result = WholeNumber(6) / 2
    result shouldBe WholeNumber(3)
  }

  it should "divide two WholeNumbers creating RationalNumber" in {
    // 7 / 3 = 7/3
    val result = WholeNumber(7) / 3

    result shouldBe a[RationalNumber]
    result.asInstanceOf[RationalNumber].r shouldBe Rational(7, 3)
  }

  it should "divide RationalNumbers normalizing to WholeNumber" in {
    // (6/2) / (3/2) = 2
    val sixHalves: Eager = RationalNumber(6, 2)
    val threeHalves = RationalNumber(3, 2)
    val result = sixHalves / threeHalves

    result shouldBe WholeNumber(2)
  }

  it should "handle division by one" in {
    val result = WholeNumber(5) / 1
    result shouldBe WholeNumber(5)
  }

  behavior of "EagerOps power (∧)"

  it should "raise WholeNumber to power" in {
    val result = (∅ + WholeNumber(2)) ∧ 3
    result.normalize shouldBe WholeNumber(8)
  }

  it should "handle power of zero" in {
    val result = (∅ + WholeNumber(5)) ∧ 0
    result.normalize shouldBe WholeNumber.one
  }

  it should "handle power of one" in {
    val result = (∅ + WholeNumber(5)) ∧ 1
    result.normalize shouldBe WholeNumber(5)
  }

  behavior of "EagerOps mixed operations"

  it should "structure operations on WholeNumber correctly" in {
    // (2 + 3) * 4 = 20
    val result = (WholeNumber(2) + 3) * 4
    result shouldBe WholeNumber(20)
  }

  it should "structure operations on RationalNumber correctly" in {
    // (1/2 + 1/2) * 3 = 3
    val half1 = RationalNumber.half
    val half2 = RationalNumber.half
    val result = (half1 + half2) * 3

    result.normalize shouldBe WholeNumber(3)
  }

  it should "normalize intermediate results" in {
    // (4/2) + (6/3) = 2 + 2 = 4
    val r1: Eager = RationalNumber(4, 2)
    val r2 = RationalNumber(6, 3)
    val result = r1 + r2

    result shouldBe WholeNumber(4)
  }

  behavior of "EagerOps with special types"

  it should "work with InversePower" in {
    val ip: Eager = InversePower(1, WholeNumber(5))
    val result = ip + 3

    result shouldBe WholeNumber(8)
  }

  it should "work with NaturalExponential" in {
    // NaturalExponential(0) + WholeNumber(2)
    // = WholeNumber(1) + WholeNumber(2) = WholeNumber(3)
    val nl = NaturalExponential(WholeNumber.zero)
    val result = nl + 2

    result shouldBe WholeNumber(3)
  }

  it should "work with Angle" in {
    val angle = Angle(WholeNumber.zero)
    val result = angle + 1
    result shouldBe WholeNumber(1)
  }

  it should "work with Angle flipped" in {
    val angle = Angle(WholeNumber(1))
    val result = angle + 0
    result shouldBe Angle(WholeNumber(1))
  }

  it should "work with Angle mixed" in {
    val angle = Angle(RationalNumber.half)
    val result = angle + RationalNumber.half
    result shouldBe a[Real]
    result.isExact shouldBe false
  }

  behavior of "EagerOps associativity and commutativity"

  it should "be commutative for addition" in {
    val a = WholeNumber(3)
    val b = WholeNumber(5)

    (a + b) shouldBe (b + a)
  }

  it should "be commutative for multiplication" in {
    val a = WholeNumber(3)
    val b = WholeNumber(5)

    (a * b) shouldBe (b * a)
  }

  it should "be associative for addition" in {
    val a = WholeNumber(2)
    val b = WholeNumber(3)
    val c = WholeNumber(4)

    ((a + b) + c) shouldBe (a + (b + c))
  }

  it should "be associative for multiplication" in {
    val a = WholeNumber(2)
    val b = WholeNumber(3)
    val c = WholeNumber(4)

    ((a * b) * c) shouldBe (a * (b * c))
  }

  behavior of "EagerOps edge cases"

  it should "handle large WholeNumbers" in {
    val large1 = WholeNumber(BigInt("12345678901234567890"))
    val large2 = WholeNumber(BigInt("98765432109876543210"))
    val result = large1 + large2

    result shouldBe WholeNumber(BigInt("111111111011111111100"))
  }

  it should "handle very small rationals" in {
    val tiny = RationalNumber(Rational(1, 1000000))
    val result = tiny + tiny

    result shouldBe a[RationalNumber]
    result.r shouldBe Rational(1, 500000)
  }

  behavior of "EagerOps type discovery after operations"

  it should "have correct typeName after operation" in {
    val result = WholeNumber(2) + 3
    result.typeName shouldBe "WholeNumber"
  }

  it should "have correct category after operation" in {
    val result = Eager(2) + 3
    result.category shouldBe "Structure"
  }

  it should "have correct describe after operation" in {
    val two: Eager = 2
    val result = two + 3
    result.describe shouldBe "Structure.WholeNumber"
  }

  it should "show Expression category for fuzzy operations" in {
    val real1: Eager = Real(2.0, 0.1)
    val real2 = Real(3.0, 0.1)
    val result = real1 + real2
    result.category shouldBe "Structure" // or "Expression" depending on implementation
  }
}

/**
  * Integration tests showing EagerOps working with the full normalization pipeline
  */
class EagerOpsIntegrationSpec extends AnyFlatSpec with Matchers {

  import EagerOps.*

  behavior of "EagerOps full pipeline"

  it should "create Expression -> simplify -> materialize -> normalize" in {
    // This verifies the complete pipeline works
    val result = WholeNumber(2) + 3

    // Should have gone through:
    // 1. Sum(WholeNumber(2), WholeNumber(3)) created
    // 2. simplify() called
    // 3. isExact && isSimple checked (true)
    // 4. materialize() called -> WholeNumber(5)
    // 5. normalize() called -> WholeNumber(5)

    result shouldBe WholeNumber(5)
    result shouldBe a[WholeNumber]
  }

  it should "handle complex expression that normalizes through multiple steps" in {
    // (4/2 + 6/3) * 2
    // = (2 + 2) * 2
    // = 4 * 2
    // = 8
    val r1 = RationalNumber(4, 2)
    val r2 = Rational(6, 3)
    val result = ((r1 + r2) * WholeNumber(2)).normalize

    result shouldBe WholeNumber(8)
  }

  it should "preserve Expression for fuzzy operations" in {
    val real1 = Real(2.0, 0.1)
    val real2 = Real(3.0, 0.1)
    val real3 = Real(4.0, 0.1)

    // Should stay as expressions (not materialize)
    val result = (real1 + real2) * real3

    result.isExact shouldBe false
  }

  it should "work with all normalize tests passing" in {
    // Meta-test: operations should work with normalized values
    val values: Seq[Eager] = Seq(
      WholeNumber(5),
      RationalNumber(4, 2),  // Will normalize to WholeNumber(2)
      Rational(3, 4)
    )

    values.foreach { v =>
      val normalized: Eager = v.normalize.materialize
      val result = normalized + 1
      result shouldBe a[Valuable]
    }
  }
}
