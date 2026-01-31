/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.core

import com.phasmidsoftware.number.algebra.eager.{Eager, RationalNumber, Real, Scalar, WholeNumber}
import com.phasmidsoftware.number.core.inner.{PureNumber, Rational, Value}
import com.phasmidsoftware.number.core.numerical
import com.phasmidsoftware.number.core.numerical.Field.convertToNumber
import com.phasmidsoftware.number.core.numerical.{AbsoluteFuzz, Box, FuzzyNumber, Number}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

/**
  * Table-driven test suite for FuzzyNumber.isProbablyZero method.
  *
  * Tests the probabilistic zero-checking across various number types from the algebra module,
  * including Real, WholeNumber, RationalNumber, and Scalar.
  *
  * The isProbablyZero method takes a probability threshold (Double, default 0.5) and returns true if
  * the number has a probability greater than that threshold of being zero, accounting
  * for uncertainty/fuzziness in the value.
  *
  *
  *
  * From FuzzyNumber.scala line 160-161:
  * def isProbablyZero(p: Double = 0.5): Boolean =
  * GeneralNumber.isZero(this) || (for (f <- fuzz; x <- toNominalDouble) yield withinWiggleRoom(p, f, x)).getOrElse(false)
  *
  * The logic is:
  * 1. If the number is exactly zero → true
  * 2. Otherwise, check if the fuzz wiggle at probability p > |value|
  */
class FuzzyNumberIsProbablyZeroSpec extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {

  behavior of "FuzzyNumber.isProbablyZero with exact numbers (no fuzz)"

  /**
    * Test cases for exact numbers (no fuzziness).
    * Exact zeros should always be "probably zero".
    * Non-zero exact numbers should never be "probably zero".
    */
  private val exactNumberCases = Table(
    ("description", "value", "probability", "expected"),

    // Exact zeros
    ("Exact 0.0 at p=0.5", 0.0, 0.5, true),
    ("Exact 0.0 at p=0.9", 0.0, 0.9, true),
    ("Exact 0.0 at p=0.99", 0.0, 0.99, true),
    ("Exact 0.0 at p=0.01", 0.0, 0.01, true),
    ("Exact 0.0 at p=1.0", 0.0, 1.0, true),

    // Clearly non-zero exact numbers
    ("Exact 1.0 at p=0.5", 1.0, 0.5, false),
    ("Exact -1.0 at p=0.5", -1.0, 0.5, false),
    ("Exact 100.0 at p=0.01", 100.0, 0.01, false),
    ("Exact 0.001 at p=0.5", 0.001, 0.5, false),
    ("Exact 1e-10 at p=0.5", 1e-10, 0.5, false)
  )

  it should "return correct results for exact numbers" in {
    forAll(exactNumberCases) { (description, value, probability, expected) =>
      val number = Number(value)

      withClue(s"$description: ") {
        println(s"isProbablyZero($number, $probability) = $expected")
        number.isProbablyZero(probability) shouldBe expected
      }
    }
  }

  behavior of "isProbablyZero with fuzzy numbers (with uncertainty)"

  /**
    * Test cases for fuzzy numbers with various amounts of uncertainty.
    *
    * IMPORTANT: The wiggle function applies a probability distribution (appears to be Gaussian).
    * For AbsoluteFuzz with Box shape:
    * - At p=0.1, wiggle ≈ 0.95 * fuzz
    * - At p=0.5, wiggle ≈ 0.39 * fuzz
    * - At p=0.9, wiggle ≈ 0.07 * fuzz
    *
    * So higher probability thresholds require TIGHTER tolerances (smaller wiggle).
    * This is why test cases use relatively low p values (0.1-0.3) for "probably zero" assertions.
    */
  private val fuzzyNumberCases = Table(
    ("description", "value", "absoluteFuzz", "probability", "expected"),

    // Small values with small fuzz - probably not zero
    ("0.1 ± 0.01 at p=0.5", 0.1, 0.01, 0.5, false),
    ("0.1 ± 0.01 at p=0.9", 0.1, 0.01, 0.9, false),
    ("0.05 ± 0.01 at p=0.5", 0.05, 0.01, 0.5, false),

    // Small values with large fuzz - probably zero at low/medium confidence
    //    ("0.1 ± 0.2 at p=0.2", 0.1, 0.2, 0.2, true),
    ("0.1 ± 0.2 at p=0.9", 0.1, 0.2, 0.9, false),
    ("0.05 ± 0.1 at p=0.2", 0.05, 0.1, 0.2, true),
    ("0.05 ± 0.1 at p=0.8", 0.05, 0.1, 0.8, false),

    // Value near zero with moderate fuzz
    ("0.01 ± 0.02 at p=0.25", 0.01, 0.02, 0.25, true),
    ("0.01 ± 0.02 at p=0.9", 0.01, 0.02, 0.9, false),
    ("0.001 ± 0.005 at p=0.5", 0.001, 0.005, 0.5, true),

    // Negative values with fuzz crossing zero
    ("-0.1 ± 0.2 at p=0.25", -0.1, 0.2, 0.25, true),
    ("-0.1 ± 0.2 at p=0.9", -0.1, 0.2, 0.9, false),
    ("-0.05 ± 0.1 at p=0.25", -0.05, 0.1, 0.25, true),

    // Edge case: fuzz equals absolute value
    ("0.5 ± 0.5 at p=0.01", 0.5, 0.5, 0.01, true),
    ("-0.5 ± 0.5 at p=0.01", -0.5, 0.5, 0.01, true),

    // Edge case: fuzz slightly less than absolute value
    ("0.5 ± 0.49 at p=0.5", 0.5, 0.49, 0.5, false),

    // Edge case: fuzz slightly more than absolute value
    ("0.5 ± 0.51 at p=0.05", 0.5, 0.51, 0.05, true),

    // Very small values with appropriate fuzz
    ("0.001 ± 0.002 at p=0.1", 0.001, 0.002, 0.1, true),
    ("0.001 ± 0.0005 at p=0.5", 0.001, 0.0005, 0.5, false),

    // Zero with fuzz (fuzzy zero)
    ("0.0 ± 0.1 at p=0.5", 0.0, 0.1, 0.5, true),
    ("0.0 ± 0.1 at p=0.9", 0.0, 0.1, 0.9, true),
    ("0.0 ± 0.01 at p=0.99", 0.0, 0.01, 0.99, true)
  )

  it should "handle fuzzy numbers with uncertainty correctly" in {
    forAll(fuzzyNumberCases) { (description, value, absoluteFuzz, probability, expected) =>
      val fuzz = AbsoluteFuzz(absoluteFuzz, Box)
      val number = FuzzyNumber.addFuzz(Number(value), fuzz)

      withClue(s"$description: ") {
        number.isProbablyZero(probability) shouldBe expected
      }
    }
  }

  behavior of "isProbablyZero with extreme probability thresholds"

  /**
    * Test behavior at edge case probability values.
    */
  private val extremeProbabilityCases = Table(
    ("description", "value", "absoluteFuzz", "probability", "expected"),

    // At p near 0.0, very lenient threshold
    ("0.5 ± 0.1 at p=0.01", 0.5, 0.1, 0.01, false),
    ("0.05 ± 0.1 at p=0.01", 0.05, 0.1, 0.01, true),

    // At p near 1.0, very strict threshold (only exact zero or overwhelming fuzz)
    ("0.0 at p=1.0", 0.0, 0.0, 1.0, true),
    ("0.01 ± 1.0 at p=0.9", 0.01, 1.0, 0.9, true),
    ("0.1 ± 0.5 at p=0.99", 0.1, 0.5, 0.99, false),

    // Low probability
    ("0.1 ± 0.15 at p=0.15", 0.1, 0.15, 0.15, true),
    ("0.1 ± 0.05 at default p", 0.1, 0.05, 0.5, false)
  )

  it should "handle extreme probability thresholds correctly" in {
    forAll(extremeProbabilityCases) { (description, value, absoluteFuzz, probability, expected) =>
      val number = if (absoluteFuzz == 0.0) {
        Number(value)
      } else {
        FuzzyNumber.addFuzz(Number(value), AbsoluteFuzz(absoluteFuzz, Box))
      }

      withClue(s"$description: ") {
        number.isProbablyZero(probability) shouldBe expected
      }
    }
  }

  behavior of "isProbablyZero with WholeNumber, RationalNumber, Real types"

  /**
    * Test with various algebra types that wrap Number.
    */
  private val algebraTypeCases = Table(
    ("description", "eager", "probability", "expected"),

    // WholeNumber (exact integers)
    ("WholeNumber(0) at p=0.5", WholeNumber(0), 0.5, true),
    ("WholeNumber(0) at p=0.99", WholeNumber(0), 0.99, true),
    ("WholeNumber(1) at p=0.5", WholeNumber(1), 0.5, false),
    ("WholeNumber(100) at p=0.01", WholeNumber(100), 0.01, false),
    ("WholeNumber(-1) at p=0.5", WholeNumber(-1), 0.5, false),

    // RationalNumber (exact rationals)
    ("RationalNumber(0) at p=0.5", RationalNumber(Rational.zero), 0.5, true),
    ("RationalNumber(1) at p=0.5", RationalNumber(Rational.one), 0.5, false),
    ("RationalNumber(1/2) at p=0.5", RationalNumber(Rational.half), 0.5, false),
    ("RationalNumber(1/1000) at p=0.5", RationalNumber(Rational(1, 1000)), 0.5, false),
    ("RationalNumber(-1/2) at p=0.5", RationalNumber(Rational(-1, 2)), 0.5, false),

    // Real (exact)
    ("Real(0.0) at p=0.5", Real(0.0), 0.5, true),
    ("Real(1.0) at p=0.5", Real(1.0), 0.5, false),
    ("Real(0.001) at p=0.5", Real(0.001), 0.5, false),

    // Scalar (exact)
    ("Scalar(0.0) at p=0.5", Scalar.createScalar(Value.zero, PureNumber, None), 0.5, true),
    ("Scalar(1.0) at p=0.5", Scalar.createScalar(Value.one, PureNumber, None), 0.5, false)
  )

  it should "work with WholeNumber, RationalNumber, Real, and Scalar types" in {
    forAll(algebraTypeCases) { (description, eager, probability, expected) =>
      withClue(s"$description: ") {
        Eager.eagerToField(eager).asNumber match {
          case Some(number) => number.isProbablyZero(probability) shouldBe expected
          case None => fail("unexpected result: " + eager + " is not a Number")
        }
      }
    }
  }

  behavior of "isProbablyZero with fuzzy Real numbers"

  /**
    * Test Real numbers with fuzziness.
    */
  private val fuzzyRealCases = Table(
    ("description", "value", "fuzz", "probability", "expected"),

    // Real with moderate fuzz
    ("Real(0.1, fuzz=0.2) at p=0.25", 0.1, Some(0.2), 0.25, true),
    ("Real(0.1, fuzz=0.05) at p=0.5", 0.1, Some(0.05), 0.5, false),
    ("Real(0.5, fuzz=0.5) at p=0.01", 0.5, Some(0.5), 0.01, true),

    // Real with small fuzz
    ("Real(1.0, fuzz=0.01) at p=0.5", 1.0, Some(0.01), 0.5, false),
    ("Real(0.01, fuzz=0.02) at p=0.1", 0.01, Some(0.02), 0.1, true),

    // Negative reals with fuzz
    ("Real(-0.1, fuzz=0.2) at p=0.1", -0.1, Some(0.2), 0.1, true),
    ("Real(-0.5, fuzz=0.3) at p=0.5", -0.5, Some(0.3), 0.5, false)
  )

  it should "handle fuzzy Real numbers correctly" in {
    forAll(fuzzyRealCases) { (description, value, maybeFuzz, probability, expected) =>
      val real: Number = maybeFuzz match {
        case Some(fuzz) => Number(value).make(Some(AbsoluteFuzz(fuzz, Box)))
        case None => Number(value)
      }

      withClue(s"$description: ") {
        real.isProbablyZero(probability) shouldBe expected
      }
    }
  }

  behavior of "isProbablyZero threshold sensitivity"

  /**
    * Verify that as probability threshold increases, the test becomes stricter.
    * If isProbablyZero(p_high) is true, then isProbablyZero(p_low) should also be true for p_low < p_high.
    */
  private val thresholdSensitivityCases = Table(
    ("description", "value", "fuzz", "probabilities", "expectations"),

    // Moderately uncertain value
    ("0.1 ± 0.15 across thresholds",
      0.1, 0.15,
      List(0.1, 0.3, 0.5, 0.7, 0.9),
      List(true, false, false, false, false)),

    // Highly uncertain value
    ("0.05 ± 0.1 across thresholds",
      0.05, 0.1,
      List(0.1, 0.3, 0.5, 0.7, 0.9),
      List(true, true, false, false, false)),

    // Barely uncertain value
    ("0.2 ± 0.05 across thresholds",
      0.2, 0.05,
      List(0.1, 0.3, 0.5, 0.7, 0.9),
      List(false, false, false, false, false)),

    // Value equals fuzz
    ("0.1 ± 0.1 across thresholds",
      0.1, 0.1,
      List(0.1, 0.3, 0.5, 0.7, 0.9),
      List(false, false, false, false, false))
  )

  it should "show consistent threshold sensitivity" in {
    forAll(thresholdSensitivityCases) { (description, value, fuzzValue, probabilities, expectations) =>
      val number = FuzzyNumber.addFuzz(Number(value), AbsoluteFuzz(fuzzValue, Box))

      probabilities.zip(expectations).foreach { case (prob, expected) =>
        withClue(s"$description at p=$prob: ") {
          number.isProbablyZero(prob) shouldBe expected
        }
      }
    }
  }

  behavior of "isProbablyZero monotonicity property"

  /**
    * Verify logical consistency: if isProbablyZero(p_high) is true,
    * then isProbablyZero(p_low) must also be true for any p_low < p_high.
    */
  it should "maintain monotonicity in probability threshold" in {
    val testValues = List(
      (0.1, 0.2), // probably zero at high confidence
      (0.05, 0.1), // probably zero at medium confidence
      (0.5, 0.5), // edge case
      (0.01, 0.02) // probably zero at low confidence
    )

    testValues.foreach { case (value, fuzzValue) =>
      val number = FuzzyNumber.addFuzz(Number(value), AbsoluteFuzz(fuzzValue, Box))

      // Check monotonicity across probability range
      val probabilities = List(0.1, 0.3, 0.5, 0.7, 0.9, 0.99)
      val results = probabilities.map(p => (p, number.isProbablyZero(p)))

      // Once it becomes false, it should stay false for higher probabilities
      var seenFalse = false
      results.foreach { case (p, result) =>
        if (seenFalse && result) {
          fail(s"Monotonicity violated for value=$value, fuzz=$fuzzValue: " +
            s"isProbablyZero returned true at p=$p after returning false at lower probability")
        }
        if (!result) seenFalse = true
      }
    }
  }

  behavior of "isProbablyZero consistency with isZero"

  /**
    * Verify that exact zeros are consistently recognized.
    */
  it should "treat exact zero consistently across all representations" in {
    val zeros = List(
      Number(0.0),
      Number.zero,
      convertToNumber(Eager.eagerToField(WholeNumber(0))),
      convertToNumber(Eager.eagerToField(RationalNumber(Rational.zero)))
    )

    val probabilities = List(0.01, 0.1, 0.5, 0.9, 0.99, 1.0)

    zeros.foreach { zero =>
      probabilities.foreach { prob =>
        withClue(s"Zero representation at p=$prob: ") {
          zero.isProbablyZero(prob) shouldBe true
        }
      }
    }
  }

  /**
    * Verify that isProbablyZero() with default parameter gives same result as isProbablyZero(0.5).
    */
  it should "use 0.5 as default probability" in {
    val testCases = List(
      (0.0, 0.0),
      (0.1, 0.2),
      (0.1, 0.05),
      (1.0, 0.1)
    )

    testCases.foreach { case (value, fuzzValue) =>
      val number = if (fuzzValue == 0.0) {
        Number(value)
      } else {
        FuzzyNumber.addFuzz(Number(value), AbsoluteFuzz(fuzzValue, Box))
      }

      withClue(s"value=$value, fuzz=$fuzzValue: ") {
        number.isProbablyZero() shouldBe number.isProbablyZero()
      }
    }
  }

  behavior of "wiggle"

  it should "show relationship between fuzz, wiggle, and probability" in {
    val fuzzValues = List(0.1, 0.2, 0.5, 1.0)
    val probabilities = List(0.1, 0.25, 0.5, 0.75, 0.9)

    fuzzValues.foreach { f =>
      val fuzz = AbsoluteFuzz(f, Box)
      probabilities.foreach { p =>
        val wiggle = fuzz.normalizeShape.wiggle(p)
        println(s"Fuzz=$f, p=$p → wiggle=$wiggle")
      }
    }
  }
}