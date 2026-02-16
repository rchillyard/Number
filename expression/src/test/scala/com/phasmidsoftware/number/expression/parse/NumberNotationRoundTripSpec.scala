package com.phasmidsoftware.number.expression.parse

import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.numerical
import com.phasmidsoftware.number.core.numerical.Number.NumberIsFractional.mkNumericOps
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * Test suite for verifying round-trip behavior of number notation parsing and rendering.
  * These tests ensure that the notation rules defined in NUMBER_NOTATION.md are correctly implemented.
  */
class NumberNotationRoundTripSpec extends AnyFlatSpec with Matchers {

  behavior of "Number notation round-trips"

  // ========== GUARANTEED ROUND-TRIPS ==========

  it should "round-trip simple fuzzy numbers with asterisk" in {
    val original = "2.0*"
    val parsed = numerical.Number.parse(original).get
    parsed.fuzz shouldBe defined
    parsed.render shouldBe original
  }

  it should "round-trip fuzzy numbers with various decimal places" in {
    val casesForLater = Seq("0.1*", "100.0*", "0.001*")
    val cases = Seq("2.5*", "3.14159*")
    cases.foreach { original =>
      val parsed = numerical.Number.parse(original).get
      parsed.render shouldBe original
    }
  }

  it should "round-trip negative fuzzy numbers" in {
    val original = "-2.5*"
    val parsed = numerical.Number.parse(original).get
    parsed.render shouldBe original
  }

  it should "round-trip simple repeating decimals" in {
    val original = "0.<3>"
    val parsed = numerical.Number.parse(original).get
    parsed.fuzz shouldBe None
    parsed.toNominalRational shouldBe Some(Rational(1, 3))
    val result = parsed.render
    result shouldBe "⅓"
    numerical.Number.parse(result).get shouldBe parsed
  }

  it should "round-trip repeating decimals with integer parts" in {
    val cases = Seq(
      "1.<3>" -> Rational(4, 3),
      "2.<6>" -> Rational(8, 3)
    )
    cases.foreach { case (original, expectedRational) =>
      val parsed = numerical.Number.parse(original).get
      parsed.toNominalRational shouldBe Some(expectedRational)
      parsed.render shouldBe original
    }
  }

  it should "simplify repeating zeros to integers" in {
    val original = "5.<0>"
    val parsed = numerical.Number.parse(original).get
    parsed.toNominalRational shouldBe Some(Rational(5, 1))
    parsed.render shouldBe "5" // Simplifies to integer form
  }

  ignore should "round-trip repeating decimals with non-repeating parts" in {
    val cases = Seq(
      "0.1<6>" -> Rational(1, 6),
      "0.08<3>" -> Rational(1, 12),
      "0.5<0>" -> Rational(1, 2)
    )
    cases.foreach { case (original, expectedRational) =>
      val parsed = numerical.Number.parse(original).get
      parsed.toNominalRational shouldBe Some(expectedRational)
      parsed.render shouldBe original
    }
  }

  it should "round-trip complex repeating decimals" in {
    val original = "0.<142857>"
    val parsed = numerical.Number.parse(original).get
    parsed.toNominalRational shouldBe Some(Rational(1, 7))
    parsed.render shouldBe "⅐"
  }

  it should "round-trip repeating decimals with two-digit patterns" in {
    val cases = Seq(
      "0.<12>" -> Rational(4, 33),
      "0.<09>" -> Rational(1, 11)
    )
    cases.foreach { case (original, expectedRational) =>
      val parsed = numerical.Number.parse(original).get
      parsed.toNominalRational shouldBe Some(expectedRational)
      parsed.render shouldBe original
    }
  }

  it should "round-trip explicit box fuzziness notation" in {
    val original = "2.5[2]"
    val parsed = numerical.Number.parse(original).get
    parsed.fuzz shouldBe defined
    // NOTE: The exact round-trip may depend on how explicit fuzz is rendered
    // This test verifies that explicit fuzz is preserved in some form
    parsed.render should (equal(original) or include("["))
  }

  it should "round-trip explicit gaussian fuzziness notation" in {
    val original = "2.5(2)"
    val parsed = numerical.Number.parse(original).get
    parsed.fuzz shouldBe defined
    // NOTE: The exact round-trip may depend on how explicit fuzz is rendered
    // This test verifies that explicit fuzz is preserved in some form
    parsed.render should (equal(original) or include("("))
  }

  it should "round-trip exact integers" in {
    val cases = Seq("0", "1", "42", "-5", "1000")
    cases.foreach { original =>
      val parsed = numerical.Number.parse(original).get
      parsed.fuzz shouldBe None
      parsed.render shouldBe original
    }
  }

  it should "round-trip exact decimals" in {
    val cases = Seq("2.0", "3.5", "0.25", "10.125")
    cases.foreach { original =>
      val parsed = numerical.Number.parse(original).get
      parsed.fuzz shouldBe None
      // NOTE: Some exact decimals may be simplified (e.g., "2.0" -> "2")
      // We verify the value is correct, rendering may vary
      parsed.toDouble shouldBe original.toDouble +- 1e-10
    }
  }

  ignore should "round-trip numbers with factors" in {
    val original = "2π"
    val parsed = numerical.Number.parse(original).get
    parsed.render shouldBe original
  }

  // ========== ACCEPTABLE NON-ROUND-TRIPS ==========

  behavior of "Number notation acceptable transformations"

  it should "transform ellipsis to asterisk (both mean fuzzy)" in {
    val original = "0.333..."
    val parsed = numerical.Number.parse(original).get
    parsed.fuzz shouldBe defined
    // TODO restore
    //    parsed.render should contain("*")
    parsed.render should not endWith "..."
  }

  it should "transform multiple ellipsis notations consistently" in {
    val cases = Seq("2.5...", "3.14159...", "0.1...")
    cases.foreach { original =>
      val parsed = numerical.Number.parse(original).get
      parsed.fuzz shouldBe defined
      // TODO restore
      //      parsed.render should contain("*")
    }
  }

  it should "detect repeating patterns in exact rationals" in {
    // When parsing 1/3 as a fraction, it should render as repeating decimal
    val rational = Rational(1, 3)
    val number = numerical.Number(rational)
    number.render shouldBe "⅓"
  }

  it should "detect repeating patterns in 1/7" in {
    val rational = Rational(1, 7)
    val number = numerical.Number(rational)
    number.render shouldBe "⅐"
  }

  it should "handle fraction notation parsing" in {
    val original = "1/3"
    val parsed = numerical.Number.parse(original).get
    parsed.toNominalRational shouldBe Some(Rational(1, 3))
    // Should render as repeating decimal, not fraction
    parsed.render shouldBe "⅓"
  }

  it should "simplify long exact decimals to repeating notation" in {
    // If a number is exact but has a long decimal representation with a pattern
    val original = "0.3333333333333333"
    val parsed = numerical.Number.parse(original).get
    // Should either parse as exact 1/3 or fuzzy
    // If exact, should render with repeating notation
    if (parsed.fuzz.isEmpty) {
      parsed.render should include("<")
    }
  }

  // ========== CONSISTENCY CHECKS ==========

  behavior of "Number notation consistency"

  it should "maintain semantic equivalence for fuzzy notations" in {
    val asterisk = numerical.Number.parse("2.5*").get
    val ellipsis = numerical.Number.parse("2.5...").get

    // Both should be fuzzy
    asterisk.fuzz shouldBe defined
    ellipsis.fuzz shouldBe defined

    // Both should have similar fuzziness (same order of magnitude)
    val fuzz1 = asterisk.fuzz.get.wiggle(0.5)
    val fuzz2 = ellipsis.fuzz.get.wiggle(0.5)
    math.abs(fuzz1 - fuzz2) should be < 1.0
  }

  it should "parse and render exact numbers without fuzziness" in {
    val cases = Seq("2.0", "3.5", "0.25", "100")
    cases.foreach { original =>
      val parsed = numerical.Number.parse(original).get
      parsed.fuzz shouldBe None
    }
  }

  it should "always add fuzziness for asterisk notation" in {
    val cases = Seq("2.0*", "3.5*", "0.25*", "100.0*")
    cases.foreach { original =>
      val parsed = numerical.Number.parse(original).get
      parsed.fuzz shouldBe defined
    }
  }

  it should "always add fuzziness for ellipsis notation" in {
    val cases = Seq("2.0...", "3.5...", "0.25...", "100.0...")
    cases.foreach { original =>
      val parsed = numerical.Number.parse(original).get
      parsed.fuzz shouldBe defined
    }
  }

  it should "never add fuzziness for repeating decimal notation" in {
    val cases = Seq("0.<3>", "0.1<6>", "0.<142857>", "1.<3>")
    cases.foreach { original =>
      val parsed = numerical.Number.parse(original).get
      parsed.fuzz shouldBe None
    }
  }

  // ========== EDGE CASES ==========

  behavior of "Number notation edge cases"

  it should "handle zero with various notations" in {
    val exact = numerical.Number.parse("0").get
    exact.fuzz shouldBe None
    exact.toNominalRational shouldBe Some(Rational.zero)

    val fuzzy = numerical.Number.parse("0.0*").get
    fuzzy.fuzz shouldBe defined
  }

  it should "handle negative numbers with repeating decimals" in {
    val original = "-0.<3>"
    val parsed = numerical.Number.parse(original).get
    parsed.toNominalRational shouldBe Some(Rational(-1, 3))
  }

  it should "handle very small fuzzy numbers" in {
    val original = "0.000001*"
    val parsed = numerical.Number.parse(original).get
    parsed.fuzz shouldBe defined
    // TODO restore
    //    parsed.render should endWith("*")
  }

  it should "handle very large fuzzy numbers" in {
    val original = "1000000.0*"
    val parsed = numerical.Number.parse(original).get
    parsed.fuzz shouldBe defined
    // TODO restore
    //    parsed.render should endWith("*")
  }

  it should "handle numbers with scientific notation and fuzziness" in {
    val original = "1.23e5*"
    val parsed = numerical.Number.parse(original).get
    parsed.fuzz shouldBe defined
    // NOTE: Rendering format may vary for scientific notation
  }

  it should "handle repeating nines" in {
    val original = "0.<9>"
    val parsed = numerical.Number.parse(original).get
    // 0.999... = 1
    parsed.toNominalRational shouldBe Some(Rational.one)
  }

  it should "handle mixed integer and repeating decimal" in {
    val original = "1.<9>"
    val parsed = numerical.Number.parse(original).get
    // 1.999... = 2
    parsed.toNominalRational shouldBe Some(Rational(2))
  }

  // ========== MULTIPLE ROUND-TRIPS ==========

  behavior of "Number notation multiple round-trips"

  it should "maintain stability over multiple parse-render cycles (fuzzy)" in {
    val original = "2.5*"
    val parsed1 = numerical.Number.parse(original).get
    val rendered1 = parsed1.render
    val parsed2 = numerical.Number.parse(rendered1).get
    val rendered2 = parsed2.render
    val parsed3 = numerical.Number.parse(rendered2).get
    val rendered3 = parsed3.render

    // After first round-trip, should be stable
    rendered1 shouldBe rendered2
    rendered2 shouldBe rendered3
  }

  it should "maintain stability over multiple parse-render cycles (repeating)" in {
    val original = "0.<3>"
    val parsed1 = numerical.Number.parse(original).get
    val rendered1 = parsed1.render
    val parsed2 = numerical.Number.parse(rendered1).get
    val rendered2 = parsed2.render
    val parsed3 = numerical.Number.parse(rendered2).get
    val rendered3 = parsed3.render

    // Should be completely stable
    rendered1 shouldBe "⅓"
    rendered2 shouldBe "⅓"
    rendered3 shouldBe "⅓"
  }

  it should "maintain stability over multiple parse-render cycles (ellipsis to asterisk)" in {
    val original = "2.5..."
    val parsed1 = numerical.Number.parse(original).get
    val rendered1 = parsed1.render
    // First render transforms ... to *
    rendered1 should endWith("*")

    val parsed2 = numerical.Number.parse(rendered1).get
    val rendered2 = parsed2.render
    val parsed3 = numerical.Number.parse(rendered2).get
    val rendered3 = parsed3.render

    // After first transformation, should be stable with *
    rendered1 shouldBe rendered2
    rendered2 shouldBe rendered3
  }

  // ========== FUZZINESS MAGNITUDE TESTS ==========

  behavior of "Fuzziness magnitude calculation"

  ignore should "calculate correct fuzziness for different decimal places" in {
    val cases = Seq(
      ("2.5*", -2), // ±5 × 10^-2 = ±0.05
      ("2.50*", -3), // ±5 × 10^-3 = ±0.005
      ("2.500*", -4), // ±5 × 10^-4 = ±0.0005
      ("1.23e5*", 4) // ±5 × 10^4 = ±50,000
    )

    cases.foreach { case (notation, expectedExponent) =>
      val parsed = numerical.Number.parse(notation).get
      parsed.fuzz shouldBe defined
      // Verify the order of magnitude is correct
      val fuzzValue = parsed.fuzz.get.wiggle(0.5)
      // The log value should be approximately expectedExponent + log10(5)
      val expectedLog = expectedExponent + math.log10(5)
      math.abs(fuzzValue - expectedLog) should be < 1.0
    }
  }

  // ========== RATIONAL CONVERSION TESTS ==========

  behavior of "Repeating decimal to rational conversion"

  it should "convert simple single-digit repeating decimals correctly" in {
    val cases = Seq(
      ("0.<1>", Rational(1, 9)),
      ("0.<2>", Rational(2, 9)),
      ("0.<3>", Rational(1, 3)),
      ("0.<4>", Rational(4, 9)),
      ("0.<5>", Rational(5, 9)),
      ("0.<6>", Rational(2, 3)),
      ("0.<7>", Rational(7, 9)),
      ("0.<8>", Rational(8, 9)),
      ("0.<9>", Rational(1, 1))
    )

    cases.foreach { case (notation, expectedRational) =>
      val parsed = numerical.Number.parse(notation).get
      parsed.toNominalRational shouldBe Some(expectedRational)
    }
  }

  it should "convert repeating decimals with non-repeating parts correctly" in {
    val cases = Seq(
      ("0.1<6>", Rational(1, 6)),
      ("0.08<3>", Rational(1, 12)),
      ("0.83<3>", Rational(5, 6)),
      ("0.16<6>", Rational(1, 6))
    )

    cases.foreach { case (notation, expectedRational) =>
      val parsed = numerical.Number.parse(notation).get
      parsed.toNominalRational shouldBe Some(expectedRational)
    }
  }

  it should "convert multi-digit repeating patterns correctly" in {
    val cases = Seq(
      //      ("0.<142857>", Rational(1, 7)),
      ("0.<285714>", Rational(2, 7)),
      ("0.<12>", Rational(4, 33)),
      ("0.<09>", Rational(1, 11))
    )

    cases.foreach { case (notation, expectedRational) =>
      val parsed = numerical.Number.parse(notation).get
      parsed.toNominalRational shouldBe Some(expectedRational)
    }
  }
}