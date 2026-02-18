/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.numerical

import com.phasmidsoftware.number.core.inner.PureNumber
import com.phasmidsoftware.number.core.numerical.{AbsoluteFuzz, Box, Gaussian, RelativeFuzz}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * Tests for Phase 3 of the rendering refactor: FuzzyNumber.show with percentage notation.
  *
  * Key principle: show converts fuzziness to relative (percentage) form.
  * render always produces absolute notation ([n], (n), *).
  * show produces human-friendly percentage notation (100±1%).
  */
class NumberShowFuzzySpec extends AnyFlatSpec with Matchers {

  // ---------------------------------------------------------------------------
  // AbsoluteFuzz → converted to relative percentage in show
  // ---------------------------------------------------------------------------

  behavior of "FuzzyNumber.show (AbsoluteFuzz → percentage)"

  it should "show 100.0 with AbsoluteFuzz(1.0, Gaussian) as 100±1%" in {
    FuzzyNumber(Number.parse("100").get.nominalValue, PureNumber,
      Some(AbsoluteFuzz(1.0, Gaussian))).show shouldBe "100±1.0%"
  }

  it should "show 9.81 with AbsoluteFuzz(0.005, Box) as a percentage" in {
    val n = Number.parse("9.81*").get
    // 0.005/9.81 ≈ 0.051% -- show gives percentage form
    n.show should include("±")
    n.show should endWith("%")
  }

  it should "show differs from render for fuzzy numbers" in {
    val n = Number.parse("9.81*").get
    n.show should not be n.render
  }

  // ---------------------------------------------------------------------------
  // RelativeFuzz → already relative, directly shown as percentage
  // ---------------------------------------------------------------------------

  behavior of "FuzzyNumber.show (RelativeFuzz → percentage)"

  it should "show value with 1% relative fuzz as ±1%" in {
    FuzzyNumber(Number.parse("100").get.nominalValue, PureNumber,
      Some(RelativeFuzz(0.01, Gaussian))).show shouldBe "100±1.0%"
  }

  it should "show value with 0.5% relative fuzz as ±0.50%" in {
    FuzzyNumber(Number.parse("100").get.nominalValue, PureNumber,
      Some(RelativeFuzz(0.005, Gaussian))).show shouldBe "100±0.50%"
  }

  it should "show value with 10% relative fuzz as ±10%" in {
    FuzzyNumber(Number.parse("100").get.nominalValue, PureNumber,
      Some(RelativeFuzz(0.1, Gaussian))).show shouldBe "100±10.0%"
  }

  // ---------------------------------------------------------------------------
  // show vs render contract for fuzzy numbers
  // ---------------------------------------------------------------------------

  behavior of "FuzzyNumber show vs render contract"

  it should "render uses absolute notation, show uses percentage" in {
    val n = FuzzyNumber(Number.parse("100").get.nominalValue, PureNumber,
      Some(AbsoluteFuzz(1.0, Gaussian)))
    n.render should include("E") // scientific notation with absolute fuzz
    n.show should endWith("%") // percentage notation
  }

  it should "show includes ± separator" in {
    val n = FuzzyNumber(Number.parse("9.81").get.nominalValue, PureNumber,
      Some(AbsoluteFuzz(0.01, Box)))
    n.show should include("±")
  }

  // ---------------------------------------------------------------------------
  // Exact FuzzyNumber (no fuzz) falls back to render
  // ---------------------------------------------------------------------------

  behavior of "FuzzyNumber.show (no fuzz → render fallback)"

  it should "fall back to render when fuzz is None" in {
    val n = FuzzyNumber(Number.parse("42").get.nominalValue, PureNumber, None)
    n.show shouldBe n.render
  }

  // ---------------------------------------------------------------------------
  // Real.show delegates correctly for fuzzy values
  // ---------------------------------------------------------------------------

  behavior of "Real.show (fuzzy delegation)"

  it should "show Real with relative fuzz as percentage" in {
    val n = FuzzyNumber(Number.parse("100").get.nominalValue, PureNumber,
      Some(RelativeFuzz(0.01, Gaussian)))
    Real(n).show shouldBe "100±1.0%"
  }

  it should "show Real with absolute fuzz as percentage" in {
    val n = FuzzyNumber(Number.parse("100").get.nominalValue, PureNumber,
      Some(AbsoluteFuzz(1.0, Gaussian)))
    Real(n).show shouldBe "100±1.0%"
  }
}