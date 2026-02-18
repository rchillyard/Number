/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.numerical

import cats.Show
import com.phasmidsoftware.number.core.inner.{PureNumber, Rational}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * Tests for the `show` method introduced in Phase 1 of the rendering refactor.
  *
  * Covers:
  *   - ExactNumber.show: vulgar fractions, integers, general fallback to render
  *   - FuzzyNumber.show: trailing zero stripping before uncertainty markers
  *   - Real.show: delegation to underlying Number
  *   - NumberLike.show: default fallback to render
  *   - Cats Show[NumberLike] typeclass instance
  */
class NumberShowSpec extends AnyFlatSpec with Matchers {

  // ---------------------------------------------------------------------------
  // ExactNumber.show — vulgar fractions
  // ---------------------------------------------------------------------------

  behavior of "ExactNumber.show (vulgar fractions)"

  it should "show 1/2 as ½" in {
    Number(Rational(1, 2)).show shouldBe "½"
  }

  it should "show 1/3 as ⅓" in {
    Number(Rational(1, 3)).show shouldBe "⅓"
  }

  it should "show 2/3 as ⅔" in {
    Number(Rational(2, 3)).show shouldBe "⅔"
  }

  it should "show 1/4 as ¼" in {
    Number(Rational(1, 4)).show shouldBe "¼"
  }

  it should "show 3/4 as ¾" in {
    Number(Rational(3, 4)).show shouldBe "¾"
  }

  it should "show 1/5 as ⅕" in {
    Number(Rational(1, 5)).show shouldBe "⅕"
  }

  it should "show 1/8 as ⅛" in {
    Number(Rational(1, 8)).show shouldBe "⅛"
  }

  it should "show 3/8 as ⅜" in {
    Number(Rational(3, 8)).show shouldBe "⅜"
  }

  it should "show 5/8 as ⅝" in {
    Number(Rational(5, 8)).show shouldBe "⅝"
  }

  it should "show 7/8 as ⅞" in {
    Number(Rational(7, 8)).show shouldBe "⅞"
  }

  it should "show 1/7 as ⅐" in {
    Number(Rational(1, 7)).show shouldBe "⅐"
  }

  it should "show 1/9 as ⅑" in {
    Number(Rational(1, 9)).show shouldBe "⅑"
  }

  it should "show 1/10 as ⅒" in {
    Number(Rational(1, 10)).show shouldBe "⅒"
  }

  // ---------------------------------------------------------------------------
  // ExactNumber.show — non-vulgar rationals fall back to render
  // ---------------------------------------------------------------------------

  behavior of "ExactNumber.show (non-vulgar rationals)"

  it should "fall back to render for 1/11 (no vulgar fraction available)" in {
    val n = Number(Rational(1, 11))
    n.show shouldBe n.render
  }

  it should "fall back to render for 5/7 (no vulgar fraction available)" in {
    val n = Number(Rational(5, 7))
    n.show shouldBe n.render
  }

  // ---------------------------------------------------------------------------
  // ExactNumber.show — integers
  // ---------------------------------------------------------------------------

  behavior of "ExactNumber.show (integers)"

  it should "show 0 as \"0\"" in {
    Number(0).show shouldBe "0"
  }

  it should "show 1 as \"1\"" in {
    Number(1).show shouldBe "1"
  }

  it should "show -3 as \"-3\"" in {
    Number(-3).show shouldBe "-3"
  }

  it should "show 42 as \"42\"" in {
    Number(42).show shouldBe "42"
  }

  // ---------------------------------------------------------------------------
  // ExactNumber.show — non-PureNumber factors fall back to render
  // ---------------------------------------------------------------------------

  behavior of "ExactNumber.show (non-PureNumber factors)"

  it should "fall back to render for pi" in {
    Number.pi.show shouldBe Number.pi.render
  }

  it should "fall back to render for e" in {
    Number.e.show shouldBe Number.e.render
  }

  // ---------------------------------------------------------------------------
  // FuzzyNumber.show — no fuzz (fallback)
  // ---------------------------------------------------------------------------

  behavior of "FuzzyNumber.show (no uncertainty marker)"

  ignore should "fall back to render when there is no uncertainty marker" in {
    // A FuzzyNumber with None fuzz renders like an ExactNumber
    val n = FuzzyNumber(Number.parse("3*").get.nominalValue, PureNumber, None)
    n.show shouldBe n.render
  }

  // ---------------------------------------------------------------------------
  // Real.show — delegation
  // ---------------------------------------------------------------------------

  behavior of "Real.show"

  it should "delegate to the underlying Number for an exact rational" in {
    Real(Rational(1, 3)).show shouldBe "⅓"
  }

  it should "delegate to the underlying Number for an integer" in {
    Real(5).show shouldBe "5"
  }

  it should "delegate to the underlying Number for a fuzzy value" in {
    val r = Real(Number.parse("1.10*").get)
    r.show shouldBe "1.1±0.45%"
  }

  // ---------------------------------------------------------------------------
  // show vs render — the key contract
  // ---------------------------------------------------------------------------

  behavior of "show vs render contract"

  it should "show and render agree for exact integers" in {
    val n = Number(7)
    n.show shouldBe n.render
  }

  it should "show may differ from render for vulgar fractions" in {
    val n = Number(Rational(1, 3))
    //    n.show should not be n.render
    n.show shouldBe "⅓"
  }

  // ---------------------------------------------------------------------------
  // Cats Show[NumberLike] typeclass
  // ---------------------------------------------------------------------------

  behavior of "Cats Show[NumberLike]"

  it should "be available implicitly" in {
    val show = implicitly[Show[NumberLike]]
    show.show(Number(Rational(1, 2))) shouldBe "½"
  }

  it should "use show not render for vulgar fractions" in {
    val n: NumberLike = Number(Rational(1, 4))
    n.show shouldBe "¼"
  }
}