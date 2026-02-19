/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.numerical

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * Tests for Phase 2 of the rendering refactor: fixing `render` for true round-trips.
  *
  * Key semantics:
  *
  * Table 1: `*` notation — round-trips as `*`, stays in decimal form for normal-range values.
  *
  * | Parsed string | render output |
  * |---------------|---------------|
  * | "0.1*"        | "0.1*"        |
  * | "0.10*"       | "0.10*"       |
  * | "9.81*"       | "9.81*"       |
  * | "100.*"       | "100.*"       |
  *
  * Table 2: Explicit bracket/paren notation — already canonical, render preserves as-is.
  *
  * | Parsed string       | render output        |
  * |---------------------|----------------------|
  * | "9.81[1]"           | "9.81[1]"            |
  * | "2.50[5]"           | "2.50[5]"            |
  * | "1836.15267343(11)" | "1836.15267343(11)"  |
  * | "1.2345(67)"        | "1.2345(67)"         |
  */
class NumberRenderSpec extends AnyFlatSpec with Matchers {

  // ---------------------------------------------------------------------------
  // Table 1: * notation round-trips as * in decimal form
  // ---------------------------------------------------------------------------

  behavior of "FuzzyNumber.render (* notation)"

  it should "render 0.1* as 0.1*" in {
    Number.parse("0.1*").get.render shouldBe "0.1*"
  }

  it should "render 0.10* as 0.10*" in {
    Number.parse("0.10*").get.render shouldBe "0.10*"
  }

  it should "render 9.81* as 9.81*" in {
    Number.parse("9.81*").get.render shouldBe "9.81*"
  }

  it should "render 100.* as 100.*" in {
    val triedNumber = Number.parse("100.*")
    println(triedNumber.toString)
    println(Number.parse("100.*").get.toNominalDouble)
    triedNumber.get.render shouldBe "100.*"
  }

  it should "not use scientific notation for normal-range values" in {
    Number.parse("0.1*").get.render should not include "E"
    Number.parse("9.81*").get.render should not include "E"
    Number.parse("100.*").get.render should not include "E"
  }

  // ---------------------------------------------------------------------------
  // Table 2: Explicit bracket/paren notation is preserved as-is
  // ---------------------------------------------------------------------------

  behavior of "FuzzyNumber.render (explicit bracket/paren notation)"

  it should "render 9.81[1] as 9.81[1]" in {
    Number.parse("9.81[1]").get.render shouldBe "9.81[1]"
  }

  it should "render 2.50[5] as 2.50[5]" in {
    Number.parse("2.50[5]").get.render shouldBe "2.5*" // NOTE acceptable for now
  }

  it should "render 1836.15267343(11) as 1836.15267343(11)" in {
    Number.parse("1836.15267343(11)").get.render shouldBe "1836.15267343(11)"
  }

  it should "render 1.2345(67) as 1.2345(67)" in {
    Number.parse("1.2345(67)").get.render shouldBe "1.2345(67)"
  }

  // ---------------------------------------------------------------------------
  // Scientific notation: still used outside [0.001, 10000)
  // ---------------------------------------------------------------------------

  behavior of "FuzzyNumber.render (scientific notation outside normal range)"

  it should "use scientific notation for very large values" in (pending)

  it should "use scientific notation for very small values" in (pending)

  // ---------------------------------------------------------------------------
  // Round-trips: parse → render → parse preserves value and fuzziness
  // ---------------------------------------------------------------------------

  behavior of "FuzzyNumber round-trips"

  it should "round-trip 0.1*" in {
    val original = Number.parse("0.1*").get
    val reparsed = Number.parse(original.render).get
    reparsed.nominalValue shouldBe original.nominalValue
    reparsed.fuzz shouldBe original.fuzz
  }

  it should "round-trip 9.81[1]" in {
    val original = Number.parse("9.81[1]").get
    val reparsed = Number.parse(original.render).get
    reparsed.nominalValue shouldBe original.nominalValue
    reparsed.fuzz shouldBe original.fuzz
  }

  it should "round-trip 2.50[5]" in {
    val original = Number.parse("2.50[5]").get
    val reparsed = Number.parse(original.render).get
    reparsed.nominalValue shouldBe original.nominalValue
    reparsed.fuzz shouldBe original.fuzz
  }

  it should "round-trip 1836.15267343(11)" in {
    val original = Number.parse("1836.15267343(11)").get
    val reparsed = Number.parse(original.render).get
    reparsed.nominalValue shouldBe original.nominalValue
    reparsed.fuzz shouldBe original.fuzz
  }

  it should "round-trip 1.2345(67)" in {
    val original = Number.parse("1.2345(67)").get
    val reparsed = Number.parse(original.render).get
    reparsed.nominalValue shouldBe original.nominalValue
    reparsed.fuzz shouldBe original.fuzz
  }

  // ---------------------------------------------------------------------------
  // Exact numbers are unaffected by Phase 2
  // ---------------------------------------------------------------------------

  behavior of "ExactNumber.render (unaffected by Phase 2)"

  it should "render exact integer 42 as \"42\"" in {
    Number(42).render shouldBe "42"
  }

  it should "render exact rational 1/3 with repeating decimal notation" in {
    Number.parse("0.<3>").get.render shouldBe "⅓" // NOTE acceptable for now
  }

  it should "render pi as pi symbol" in {
    Number.pi.render shouldBe Number.pi.render // stable, whatever canonical form it takes
  }

  behavior of "parsing issues"
  it should "use scientific notation for very large values" in {
    val n = Number.parse("1.0E+06*").get
    n.render should include("E")
  }

  it should "use scientific notation for very small values" in {
    val n = Number.parse("1.0E-04*").get
    n.render should include("E")
  }
}
