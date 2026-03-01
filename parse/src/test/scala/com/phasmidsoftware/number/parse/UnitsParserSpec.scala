package com.phasmidsoftware.number.parse

import com.phasmidsoftware.number.algebra.eager.RationalNumber
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.dimensions.core.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * A specification class for testing the functionality of the `UnitsParser` component.
  * TODO what are all these println statements?
  *
  * This class verifies the ability of `UnitsParser` to accurately parse and interpret various unit formats,
  * including simple units, derived units, composite units, and units using different notations.
  *
  * The tests are structured using the behavior-driven development (BDD) style provided by `AnyFlatSpec`.
  * The common behaviors and edge cases of `UnitsParser` are described and validated to ensure that it behaves
  * as expected for both valid inputs (e.g., "m", "kg", "m³") and invalid inputs (e.g., "xyz", "m//s").
  *
  * Key functionality covered includes:
  * - Parsing base units like "m" (meter), "s" (second), and "kg" (kilogram).
  * - Supporting unit prefixes, such as "km" (kilometer) and "cm" (centimeter).
  * - Handling derived units like "N" (Newton) and "J" (Joule).
  * - Parsing units with superscripts, such as "m²" for square meters and "m³" for cubic meters.
  * - Recognizing alternative notations, e.g., caret notation ("m^2", "m^3") for powers and slash notation ("m/s").
  * - Handling product ("kg·m", "kg*m") and quotient units ("m/s", "m/s²").
  * - Validating specialized units like velocity ("km/h") and gravitational constant units.
  * - Failing gracefully on unrecognized or invalid input.
  */
class UnitsParserSpec extends AnyFlatSpec with Matchers {

  behavior of "UnitsParser"

  it should "parse simple base units" in {
    UnitsParser.parseUnit("m") match {
      case Right(unit) =>
        unit shouldBe Meter
        unit.symbol shouldBe "m"
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  it should "parse kilogram" in {
    UnitsParser.parse("kg") match {
      case Right(unit) =>
        unit.symbol shouldBe "kg"
        unit shouldBe Kilogram
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  it should "parse seconds" in {
    UnitsParser.parse("s") match {
      case Right(unit) =>
        unit.symbol shouldBe "s"
        unit shouldBe Second
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  it should "parse m cubed step by step" in {
    // First, does the parser see the ³?
    val input = "m³"
    println(s"Parsing: $input")

    val result = UnitsParser.parse(input)
    println(s"Result: $result")

    result match {
      case Right(unit) =>
        println(s"Unit type: ${unit.getClass.getName}")
        println(s"Unit symbol: ${unit.symbol}")
        unit match {
          case p: PowerUnit[?] =>
            println(s"Power: ${p.power}")
            println(s"Base: ${p.base}")
          case _ =>
            println(s"Not a PowerUnit, it's: ${unit}")
        }
      case Left(err) => println(s"Error: $err")
    }
  }

  it should "parse squared units with superscript" in {
    UnitsParser.parse("m²") match {
      case Right(unit) =>
        unit.symbol shouldBe "m²"
        unit shouldBe a[PowerUnit[?]]
      case Left(err) => fail(s"Parse failed: $err")
    }
  }
  it should "check character encoding" in {
    val testChar = "³"
    val parserChar = "³"
    println(s"Test char bytes: ${testChar.getBytes.mkString(",")}")
    println(s"Parser char bytes: ${parserChar.getBytes.mkString(",")}")
    testChar shouldBe parserChar
  }

  it should "parse cubed units with superscript" in {
    UnitsParser.parse("m³") match {
      case Right(unit) =>
        unit.symbol shouldBe "m³"
        unit shouldBe a[PowerUnit[?]]
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  it should "parse inverted units with superscript" in {
    UnitsParser.parse("s⁻¹") match {
      case Right(unit) =>
        unit.symbol shouldBe "s⁻¹"
        unit shouldBe a[PowerUnit[?]]
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  it should "parse squared units with caret notation" in {
    UnitsParser.parse("m^2") match {
      case Right(unit) =>
        unit.symbol shouldBe "m²"
        unit shouldBe a[PowerUnit[?]]
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  it should "parse cubed units with caret notation" in {
    UnitsParser.parse("m^3") match {
      case Right(unit) =>
        unit.symbol shouldBe "m³"
        unit shouldBe a[PowerUnit[?]]
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  it should "parse negative exponent with caret" in {
    UnitsParser.parse("s^-1") match {
      case Right(unit) =>
        unit.symbol shouldBe "s⁻¹"
        unit shouldBe a[PowerUnit[?]]
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  it should "parse quotient units with slash" in {
    UnitsParser.parse("m/s") match {
      case Right(unit) =>
        unit.symbol shouldBe "m/s"
        unit shouldBe a[QuotientUnit[?]]
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  it should "parse complex quotient units" in {
    UnitsParser.parse("m/s²") match {
      case Right(unit) =>
        unit.symbol shouldBe "m/s²"
        unit match {
          case q: QuotientUnit[?] =>
            q.numerator shouldBe Meter
            q.denominator.symbol shouldBe "s²"
          case _ => fail("Expected QuotientUnit")
        }
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  it should "parse product units with dot" in {
    UnitsParser.parse("kg·m") match {
      case Right(unit) =>
        unit.symbol shouldBe "kg·m"
        unit shouldBe a[ProductUnit[?]]
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  it should "parse product units with asterisk" in {
    UnitsParser.parse("kg*m") match {
      case Right(unit) =>
        unit.symbol shouldBe "kg·m"
        unit shouldBe a[ProductUnit[?]]
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  it should "parse complex composite units" in {
    UnitsParser.parse("kg·m/s²") match {
      case Right(unit) =>
        unit.symbol shouldBe "N"
        unit shouldBe a[Newton$]
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  it should "parse Newton (force units)" in {
    UnitsParser.parse("N") match {
      case Right(unit) =>
        unit.symbol shouldBe "N"
        unit shouldBe Newton
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  it should "parse Joule (energy units)" in {
    UnitsParser.parse("J") match {
      case Right(unit) =>
        unit.symbol shouldBe "J"
        unit shouldBe Joule
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  it should "parse kilometers" in {
    UnitsParser.parse("km") match {
      case Right(unit) =>
        unit.symbol shouldBe "km"
        unit shouldBe Kilometer
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  it should "parse centimeters" in {
    UnitsParser.parse("cm") match {
      case Right(unit) =>
        unit.symbol shouldBe "cm"
        unit shouldBe Centimeter
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  it should "parse grams" in {
    UnitsParser.parse("g") match {
      case Right(unit) =>
        unit.symbol shouldBe "g"
        unit shouldBe Gram
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  it should "parse minutes" in {
    UnitsParser.parse("min") match {
      case Right(unit) =>
        unit.symbol shouldBe "min"
        unit shouldBe Minute
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  it should "parse hours" in {
    UnitsParser.parse("h") match {
      case Right(unit) =>
        unit.symbol shouldBe "h"
        unit shouldBe Hour
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  it should "fail on unrecognized unit" in {
    UnitsParser.parse("xyz") match {
      case Right(_) => fail("Should not have parsed unrecognized unit")
      case Left(UnitError(_)) => // expected
      case _ => fail("Logic error: should have returned a UnitError")
    }
  }
  
  it should "fail on invalid syntax" in {
    UnitsParser.parse("m//s") match {
      case Right(_) => fail("Should not have parsed invalid syntax")
      case Left(UnitError(err)) => err should not be empty
      case _ => fail("Logic error: should have returned a UnitError")
    }
  }

  it should "parse product of three units" in {
    UnitsParser.parse("kg·m·s") match {
      case Right(unit) =>
        unit shouldBe a[ProductUnit[?]]
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  it should "parse velocity correctly" in {
    UnitsParser.parse("km/h") match {
      case Right(unit) =>
        unit.symbol shouldBe "km/h"
        unit.toSI shouldBe RationalNumber(Rational(5, 18))
        unit shouldBe a[ScaledUnit[?]]
      // Optionally check it has the right components
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  it should "parse unknown velocity correctly" in {
    UnitsParser.parse("ch/h") match {
      case Right(unit) =>
        unit.symbol shouldBe "ch/h"
        unit.toSI shouldBe RationalNumber(Rational(1397, 250_000))
        unit shouldBe a[QuotientUnit[?]]
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  behavior of "CompositeUnits"

  it should "parse the Gravitation constant units" in {
    val perhapsKepler = UnitsParser.parse("m³/(kg·s²)")
    println(s"perhapsKepler = $perhapsKepler")
    perhapsKepler.isRight shouldBe true
  }


}