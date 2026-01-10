package com.phasmidsoftware.number.parse

import com.phasmidsoftware.number.dimensions.core.*
import fastparse.Parsed
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UnitsParserSpec extends AnyFlatSpec with Matchers {

  behavior of "UnitsParser"

  it should "parse just 'm' directly" in {
    val result = fastparse.parse("m", p => UnitsParser.knownUnitParser(using p))
    result match {
      case Parsed.Success(u, _) => u shouldBe Meter
      case f: Parsed.Failure => fail(s"Failed: ${f.trace().longMsg}")
    }
  }

  it should "parse simple base units" in {
    UnitsParser.parse("m") match {
      case Right(unit) =>
        unit.symbol shouldBe "m"
        unit shouldBe Meter
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

  it should "parse squared units with superscript" in {
    UnitsParser.parse("m²") match {
      case Right(unit) =>
        unit.symbol shouldBe "m²"
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  ignore should "parse cubed units with caret notation" in {
    UnitsParser.parse("m^3") match {
      case Right(unit) =>
        unit.symbol shouldBe "m³" // NOT "m^3"
        unit shouldBe a[PowerUnit[?]]
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  ignore should "parse cubed units with superscript" in {
    UnitsParser.parse("m³") match {
      case Right(unit) =>
        unit.symbol shouldBe "m³"
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  ignore should "parse inverted units with superscript" in {
    UnitsParser.parse("s⁻¹") match {
      case Right(unit) =>
        unit.symbol shouldBe "s⁻¹"
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  ignore should "parse squared units with caret notation" in {
    UnitsParser.parse("m^2") match {
      case Right(unit) =>
        unit.symbol shouldBe "m²"
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  ignore should "parse negative exponent with caret" in {
    UnitsParser.parse("s^-1") match {
      case Right(unit) =>
        unit.symbol shouldBe "s⁻¹"
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
        unit.symbol shouldBe "kg·m/s²"
        unit shouldBe a[QuotientUnit[?]]
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
      case Left(err) => err should include("Expected")
    }
  }

  ignore should "fail on invalid syntax" in {
    UnitsParser.parse("m//s") match {
      case Right(_) => fail("Should not have parsed invalid syntax")
      case Left(err) => err.length should be > 0
    }
  }

  it should "parse product of three units" in {
    UnitsParser.parse("kg·m·s") match {
      case Right(unit) =>
        unit shouldBe a[ProductUnit[?]]
      case Left(err) => fail(s"Parse failed: $err")
    }
  }

  ignore should "parse velocity correctly" in {
    UnitsParser.parse("km/h") match {
      case Right(unit) =>
        unit.symbol shouldBe "km/h"
        unit shouldBe KilometerPerHour
      case Left(err) => fail(s"Parse failed: $err")
    }
  }
}