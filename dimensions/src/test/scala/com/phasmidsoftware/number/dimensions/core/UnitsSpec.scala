package com.phasmidsoftware.number.dimensions.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class UnitsSpec extends AnyFlatSpec with should.Matchers {

  behavior of "SI Base Units"

  it should "have meter as base unit of length" in {
    Meter.toSI shouldBe 1.0
    Meter.symbol shouldBe "m"
  }

  it should "have kilogram as base unit of mass" in {
    Kilogram.toSI shouldBe 1.0
    Kilogram.symbol shouldBe "kg"
  }

  it should "have second as base unit of time" in {
    Second.toSI shouldBe 1.0
    Second.symbol shouldBe "s"
  }

  behavior of "Length Units Conversions"

  it should "convert kilometers to meters" in {
    Kilometer.toSI shouldBe 1000.0
  }

  it should "convert centimeters to meters" in {
    Centimeter.toSI shouldBe 0.01
  }

  it should "convert inches to meters" in {
    Inch.toSI shouldBe 0.0254
  }

  it should "convert feet to meters" in {
    Foot.toSI shouldBe 0.3048 +- 0.0001
  }

  it should "convert miles to meters" in {
    Mile.toSI shouldBe 1609.344 +- 0.001
  }

  behavior of "Mass Units Conversions"

  it should "convert grams to kilograms" in {
    Gram.toSI shouldBe 0.001
  }

  it should "convert pounds to kilograms" in {
    Pound.toSI shouldBe 0.45359237 +- 0.0000001
  }

  behavior of "Time Units Conversions"

  it should "convert minutes to seconds" in {
    Minute.toSI shouldBe 60.0
  }

  it should "convert hours to seconds" in {
    Hour.toSI shouldBe 3600.0
  }

  it should "convert days to seconds" in {
    Day.toSI shouldBe 86400.0
  }

  behavior of "Angular Measures"

  it should "have radian as base unit of angle" in {
    Radian.toSI shouldBe 1.0
    Radian.symbol shouldBe "rad"
  }

  it should "convert degrees to radians" in {
    Degree.toSI shouldBe (Math.PI / 180.0) +- 0.000001
  }

  it should "convert 90 degrees to π/2 radians" in {
    (90.0 * Degree.toSI) shouldBe (Math.PI / 2.0) +- 0.000001
  }

  it should "convert 180 degrees to π radians" in {
    (180.0 * Degree.toSI) shouldBe Math.PI +- 0.000001
  }

  it should "convert full turn to 2π radians" in {
    Turn.toSI shouldBe (2.0 * Math.PI) +- 0.000001
  }

  behavior of "Derived SI Units"

  it should "have Newton as unit of force" in {
    Newton.symbol shouldBe "N"
    Newton.toSI shouldBe 1.0
  }

  it should "have Joule as unit of energy" in {
    Joule.symbol shouldBe "J"
    Joule.toSI shouldBe 1.0
  }

  it should "have Watt as unit of power" in {
    Watt.symbol shouldBe "W"
    Watt.toSI shouldBe 1.0
  }

  it should "have Pascal as unit of pressure" in {
    Pascal.symbol shouldBe "Pa"
    Pascal.toSI shouldBe 1.0
  }

  it should "have Hertz as unit of frequency" in {
    Hertz.symbol shouldBe "Hz"
    Hertz.toSI shouldBe 1.0
  }

  behavior of "Velocity Units"

  it should "convert km/h to m/s" in {
    KilometerPerHour.toSI shouldBe (1000.0 / 3600.0) +- 0.0001
  }

  it should "verify 100 km/h is approximately 27.78 m/s" in {
    (100.0 * KilometerPerHour.toSI) shouldBe 27.78 +- 0.01
  }

  it should "convert mph to m/s" in {
    MilePerHour.toSI shouldBe 0.44704 +- 0.00001
  }

  behavior of "Type Safety"

  it should "enforce that Meter has Length dimension" in {
    summon[Meter.type <:< Unit[Length]]
  }

  it should "enforce that Newton has Force dimension" in {
    summon[Newton.type <:< Unit[Force]]
  }

  it should "enforce that Radian has Angle dimension" in {
    summon[Radian.type <:< Unit[Angle]]
  }

  it should "verify Angle is dimensionless" in {
    summon[Angle =:= Dimensionless]
  }
}