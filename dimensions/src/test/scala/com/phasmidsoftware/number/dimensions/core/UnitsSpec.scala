package com.phasmidsoftware.number.dimensions.core

import com.phasmidsoftware.number.algebra.eager.{Eager, RationalNumber, WholeNumber}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class UnitsSpec extends AnyFlatSpec with should.Matchers {

  behavior of "SI Base Units"

  it should "have meter as base unit of length" in {
    Meter.toSI === Eager.one shouldBe true
    Meter.symbol shouldBe "m"
  }

  it should "have kilogram as base unit of mass" in {
    Kilogram.toSI shouldBe Eager.one
    Kilogram.symbol shouldBe "kg"
  }

  it should "have second as base unit of time" in {
    Second.toSI shouldBe Eager.one
    Second.symbol shouldBe "s"
  }

  behavior of "Length Units Conversions"

  it should "convert kilometers to meters" in {
    Kilometer.toSI shouldBe WholeNumber(1000)
  }

  it should "convert centimeters to meters" in {
    Centimeter.toSI shouldBe RationalNumber(1, 100)
  }

  it should "convert inches to meters" in {
    Inch.toSI.toDouble shouldBe 0.0254
  }

  it should "convert feet to meters" in {
    Foot.toSI.toDouble shouldBe 0.3048 +- 0.0001
  }

  it should "convert miles to meters" in {
    Mile.toSI.toDouble shouldBe 1609.344 +- 0.001
  }

  behavior of "Mass Units Conversions"

  it should "convert grams to kilograms" in {
    Gram.toSI.toDouble shouldBe 0.001
  }

  it should "convert pounds to kilograms" in {
    Pound.toSI.toDouble shouldBe 0.45359237 +- 0.0000001
  }

  behavior of "Time Units Conversions"

  it should "convert minutes to seconds" in {
    Minute.toSI shouldBe WholeNumber(60)
  }

  it should "convert hours to seconds" in {
    Hour.toSI shouldBe WholeNumber(3600)
  }

  it should "convert days to seconds" in {
    Day.toSI shouldBe WholeNumber(86400)
  }

  behavior of "Angular Measures"

  it should "have radian as base unit of angle" in {
    Radian.toSI shouldBe WholeNumber.one
    Radian.symbol shouldBe "rad"
  }

  //  it should "convert degrees to radians" in {
  //    Degree.toSI.toDouble shouldBe (Math.PI / 180.0) +- 0.000001
  //  }

  behavior of "Derived SI Units"

  it should "have Newton as unit of force" in {
    Newton.symbol shouldBe "N"
    Newton.toSI shouldBe WholeNumber.one
  }

  it should "have Joule as unit of energy" in {
    Joule.symbol shouldBe "J"
    Joule.toSI shouldBe WholeNumber.one
  }

  it should "have Watt as unit of power" in {
    Watt.symbol shouldBe "W"
    Watt.toSI shouldBe WholeNumber.one
  }

  it should "have Pascal as unit of pressure" in {
    Pascal.symbol shouldBe "Pa"
    Pascal.toSI shouldBe WholeNumber.one
  }

  it should "have Hertz as unit of frequency" in {
    Hertz.symbol shouldBe "Hz"
    Hertz.toSI shouldBe WholeNumber.one
  }

  behavior of "Velocity Units"

  it should "convert km/h to m/s" in {
    KilometerPerHour.toSI.toDouble shouldBe (1000.0 / 3600.0) +- 0.0001
  }

  it should "verify 100 km/h is approximately 27.78 m/s" in {
    (100.0 * KilometerPerHour.toSI.toDouble) shouldBe 27.78 +- 0.01
  }

  it should "convert mph to m/s" in {
    MilePerHour.toSI.toDouble shouldBe 0.44704 +- 0.00001
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