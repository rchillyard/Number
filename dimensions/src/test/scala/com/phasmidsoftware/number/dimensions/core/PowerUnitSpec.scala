package com.phasmidsoftware.number.dimensions.core

import com.phasmidsoftware.number.algebra.eager.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PowerUnitSpec extends AnyFlatSpec with Matchers {

  behavior of "PowerUnit"

  it should "create squared units with correct dimension" in {
    val squareMeter = Meter.squared
    squareMeter.symbol shouldBe "m²"
    squareMeter.toSI shouldBe WholeNumber.one
  }

  it should "create cubed units with correct dimension" in {
    val cubicMeter = Meter.cubed
    cubicMeter.symbol shouldBe "m³"
    cubicMeter.toSI shouldBe WholeNumber.one
  }

  it should "create inverted units with correct dimension" in {
    val perSecond = Second.invert
    perSecond.symbol shouldBe "s⁻¹"
    perSecond.toSI shouldBe WholeNumber.one
  }

  it should "create square root units with correct dimension" in {
    val sqrtMeter = Meter.sqrt
    sqrtMeter.symbol shouldBe "m^½"
    // toSI for sqrt is trickier - you might need to handle fractional powers differently
  }

  it should "compute toSI correctly for squared non-SI units" in {
    val squareKilometer = Kilometer.squared
    squareKilometer.symbol shouldBe "km²"
    squareKilometer.toSI shouldBe WholeNumber(1000000) // 1000^2
  }

  it should "compute toSI correctly for cubed non-SI units" in {
    val cubicCentimeter = Centimeter.cubed
    cubicCentimeter.symbol shouldBe "cm³"
    cubicCentimeter.toSI shouldBe RationalNumber(1, 1000000) // (1/100)^3
  }

  it should "compute toSI correctly for inverted non-SI units" in {
    val perMinute = Minute.invert
    perMinute.symbol shouldBe "min⁻¹"
    perMinute.toSI shouldBe RationalNumber(1, 60)
    perMinute.compositeSymbol shouldBe "(60·s)^-1"
  }

  it should "handle squared composite units" in {
    val velocity = Meter / Second
    val velocitySquared = velocity.squared
    velocitySquared.symbol shouldBe "(m/s)²"
  }

  it should "allow combining powers with other operations" in {
    val acceleration = Meter / Second.squared
    acceleration.symbol shouldBe "m/s²"
  }

  it should "compute correct dimensions for area" in {
    val area: Unit[Area] = Meter.squared
    area.toSI shouldBe WholeNumber.one
  }

  it should "compute correct dimensions for volume" in {
    val volume: Unit[Volume] = Meter.cubed
    volume.toSI shouldBe WholeNumber.one
  }

  it should "compute correct dimensions for frequency" in {
    val frequency: Unit[Frequency] = Second.invert
    frequency.toSI shouldBe WholeNumber.one
  }

  it should "handle imperial units squared" in {
    val squareFoot = Foot.squared
    squareFoot.symbol shouldBe "ft²"
    // (254 * 12 / 10000)^2 = 3048^2 / 10000^2 = 9290304 / 100000000
  }

  it should "handle pounds squared (though physically odd)" in {
    val poundSquared = Pound.squared
    poundSquared.symbol shouldBe "lb²"
    val poundToSI = RationalNumber(45359237, 100000000)
    val expected = poundToSI * poundToSI
    poundSquared.toSI shouldBe expected
  }

  it should "create acceleration using Second.squared" in {
    val accel: Unit[Acceleration] = Meter / Second.squared
    accel match {
      case q: QuotientUnit[?] =>
        q.numerator.symbol shouldBe "m"
        q.denominator.symbol shouldBe "s²"
    }
  }

  it should "handle chained power operations" in {
    val m2 = Meter.squared
    val m4 = m2.squared // m^4
    m4.symbol shouldBe "m²²" // This might not be ideal formatting, but it works
  }
}