package com.phasmidsoftware.number.top

import com.phasmidsoftware.number.algebra.eager.*
import com.phasmidsoftware.number.dimensions.core.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class QuantityLaTeXSpec extends AnyFlatSpec with Matchers {

  behavior of "Quantity LaTeX rendering"

  it should "render simple quantity with base unit" in {
    val distance = Quantity(WholeNumber(5), Meter)
    distance.renderLaTeX shouldBe "5\\,\\text{m}"
  }

  it should "render rational number with unit" in {
    val distance = Quantity(RationalNumber(22, 7), Meter)
    distance.renderLaTeX shouldBe "3.\\overline{142857}\\,\\text{m}"
  }

  it should "render quotient units as fractions" in {
    val velocity = Quantity(WholeNumber(10), Meter / Second)
    velocity.renderLaTeX shouldBe "10\\,\\frac{\\text{m}}{\\text{s}}"
  }

  it should "render product units with cdot" in {
    val area = Quantity(WholeNumber(15), Meter * Meter)
    area.renderLaTeX shouldBe "15\\,\\text{m}\\cdot\\text{m}"
  }

  it should "render squared units with superscript 2" in {
    val area = Quantity(WholeNumber(25), Meter.squared)
    area.renderLaTeX shouldBe "25\\,\\text{m}^{2}"
  }

  it should "render cubed units with superscript 3" in {
    val volume = Quantity(WholeNumber(8), Meter.cubed)
    volume.renderLaTeX shouldBe "8\\,\\text{m}^{3}"
  }

  it should "render inverted units with superscript -1" in {
    val frequency = Quantity(WholeNumber(50), Second.invert)
    frequency.renderLaTeX shouldBe "50\\,\\text{s}^{-1}"
  }

  it should "render square root units with fractional exponent" in {
    val sqrtLength = Quantity(WholeNumber(4), Meter.sqrt)
    sqrtLength.renderLaTeX shouldBe "4\\,\\text{m}^{\\frac{1}{2}}"
  }

  it should "render acceleration with nested power" in {
    val accel = Quantity(RationalNumber(98, 10), Meter / Second.squared)
    accel.renderLaTeX shouldBe "9.8\\,\\frac{\\text{m}}{\\text{s}^{2}}"
  }

  it should "render compound units with parentheses when needed" in {
    val velocity = Meter / Second
    val velocitySquared = Quantity(WholeNumber(100), velocity.squared)
    velocitySquared.renderLaTeX shouldBe "100\\,\\left(\\frac{\\text{m}}{\\text{s}}\\right)^{2}"
  }

  it should "render scaled units with custom symbols" in {
    val distance = Quantity(WholeNumber(5), Kilometer)
    distance.renderLaTeX shouldBe "5\\,\\text{km}"
  }

  it should "render SI derived units" in {
    val force = Quantity(WholeNumber(10), Newton)
    force.renderLaTeX shouldBe "10\\,\\text{N}"
  }

  it should "render complex composite units" in {
    val energy = Quantity(WholeNumber(50), Kilogram * Meter.squared / Second.squared)
    // Should render as kg·m²/s²
    energy.renderLaTeX should include("\\text{kg}")
    energy.renderLaTeX should include("\\text{m}^{2}")
    energy.renderLaTeX should include("\\text{s}^{2}")
  }

  it should "render imperial units" in {
    val distance = Quantity(WholeNumber(12), Foot)
    distance.renderLaTeX shouldBe "12\\,\\text{ft}"
  }

  it should "render velocity in km/h" in {
    val speed = Quantity(WholeNumber(100), KilometerPerHour)
    speed.renderLaTeX shouldBe "100\\,\\text{km/h}"
  }

  it should "render negative exponents correctly" in {
    val perSecondSquared = Quantity(WholeNumber(5), Second.squared.invert)
    perSecondSquared.renderLaTeX shouldBe "5\\,\\text{s}^{2}^{-1}"
  }

  it should "render fractional values with units" in {
    val distance = Quantity(RationalNumber(1, 2), Mile)
    distance.renderLaTeX shouldBe "½\\,\\text{mi}"
  }

  it should "handle zero values" in {
    val distance = Quantity(WholeNumber(0), Meter)
    distance.renderLaTeX shouldBe "0\\,\\text{m}"
  }

  it should "handle negative values" in {
    val distance = Quantity(WholeNumber(-5), Meter)
    distance.renderLaTeX shouldBe "-5\\,\\text{m}"
  }

  behavior of "PhysicalUnit LaTeX rendering"

  it should "render base SI unit" in {
    Meter.renderLaTeX shouldBe "\\text{m}"
  }

  it should "render quotient unit" in {
    (Meter / Second).renderLaTeX shouldBe "\\frac{\\text{m}}{\\text{s}}"
  }

  it should "render product unit" in {
    (Kilogram * Meter).renderLaTeX shouldBe "\\text{kg}\\cdot\\text{m}"
  }

  it should "render squared unit" in {
    Meter.squared.renderLaTeX shouldBe "\\text{m}^{2}"
  }

  it should "render cubed unit" in {
    Second.cubed.renderLaTeX shouldBe "\\text{s}^{3}"
  }

  it should "render inverted unit" in {
    Second.invert.renderLaTeX shouldBe "\\text{s}^{-1}"
  }

  it should "render complex nested unit" in {
    val unit = Kilogram * Meter / Second.squared
    unit.renderLaTeX shouldBe "\\frac{\\text{kg}\\cdot\\text{m}}{\\text{s}^{2}}"
  }

  it should "render power of composite unit with parentheses" in {
    val unit = (Meter / Second).squared
    unit.renderLaTeX shouldBe "\\left(\\frac{\\text{m}}{\\text{s}}\\right)^{2}"
  }
}