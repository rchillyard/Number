package com.phasmidsoftware.number.dimensions.core

import com.phasmidsoftware.number.core.inner.Rational
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DimensionWitnessSpec extends AnyFlatSpec with Matchers {

  behavior of "BaseDimWitness"

  it should "create dimensionless witness" in {
    val witness = DimensionWitness.dimensionless
    witness.m shouldBe Rational.zero
    witness.l shouldBe Rational.zero
    witness.t shouldBe Rational.zero
    witness.i shouldBe Rational.zero
    witness.θ shouldBe Rational.zero
    witness.n shouldBe Rational.zero
    witness.j shouldBe Rational.zero
  }

  it should "create base unit witnesses correctly" in {
    DimensionWitness.mass.m shouldBe Rational.one
    DimensionWitness.length.l shouldBe Rational.one
    DimensionWitness.time.t shouldBe Rational.one
    DimensionWitness.current.i shouldBe Rational.one
    DimensionWitness.temperature.θ shouldBe Rational.one
    DimensionWitness.amount.n shouldBe Rational.one
    DimensionWitness.luminosity.j shouldBe Rational.one
  }

  behavior of "multiplication (*)"

  it should "multiply two base dimensions" in {
    val result = DimensionWitness.length * DimensionWitness.time
    result.l shouldBe Rational.one
    result.t shouldBe Rational.one
    result.m shouldBe Rational.zero
  }

  it should "produce area from length * length" in {
    val result = DimensionWitness.length * DimensionWitness.length
    result shouldBe DimensionWitness.area
    result.l shouldBe Rational(2)
  }

  it should "produce volume from area * length" in {
    val result = DimensionWitness.area * DimensionWitness.length
    result shouldBe DimensionWitness.volume
    result.l shouldBe Rational(3)
  }

  it should "produce velocity from length * time^-1" in {
    val timeInverse = DimensionWitness.time.pow(Rational(-1))
    val result = DimensionWitness.length * timeInverse
    result shouldBe DimensionWitness.velocity
  }

  it should "produce force from mass * acceleration" in {
    val result = DimensionWitness.mass * DimensionWitness.acceleration
    result shouldBe DimensionWitness.force
    result.m shouldBe Rational.one
    result.l shouldBe Rational.one
    result.t shouldBe Rational(-2)
  }

  it should "produce energy from force * length" in {
    val result = DimensionWitness.force * DimensionWitness.length
    result shouldBe DimensionWitness.energy
  }

  it should "be associative" in {
    val a = DimensionWitness.length
    val b = DimensionWitness.mass
    val c = DimensionWitness.time
    (a * b) * c shouldBe a * (b * c)
  }

  it should "have dimensionless as identity" in {
    val witness = DimensionWitness.force
    witness * DimensionWitness.dimensionless shouldBe witness
    DimensionWitness.dimensionless * witness shouldBe witness
  }

  behavior of "division (/)"

  it should "divide two base dimensions" in {
    val result = DimensionWitness.length / DimensionWitness.time
    result shouldBe DimensionWitness.velocity
    result.l shouldBe Rational.one
    result.t shouldBe Rational(-1)
  }

  it should "produce acceleration from velocity / time" in {
    val result = DimensionWitness.velocity / DimensionWitness.time
    result shouldBe DimensionWitness.acceleration
  }

  it should "produce pressure from force / area" in {
    val result = DimensionWitness.force / DimensionWitness.area
    result shouldBe DimensionWitness.pressure
  }

  it should "produce power from energy / time" in {
    val result = DimensionWitness.energy / DimensionWitness.time
    result shouldBe DimensionWitness.power
  }

  it should "produce voltage from power / current" in {
    val result = DimensionWitness.power / DimensionWitness.current
    result shouldBe DimensionWitness.voltage
  }

  it should "produce resistance from voltage / current" in {
    val result = DimensionWitness.voltage / DimensionWitness.current
    result shouldBe DimensionWitness.resistance
  }

  it should "produce dimensionless when dividing equal dimensions" in {
    val result = DimensionWitness.length / DimensionWitness.length
    result shouldBe DimensionWitness.dimensionless
  }

  it should "be inverse of multiplication" in {
    val a = DimensionWitness.force
    val b = DimensionWitness.length
    (a * b) / b shouldBe a
  }

  behavior of "pow"

  it should "raise dimension to integer power" in {
    val result = DimensionWitness.length.pow(Rational(2))
    result shouldBe DimensionWitness.area
  }

  it should "raise dimension to power 3" in {
    val result = DimensionWitness.length.pow(Rational(3))
    result shouldBe DimensionWitness.volume
  }

  it should "raise dimension to negative power" in {
    val result = DimensionWitness.time.pow(Rational(-1))
    result shouldBe DimensionWitness.frequency
  }

  it should "raise dimension to fractional power" in {
    val result = DimensionWitness.length.pow(Rational(1, 2))
    result.l shouldBe Rational(1, 2)
    result.m shouldBe Rational.zero
  }

  it should "produce dimensionless when raising to power 0" in {
    val result = DimensionWitness.force.pow(Rational.zero)
    result shouldBe DimensionWitness.dimensionless
  }

  it should "satisfy (d^a)^b = d^(a*b)" in {
    val d = DimensionWitness.length
    val a = Rational(2)
    val b = Rational(3)
    d.pow(a).pow(b) shouldBe d.pow(a * b)
  }

  behavior of "toCompositeSymbol"

  it should "format dimensionless as '1'" in {
    DimensionWitness.dimensionless.toCompositeSymbol shouldBe "1"
  }

  it should "format base units correctly" in {
    DimensionWitness.mass.toCompositeSymbol shouldBe "kg"
    DimensionWitness.length.toCompositeSymbol shouldBe "m"
    DimensionWitness.time.toCompositeSymbol shouldBe "s"
    DimensionWitness.current.toCompositeSymbol shouldBe "A"
    DimensionWitness.temperature.toCompositeSymbol shouldBe "K"
    DimensionWitness.amount.toCompositeSymbol shouldBe "mol"
    DimensionWitness.luminosity.toCompositeSymbol shouldBe "cd"
  }

  it should "format area with superscript" in {
    DimensionWitness.area.toCompositeSymbol shouldBe "m²"
  }

  it should "format volume with superscript" in {
    DimensionWitness.volume.toCompositeSymbol shouldBe "m³"
  }

  it should "format velocity correctly" in {
    DimensionWitness.velocity.toCompositeSymbol shouldBe "m/s"
  }

  it should "format acceleration correctly" in {
    DimensionWitness.acceleration.toCompositeSymbol shouldBe "m/s²"
  }

  it should "format force correctly" in {
    DimensionWitness.force.toCompositeSymbol shouldBe "kg·m/s²"
  }

  it should "format energy correctly" in {
    DimensionWitness.energy.toCompositeSymbol shouldBe "kg·m²/s²"
  }

  it should "format power correctly" in {
    DimensionWitness.power.toCompositeSymbol shouldBe "kg·m²/s³"
  }

  it should "format pressure correctly" in {
    DimensionWitness.pressure.toCompositeSymbol shouldBe "kg/m·s²"
  }

  it should "format charge correctly" in {
    DimensionWitness.charge.toCompositeSymbol shouldBe "s·A"
  }

  it should "format voltage correctly" in {
    DimensionWitness.voltage.toCompositeSymbol shouldBe "kg·m²/s³·A"
  }

  it should "format resistance correctly" in {
    DimensionWitness.resistance.toCompositeSymbol shouldBe "kg·m²/s³·A²"
  }

  it should "format frequency correctly" in {
    DimensionWitness.frequency.toCompositeSymbol shouldBe "1/s"
  }

  it should "format fractional exponents" in {
    val sqrtLength = DimensionWitness.length.pow(Rational(1, 2))
    sqrtLength.toCompositeSymbol shouldBe "m^(1/2)"
  }

  it should "format large integer exponents" in {
    val lengthToFourth = DimensionWitness.length.pow(Rational(4))
    lengthToFourth.toCompositeSymbol shouldBe "m^4"
  }

  it should "format complex derived units" in {
    // Create something like kg^2·m^3/(s^4·A)
    val complex = BaseDimWitness(
      m = Rational(2),
      l = Rational(3),
      t = Rational(-4),
      i = Rational(-1),
      θ = Rational.zero,
      n = Rational.zero,
      j = Rational.zero
    )
    complex.toCompositeSymbol shouldBe "kg²·m³/s^4·A"
  }

  behavior of "equality"

  it should "be equal when all exponents match" in {
    val w1 = BaseDimWitness(1, 2, -3, 0, 0, 0, 0)
    val w2 = BaseDimWitness(1, 2, -3, 0, 0, 0, 0)
    w1 shouldBe w2
  }

  it should "not be equal when exponents differ" in {
    val w1 = DimensionWitness.force
    val w2 = DimensionWitness.energy
    w1 should not be w2
  }

  behavior of "derived dimension constants"

  it should "have correct derived dimensions defined" in {
    // Test a few key derived dimensions
    DimensionWitness.area shouldBe DimensionWitness.length.pow(Rational(2))
    DimensionWitness.volume shouldBe DimensionWitness.length.pow(Rational(3))
    DimensionWitness.velocity shouldBe DimensionWitness.length / DimensionWitness.time
    DimensionWitness.acceleration shouldBe DimensionWitness.velocity / DimensionWitness.time
    DimensionWitness.force shouldBe DimensionWitness.mass * DimensionWitness.acceleration
    DimensionWitness.energy shouldBe DimensionWitness.force * DimensionWitness.length
    DimensionWitness.power shouldBe DimensionWitness.energy / DimensionWitness.time
    DimensionWitness.pressure shouldBe DimensionWitness.force / DimensionWitness.area
  }
}