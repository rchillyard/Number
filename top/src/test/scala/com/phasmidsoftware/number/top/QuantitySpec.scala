package com.phasmidsoftware.number.top

import com.phasmidsoftware.number.algebra.core.Valuable
import com.phasmidsoftware.number.algebra.eager.*
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.dimensions.core.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class QuantitySpec extends AnyFlatSpec with Matchers {

  behavior of "Quantity"

  it should "render correctly with space separator" in {
    val distance = Quantity(5, Meter)
    distance.render shouldBe "5 m"
  }

  it should "render with compound units" in {
    val velocity = Quantity(10, Meter / Second)
    velocity.render shouldBe "10 m/s"
  }

  it should "convert between units of the same dimension" in {
    val distance = Quantity(1000, Meter)
    val inKm = distance.in(Kilometer)

    inKm shouldBe defined
    inKm.get.value shouldBe WholeNumber(1)
    inKm.get.unit shouldBe Kilometer
  }

  it should "convert meters to centimeters" in {
    val distance = Quantity(5, Meter)
    val inCm = distance.in(Centimeter)

    inCm shouldBe defined
    inCm.get.value shouldBe WholeNumber(500)
    inCm.get.unit shouldBe Centimeter
  }

  it should "multiply quantities correctly" in {
    val length = Quantity(5, Meter)
    val width = Quantity(3, Meter)
    val q = length * width
    q.value.materialize shouldBe WholeNumber(15)
    q.unit match {
      case p: ProductUnit[?] =>
        p.left.symbol shouldBe "m"
        p.right.symbol shouldBe "m"
    }
  }

  it should "divide quantities correctly" in {
    val distance = Quantity(100, Meter)
    val time = Quantity(10, Second)
    val velocity = distance / time
    velocity.value.materialize shouldBe WholeNumber(10)
    velocity.unit match {
      case q: QuotientUnit[?] =>
        q.numerator.symbol shouldBe "m"
        q.denominator.symbol shouldBe "s"
    }
  }

  it should "add quantities 1" in {
    val a = Quantity(30, Meter)
    val b = Quantity(100, Meter)
    val maybeSum = a + b
    maybeSum.isDefined shouldBe true
    maybeSum.get shouldBe Quantity(130, Meter)
  }

  it should "add quantities 2" in {
    val a = Quantity(30, Meter)
    val b = Quantity(100, Foot)
    val maybeSum = a + b
    maybeSum.isDefined shouldBe true
    maybeSum.get shouldBe Quantity(RationalNumber(1512, 25), Meter)
  }

  it should "add quantities 3" in {
    val a = Quantity(30, Meter)
    val b = Quantity(100, Foot)
    val maybeSum = b + a
    maybeSum.isDefined shouldBe true
    maybeSum.get shouldBe Quantity(RationalNumber(25200, 127), Foot)
  }

  it should "compute force from mass and acceleration" in {
    val mass = Quantity(2, Kilogram)
    val acceleration = Quantity(10, Meter / Second.squared)
    val force = mass * acceleration
    force.value.materialize shouldBe WholeNumber(20)
    // Force has dimension [M¹L¹T⁻²]
  }

  it should "handle rational numbers" in {
    val distance = Quantity(Rational(5, 2), Meter)
    distance.render shouldBe "2.5 m"
  }

  it should "convert with rational scale factors" in {
    val distance = Quantity(1, Foot)
    val inMeters = distance.in(Meter)

    inMeters shouldBe defined
    inMeters.get.unit shouldBe Meter
    // 1 foot = 0.3048 meters = 3048/10000
  }

  it should "multiply quantities with different units" in {
    val force = Quantity(10, Newton)
    val distance = Quantity(5, Meter)
    val energy = force * distance

    energy.value.materialize shouldBe WholeNumber(50)
    // Energy should have dimension [M¹L²T⁻²]
  }

  it should "handle division resulting in dimensionless quantities" in {
    val length1 = Quantity(10, Meter)
    val length2 = Quantity(5, Meter)
    val ratio = length1 / length2
    ratio.value.materialize shouldBe WholeNumber(2)
    // Result should be dimensionless [L¹/L¹ = L⁰]
  }

  it should "chain operations" in {
    val mass = Quantity(5, Kilogram)
    val velocity = Quantity(10, Meter / Second)
    val time = Quantity(2, Second)

    // momentum = mass * velocity
    val momentum = mass * velocity
    // force = momentum / time = mass * velocity / time = mass * acceleration
    val force = momentum / time
    force.value.materialize shouldBe WholeNumber(25)
  }

  it should "handle squared units" in {
    val length = Quantity(4, Meter)
    val area = Quantity(16, Meter.squared)

    area.render shouldBe "16 m²"
  }

  it should "convert between imperial and metric" in {
    val distanceMiles = Quantity(1, Mile)
    val distanceKm = distanceMiles.in(Kilometer)

    distanceKm shouldBe defined
    // 1 mile ≈ 1.609344 km
    distanceKm.get.unit shouldBe Kilometer
  }

  it should "handle complex unit conversions" in {
    val speed = Quantity(100, KilometerPerHour)
    val speedMps = speed.in(Meter / Second)

    speedMps shouldBe defined
    speedMps.get.unit match {
      case q: QuotientUnit[?] =>
        q.numerator.symbol shouldBe "m"
        q.denominator.symbol shouldBe "s"
    }
  }
}