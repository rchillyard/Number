package com.phasmidsoftware.number.top

import com.phasmidsoftware.number.algebra.eager.{ExactNumber, RationalNumber, WholeNumber}
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.dimensions.core.CompositeUnits.{C, Chain}
import com.phasmidsoftware.number.dimensions.core.*
import com.phasmidsoftware.number.top.Quantity
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class QuantityWorksheetSpec extends AnyFlatSpec with should.Matchers {

  behavior of "QuantityWorksheet"
  it should "work" in {

    // We will develop the famous FFF system of units [[https://en.wikipedia.org/wiki/FFF_system]]

    val Fortnight: PhysicalUnit[Time] = Day.scaled(14, "ftn")
    val Furlong: PhysicalUnit[Length] = Chain.scaled(10, "fur")
    val Firkin: PhysicalUnit[Mass] = Pound.scaled(90, "fir")

    import com.phasmidsoftware.number.dimensions.core./

    val FurlongPerFortnight: PhysicalUnit[Velocity] = Furlong / Fortnight

    Fortnight.toSI shouldBe WholeNumber(14 * 24 * 60 * 60)
    Furlong.toSI shouldBe RationalNumber(Rational(25146, 125))
    Firkin.toSI shouldBe RationalNumber(Rational(408233133, 10000000))

    // Supposedly, the VAX has a tick value of a microfortnight.
    // Let's see what that would be in seconds.
    Quantity(Rational(1000000).invert, Fortnight).in(Second) match {
      case Some(q@Quantity[Time] (RationalNumber (x, false), units) ) =>
        x shouldBe Rational (756, 625)
        q.unit.symbol shouldBe "s"
      case _ =>
        fail("No VAX tick value found.")
    }

    // Now, let's calculate the speed of light in FFF units
    Quantity(1, C).in(FurlongPerFortnight) match {
      case Some(q@Quantity[Velocity] (RationalNumber (x, false), units) ) =>
        x shouldBe Rational(2518256647200000L,1397)
        x.toDouble shouldBe 1.8026E12 +- 1E8
        q.unit.symbol shouldBe "fur/ftn"
      case _ =>
        fail("No speed of light found.")
    }
  }
}
