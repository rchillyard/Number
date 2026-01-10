// This is the Quantity worksheet

import com.phasmidsoftware.number.algebra.eager.RationalNumber
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.dimensions.core.CompositeUnits.{C, Chain, GallonImp}
import com.phasmidsoftware.number.dimensions.core.{Day, *, given}
import com.phasmidsoftware.number.top.Quantity

// We will develop the famous FFF system of units [[https://en.wikipedia.org/wiki/FFF_system]]

val Fortnight: Unit[Time] = Day.scaled(14, "ftn")
val Furlong: Unit[Length] = Chain.scaled(10, "fur")
val Firkin: Unit[Mass] = Pound.scaled(90, "fir")
val FurlongPerFortnight: Unit[Velocity] = Furlong / Fortnight

// Supposedly, the VAX has a tick value of a microfortnight.
// Let's see what that would be in seconds.
Quantity(Rational(1000000).invert, Fortnight).in(Second) match {
  case Some(q@Quantity[Time] (RationalNumber (x, false), units) ) =>
    println (q.renderLaTeX) // should be "1.2096 s" according to Wikipedia
  case _ =>
    println("No VAX tick value found.")
}

// Now, let's calculate the speed of light in FFF units
Quantity(1, C).in(FurlongPerFortnight) match {
  case Some(q@Quantity[Velocity] (RationalNumber (x, false), units) ) =>
    println (q.renderLaTeX) // the value cannot be rendered exactly using decimal notation.
    println (x.toDouble) // should be 1.8026E12 according to Wikipedia
  case _ =>
    println("No speed of light found.")
}
