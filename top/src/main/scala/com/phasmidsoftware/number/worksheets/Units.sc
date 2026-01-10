// This is the Units worksheet

import com.phasmidsoftware.number.dimensions.core.CompositeUnits.{C, Chain, GallonImp}
import com.phasmidsoftware.number.dimensions.core.{Day, *, given}
import com.phasmidsoftware.number.top.Quantity

// We will develop the famous FFF system of units [[https://en.wikipedia.org/wiki/FFF_system]]

val Fortnight: Unit[Time] = Day.scaled(14, "ftn")
val Furlong: Unit[Length] = Chain.scaled(10, "fur")
val Firkin: Unit[Mass] = Pound.scaled(90, "fir")
val FurlongPerFortnight: Unit[DivDim[Length, Time]] = Furlong / Fortnight

// Calculate the speed of light in FFF units...

val c: Quantity[Velocity] = Quantity(1, C)
val maybeLightSpeed: Option[Quantity[Velocity]] = c.in(FurlongPerFortnight)
maybeLightSpeed match {
  case Some(q) => println(q.renderLaTeX)
  case None => println("No speed of light found.")
}
