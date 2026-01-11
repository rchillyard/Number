package com.phasmidsoftware.number.top

import com.phasmidsoftware.number.dimensions.core.MetersPerSecond
import com.phasmidsoftware.number.parse.getOrThrow

object Constants {

  val c = Quantity(299792458, MetersPerSecond)

  val Gmaybe = Quantity.parse("6.6742(10)E-11", "m³/(kg·s²)")

  lazy val G = getOrThrow(Gmaybe)
}
