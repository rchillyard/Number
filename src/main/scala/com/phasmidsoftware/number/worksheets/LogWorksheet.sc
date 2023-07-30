/**
  * This worksheet illustrates the use of Log-factor numbers..
  */

import com.phasmidsoftware.number.core.{Constants, ExactNumber, NatLog, Number}

// One way to express e
ExactNumber(1, NatLog)
// Another way to express e
Number.e

val iPi = Number.pi.multiply(Constants.i)

Number.e.power(iPi).add(Constants.one)

// Euler's identity:
val minusOne = Number.e.power(iPi).render





