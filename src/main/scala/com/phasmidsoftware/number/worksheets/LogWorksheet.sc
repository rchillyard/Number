/**
  * This worksheet illustrates the use of Log-factor numbers..
  */

import com.phasmidsoftware.number.core.{ExactNumber, NatLog, Number}

// One way to express e
ExactNumber(1, NatLog)
// Another way to express e
Number.e

val iPi = Number.pi.multiply(Number.i)

Number.e.power(iPi).add(Number.one)

// Euler's identity:
val minusOne = Number.e.power(iPi).render





