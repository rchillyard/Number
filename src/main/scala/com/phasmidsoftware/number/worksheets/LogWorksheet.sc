/**
  * This worksheet illustrates the use of Log-factor numbers..
  */

import com.phasmidsoftware.number.core.inner.NatLog
import com.phasmidsoftware.number.core.{Constants, ExactNumber, Number}

// One way to express e
ExactNumber(1, NatLog)
// Another way to express e
Number.e

val iPi = Number.pi.multiply(Constants.i)

Number.e.power(iPi).add(Constants.one)

// Euler's identity (should result in -1):
val minusOne = Number.e.power(iPi).render

// What about finding the log of a number?
// Here he want to know the Nat Log of e (1 obviously)
Number.e.log

// What about the natural log of 2?
Number.two.log






