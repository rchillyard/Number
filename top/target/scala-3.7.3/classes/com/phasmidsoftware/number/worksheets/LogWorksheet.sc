/**
  * This worksheet illustrates the use of core Log-factor numbers..
  */

import com.phasmidsoftware.number.core.inner.{Euler, NatLog}
import com.phasmidsoftware.number.core.numerical.{Constants, ExactNumber, Number}

// One way to express e
ExactNumber(1, NatLog)
// Another way to express e
Number.e

val iPi = Number.pi.multiply(Constants.i)

Number.e.power(iPi).add(Constants.one)

// Euler's identity (should result in -1):
val minusOne = Number.e.power(iPi).render

// We can also get -1 the following way:
val EulersIdentity = ExactNumber(1, Euler)
EulersIdentity.simplify

// What about finding the log of a number?
// Here he want to know the natural log of `e` (1 obviously)
Number.e.ln

// What about the natural log of 2?
Number.two.ln






