/**
  * This worksheet illustrates the use of Expressions (for lazy evaluation).
  *
  */

import com.phasmidsoftware.number.core.Rational
import com.phasmidsoftware.number.misc.ConFrac

val one = LazyList.continually(1L)
val targetPhi = ConFrac.simple(one)
val epsilon = 1E-6

def imprecise(r: Rational): Boolean = 1.0 / r.d.toDouble / r.d.toDouble / math.sqrt(5) > epsilon

val cf = targetPhi.takeWhile(imprecise)
val phi = cf.toRational.toDouble

val conv = targetPhi.convergents.take(10).toList
conv map (r => r.toDouble)

val targetPi = ConFrac.PiSimple
val rs = targetPi.convergents.take(10).toList.map(r => r.toRationalString)


