/**
  * This worksheet illustrates the use of Expressions (for lazy evaluation).
  *
  */

import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.misc.ConFrac

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

val ramanujan2 = ConFrac.simple(LazyList.from(1) map (_.toLong) take 50)
println(ramanujan2.convergents.take(20).toList.map(_.toRationalString))
println(ramanujan2.toRational)

//1.433127426722311758317183455775992
