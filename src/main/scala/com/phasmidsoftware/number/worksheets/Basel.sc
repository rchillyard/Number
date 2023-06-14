/**
  * This worksheet illustrates the solution to the Basel problem -- approximation of pi.
  */

import com.phasmidsoftware.number.core.{AbsoluteFuzz, Box, Number, Rational, Value}

def inverseSquare(x: Int): Rational = Rational.one / (x * x)

val terms: LazyList[Rational] = LazyList.from(1) map inverseSquare

def calculatePiByBaselMethod(tolerance: Double) = {
    val significantTerms = terms takeWhile (x => x.toDouble > tolerance) to List

    val insignificantTerms = terms map (x => x.toDouble) dropWhile (x => x > tolerance) takeWhile (x => x > tolerance/10000) to List

    val pi: Rational = significantTerms.sum * 6

    val error: Double = insignificantTerms.sum * 6

    val piSquared: Number = Number.create(Value.fromRational(pi), AbsoluteFuzz(error, Box))

    piSquared.sqrt
}
// The first error bound seems a little off... pi is between 3.08 and 3.14
// NOTE, actually we are using Box for the error which isn't accurate since all terms are positive.
calculatePiByBaselMethod(1E-3)

// This time, we get pi between 3.13969 and 3.14157 which is, again, a little off.
calculatePiByBaselMethod(1E-6)
