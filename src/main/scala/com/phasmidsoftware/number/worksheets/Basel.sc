import com.phasmidsoftware.number.core.{AbsoluteFuzz, Box, Number, Rational, Value}

com.phasmidsoftware.number.core.Number

def inverseSquare(x: Int): Rational = Rational.one / (x * x)

val terms: LazyList[Rational] = LazyList.from(1) map inverseSquare

val significantTerms = terms takeWhile (x => x.toDouble > 1E-6) to List

val insignificantTerms = terms map (x => x.toDouble) dropWhile (x => x > 1E-6) takeWhile (x => x > 1E-8) to List

val size = insignificantTerms.size

val pi: Rational = significantTerms.sum * 6

val error: Double = insignificantTerms.sum * 6

val piSquared: Number = Number.create(Value.fromRational(pi), AbsoluteFuzz(error, Box))

val pi = piSquared.sqrt