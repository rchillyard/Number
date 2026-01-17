package com.phasmidsoftware.number.core.misc

import com.phasmidsoftware.number.core.inner.{Rational, Value}
import com.phasmidsoftware.number.core.numerical.{AbsoluteFuzz, Box, Number}
import org.scalatest.matchers.should

class BaselSpec extends org.scalatest.flatspec.AnyFlatSpec with should.Matchers {

  def inverseSquare(x: Int): Rational = Rational.one / (x * x)

  val terms: LazyList[Rational] = LazyList.from(1) map inverseSquare

  private def calculatePiByBaselMethod(tolerance: Double): Number = {
    val significantTerms = terms takeWhile (x => x.toDouble > tolerance) to List

    val insignificantTerms = terms map (x => x.toDouble) dropWhile (x => x > tolerance) takeWhile (x => x > tolerance / 10000) to List

    val pi: Rational = significantTerms.sum * 6

    val error: Double = insignificantTerms.sum * 6

    val piSquared: Number = Number.create(Value.fromRational(pi), AbsoluteFuzz(error, Box))

    piSquared.sqrt
  }

  behavior of "Basel"

  it should "work" in {

    // The first error bound seems a little off... pi is between 3.08 and 3.14
    // NOTE, actually we are using Box for the error which isn't accurate since all terms are positive.
    calculatePiByBaselMethod(1E-3).toNominalDouble.get shouldBe math.Pi +- 0.1

    // This time, we get pi between 3.13969 and 3.14157 which is, again, a little off.
    calculatePiByBaselMethod(1E-6).toNominalDouble.get shouldBe math.Pi +- 1E-3
  }
}
