package com.phasmidsoftware.number.misc

import com.phasmidsoftware.number.core.Rational
import org.scalatest.flatspec
import org.scalatest.matchers.should
import org.scalatest.tagobjects.Slow

class ContinuedFractionFuncSpec extends flatspec.AnyFlatSpec with should.Matchers {

  val goldenRatio: Double = (1 + math.sqrt(5)) / 2

  behavior of "ContinuedFraction.e"

  it should "define ContinuedFraction.FourOverPiLeibniz" taggedAs Slow in {
    val z: ContinuedFraction = ContinuedFraction.FourOverPiLeibniz
    val q: Rational = z.toRational(1000).invert
    q.toDouble shouldBe math.Pi / 4 +- 2.5E-4
  }
}
