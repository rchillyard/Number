/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.numerical

import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.numerical.Number.NumberOps
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class InfiniteSeriesSpec extends AnyFlatSpec with should.Matchers {

  behavior of "InfiniteSeries"
  val basel: InfiniteSeries[Number] = InfiniteSeries(LazyList.from(1).map(x => Rational(x).invert.square), 0.001)
  it should "terms" in {

  }
  it should "convergenceRate" in {
    basel.convergenceRate shouldBe 0.001
  }
  it should "nTerms" in {

  }
  it should "evaluateToTolerance 0.001" in {
    val xy = basel.evaluateToTolerance(0.001)
    xy.isSuccess shouldBe true
    val pi = (6 * xy.get).sqrt
    pi.render shouldBe "3.1111323022281687±31.0%"
    pi.isSame(Number.pi) shouldBe true
  }
  it should "evaluateToTolerance 0.00001" in {
    val xy = basel.evaluateToTolerance(0.00001)
    xy.isSuccess shouldBe true
    val pi = (6 * xy.get).sqrt
    pi.render shouldBe "3.1385740505663335±0.30%"
    pi.isSame(Number.pi) shouldBe true
  }
  it should "evaluateToTolerance 0.000001" in {
    val xy = basel.evaluateToTolerance(0.000001)
    xy.isSuccess shouldBe true
    val pi = (6 * xy.get).sqrt
    pi.render shouldBe "3.140637100985938±0.030%"
    pi.fuzzyCompare(Number.pi, 0.25) shouldBe 0
  }

}
