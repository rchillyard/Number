/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.inner.{InfiniteSeries, Rational}

class InfiniteSeriesFuncSpec extends AnyFlatSpec with should.Matchers {

  behavior of "InfiniteSeries"

  val basel: InfiniteSeries[Number] = InfiniteSeries(LazyList.from(1).map(x => Rational(x).invert.square), 0.001)
  it should "evaluateToTolerance 0.0000001" in {
    val xy = basel.evaluateToTolerance(0.0000001)
    xy.isSuccess shouldBe true
    val pi = (6 * xy.get).sqrt
    pi.render shouldBe "3.141290685037535±0.0030%"
    pi.fuzzyCompare(Number.pi, 0.01) shouldBe 0
  }

}
