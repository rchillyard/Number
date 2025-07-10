/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.inner.Series
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.language.implicitConversions
import scala.math.Numeric.DoubleIsFractional

class PowerSeriesSpec extends AnyFlatSpec with should.Matchers {

  behavior of "PowerSeries"


  it should "apply" in {
    val coefficients: Seq[FuzzyDouble] = Seq(1, 2, 3, 4, 5)
    val powerSeries: FinitePowerSeries[Double, FuzzyDouble] = FinitePowerSeries[Double, FuzzyDouble](coefficients)(FuzzyDouble.apply)
    val series: Series[FuzzyDouble] = powerSeries(2)
    val value: Option[FuzzyDouble] = series.evaluate(Some(2))
    value.isDefined shouldBe true
    val result: FuzzyDouble = value.get
    result should matchPattern { case FuzzyDouble(5, _) => }
  }
}
