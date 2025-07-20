/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.language.implicitConversions
import scala.math.Numeric.DoubleIsFractional
import scala.util.Try

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

  behavior of "TaylorSeries"
  it should "approximate sine about x = 0" in {
    val point = Number.zeroR
    val taylorSeries: TaylorSeries = TaylorSeries.createSine(point)
    val coefficients: Seq[Number] = taylorSeries.coefficients.take(5)
    val piOver100: Number = Number.pi.divide(Real(100)).asReal.get.x
    val sinePiOver100: Series[Number] = taylorSeries.apply(piOver100)
    println(sinePiOver100.render(10))
    val result: Try[Number] = sinePiOver100.evaluateToTolerance(1E-8)
    result.isSuccess shouldBe true
    val difference = result.get.doSubtract(piOver100)
    difference.isProbablyZero(0.25) shouldBe true
  }
}
