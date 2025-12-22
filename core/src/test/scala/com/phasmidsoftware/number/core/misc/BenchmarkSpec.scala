/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.misc

import com.phasmidsoftware.number.core.misc.Pi.getPoints
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.Random

class BenchmarkSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Benchmark"

  // NOTE this is quite likely to fail, especially in CircleCI
  it should "do 100rRepetitions" in {
    import com.phasmidsoftware.number.core.misc.Benchmark._
    implicit val r: Random = Random
    val N = 1_000_000

    val (result, milliseconds) = 1.times {
      calculatePi(N)
    }

    // NOTE that these tolerances can be widened if necessary
    result shouldBe 3.141592653589793 +- 1E-2
    milliseconds shouldBe 800.0 +- 650
  }

  def calculatePi(n: Int)(implicit r: Random): Double = {
    val points: List[(Double, Double)] = getPoints(n).toList
    4.0 * points.length / n
  }

}
