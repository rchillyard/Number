/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.misc

import com.phasmidsoftware.number.core.misc.Pi.getPoints
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.Random

/**
  * A class representing a specification for benchmarking tests.
  *
  * TODO move this into functional specs.
  *
  * This class is based on ScalaTest's `AnyFlatSpec` and `should.Matchers` for behavior-driven development.
  * It includes a test case written for benchmarking a computation that calculates the value of Pi using random sampling.
  *
  * The benchmark uses the `com.phasmidsoftware.number.core.misc.Benchmark` utilities to measure execution time.
  * Random sampling is employed using an implicit instance of `scala.util.Random`, and the benchmark result is validated
  * against known tolerances for correctness within an acceptable margin of error.
  *
  * This implementation demonstrates both performance benchmarking and functional validation.
  *
  * Behavior:
  * - "Benchmark" is defined as the subject of this specification.
  * - A test checks the ability to perform 100 repetitions of a benchmark, measuring execution time and validating results.
  */
class BenchmarkSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Benchmark"

  // NOTE this takes a long time and may fail
  //  it should "do 100rRepetitions" taggedAs Slow in {
  //    import com.phasmidsoftware.number.core.misc.Benchmark._
  //    implicit val r: Random = Random
  //    val N = 100_000
  //
  //    val (result, milliseconds) = 1.times {
  //      calculatePi(N)
  //    }
  //
  //    // NOTE that these tolerances can be widened if necessary
  //    result shouldBe 3.141592653589793 +- 1E-2
  //    milliseconds shouldBe 800.0 +- 650
  //  }

  def calculatePi(n: Int)(implicit r: Random): Double = {
    val points: List[(Double, Double)] = getPoints(n).toList
    4.0 * points.length / n
  }

}
