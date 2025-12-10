/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.misc

import com.phasmidsoftware.number.core.misc.Variance
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VarianceSpec extends AnyFlatSpec with Matchers {

  behavior of "sumOfSquares"
  it should "work (pair)" in {
    Variance.sumOfSquares(3, 4) shouldBe 25.0
    Variance.sumOfSquares(5, 12) shouldBe 169.0
  }
  it should "work for first (5) integers" in {
    val seq: Seq[Double] = Seq(1, 2, 3, 4, 5)
    Variance.sumOfSquares(seq) shouldBe 55.0
  }
  it should "sum (n) squares according to formula" in {
    def sumOfConsecutiveSquares(n: Int): Double = n * n * n / 3.0 + n * n / 2.0 + n / 6.0

    def consecutiveSquares(n: Int): Seq[Double] = (1 to n).map(x => x)

    Variance.sumOfSquares(consecutiveSquares(1)) shouldBe sumOfConsecutiveSquares(1) +- 1E-6
    Variance.sumOfSquares(consecutiveSquares(2)) shouldBe sumOfConsecutiveSquares(2) +- 1E-6
    Variance.sumOfSquares(consecutiveSquares(3)) shouldBe sumOfConsecutiveSquares(3) +- 1E-6
    Variance.sumOfSquares(consecutiveSquares(4)) shouldBe sumOfConsecutiveSquares(4) +- 1E-6
    Variance.sumOfSquares(consecutiveSquares(5)) shouldBe sumOfConsecutiveSquares(5) +- 1E-6
    Variance.sumOfSquares(consecutiveSquares(6)) shouldBe sumOfConsecutiveSquares(6) +- 1E-6
    Variance.sumOfSquares(consecutiveSquares(7)) shouldBe sumOfConsecutiveSquares(7) +- 1E-6
    Variance.sumOfSquares(consecutiveSquares(8)) shouldBe sumOfConsecutiveSquares(8) +- 1E-6
  }

  behavior of "rootSumSquares"
  it should "work (pair)" in {
    Variance.rootSumSquares(3, 4) shouldBe 5
    Variance.rootSumSquares(5, 12) shouldBe 13
  }
  it should "work for first (5) integers" in {
    val seq: Seq[Double] = Seq(1, 2, 3, 4, 5)
    Variance.rootSumSquares(seq) shouldBe math.sqrt(55.0)
  }
  it should "sum (n) squares according to formula" in {
    def rootSumOfConsecutiveSquares(n: Int): Double = math.sqrt(n * n * n / 3.0 + n * n / 2.0 + n / 6.0)

    def consecutiveSquares(n: Int): Seq[Double] = (1 to n).map(x => x)

    Variance.rootSumSquares(consecutiveSquares(1)) shouldBe rootSumOfConsecutiveSquares(1) +- 1E-6
    Variance.rootSumSquares(consecutiveSquares(2)) shouldBe rootSumOfConsecutiveSquares(2) +- 1E-6
    Variance.rootSumSquares(consecutiveSquares(3)) shouldBe rootSumOfConsecutiveSquares(3) +- 1E-6
    Variance.rootSumSquares(consecutiveSquares(4)) shouldBe rootSumOfConsecutiveSquares(4) +- 1E-6
    Variance.rootSumSquares(consecutiveSquares(5)) shouldBe rootSumOfConsecutiveSquares(5) +- 1E-6
    Variance.rootSumSquares(consecutiveSquares(6)) shouldBe rootSumOfConsecutiveSquares(6) +- 1E-6
    Variance.rootSumSquares(consecutiveSquares(7)) shouldBe rootSumOfConsecutiveSquares(7) +- 1E-6
    Variance.rootSumSquares(consecutiveSquares(8)) shouldBe rootSumOfConsecutiveSquares(8) +- 1E-6
  }

  behavior of "rootMeanSquare"
  it should "work (pair)" in {
    Variance.rootMeanSquare(3, 4) shouldBe math.sqrt(5 * 5 / 2.0)
    Variance.rootMeanSquare(5, 12) shouldBe math.sqrt(13 * 13 / 2.0)
  }
  it should "work for first (5) integers" in {
    val seq: Seq[Double] = Seq(1, 2, 3, 4, 5)
    Variance.rootMeanSquare(seq) shouldBe math.sqrt(55.0 / 5.0)
  }
  it should "sum (n) squares according to formula" in {
    def rootMeanOfConsecutiveSquares(n: Int): Double = math.sqrt(n * n / 3.0 + n / 2.0 + 1 / 6.0)

    def consecutiveSquares(n: Int): Seq[Double] = (1 to n).map(x => x)

    Variance.rootMeanSquare(consecutiveSquares(1)) shouldBe rootMeanOfConsecutiveSquares(1) +- 1E-6
    Variance.rootMeanSquare(consecutiveSquares(2)) shouldBe rootMeanOfConsecutiveSquares(2) +- 1E-6
    Variance.rootMeanSquare(consecutiveSquares(3)) shouldBe rootMeanOfConsecutiveSquares(3) +- 1E-6
    Variance.rootMeanSquare(consecutiveSquares(4)) shouldBe rootMeanOfConsecutiveSquares(4) +- 1E-6
    Variance.rootMeanSquare(consecutiveSquares(5)) shouldBe rootMeanOfConsecutiveSquares(5) +- 1E-6
    Variance.rootMeanSquare(consecutiveSquares(6)) shouldBe rootMeanOfConsecutiveSquares(6) +- 1E-6
    Variance.rootMeanSquare(consecutiveSquares(7)) shouldBe rootMeanOfConsecutiveSquares(7) +- 1E-6
    Variance.rootMeanSquare(consecutiveSquares(8)) shouldBe rootMeanOfConsecutiveSquares(8) +- 1E-6
  }
}
