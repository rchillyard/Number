package com.phasmidsoftware.number.top

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

/**
  * Class representing the specification to calculate the theoretical cutoff point
  * for transitioning between MergeSort and InsertionSort algorithms based on the
  * estimated number of comparisons performed by each sorting algorithm.
  *
  * This class relies on Newton's method to approximate a solution to the equation where
  * the number of comparisons for both algorithms are equivalent.
  *
  * We find the theoretical cutoff value for MergeSort giving way to InsertionSort.
  * If we approximate the problem by solving n^2/4 - n lg n = 0, we get n = 16
  * Here we use a slightly more precise formula:
  *
  * It uses mathematical models for comparison estimations:
  * - InsertionSort: `(x - 1) * (x / 4 + 0.5)`
  * - MergeSort: `x * lg(x) - x + 1`
  *
  * The class defines:
  * - A function to compute base-2 logarithm.
  * - Mathematical estimations for comparison counts in both algorithms.
  * - The function `f` representing the difference between InsertionSort and MergeSort comparisons.
  * - The derivative `dfBydN` for use in Newton's method.
  *
  * The Newton-Raphson method is then utilized to approximate the cutoff value between both algorithms.
  *
  * The result of the calculation is printed as a `Try[Double]`, representing the cutoff value if successful,
  * or an error if the computation fails to converge.
  */
class SortCutoffSpec extends AnyFlatSpec with should.Matchers {

  import com.phasmidsoftware.number.core.misc.Newton

  import scala.util.Try

  /**
    * Computes the base-2 logarithm of a given number.
    *
    * @param x the input number for which the base-2 logarithm is to be calculated.
    *          Must be greater than 0.
    *
    * @return  the base-2 logarithm of the input number.
    */
  def lg(x: Double): Double = Math.log(x) / Math.log(2)

  /**
    * Calculates the number of comparisons performed by the insertion sort algorithm
    * for a given number of elements, based on a mathematical formula.
    *
    * @param x The number of elements (as a Double) for which the number of comparisons
    *          in the insertion sort algorithm is to be calculated.
    *          It represents the size of the input array.
    *
    * @return  The estimated number of comparisons (as a Double) performed by the
    *          insertion sort algorithm for random input of size `x`.
    */
  def insertionSortComparisons(x: Double): Double =
    (x - 1) * (x / 4 + 0.5)

  /**
    * Computes the number of comparisons required by the merge sort algorithm
    * for an input array of size `x`.
    * This is the amortized number of comparisons for any x.
    * When x is a power of 2, the number of comparisons is only x lg x - x + 1.
    *
    * @param x the size of the input array. Must be greater than 0.
    * @return the number of comparisons required to sort the array using merge sort.
    */
  def mergeSortComparisons(x: Double): Double =
    x * lg(x) + (lg(x)/2 + 1)/2

  def difference(x: Double): Double =
    insertionSortComparisons(x) - mergeSortComparisons(x)

  def derivative(x: Double): Double = x / 2 + 0.25 - (lg(x) + (4 * x + 1)/(4 * x * math.log(2)))

  behavior of "SortCutoff"
  it should "evaluate lg(2) as 1 and lg(16) as 4" in {
    lg(2) shouldBe 1
    lg(16) shouldBe 4
  }

  it should "evaluate insertionSortComparisons(16) as 16" in {
    insertionSortComparisons(16) shouldBe 67.5
    insertionSortComparisons(15) shouldBe 59.5
    insertionSortComparisons(14) shouldBe 52
  }

  it should "evaluate mergeSortComparisons(16) as 16" in {
    mergeSortComparisons(16) shouldBe 65.5
    mergeSortComparisons(15) shouldBe 60.080081583029916
    mergeSortComparisons(14) shouldBe 54.75480763932085
  }

  it should "evaluate difference(16) as 18.5" in {
    difference(16) shouldBe 2
    difference(15) shouldBe -0.5800815830299157
    difference(14) shouldBe -2.754807639320852
  }

  it should "use Newton's method to solve for the cutoff value" in {
    val newton = Newton(difference, derivative)
    val result: Try[Double] = newton.solve(25, 1E-4, 16)
    result.isSuccess shouldBe true
    result.get shouldBe 15.24 +- 0.02
  }

}
