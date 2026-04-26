/**
  * This worksheet demonstrates the use of Newton's method to solve for the theoretical cutoff value
  * for transitioning between MergeSort and InsertionSort algorithms based on the
  * estimated number of comparisons performed by each sorting algorithm.
  * If we approximate by solving n^2/4 - n lg n = 0, we get n = 16
  * Here we use a slightly more precise formula and should get the value of 15.24.
  */
import com.phasmidsoftware.number.core.misc.Newton
import com.phasmidsoftware.number.core.numerical.Number
import scala.util.Try

/**
  * Computes the base-2 logarithm of a given number.
  *
  * @param x the input number for which the base-2 logarithm is to be calculated.
  *          Must be greater than 0.
  * @return the base-2 logarithm of the input number.
  */
def lg(x: Double): Double = Math.log(x) / Math.log(2)

/**
  * Calculates the number of comparisons performed by the insertion sort algorithm
  * for a given number of elements, based on a mathematical formula.
  *
  * @param x The number of elements (as a Double) for which the number of comparisons
  *          in the insertion sort algorithm is to be calculated.
  *          It represents the size of the input array.
  * @return The estimated number of comparisons (as a Double) performed by the
  *         insertion sort algorithm for random input of size `x`.
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
  x * lg(x) + (lg(x) / 2 + 1) / 2

/**
  * Computes the difference in the number of comparisons performed by the insertion sort
  * and the merge sort algorithms for a given input size `x`.
  *
  * @param x The size of the input array (as a Double) for which the difference in the
  *          number of comparisons between insertion sort and merge sort is to be calculated.
  * @return The difference (as a Double) between the number of comparisons performed by
  *         insertion sort and merge sort for an input of size `x`.
  */
def difference(x: Double): Double =
  insertionSortComparisons(x) - mergeSortComparisons(x)

/**
  * Computes the derivative of the difference function
  * It is represented mathematically as:
  * x / 2 + 0.25 - (lg(x) + (4 * x + 1) / (4 * x * math.log(2)))
  *
  * @param x the input value for which the derivative is to be calculated.
  *          Must be greater than 0 since the `lg` function (logarithm base 2) is undefined for non-positive values.
  * @return the result of the derivative evaluated at the given input value.
  */
def derivative(x: Double): Double = x / 2 + 0.25 - (lg(x) + (4 * x + 1) / (4 * x * math.log(2)))

val newton = Newton(difference, derivative)
val result: Try[Double] = newton.solve(25, 1E-4, 16)