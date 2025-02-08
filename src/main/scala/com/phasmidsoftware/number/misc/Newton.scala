package com.phasmidsoftware.number.misc

import scala.annotation.tailrec
import scala.util._

/**
 * A case class for solving equations using Newton's approximation method (Newton-Raphson).
 *
 * @constructor Creates a new Newton solver with the given function and its derivative.
 * @param f      The function whose root is to be found.
 * @param dfbydx The derivative of the function.
 */
case class Newton(f: Double => Double, dfbydx: Double => Double) {

  private def step(x: Double, y: Double) = x - y / dfbydx(x)

  /**
   * Solves for the root of a given function using an iterative approach.
   * This method applies the Newton-Raphson technique to approximate the solution, stopping
   * when the function value is below the specified threshold or after the maximum number
   * of attempts have been exhausted.
   *
   * @param tries     The maximum number of iterations allowed for convergence.
   * @param threshold The acceptable tolerance for the solution, where the function result is considered sufficiently close to zero.
   * @param initial   The initial guess for the root of the function.
   * @return A `Try` containing the approximated root as a `Success` if the method converges,
   *         or a `Failure` with an exception if the solution fails to converge within the given number of tries.
   */
  def solve(tries: Int, threshold: Double, initial: Double): Try[Double] = {
    @tailrec def inner(r: Double, n: Int): Try[Double] = {
      val y = f(r)
      if (math.abs(y) < threshold) Success(r)
      else if (n == 0) Failure(new Exception("failed to converge"))
      else inner(step(r, y), n - 1)
    }

    inner(initial, tries)
  }
}
