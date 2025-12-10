package com.phasmidsoftware.number.core.misc

import scala.annotation.tailrec
import scala.util._

/**
  * A case class for solving equations using Newton's approximation method (Newton-Raphson)
  * [[https://en.wikipedia.org/wiki/Newton%27s_method]].
  * Newton-Raphson is a special case of Householder's more general method
  * [[https://en.wikipedia.org/wiki/Householder%27s_method]].
  * CONSIDER implementing Halley's method [[https://en.wikipedia.org/wiki/Halley%27s_method]].
  *
  * @constructor Creates a new Newton solver with the given function and its derivative.
  * @param f      The function whose root is to be found.
  * @param dfByDx The derivative of the function.
  */
case class Newton(f: Double => Double, dfByDx: Double => Double) {

  /**
    * Solves the given equation using the Newton-Raphson method by iteratively refining an initial guess.
    * The method attempts to find a root of the function f(x) = 0 within a specified threshold of accuracy.
    * If the solution does not converge within the given number of tries, it returns a failure.
    *
    * @param tries     The maximum number of iterations allowed for convergence.
    * @param threshold The convergence threshold, representing the acceptable difference between f(x) and 0.
    * @param initial   The initial guess for the root of the equation.
    * @return A `Try[Double]` containing the root if converged successfully, or a `Failure` if the method fails.
    */
  def solve(tries: Int, threshold: Double, initial: Double): Try[Double] = {

    /**
      * Computes the next step in the Newton-Raphson iteration to approximate the root of a function.
      *
      * @param x The current approximation of the root.
      * @param y The value of the function at the current approximation (f(x)).
      * @return (Optionally) the next approximation of the root by applying the Newton-Raphson formula.
      *         If the result is None, then this means that the slope is zero (an inflexion point).
      */
    def step(x: Double, y: Double): Option[Double] = {
      val slope = dfByDx(x)
      Option.when(slope != 0)(x - y / slope)
    }

    /**
      * Tail-recursive method for solving Newton's method.
      *
      * @param r the best estimate as of now. When the recursion terminates, `r` will be returned, wrapped in `Success`.
      * @param n the number of steps remaining. If `n` reaches zero, the recursion terminates with a `Failure`.
      * @return a `Try[Double]`.
      */
    @tailrec def inner(r: Double, n: Int): Try[Double] = {
      val y = f(r)
      if (math.abs(y) < threshold) Success(r)
      else if (n == 0) Failure(new Exception(s"failed to converge after $tries tries"))
      else step(r, y) match {
        case Some(x) => inner(x, n - 1)
        case None => Success(r) // CONSIDER failing this case
      }
    }

    inner(initial, tries)
  }
}
