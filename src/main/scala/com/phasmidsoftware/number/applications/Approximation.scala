package com.phasmidsoftware.number.applications

import com.phasmidsoftware.number.core.Field.convertToNumber
import com.phasmidsoftware.number.core.FuzzyNumber.NumberIsFuzzy
import com.phasmidsoftware.number.core.Number

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Approximation {

  /**
    * Method used by solve to iterate towards the the solution.
    * This method of iterating towards a root of a continuously differentiable function implements Householder's method in general.
    * The number of functions provided is the "order" of the method.
    * The Newton-Raphson method is Householder's method, order 1.
    * The Halley method is Householder's method, order 2.
    * And so on.
    *
    * @param functions the derivatives of the function, starting with the 0th, i.e. the function itself.
    */
  def iterate(functions: (Double => Double)*): Number => Try[Number] = {
    // NOTE: for now, we ignore anything beyond the first derivative, i.e. we just implement Newton.
    x =>
      println(x)
      require(functions.size > 1, "iterate: insufficient functions provided")
      val f = functions(0)
      val dfByDx = functions(1)
      for {
        p <- evaluate(f)(x)
        q <- evaluate(dfByDx)(x)
        r = p.divide(q)
        t = x.doSubtract(convertToNumber(r))
      } yield t
  }

  def converged(f: Double => Double)(p: Double)(x: Number): Try[Boolean] =
    evaluate(f)(x) map (n => NumberIsFuzzy.same(p)(n, Number.zero))

  def evaluate(f: Double => Double)(x: Number): Try[Number] = x.applyFunc(f, _ => 0)

  /**
    * This method of finding a root of a continuously differentiable function implements Householder's method in general.
    * The number of functions provided is the "order" of the method.
    * The Newton-Raphson method is Householder's method, order 1.
    * The Halley method is Householder's method, order 2.
    * And so on.
    * f[n](x) is the nth derivative of x, such that f[0](x) = f(x) and f[1](x) = f'(x) = d(f(x))/dx.
    *
    * @param probability the confidence we need for the resulting root to be the same as zero.
    * @param functions   a list of the derivative functions: f[0](x), f[1](x), f[2](x), f[3](x), etc.
    * @param x0          the initial guess.
    */
  def solve(probability: Double, functions: (Double => Double)*)(x0: Number): Try[Number] = {
    val tester: Number => Try[Boolean] = converged(functions(0))(probability)
    val iterator = iterate(functions: _*)

    @tailrec
    def inner(xy: Try[Number]): Try[Number] = xy match {
      case Success(x) => tester(x) match {
        case Success(b) if b => Success(x)
        case Success(_) => inner(iterator(x))
        case Failure(z) => Failure(z)
      }
      case Failure(z) => Failure(z)
    }

    inner(Success(x0))
  }
}
