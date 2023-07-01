package com.phasmidsoftware.number.applications

import com.phasmidsoftware.number.core.Field.convertToNumber
import com.phasmidsoftware.number.core.FuzzyNumber.NumberIsFuzzy
import com.phasmidsoftware.number.core.{Field, Number}
import scala.annotation.tailrec
import scala.math.Fractional.Implicits.infixFractionalOps
import scala.util.{Failure, Success, Try}

/**
  * Object which deals with the approximations of function values.
  */
object Approximation {

  /**
    * Method used by solve to iterate towards a solution.
    * This method of iterating towards a root of a continuously differentiable function implements Householder's method in general.
    * The number of functions provided is the "order" of the method.
    * The Newton-Raphson method is Householder's method, order 1.
    * The Halley method is Householder's method, order 2.
    * And so on.
    *
    * @param functions the derivatives of the function, starting with the 0th, i.e. the function itself.
    */
  def iterate(functions: (Double => Double)*): Number => Try[Number] = {
    x =>
      val f = functions(0)
      val dfByDx = functions(1)
      for {
        p <- evaluate(f, dfByDx)(x)
//        _ = println(s"iterate: x=$x; f(x)=$p")
        q <- evaluateWithoutDerivative(dfByDx)(x)
        r = p.divide(q)
        s <- correction(r, q, x, functions)
        t = x.doSubtract(convertToNumber(s))
      } yield t
  }

  /**
    * Method to determine if the approximation can be considered converged.
    * We use the convolution of the probability density functions (PDFs) to determine if the probability of overlap exceeds the value p.
    *
    * @param f      the function.
    * @param dfByDx the derivative function.
    * @param p      a probability between 0 and 1 -- 0 would always result in true; 1 will result in false unless x1 actually is x2.
    * @param x      a Number which will be compared with zero.
    * @return a Try[Boolean].
    */
  def converged(f: Double => Double, dfByDx: Double => Double)(p: Double)(x: Number): Try[Boolean] =
    evaluate(f, dfByDx)(x) map (n => NumberIsFuzzy.same(p)(n, Number.zero))

  /**
    * Method to evaluate function f at value x.
    * We ignore the derivative of the function at x.
    * NOTE: this has consequences in terms of the precision of the result.
    *
    * @param f the function.
    * @param x the value to pass into f.
    * @return the function's value at x.
    */
  def evaluateWithoutDerivative(f: Double => Double)(x: Number): Try[Number] = evaluate(f, _ => 0)(x)

  /**
    * Method to evaluate function f at value x.
    *
    * @param f the function.
    * @param dfByDx the derivative of the function.
    * @param x the value to pass into f.
    * @return the function's value at x.
    */
  def evaluate(f: Double => Double, dfByDx: Double => Double)(x: Number): Try[Number] = x.applyFunc(f, dfByDx)

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
    require(functions.size > 1, "solve: insufficient functions provided")

    val tester: Number => Try[Boolean] = converged(functions(0), functions(1))(probability)
    val iterator: Number => Try[Number] = iterate(functions: _*)

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

  /**
    * This method evaluates the correction for higher derivatives.
    * NOTE only the Halley version is currently implemented (where n = 3).
    *
    * @param r the ratio of f(x) to f'(x).
    * @param q the value of f'(x).
    * @param x the value of x.
    * @param f the list of derivate functions: f, f', f&#39;&#39;, etc.
    * @return the correction term.
    */
  private def correction(r: Field, q: Field, x: Number, f: Seq[Double => Double]): Try[Number] = f.length match {
    case n if n < 3 =>
      Success(convertToNumber(r))
    case 3 => // Halley's method
      for {
        y <- evaluateWithoutDerivative(f(2))(x)
        z = y divide (r * Number.two)
        w = r.divide(Number.one - convertToNumber(r * convertToNumber(z)))
      } yield convertToNumber(w)
    case n =>
      Failure(com.phasmidsoftware.number.core.NumberException(s"Approximation.iterate: does not implement correction with $n functions"))
  }
}
