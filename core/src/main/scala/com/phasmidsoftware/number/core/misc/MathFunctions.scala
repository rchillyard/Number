package com.phasmidsoftware.number.core.misc

import scala.util.{Failure, Success, Try}

object MathFunctions:

  /**
    * Computes the exponential function e raised to the specified power.
    *
    * @param y the exponent to which Euler's number is raised
    * @return the value of e raised to the power of `y`
    */
  def exp(y: Double): Try[Double] = y match
    case 0.0 => Success(1.0)
    case 1.0 => Success(math.E)
    case _ => Try(math.exp(y))

  /**
    * Computes the natural logarithm of the specified value.
    *
    * @param y the value whose natural logarithm is computed
    * @return a `Try` containing the natural logarithm of `y`
    */
  def log(y: Double): Try[Double] = y match
    case math.E => Success(1.0)
    case 1.0 => Success(0.0)
    // NOTE math.log returns NaN rather than throwing, so Try would give Success(NaN) and the
    // caller would have to test for it. A Try-returning method should report a domain error.
    case _ if y < 0 => Failure(MathFunctionsException(s"log: not defined for negative argument: $y"))
    case _ => Try(math.log(y))

  /**
    * Computes the sine of the specified angle in radians.
    *
    * @param x the angle in radians
    * @return a Try containing the sine of the specified angle
    */
  def sin(x: Double): Try[Double] = 2 * x match {
    case 0.0 => Success(0.0)
    case math.Pi => Success(1.0)
    case y if y < 0 => sin(-x).map(-_) // NOTE recurse on -x, not on y (which is 2 * x)
    case _ => Success(math.sin(x))
  }

  /**
    * Computes the cosine of the specified angle.
    *
    * @param x the angle in radians whose cosine is computed
    * @return a `Try` containing the cosine of `x`
    */
  def cos(x: Double): Try[Double] = 2 * x match {
    case 0.0 => Success(1.0)
    case math.Pi => Success(0.0)
    // NOTE cos is even, so no negation of the result -- but the branch is still needed, or a
    // negative argument never reaches the exact cases above: cos(Pi/2) would be exactly 0.0
    // while cos(-Pi/2) came back as 6.123233995736766E-17.
    case y if y < 0 => cos(-x)
    case _ => Success(math.cos(x))
  }

/**
  * Exception thrown for a domain error in [[MathFunctions]].
  *
  * @param s a message describing the error.
  */
case class MathFunctionsException(s: String) extends Exception(s)
