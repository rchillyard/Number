package com.phasmidsoftware.number.algebra

import com.phasmidsoftware.number.core.inner.Rational
import scala.util.Try

/**
  * A trait for types `T` that can be raised to a rational or integer power.
  *
  * CONSIDER eliminating this trait.
  *
  * This trait provides methods for performing operations where values of type `T`
  * are raised to an exponent represented as a rational or integer. It is 
  * particularly useful in mathematical computations involving such exponentiation
  * operations while supporting rational numbers as exponents.
  *
  * @tparam T the type of values that can be exponentiated
  */
trait RationalPowers[T] {

  /**
    * Raises a value of type `T` to a rational power.
    *
    * @param x the base value of type `T`
    * @param p the rational exponent
    * @return the result of raising the base `x` to the power `p`
    */
  def pow(x: T, p: Rational): Try[T]

  /**
    * Raises a value of type T to the power of an integer n.
    *
    * @param x the base value to be raised to the power
    * @param n the exponent to which the base value is raised
    * @return the result of raising x to the power n
    */
  def pow(x: T, n: Int): Try[T] = pow(x, Rational(n))
}
