/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.core

/**
  * Trait defining the behavior of "numerical" objects.
  */
trait Numerical extends NumberLike {

  /**
    * Method to determine if this Numerical is equivalent to another Numerical object (x).
    *
    * @param x the other Numerical.
    * @return true if they are most probably the same, otherwise false.
    */
  def isSame(x: Numerical): Boolean

  /**
    * Method to determine if this Field has infinite magnitude.
    *
    * @return true if the magnitude of this Field is infinite.
    */
  def isInfinite: Boolean

  /**
    * Method to determine if this Field has zero magnitude.
    * Zero is the additive identity.
    *
    * @return true if the magnitude of this Field is zero.
    */
  def isZero: Boolean

  /**
    * Method to determine if this Field has unity magnitude.
    * Unity is the multiplicative identity.
    *
    * @return true if the magnitude of this Field is one.
    */
  def isUnity: Boolean

  /**
    * Determine the "sign" of this field.
    * For a real-valued quantity (Real or Number), we try to determine if it is to the right, left or at the origin.
    * For a complex number, we get the signum of the real part.
    *
    * @return +1 if to the right of the origin, -1 if to the left, 0 if at the origin.
    */
  def signum: Int

  /**
    * Change the sign of this Field.
    */
  def unary_- : Field

  /**
    * Yields the inverse of this Field.
   * This Number is first normalized so that its factor is PureNumber, since we cannot directly invert Numbers with other
    * factors.
    */
  def invert: Field

  /**
    * Method to "normalize" a field.
    *
    * @return a Field which is in canonical form.
    */
  def normalize: Field

  /**
    * Method to return this Field as a Complex.
    * If this is a Real number x, return ComplexPolar(x) otherwise, return this.
    *
    * @return a Complex.
    */
  def asComplex: Complex

  /**
    * Method to return this Field as a Real, if possible.
    * If this is a Real number x, return Some(x) otherwise, return None.
    *
    * @return an Option[Real].
    */
  def asReal: Option[Real]
}
