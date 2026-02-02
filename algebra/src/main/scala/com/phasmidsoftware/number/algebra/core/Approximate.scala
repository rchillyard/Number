/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.core

import com.phasmidsoftware.number.algebra.eager.Real
import com.phasmidsoftware.number.algebra.util.AlgebraException
import com.phasmidsoftware.number.algebra.util.FP.recover

/**
  * Trait representing a structure that supports approximation.
  *
  * The `Approximate` trait provides a contract for classes that need
  * to define how their numerical representation can be approximated
  * in some way, typically to account for uncertainty, imprecision,
  * or computational limitations.
  */
trait Approximate {
  /**
    * Attempts to recover a `Real` value using the `approximation` method of the current instance.
    *
    * If an approximation is available, it will be converted to a `Real`. Otherwise, an 
    * `AlgebraException` is thrown, indicating that the conversion to a `Real` was not possible
    * for the current class instance.
    *
    * @return the recovered `Real` value based on an available approximation, or throws an
    *         `AlgebraException` if no valid approximation is found.
    */
  def fuzzy: Real =
    recover(approximation(true))(AlgebraException(s"fuzzy: unable to convert a ${this.getClass.getSimpleName} to Real (${this.toString})"))

  /**
    * Attempts to compute an approximate representation of the current value.
    *
    * This method provides an optional approximation of the value represented by 
    * the implementing class. The approximation may account for uncertainties or 
    * computational limitations. By default, this method does not force computation 
    * of the approximation unless explicitly requested.
    *
    * @param force a boolean flag indicating whether to force computation of 
    *              the approximation. If `true`, the method will attempt to 
    *              generate an approximation even if such computation 
    *              is resource-intensive or not strictly necessary.
    *
    * @return an `Option` containing the approximate value as a `Real` if available, 
    *         or `None` if no approximation can be computed.
    */
  def approximation(force: Boolean = false): Option[Real]

  /**
    * Converts the approximate representation of this number into a `Double`.
    *
    * This method attempts to approximate the value of this number by invoking 
    * the `approximation` method with `force = true`. If the approximation exists, 
    * it is converted to a `Double`. If no approximation can be obtained, an 
    * `AlgebraException` is thrown to indicate a logic error.
    *
    * @return the approximate value as a `Double`.
    * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if no approximation can be obtained.
    */
  lazy val toDouble: Double = {
    // NOTE: it is possible for this to recurse infinitely if approximation(true) returns `Some(this)`.
    recover(approximation(true).map(_.toDouble))(AlgebraException("Approximate.toDouble: logic error"))
  }
}
