/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.core

import com.phasmidsoftware.number.algebra.eager.{Complex, Eager, Real}
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
    * Attempts to yield an `Eager` value from the current `Approximate`.
    *
    * This method leverages the `approximation` mechanism to compute an eager evaluation
    * of the value represented by the instance. It handles potential errors during the
    * conversion process by raising an `AlgebraException` if the approximation fails.
    *
    * @return an `Eager` representation of the approximated value if recovery is successful;
    *         otherwise, an `AlgebraException` is thrown indicating the failure to convert to `Real`.
    */
  def fuzzy: Eager
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
    * Attempts to compute an approximate complex representation of the current value.
    *
    * This method provides an optional approximation in the form of an `Eager` type.
    * By default, it does not perform the computation unless explicitly requested
    * through the `force` flag. The method may return `None` if no approximation
    * can be computed or if the operation fails.
    *
    * @param force a boolean flag indicating whether to force computation of the 
    *              approximation. If `true`, the method will attempt to perform
    *              a complex approximation even if it is resource-intensive or
    *              otherwise unwarranted.
    *
    * @return an `Option` containing the approximate value as an `Eager` if successful, 
    *         or `None` if no approximation is available.
    */
  def approximationComplex(force: Boolean = false): Option[Eager] = None

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
  lazy val toDouble: Double = this match {
    case x: Complex =>
      x.complex.modulus.toNominalDouble.getOrElse(throw AlgebraException(s"Complex.toDouble: no approximation: $x"))
    case _ =>
      // NOTE: it is possible for this to recurse infinitely if approximation(true) returns `Some(this)`.
      recover(approximation(true).map(_.toDouble))(AlgebraException("Approximate.toDouble: logic error"))
  }
}
