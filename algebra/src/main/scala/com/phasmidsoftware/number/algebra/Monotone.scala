package com.phasmidsoftware.number.algebra

import com.phasmidsoftware.number.algebra
import com.phasmidsoftware.number.algebra.Structure
import com.phasmidsoftware.number.core.inner.*
import scala.reflect.ClassTag

/**
  * Represents a `Monotone`, which is one-dimensional `Structure` that can be ordered
  * and supports various mathematical operations and properties. Monotones include both
  * exact and approximate numerical entities.
  *
  * Monotone does not support ordering because not all Monotones are comparable.
  *
  * Multidimensional mathematical quantities such as Complex cannot be represented by a `Monotone` object.
  */
trait Monotone extends Structure with Approximate {

  /**
    * Method to determine if this `Structure` object is exact.
    * For instance, `Number.pi` is exact, although if you converted it into a `PureNumber`, it would no longer be exact.
    *
    * @return true if this `Structure` object is exact in the context of no factor, else false.
    */
  def isExact: Boolean = approximation().isEmpty

  /**
    * Attempts to yield a factor for the instance, if available.
    *
    * A `Factor` is a representation of the underlying numerical domain, for example, `PureNumber`, `Radian`, etc.
    *
    * @return an `Option[Factor]` containing the factor representation of this object,
    *         or `None` if factorization is not applicable or unavailable.
    */
  def maybeFactor(context: Context): Option[Factor]

  /**
    * Attempts to approximate the current instance to a `Real` value.
    * If the instance is already of type `Real`, it is simply returned, wrapped inside `Some`.
    * Otherwise, depending on the value of `force`, it either attempts a conversion
    * to a default `Real` (if `force` is true), or returns `None`.
    *
    * CONSIDER moving this up into Approximate.
    *
    * NOTE that this method tries to keep exact quantities exact.
    *
    * @param force a boolean flag indicating whether to force the conversion to a default `Real`
    *              value when the current instance is not of type `Real`
    * @return an `Option[Real]` containing the approximated value if successful, or `None` if approximation fails
    */
  def approximation(force: Boolean = false): Option[Real] = this match {
    case real: Real => Some(real)
    case _ if force => convert(Real.zero)
    case _ => None
  }

  /**
    * Determines if the current number is equal to zero.
    *
    * @return true if the number is zero, false otherwise
    */
  def isZero: Boolean

  /**
    * Determines the sign of the Monotone value represented by this instance.
    * Returns an integer indicating whether the value is positive, negative, or zero.
    *
    * @return 1 if the value is positive, -1 if the value is negative, and 0 if the value is zero
    */
  def signum: Int

}

/**
  * Represents a `Monotone`, which is one-dimensional `Structure` that can be ordered
  * and supports various mathematical operations and properties. Monotones include both
  * exact and approximate numerical entities.
  *
  * Monotone does not support ordering because not all Monotones are comparable.
  *
  * Multidimensional mathematical quantities such as Complex cannot be represented by a `Monotone` object.
  */
trait Transformed extends Monotone {

  /**
    * Defines a transformation that transforms a `Monotone` instance into a corresponding `Scalar` value. 
    *
    * The transformation defines how a `Monotone` is interpreted or converted in the context of `Scalar`.
    *
    * @return a transformation that maps a `Monotone` object to a `Scalar` result
    */
  def transformation[T: ClassTag]: Option[T]
}
