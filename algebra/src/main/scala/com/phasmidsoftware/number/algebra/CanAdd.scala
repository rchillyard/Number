package com.phasmidsoftware.number.algebra

import algebra.ring.AdditiveCommutativeMonoid
import cats.kernel.CommutativeGroup
import scala.reflect.ClassTag

/**
  * Behavior that includes the addition of an instance of a similar type.
  * This is intended to be used as a mixin, not as a typeclass.
  *
  * @tparam T the underlying type.
  */
trait CanAdd[T <: Structure : ClassTag, U <: Structure] {

  def zero: T

  def +(that: T)(using AdditiveCommutativeMonoid[T]): T =
    cm.additive.combine(asT, that)

  def cm(using AdditiveCommutativeMonoid[T]): AdditiveCommutativeMonoid[T] = summon[AdditiveCommutativeMonoid[T]]

  def asT: T = this.asInstanceOf[T]

  /**
    * Adds this instance of type `T` to another `T` and returns the result as an `Option[T]`.
    * The addition may not always be valid, depending on the context or properties of the `T`s.
    *
    * @param that the `T` to be added to the current instance
    * @return an `Option[T]` containing the result of the addition, or `None` if the operation is not valid
    */
  infix def doPlus(that: U)(using AdditiveCommutativeMonoid[T]): Option[T] = {
    that match {
      case u: T => Some(cm.additive.combine(asT, u))
      case u => u.convert(asT).flatMap(x => Some(cm.additive.combine(asT, x)))
    }
  }
}

trait CanAddAndSubtract[T <: Structure : ClassTag, U <: Structure] extends CanAdd[T, U] {

  def cg(using CommutativeGroup[T]): CommutativeGroup[T] = summon[CommutativeGroup[T]]

  def negate(using CommutativeGroup[T]): T =
    cg.inverse(asT)
}

/**
  * A trait representing the ability to scale an instance of type `T` as a whole
  * using an integer multiplier.
  *
  * This trait defines a contract for types that support a scaling operation, where
  * an instance can be adjusted by multiplying it with an integer. The result of
  * the scaling operation is returned as an `Option`, allowing implementations to
  * specify cases where scaling may not be valid or possible.
  *
  * @tparam T the type of the instance that can be scaled
  */
trait CanScaleWhole[T] {

  /**
    * Scales the instance of type T by the given integer multiplier.
    *
    * This method performs a multiplication operation between the current instance and
    * the specified integer, returning an optional result. The result is defined if
    * the scaling operation is valid for the specific implementation.
    *
    * @param that the integer multiplier used to scale the instance
    * @return an Option containing the scaled result of type T, or None if the operation is invalid
    */
  def doScaleInt(that: Int): Option[T]
}

/**
  * Represents a capability of scaling instances of type `T`.
  *
  * This trait defines a method for scaling an object of type `T` using a `Number` multiplier.
  * The operation allows for the result to be optionally returned, reflecting cases where scaling
  * might not be applicable or feasible.
  *
  * @tparam T the type of the instance that can be scaled
  */
trait CanScale[T] {

  /**
    * Scales the current instance of type `T` using the given `Number` multiplier.
    *
    * This method performs a scaling operation by multiplying the current instance
    * with the provided `Number`. The result of the scaling operation is returned
    * as an `Option`, allowing for cases where the operation might not be valid or
    * possible.
    *
    * @param that the `Number` multiplier used to scale the current instance
    * @return an `Option[T]` containing the scaled instance of type `T`, or `None` if the operation cannot be performed
    */
  def doScale(that: Number): Option[T]
}

/**
  * Represents a type class for performing power operations on instances of type `T`.
  *
  * This trait defines the behavior for raising an instance of `T` to a power
  * specified by a `Scalar`. Implementations of this trait are responsible for
  * defining how the power operation is computed and whether it is valid for
  * specific inputs.
  *
  * @tparam T the type of the instance that supports the power operation
  */
trait CanPower[T] {

  /**
    * Performs a power operation using this instance and the provided `Scalar`.
    *
    * This method calculates the result of raising this instance to the power of the given `Scalar` value.
    * The result may be optionally defined, depending on the implementation or the validity of the operation.
    *
    * @param that the `Scalar` instance representing the power to which this instance is raised
    * @return an `Option[T]` containing the result of the power operation if it is defined, or `None` otherwise
    */
  infix def doPower(that: Scalar): Option[T]
}
