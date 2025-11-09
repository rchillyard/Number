package com.phasmidsoftware.number.algebra

import algebra.CommutativeGroup
import algebra.ring.{AdditiveCommutativeGroup, AdditiveCommutativeMonoid}
import scala.reflect.ClassTag

/**
  * Represents a type class for types `T` and `U`, both of which extend `Structure`, enabling addition operations
  * between these types with specific constraints. This trait provides functionality for adding instances
  * of type `T` to each other and for attempting to add instances of type `U` to `T`.
  *
  * The operations are designed to work within the context of an `AdditiveCommutativeMonoid`, ensuring
  * that the defined addition operations respect the algebraic properties of associativity, commutativity, and identity.
  *
  * Type Parameters:
  * - `T`: A type extending `Structure` that supports addition operations and satisfies the properties
  * of an additive commutative monoid.
  * - `U`: A type extending `Structure` that is potentially compatible with `T` in certain addition operations.
  */
trait CanAdd[T <: Structure : ClassTag, U <: Structure] {

  /**
    * Returns the zero value of type T.
    * This value serves as the identity element for the addition operation of T.
    *
    * @return The zero value of type T.
    */
  def zero: T

  /**
    * Adds the given instance of the same type to this instance, leveraging the properties 
    * of an `AdditiveCommutativeMonoid` to ensure associativity, commutativity, and identity.
    *
    * @param that The instance of the same type to be added to this instance.
    * @return The result of adding this instance and the provided instance.
    */
  def +(that: T)(using AdditiveCommutativeMonoid[T]): T =
    acm.additive.combine(asT, that)

  /**
    * Casts the current instance to the type parameter `T`.
    *
    * @return the current instance cast to type `T`
    */
  def asT: T = this.asInstanceOf[T]

  /**
    * Combines the current instance of type `T` with another instance of type `U`
    * using an additive operation, if the conversion and compatibility conditions are met.
    *
    * @param that                      the instance of type `U` to combine with the current instance.
    * @param AdditiveCommutativeMonoid a context parameter providing the additive operations for type `T`.
    * @return an `Option` containing the combined result of type `T` if the operation is successful, 
    *         or `None` if the combination is not feasible due to type conversion failure or other constraints.
    */
  infix def doPlus(that: U)(using AdditiveCommutativeMonoid[T]): Option[T] = {
    that match {
      case u: T => Some(acm.additive.combine(asT, u))
      case u => u.convert(asT).flatMap(x => Some(acm.additive.combine(asT, x)))
    }
  }

  /**
    * Provides the implicitly summoned instance of `AdditiveCommutativeMonoid` for the type `T`.
    *
    * @param using The context-bound `AdditiveCommutativeMonoid` instance for the type `T`.
    * @return The summoned `AdditiveCommutativeMonoid[T]` instance.
    */
  private def acm(using AdditiveCommutativeMonoid[T]): AdditiveCommutativeMonoid[T] = summon[AdditiveCommutativeMonoid[T]]
}

trait CanAddAndSubtract[T <: Structure : ClassTag, U <: Structure] extends CanAdd[T, U] {

  def cg(using AdditiveCommutativeGroup[T]): AdditiveCommutativeGroup[T] = summon[AdditiveCommutativeGroup[T]]

  def negate(using AdditiveCommutativeGroup[T]): T =
    cg.additive.inverse(asT)

  def unary_- : WholeNumber
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
