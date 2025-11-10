package com.phasmidsoftware.number.algebra

import algebra.ring.{AdditiveCommutativeGroup, AdditiveCommutativeMonoid, CommutativeRing, MultiplicativeMonoid}
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
trait CanAdd[T <: Structure : ClassTag, U <: Structure] extends Can[T] {

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

/**
  * A trait that extends `CanAdd` and provides additional functionality for types
  * supporting both addition and subtraction operations. It introduces the ability
  * to negate instances of type `T` and override the unary negation operator.
  *
  * Type Parameters:
  * - `T`: A type extending `Structure` that supports addition, subtraction, and negation operations
  * with an implicit `AdditiveCommutativeGroup[T]` evidence.
  * - `U`: A type extending `Structure` that is compatible with `T` in additive operations.
  */
trait CanAddAndSubtract[T <: Structure : ClassTag, U <: Structure] extends CanAdd[T, U] {

  /**
    * Negates the current instance of type `T` by utilizing the additive inverse.
    *
    * @param acg evidence of an implicit `AdditiveCommutativeGroup[T]` providing the
    *            additive group structure for type `T`
    * @return the additive inverse of the current instance as type `T`
    */
  def negate(using AdditiveCommutativeGroup[T]): T =
    acg.additive.inverse(asT)

  /**
    * Negates the current WholeNumber instance, producing its additive inverse.
    *
    * @return The additive inverse of the current WholeNumber.
    */
  def unary_- : WholeNumber

  private def acg(using AdditiveCommutativeGroup[T]): AdditiveCommutativeGroup[T] = summon[AdditiveCommutativeGroup[T]]
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
trait CanScale[T <: Structure, U <: Structure] {

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
  def doScale(that: U): Option[T]
}

trait CanMultiply[T <: Structure : ClassTag, U <: Structure] extends Can[T] with CanScale[T, Number] {

  /**
    * Returns the multiplicative identity element of type `T` in the context
    * of a structure that supports multiplication.
    *
    * @return the instance of type `T` that acts as the identity element for multiplication
    */
  def one: T

  /**
    * Multiplies the current instance of type `T` with another instance of type `T`, using the
    * provided `MultiplicativeMonoid` context.
    *
    * @param that the instance of type `T` to be multiplied with the current instance.
    * @return the result of multiplying the current instance by the given instance of type `T`.
    */
  def *(that: T)(using MultiplicativeMonoid[T]): T =
    mm.multiplicative.combine(asT, that)

  /**
    * Combines the current instance of type `T` with another instance of type `U`
    * using an additive operation, if the conversion and compatibility conditions are met.
    *
    * @param that            the instance of type `U` to combine with the current instance.
    * @param CommutativeRing a context parameter providing the additive operations for type `T`.
    * @return an `Option` containing the combined result of type `T` if the operation is successful,
    *         or `None` if the combination is not feasible due to type conversion failure or other constraints.
    */
  infix def doTimes(that: U)(using CommutativeRing[T]): Option[T] = {
    that match {
      case u: T => Some(mm.multiplicative.combine(asT, u))
      case u => u.convert(asT).flatMap(x => Some(mm.multiplicative.combine(asT, x)))
    }
  }

  /**
    * Retrieves an implicit instance of `MultiplicativeMonoid[T]` from the given context.
    * CONSIDER whether this is exactly the correct type to use.
    * It needs to support all required properties and it must be a superclass of `CommutativeRing`.
    *
    * @param evidence an implicit parameter of type `MultiplicativeMonoid[T]`, representing the context
    *                 within which multiplicative operations are defined for type `T`.
    * @return an instance of `MultiplicativeMonoid[T]`, which provides methods and properties
    *         for performing multiplicative operations on type `T` values.
    */
  private def mm(using MultiplicativeMonoid[T]): MultiplicativeMonoid[T] = summon[MultiplicativeMonoid[T]]
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

trait Can[T <: Structure] {
  /**
    * Casts the current instance to the type parameter `T` of the enclosing `Can` trait.
    *
    * @return the current instance as an instance of type `T`
    */
  def asT: T = this.asInstanceOf[T]
}