package com.phasmidsoftware.number.algebra

import algebra.ring.*
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
    * Computes the addition of this object and the given `Structure` object using the additive properties
    * of an `AdditiveCommutativeMonoid`. The operation returns an optional result where the addition
    * succeeds if a valid transformation and combination can be performed.
    *
    * @param that  the `Structure` object to be added to this object.
    * @param using an implicitly provided instance of `AdditiveCommutativeMonoid[T]` that defines the additive
    *              behavior for the type `T`.
    * @return an `Option` containing the result of the addition if successful, or `None` if the operation
    *         cannot be completed.
    */
  infix def plus(that: Structure)(using AdditiveCommutativeMonoid[T]): Option[T] =
    that match {
      case t: T => Some(acm.additive.combine(asT, t))
      case u => u.convert(asT).flatMap(x => Some(acm.additive.combine(asT, x)))
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
    * Subtracts the given instance of type `T` from the current instance.
    *
    * This method computes the result of subtracting the `that` instance from the current instance
    * by utilizing the properties of an additive commutative group defined for type `T`.
    *
    * @param that     the instance of type `T` to subtract from the current instance
    * @param Additive evidence of an implicit `AdditiveCommutativeGroup[T]` that provides
    *                 the additive commutative group structure supporting subtraction
    * @return the resulting value of type `T` after subtraction
    */
  def -(that: T)(using AdditiveCommutativeGroup[T]): T

  /**
    * Negates this instance of type `CanAddAndSubtract[T]` by utilizing the additive inverse.
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
  def unary_- : T

  /**
    * Retrieves the implicit evidence of an `AdditiveCommutativeGroup[T]` for the given type `T`.
    *
    * @param acg an implicit parameter providing evidence of the `AdditiveCommutativeGroup[T]` structure
    *            for the type `T`. This ensures that the type `T` satisfies the properties of an
    *            additive commutative group.
    * @return an instance of `AdditiveCommutativeGroup[T]` that represents the additive commutative
    *         group structure for the type `T`.
    */
  private def acg(using AdditiveCommutativeGroup[T]): AdditiveCommutativeGroup[T] = summon[AdditiveCommutativeGroup[T]]
}

/**
  * Represents a trait that extends the capabilities of mathematical structures defined by `T` and `U`
  * to support multiplication operations within an algebraic framework.
  *
  * This trait builds upon the foundational traits `Can`, `CanAddAndSubtract`, and `CanScale`,
  * adding multiplicative behavior and related utilities. It defines operations such as
  * obtaining the multiplicative identity, performing multiplication, and combining structures
  * in a multiplicative context with optional compatibility checks.
  *
  * It's conceivable that some algebraic structures might support multiplication but not addition.
  *
  * @tparam T the type of structure that supports multiplication, bounded by `Structure` and requiring a `ClassTag`
  * @tparam U a secondary structure type used in combinatory operations, also bounded by `Structure`
  */
trait CanMultiply[T <: Structure : ClassTag, U <: Structure] extends Can[T] {

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
    * Multiplies this instance with another algebraic structure using the provided
    * `CommutativeRing` context, if possible.
    *
    * @param that            the input `Structure` instance to be multiplied with the current instance.
    * @param CommutativeRing an implicit parameter providing the context for performing
    *                        multiplication operations on objects of type `T`.
    * @return an `Option[T]` containing the result of the multiplication if it can be
    *         successfully performed, or `None` otherwise.
    */
  infix def times(that: Structure)(using CommutativeRing[T]): Option[T] =
    that match {
      case t: T => Some(mm.multiplicative.combine(asT, t))
      case u => u.convert(asT).flatMap(x => Some(mm.multiplicative.combine(asT, x)))
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
  * Represents a trait that extends the functionalities of `CanMultiply` to support both
  * multiplication and division operations within an algebraic structure for types `T` and `U`.
  *
  * This trait builds upon the `CanMultiply` trait by introducing additional properties and
  * utilities to work within a multiplicative framework. The primary addition is the inclusion
  * of the multiplicative identity element for types that support multiplication.
  *
  * @tparam T the primary type of the structure that supports multiplication and division,
  *           constrained to extend `Structure` and requiring a `ClassTag` for runtime type resolution
  * @tparam U a secondary structure type, also constrained to extend `Structure`
  */
trait CanMultiplyAndDivide[T <: Structure : ClassTag] extends CanMultiply[T, T] {

  /**
    * Calculates the reciprocal of the given value `t` within the context of a multiplicative group.
    *
    * @param t the value for which the reciprocal is to be calculated, belonging to type `T`
    * @return the reciprocal of the given value `t`, computed within the rules of the provided `MultiplicativeGroup[T]`
    */
  def reciprocal(using MultiplicativeGroup[T]): T =
    mg.reciprocal(asT)

  /**
    * Divides the current instance by the given value `that` within the context of a `MultiplicativeGroup[T]`.
    *
    * @param that the value by which the current instance is to be divided, belonging to type `T`
    * @return the result of the division, computed as per the rules defined by the `MultiplicativeGroup[T]`
    */
  def /(that: T)(using MultiplicativeGroup[T]): T =
    mg.div(asT, that)

  /**
    * Returns the implicit `MultiplicativeGroup[T]` for the type `T`.
    * CONSIDER whether this is exactly the correct type to use.
    *
    * @param evidence an implicit parameter that provides evidence that `T` has a `MultiplicativeGroup` structure
    * @return the `MultiplicativeGroup[T]` instance for type `T`
    */
  private def mg(using MultiplicativeGroup[T]): MultiplicativeGroup[T] = summon[MultiplicativeGroup[T]]
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
  * Trait `CanScaleDouble` defines the capability to scale instances of type `T` by a `Double` factor.
  *
  * Classes or traits implementing this trait are required to provide logic for scaling
  * their instances using the `doScaleDouble` method. The result of the scaling operation
  * is wrapped in an `Option` to account for cases where the scaling may not be possible or valid.
  *
  * @tparam T the type of the instance that can be scaled by a `Double`
  */
trait CanScaleDouble[T] {

  /**
    * Scales the current instance of type `T` by the specified `Double` value.
    *
    * This method applies a scaling factor to the instance, returning an `Option`
    * that contains the scaled instance if the operation is valid. If the scaling
    * operation is not valid or feasible, `None` is returned.
    *
    * @param that the `Double` value to scale the instance by
    * @return an `Option` containing the scaled instance of type `T`, or `None`
    *         if scaling is not possible
    */
  def doScaleDouble(that: Double): Option[T]
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
    * Computes the result of raising an instance of type `T` to the power 
    * specified by the given `RationalNumber`.
    *
    * The method returns an `Option[T]` to represent the possibility of invalid
    * operations or unsupported inputs where the computation cannot be performed.
    *
    * @param that the `RationalNumber` exponent to which the instance is raised
    * @return an `Option[T]` containing the result of the power operation if valid, 
    *         or `None` if the operation could not be performed
    */
  infix def pow(that: RationalNumber): Option[T]

  /**
    * Computes the result of raising an instance of type `T` to the power 
    * specified by the given `WholeNumber`.
    *
    * This method performs the power operation and returns the result wrapped 
    * in an `Option[T]`. If the operation is invalid or cannot be performed, 
    * `None` is returned.
    *
    * @param that the `WholeNumber` exponent to which the instance is raised
    * @return an `Option[T]` containing the result of the power operation if valid, 
    *         or `None` if the operation could not be performed
    */
  infix def pow(that: WholeNumber): Option[T]
}

/**
  * Trait `Can` represents an abstraction where a type `T` which extends `Structure` can cast an instance to itself.
  * It is the base trait for all other traits that extend `Can`.
  *
  * This trait facilitates type-safe casting of instances to a specific subtype of `Structure`.
  *
  * @tparam T the type parameter which must be a subtype of `Structure`
  */
sealed trait Can[T <: Structure] {
  /**
    * Casts the current instance to the type parameter `T` of the enclosing `Can` trait.
    *
    * @return the current instance as an instance of type `T`
    */
  def asT: T = this.asInstanceOf[T]
}