/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.core

import algebra.ring.*
import com.phasmidsoftware.number.algebra.*
import com.phasmidsoftware.number.algebra.eager.{ExactNumber, Structure}
import com.phasmidsoftware.number.core.inner.Rational
import scala.reflect.ClassTag

/**
  * Represents a type class for types `T` and `U`, both of which extend `Structure`, enabling addition operations
  * between these types with specific constraints. This trait provides functionality for adding instances
  * of type `T` to each other and for attempting to add instances of type `U` to `T`.
  *
  * The operations are designed to work within the context of an `AdditiveCommutativeMonoid`, ensuring
  * that the defined addition operations respect the algebraic properties of associativity, commutativity, and identity.
  *
  * TODO we no longer need the U type parameter.
  *
  * Type Parameters:
  * - `T`: A type extending `Structure` that supports addition operations and satisfies the properties
  *   of an additive commutative monoid.
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
    * CONSIDER keeping percentage if both are percentages.
    * CONSIDER rewriting this in such a way that no exception will ever be thrown (use compiler to check, not runtime).
    *
    * @param that The instance of the same type to be added to this instance.
    * @return The result of adding this instance and the provided instance.
    */
  def +(that: U)(using AdditiveCommutativeMonoid[T], Convertible[T, U]): T =
    val t = summon[Convertible[T, U]].convert(this.asT, that)
    acm.additive.combine(asT, t)

  /**
    * Computes the addition of this object and the given `Structure` object using the additive properties
    * of an `AdditiveCommutativeMonoid`. The operation returns an optional result where the addition
    * succeeds if a valid transformation and combination can be performed.
    *
    * TODO eliminate this method and use with + instead.
    *
    * @param that  the `Structure` object to be added to this object.
    * @param using an implicitly provided instance of `AdditiveCommutativeMonoid[T]` that defines the additive
    *              behavior for the type `T`.
    *
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

trait CanNegate[T <: Structure] extends Can[T] {
  /**
    * Computes the additive inverse (negation) of an instance of type `T`.
    *
    * @param AdditiveCommutativeGroup[T] an implicit instance of `AdditiveCommutativeGroup` for the type `T`,
    *                                    providing the necessary additive group operations.
    * @return the negated value of the current instance as an instance of type `T`.
    */
  def negate(using AdditiveCommutativeGroup[T]): T =
    summon[AdditiveCommutativeGroup[T]].additive.inverse(asT)
}

/**
  * A trait that extends `CanAdd` and provides additional functionality for types
  * supporting both addition and subtraction operations. It introduces the ability
  * to negate instances of type `T` and override the unary negation operator.
  * TODO we no longer need the U type parameter.
  *
  * Type Parameters:
  * - `T`: A type extending `Structure` that supports addition, subtraction, and negation operations
  *   with an implicit `AdditiveCommutativeGroup[T]` evidence.
  * - `U`: A type extending `Structure` that is compatible with `T` in additive operations.
  */
trait CanAddAndSubtract[T <: Structure : ClassTag, U <: Structure] extends CanAdd[T, U] with CanNegate[T] {

  /**
    * Subtracts the given instance of type `T` from the current instance.
    *
    * This method computes the result of subtracting the `that` instance from the current instance
    * by utilizing the properties of an additive commutative group defined for type `T`.
    *
    * @param that  the instance of type `T` to subtract from the current instance
    * @param using evidence of an implicit `AdditiveCommutativeGroup[T]` that provides
    *              the additive commutative group structure supporting subtraction
    *
    * @return the resulting value of type `T` after subtraction
    */
  def -(that: T)(using AdditiveCommutativeGroup[T]): T

  /**
    * Negates the current WholeNumber instance, producing its additive inverse.
    *
    * @return The additive inverse of the current WholeNumber.
    */
  def unary_- : T

  /**
    * Retrieves the implicit evidence of an `AdditiveCommutativeGroup[T]` for the given type `T`.
    *
    * @param using an implicit parameter providing evidence of the `AdditiveCommutativeGroup[T]` structure
    *              for the type `T`. This ensures that the type `T` satisfies the properties of an
    *              additive commutative group.
    *
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
    * Retrieves an implicit instance of `MultiplicativeMonoid[T]` from the given context.
    * CONSIDER whether this is exactly the correct type to use.
    * It needs to support all required properties and it must be a superclass of `CommutativeRing`.
    *
    * @param using an implicit parameter of type `MultiplicativeMonoid[T]`, representing the context
    *              within which multiplicative operations are defined for type `T`.
    *
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
  */
trait CanMultiplyAndDivide[T <: Structure : ClassTag] extends CanMultiply[T, T] {

  /**
    * Calculates the reciprocal of the given value `t` within the context of a multiplicative group.
    *
    * @param using the value of MultiplicativeGroup
    * @return the reciprocal of the given value `t`, computed within the rules of the provided `MultiplicativeGroup[T]`
    */
  def reciprocal(using MultiplicativeGroup[T]): T =
    mg.reciprocal(asT) // NOTE not normalized

  /**
    * Divides the current instance by the given value `that` within the context of a `MultiplicativeGroup[T]`.
    *
    * @param that the value by which the current instance is to be divided, belonging to type `T`
    * @return the result of the division, computed as per the rules defined by the `MultiplicativeGroup[T]`
    */
  def /(that: T)(using MultiplicativeGroup[T]): T =
    mg.div(asT, that) // NOTE not normalized

  /**
    * Returns the implicit `MultiplicativeGroup[T]` for the type `T`.
    * CONSIDER whether this is exactly the correct type to use.
    *
    * @param using an implicit parameter that provides evidence that `T` has a `MultiplicativeGroup` structure
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
trait Scalable[T <: Scalable[T]] {

  /**
    * Scales the current instance by the given factor.
    *
    * This method applies a scaling operation on the instance using the provided
    * rational factor and returns the resulting scaled instance.
    *
    * The result is not guaranteed to be normalized.
    *
    * @param factor the rational number representing the scale factor
    * @return the scaled instance of type `T`
    */
  infix def *(factor: Rational): T
}

/**
  * Represents a type class for performing power operations on instances of type `T`.
  *
  * CONSIDER two parametric types
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
    * specified by the given `ExactNumber`.
    *
    * This method performs the power operation and returns the result wrapped 
    * in an `Option[T]`. If the operation is invalid or cannot be performed, 
    * `None` is returned.
    *
    * @param that the `ExactNumber` exponent to which the instance is raised
    * @return an `Option[T]` containing the result of the power operation if valid, 
    *         or `None` if the operation could not be performed
    */
  infix def pow(that: ExactNumber): Option[T]
}

/**
  * Trait `CanNormalize` provides a mechanism to normalize instances of type `T`.
  *
  * Normalization is a process of transforming an entity into a standard form, often employed in mathematical,
  * algebraic, or logical contexts to simplify representations or ensure a canonical structure.
  * Classes or traits that implement this trait must define how an instance of `T` is normalized.
  *
  * @tparam T the type of the entity that can be normalized
  */
trait CanNormalize[T <: Structure] {

  /**
    * Normalizes the current instance according to its specific mathematical or logical definition.
    *
    * @return an instance of type `T` which is a normalized form of the current instance
    */
  def normalize: T
}

/**
  * Trait `Can` represents an abstraction where a type `T` which extends `Structure` can cast an instance to itself.
  * It is the base trait for all other traits that extend `Can`.
  *
  * This trait facilitates type-safe casting of instances to a specific subtype of `Structure`.
  *
  * TODO try to redefine this so that it does not extend Structure.
  *
  * @tparam T the type parameter which must be a subtype of `Structure`
  */
sealed trait Can[T <: Structure : ClassTag] extends Structure {
  /**
    * Casts the current instance to the type parameter `T` of the enclosing `Can` trait.
    *
    * @return the current instance as an instance of type `T`
    */
  def asT: T = Structure.asT(this)
}
