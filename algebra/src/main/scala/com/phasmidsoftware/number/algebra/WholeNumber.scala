/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra

import algebra.ring.CommutativeRing
import cats.Show
import com.phasmidsoftware.number.algebra.Structure
import com.phasmidsoftware.number.algebra.WholeNumber.WholeNumberIsCommutativeRing
import com.phasmidsoftware.number.core.inner.{Factor, PureNumber, Rational}
import scala.language.implicitConversions
import scala.reflect.ClassTag
import spire.math.SafeLong

/**
  * A case class representing a whole number.
  *
  * The `WholeNumber` class models any integer in the Z domain and its associated operations,
  * enabling addition, inversion, comparison, and rendering.
  *
  * @param x a SafeLong value representing the whole number
  */
case class WholeNumber(x: SafeLong) extends CanAddAndSubtract[WholeNumber, WholeNumber] with CanMultiply[WholeNumber, WholeNumber] with Number with Q with Z {

  /**
    * Compares the current `WholeNumber` instance with another `Number` to determine their exact order.
    *
    * If the provided `Number` is a `WholeNumber`, this method compares their underlying SafeLong values.
    * If the provided `Number` is not a `WholeNumber`, the comparison cannot be performed, and `None` is returned.
    *
    * @param that the `Number` instance to compare with the current `WholeNumber` instance
    * @return an `Option[Int]`, where `Some(-1)` indicates that the current `WholeNumber` is less than the provided `WholeNumber`,
    *         `Some(0)` indicates that both WholeNumbers are equal, `Some(1)` indicates that the current `WholeNumber` is greater,
    *         and `None` is returned if the comparison cannot be made
    */
  def compareExact(that: Scalar): Option[Int] = that match {
    case WholeNumber(o) =>
      Some(x.compare(o))
//    case RationalNumber(r) =>
//      Some(Rational(x.toBigInt).compare(r))
    case _ =>
      None
  }

  /**
    * Converts the current number to a representation of the specified type `T`, if possible.
    *
    * This method attempts to convert the number to a type `T` that has implicit evidence
    * of `Ordering`. If the conversion is successful, it returns an `Option` containing the
    * resulting typed value. If the conversion is not valid or not possible for the given
    * type `T`, it returns `None`.
    *
    * @return an `Option` containing the converted value of type `T` if successful, or `None` if the conversion is not possible.
    */
  def convert[T <: Structure : ClassTag](t: T): Option[T] = t match {
//    case _: RationalNumber =>
//      val number = RationalNumber(Rational(x.toBigInt))
//      Some(number.asInstanceOf[T])
    case _: Real =>
      Some(Real(x.toDouble, None).asInstanceOf[T])
    case _ =>
      None
  }

  /**
    * Represents the additive identity element for the type `T`.
    *
    * The additive identity, commonly referred to as "zero," is the element in an
    * additive algebraic structure that, when added to any element of the structure,
    * results in the same element. For any element `x`, `x + zero` and `zero + x` should
    * equal `x`.
    */
  lazy val zero: WholeNumber = WholeNumberIsCommutativeRing.empty

  /**
    * Represents the multiplicative identity element of the structure.
    *
    * The `one` value serves as the neutral element for the multiplication operation, meaning
    * that for any instance `t` of type `T`, the equation `one * t = t * one = t` holds true.
    */
  lazy val one: WholeNumber = WholeNumber.one

  /**
    * Converts this `WholeNumber` into an `Int` representation, if possible.
    *
    * This method first converts the `WholeNumber` to its `Rational` representation and then 
    * attempts to extract the corresponding integer value. If the `WholeNumber` cannot be 
    * represented as an exact integer, the result is `None`.
    *
    * @return an `Option[Int]` containing the integer representation of this `WholeNumber`
    *         if it can be exactly represented as an `Int`, or `None` otherwise.
    */
  def toInt: Option[Int] = asRational.maybeInt

  /**
    * Converts this `Q` instance into its corresponding `Rational` representation.
    *
    * @return a `Rational` value representing this `WholeNumber`.
    */
  def asRational: Rational = Rational(x.toBigInt)

  /**
    * Scale this Real by the given scalar, provided that it is exact.
    * This method is used to scale a Real by a scalar that is known to be exact.
    * If you want to simply multiply this Real by a scalar, use the * operator.
    *
    * @param scalar the exact scalar to scale by
    * @return a scaled Real with the same relative error as this.
    */
  def scale(scalar: Scalar): Option[Number] =
    scalar match {
      case WholeNumber(i) =>
        Some(WholeNumber(x.toBigInt * i.toBigInt))
      case _ => // TODO add other cases as the become available
        None
    }

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
  def doScale(that: Number): Option[WholeNumber] =
    scale(that).asInstanceOf[Option[WholeNumber]] // TODO check this

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
  def doScaleInt(that: Int): Option[Monotone] =
    Some(WholeNumber(x * that))

  /**
    * Determines the sign of the scalar value represented by this instance.
    * Returns an integer indicating whether the value is positive, negative, or zero.
    *
    * @return 1 if the value is positive, -1 if the value is negative, and 0 if the value is zero
    */
  def signum: Int = x.compare(SafeLong.zero)

  /**
    * Determines if the current number is equal to zero.
    *
    * @return true if the number is zero, false otherwise
    */
  def isZero: Boolean = x == SafeLong.zero

  /**
    * Method to determine if this Structure object is exact.
    * For instance, `Number.pi` is exact, although if you converted it into a `PureNumber`, it would no longer be exact.
    *
    * @return true if this `Structure` object is exact in the context of No factor, else false.
    */
  override def isExact: Boolean = true

  /**
    * If this `Valuable` is exact, it returns the exact value as a `Double`.
    * Otherwise, it returns `None`.
    * NOTE: do NOT implement this method to return a Double for a Real--only for exact numbers.
    *
    * @return Some(x) where x is a Double if this is exact, else None.
    */
  def maybeDouble: Option[Double] =
    Some(x.toDouble)

  /**
    * Renders this `WholeNumber` instance as a string representation of its SafeLong.
    *
    * @return a string representation of the `WholeNumber`
    */
  def render: String = x.toString

  /**
    * Computes the additive inverse of the current `WholeNumber` instance.
    *
    * This method negates the current WholeNumber, returning a new `WholeNumber` instance
    * with the opposite value, relative to `WholeNumber.zero`.
    *
    * @return a new `WholeNumber` instance representing the additive inverse of the current WholeNumber.
    */
  def unary_- : WholeNumber =
    negate

//  /**
//    * Adds the specified `T` to this `T` instance.
//    *
//    * @param t an instance of `T` to be added to this `T`
//    * @return a new `T` representing the sum of this `T` and the given `T`
//    */
//  def +(t: WholeNumber): WholeNumber =
//    WholeNumberIsCommutativeRing.plus(this, t)
//
//  /**
//    * Subtracts the specified `T` from this `T` instance.
//    *
//    * @param t an instance of `T` to be subtracted from this `T`
//    * @return a new `Additive[T]` representing the result of the subtraction of the given `T` from this `T`
//    */
//  def -(t: WholeNumber): WholeNumber = doPlus(-t) match {
//    case Some(s) =>
//      s.asInstanceOf[WholeNumber]
//    case None =>
//      throw NumberException(s"WholeNumber.-: cannot subtract $t from $this")
//  }
//
//  /**
//    * Adds the given `Scalar` instance to the current instance, returning the result as an `Option[Scalar]`.
//    *
//    * If the provided `Scalar` is a `WholeNumber`, the method will calculate the sum and return it as `Some(WholeNumber)`.
//    * For other types of `Scalar`, the behavior is delegated to the `doPlus` method of the provided instance.
//    *
//    * @param that the `Scalar` instance to be added to the current instance
//    * @return an `Option[Scalar]` containing the result of the addition, or `None` if the addition is not valid
//    */
//  infix def doPlus(that: Scalar): Option[Scalar] = that match {
//    case a: WholeNumber =>
//      Some(this + a)
//    case x =>
//      x doPlus this
//  }

  /**
    * Computes the potential factor associated with this instance.
    *
    * @return Some(PureNumber)
    */
  def maybeFactor: Option[Factor] = Some(PureNumber)
//
//  /**
//    * Multiplies the specified `T` by this `T` instance.
//    *
//    * @param t an instance of `T` to be multiplied by this `T`
//    * @return a new `Multiplicative[T]` representing the product of this `T` and the given `T`
//    */
//  def *(t: WholeNumber): Multiplicative[WholeNumber] =
//    WholeNumberIsCommutativeRing.times(this, t)

  override def toString: String = x.toString
}

/**
  * The `WholeNumber` companion object contains utility methods, predefined constants, and
  * typeclass instances for working with WholeNumbers. WholeNumbers are represented using
  * rational numbers and comply with the algebraic structure of a commutative group.
  */
object WholeNumber {

  /**
    * Represents the additive identity for WholeNumbers.
    *
    * This value denotes zero, serving as the identity element in
    * the group structure of WholeNumbers.
    */
  lazy val zero: WholeNumber = WholeNumber(0L)
  lazy val one: WholeNumber = WholeNumber(1L)
  lazy val minusOne: WholeNumber = WholeNumber(-1L)
  lazy val two: WholeNumber = WholeNumber(2L)

  /**
    * Provides an implicit `Show` instance for the `WholeNumber` class, enabling conversion
    * of an `WholeNumber` instance to a string representation using its `render` method.
    *
    * This allows the `WholeNumber` class to integrate seamlessly with libraries or frameworks
    * requiring a `Show` typeclass instance for displaying or logging purposes.
    */
  implicit val showWholeNumber: Show[WholeNumber] = Show.show(_.render)

  /**
    * Converts an `Int` to a `WholeNumber` implicitly.
    *
    * This implicit conversion allows an `Int` to be seamlessly treated as a `WholeNumber`.
    *
    * @param x the integer value to be converted to a `WholeNumber`
    * @return the converted `WholeNumber` instance
    */
  implicit def convIntWholeNumber(x: Int): WholeNumber = WholeNumber(x)

//  implicit object wholeNumberIsAdditive extends Additive[WholeNumber] with WholeNumberIsCommutativeRing {
//    /**
//      * Represents the additive identity element for the type `T`.
//      *
//      * The additive identity, commonly referred to as "zero," is the element in an
//      * additive algebraic structure that, when added to any element of the structure,
//      * results in the same element. For any element `x`, `x + zero` and `zero + x` should
//      * equal `x`.
//      */
//    def zero: WholeNumber = empty
//
//    /**
//      * Adds the specified `T` to this `T` instance.
//      *
//      * @param t an instance of `T` to be added to this `T`
//      * @return a new `T` representing the sum of this `T` and the given `T`
//      */
//    def +(t1: WholeNumber, t2: WholeNumber): WholeNumber =
//      plus(t1, t2)
//
//    /**
//      * Subtracts the specified `T` from this `T` instance.
//      *
//      * @param t an instance of `T` to be subtracted from this `T`
//      * @return a new `Additive[T]` representing the result of the subtraction of the given `T` from this `T`
//      */
//    def -(t: WholeNumber): WholeNumber = negate(t)
//  }

  /**
    * Provides an implicit implementation of a commutative group for the `WholeNumber` type, supporting
    * group operations such as identity, combination, and inversion.
    *
    * This allows `WholeNumber` objects to adhere to the algebraic structure of a commutative group, where
    * the `combine` operation is associative and commutative, an identity element exists, and
    * each element has an additive inverse.
    */
  trait WholeNumberIsCommutativeRing extends CommutativeRing[WholeNumber] {
    /**
      * Provides the identity element for the `WholeNumber` group, representing a WholeNumber of zero.
      *
      * @return the `WholeNumber` instance zero.
      */
    def empty: WholeNumber = WholeNumber.zero

    /**
      * Computes the additive inverse of the given `WholeNumber`.
      *
      * This method negates the input WholeNumber, returning a `WholeNumber` instance
      * that represents its additive inverse, relative to `WholeNumber.zero`.
      *
      * @param x the `WholeNumber` instance to be inverted
      * @return a new `WholeNumber` instance representing the additive inverse of the input
      */
    def negate(x: WholeNumber): WholeNumber = WholeNumber(-x.x)

    /**
      * Returns the zero value of the `WholeNumber` type.
      *
      * @return The zero value representing a `WholeNumber` with the value of 0.
      */
    def zero: WholeNumber = WholeNumber.zero

    /**
      * Adds two `WholeNumber` instances together.
      *
      * This method computes the sum of the two provided `WholeNumber` objects 
      * and returns a new `WholeNumber` representing the result.
      *
      * @param x the first `WholeNumber` operand
      * @param y the second `WholeNumber` operand
      * @return a new `WholeNumber` instance representing the sum of `x` and `y`
      */
    def plus(x: WholeNumber, y: WholeNumber): WholeNumber =
      WholeNumber(x.x + y.x)

    /**
      * Returns the constant representing the number one as a `WholeNumber`.
      *
      * @return a `WholeNumber` instance representing the numeric value of one.
      */
    def one: WholeNumber = WholeNumber.one

    /**
      * Multiplies two `WholeNumber` instances.
      *
      * This method computes the product of the two provided `WholeNumber` objects
      * and returns a new `WholeNumber` representing the result.
      *
      * @param x the first `WholeNumber` operand
      * @param y the second `WholeNumber` operand
      * @return a new `WholeNumber` instance representing the product of `x` and `y`
      */
    def times(x: WholeNumber, y: WholeNumber): WholeNumber = WholeNumber(x.x * y.x)
  }

  /**
    * Provides an implicit implementation of a commutative group for the `WholeNumber` type, supporting
    * group operations such as identity, combination, and inversion.
    *
    * This allows `WholeNumber` objects to adhere to the algebraic structure of a commutative group, where
    * the `combine` operation is associative and commutative, an identity element exists, and
    * each element has an additive inverse.
    */
  implicit object WholeNumberIsCommutativeRing extends WholeNumberIsCommutativeRing
}
