package com.phasmidsoftware.number.algebra

import algebra.ring.AdditiveCommutativeMonoid
import com.phasmidsoftware.number.algebra.misc.FP
import com.phasmidsoftware.number.core.NumberException
import scala.annotation.tailrec
import scala.language.implicitConversions

/**
  * Represents a pure number that can be compared, approximated, and converted to other types.
  *
  * `Number` is a trait that extends the `Scalar` trait, adding functionality for ordered comparison.
  *
  * CONSIDER why does it not extend Ordered[Number]?
  */
trait Number extends Scalar with Ordered[Scalar] {
  /**
    * Compares this `Number` instance with a `Scalar` instance.
    *
    * This method matches the type of the input `Scalar` and performs a comparison based on its type. If the input is a `Number`,
    * it delegates to the `compare(Number)` method. If the input is a `Radians`, it first attempts to convert the `Radians`
    * into a comparable value and then performs the comparison. If the conversion is not possible, it throws a `NumberException`.
    *
    * @param that the `Scalar` instance to compare the current instance against.
    * @return an integer value:
    *         - a negative value if the current instance is less than `that`.
    *         - zero if the current instance is equal to `that`.
    *         - a positive value if the current instance is greater than `that`.
    */
  def compare(that: Scalar): Int = that match {
    case number: Number =>
      compare(number)
    case radians: Radians =>
      radians.convert(this) match {
        case Some(x) =>
          compare(x)
        case None =>
          throw NumberException(s"Number.compare(Scalar): logic error: $this, $that")
      }
  }

  /**
    * Compares the current `Number` instance with another `Number` instance.
    *
    * This method performs a comparison of two `Number` instances. If both numbers are exact, it uses exact comparison.
    * If one or both numbers are not exact, it attempts to approximate and compare. Note that in some cases, this method
    * may throw an exception if an invalid approximation logic is encountered.
    *
    * @param that the `Number` instance to compare the current instance against
    * @return an integer value:
    *         - a negative value if this `Number` is less than `that`
    *         - zero if this `Number` is equal to `that`
    *         - a positive value if this `Number` is greater than `that`
    */
  def compare(that: Number): Int =
    if isExact && that.isExact then // XXX both are exact
      FP.recover(compareExact(that))(NumberException(s"Number.compare(Number): logic error: $this, $that"))
    else if !isExact then { // XXX this is not exact
      val maybeInt: Option[Int] = for
        x <- approximation
        y <- that.approximation
      yield x.compare(y)
      FP.recover(maybeInt)(NumberException("Number.compare: Logic error"))
    }
    else // XXX this is exact and that is not exact
      -that.compare(this)

  /**
    * Provides an approximation of this number, if applicable.
    *
    * This method attempts to compute an approximate representation of the number
    * in the form of a `Real`, which encapsulates uncertainty or imprecision
    * in its value. If no meaningful approximation is possible for the number, it
    * returns `None`.
    *
    * CONSIDER moving this method up into Scalar.
    *
    * @return an `Option[Real]` containing the approximate representation
    *         of this `Number`, or `None` if no approximation is available.
    */
  def approximation: Option[Real] =
    convert(Real.zero)
//
//  /**
//    * Performs a multiplication operation on the current `Number` instance by repeated addition.
//    *
//    * This method calculates the result of multiplying the current `Number` instance by an integer `n`
//    * by repeatedly adding the instance to itself `n - 1` times.
//    *
//    * This is really scaleInt(n).
//    *
//    * @param n the multiplier, an integer value by which the current `Number` instance is to be multiplied
//    * @return a new `Number` instance representing the result of the multiplication
//    */
//  def *(n: Int): Option[Number] =
//    Range(1, n).foldLeft[Option[Number]](Some(this)) { // CONSIDER putting "Range(1,n)" back to "1 until n".
//      case (Some(a), _) =>
//        (this plus a).asInstanceOf[Option[Number]] // TODO check that this is OK and if not, fix it.
//      case (None, _) => None
//    }
//
//  /**
//    * Scales the current instance of type `T` using the given `Number` multiplier.
//    *
//    * This method performs a scaling operation by multiplying the current instance
//    * with the provided `Number`. The result of the scaling operation is returned
//    * as an `Option`, allowing for cases where the operation might not be valid or
//    * possible.
//    *
//    * @param that the `Number` multiplier used to scale the current instance
//    * @return an `Option[T]` containing the scaled instance of type `T`, or `None` if the operation cannot be performed
//    */
//  def doScale(that: Number): Option[Number] = scale(that)

  /**
    * Scale this Real by the given scalar, provided that it is exact.
    * This method is used to scale a Real by a scalar that is known to be exact.
    * If you want to simply multiply this Real by a scalar, use the * operator.
    *
    * @param scalar the exact scalar to scale by
    * @return a scaled Real with the same relative error as this.
    */
  def scale(scalar: Scalar): Option[Number]

  /**
    * A scale factor applied to this `Number`.
    *
    * The `scaleFactor` represents a multiplier that influences computations or adjustments involving this number.
    * It is commonly used to scaleFactor or manipulate the magnitude of the number in various arithmetic or operational contexts.
    */
  lazy val scaleFactor: Double = 1.0
}

/**
  * The `Number` object provides predefined constants and implicit conversions related to numerical operations.
  * It includes representations of common numbers and utilities to work with the `Number` type.
  */
object Number {
  /**
    * Represents the value `0` as an instance of `WholeNumber`.
    * It is a predefined constant in the `Number` object.
    */
  val zero: Number = WholeNumber.zero
  /**
    * Represents the number one as a `WholeNumber` instance.
    */
  val one: Number = WholeNumber.one
  /**
    * Represents the value `-1` as an instance of `WholeNumber`.
    * It is a predefined constant in the `Number` object.
    */
  val minusOne: Number = WholeNumber.minusOne
  /**
    * Represents the value `2` as an instance of `WholeNumber`.
    * It is a predefined constant in the `Number` object.
    */
  val two: Number = WholeNumber.two
  /**
    *
    */
  implicit def convIntToNumber(x: Int): Number = WholeNumber(x)

  /**
    * Provides an implicit additive commutative monoid implementation for the `Number` type.
    *
    * This object defines the `zero` element and the `plus` operation for the `Number` type,
    * conforming to the algebraic structure of an additive commutative monoid.
    *
    * The `zero` element acts as the additive identity, and the `plus` operation is
    * both associative and commutative, enabling addition of two `Number` instances.
    */
  implicit object NumberIsAdditiveCommutativeMonoid extends AdditiveCommutativeMonoid[Number] {
    /**
      * Returns the additive identity element for the `Number` type.
      *
      * This method provides the `zero` element, which serves as the neutral element in
      * addition operations for instances of `Number`. Adding `zero` to any `Number`
      * instance will result in the same `Number` instance.
      *
      * @return the additive identity element for `Number`
      */
    def zero: Number = Number.zero

    /**
      * Adds two `Number` instances together.
      *
      * The method determines the type of the given `Number` instances and performs the appropriate
      * addition operation. It supports addition for `Real`, `RationalNumber`, and `WholeNumber` types.
      * If the types are incompatible or cannot be converted to corresponding types, it throws a `NumberException`.
      *
      * This method is marked as `@tailrec` to optimize recursive addition operations.
      *
      * @param x the first `Number` instance
      * @param y the second `Number` instance
      * @return a `Number` instance representing the result of the addition of `x` and `y`
      * @throws NumberException if the types of `x` and `y` cannot be added
      */
    @tailrec
    def plus(x: Number, y: Number): Number = (x, y) match {
      case (a: Real, b: Real) =>
        a + b
      case (a, b: Real) =>
        plus(y, x)
      case (a: Real, b) =>
        FP.recover(b.convert(Real.zero).map(_ + a))(NumberException("Number.plus: logic error: cannot convert " + b + " to a Real"))
      case (a: RationalNumber, b: RationalNumber) =>
        a + b
      case (a, b: RationalNumber) =>
        plus(y, x)
      case (a: RationalNumber, b) =>
        FP.recover(b.convert(RationalNumber.zero).map(a + _))(NumberException("Number.plus: logic error: cannot convert " + b + " to a RationalNumber"))
      case (a: WholeNumber, b: WholeNumber) =>
        a + b
      case (a, b: WholeNumber) =>
        plus(y, x)
      case _ =>
        throw NumberException("Number.plus: logic error: cannot add " + x + " and " + y)
    }

  }
}