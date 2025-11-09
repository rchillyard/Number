package com.phasmidsoftware.number.algebra

import com.phasmidsoftware.number.algebra.misc.FP
import com.phasmidsoftware.number.core.NumberException
import scala.language.implicitConversions

/**
  * Represents a pure number that can be compared, approximated, and converted to other types.
  *
  * `Number` is a trait that extends the `Scalar` trait, adding functionality for ordered comparison.
  *
  * CONSIDER why does it not extend Ordered[Number]?
  */
trait Number extends Scalar with Ordered[Scalar] with CanScale[Number] {
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
//        (this doPlus a).asInstanceOf[Option[Number]] // TODO check that this is OK and if not, fix it.
//      case (None, _) => None
//    }

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
  def doScale(that: Number): Option[Number] = scale(that)

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
}