/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra

import com.phasmidsoftware.flog.Loggable
import com.phasmidsoftware.number.algebra.misc.*
import com.phasmidsoftware.number.core.inner.{Factor, PureNumber, Rational, Value}
import com.phasmidsoftware.number.core.numerical.{ExactNumber, Field, FuzzyNumber}
import com.phasmidsoftware.number.core.{inner, numerical}
import com.phasmidsoftware.number.{algebra, core}
import scala.language.implicitConversions
import scala.util.Try

/**
  * TODO rework this doc.
  * A trait representing an object that is in some sense numerical and has a value (or possibly more than one value).
  * `Valuable` does not define an order because a sub-class may not be comparable, for example, a complex number.
  *
  * The properties exposed by this trait are: `isExact`, `approximation`, `maybeDouble`.
  *
  * NOTE: this trait has the same name as the `Valuable` typeclass in the `com.phasmidsoftware.number` package,
  * but it is not the same thing.
  */
trait Valuable extends Renderable with Numeric {

  /**
    * Determines whether this `Valuable` is exact, i.e., has no approximation.
    *
    * CONSIDER it may be possible that there are non-approximatable entities that are not exact either.
    *
    * The method returns `true` if there is no approximate representation
    * available (i.e., `approximation` is `None`), indicating that the
    * entity is exact. Otherwise, it returns `false`.
    *
    * @return a `Boolean` indicating whether the entity is exact (`true`)
    *         or has an approximation (`false`).
    */
  def isExact: Boolean

  /**
    * Attempts to retrieve a factor based on the provided context.
    * This method evaluates whether there is an applicable factor within the given context.
    *
    * @param context the context in which the factor is evaluated.
    * @return an optional `Factor` if one qualifies under the provided context; otherwise, `None`.
    */
  def maybeFactor(context: Context): Option[Factor]
}

/**
  * Object `Valuable` provides utility methods and implicit conversions related to the `Valuable` trait,
  * enabling parsing and conversion of strings to `Valuable` representations.
  */
object Valuable {

  /**
    * TODO change the type of the input to `Eager`.
    *
    * Converts a `Valuable` instance into a `Field` representation.
    * If the conversion fails, it recovers by throwing a `AlgebraException`
    * with an appropriate error message indicating the failure.
    *
    * @param v the `Valuable` instance to be converted into a `Field`.
    *          This is expected to represent a numerical value.
    * @return the `Field` representation of the input `Valuable`.
    *         If conversion is not possible, a `AlgebraException` is thrown.
    */
  def valuableToField(v: Valuable): Field =
    FP.recover(valuableToMaybeField(v))(AlgebraException(s"ExpressionFunction:valuableToField: Cannot convert $v to a Field"))

  /**
    * Attempts to convert a given eager `Valuable` instance into an `Option[Field]`.
    * This method performs pattern matching on the input `Valuable` object to map
    * it to a corresponding `Field` representation if possible. If no valid mapping
    * exists, it returns `None`.
    *
    * TODO change the type of the input parameter to `Eager`.
    *
    * @param v the `Valuable` instance that is to be converted into an `Option[Field]`.
    * @return `Some(Field)` if the conversion is successful, or `None` if the
    *         `Valuable` cannot be converted.
    */
  def valuableToMaybeField(v: Valuable): Option[Field] = v match {
    case Complex(complex) =>
      Some(complex)
    case nat: Nat =>
      Some(intToField(nat.toInt, PureNumber))
    case com.phasmidsoftware.number.algebra.Real(x, fo) =>
      Some(numerical.Real(FuzzyNumber(Value.fromDouble(Some(x)), PureNumber, fo)))
    case q: Q =>
      Some(rationalToField(q.toRational, PureNumber))
    case a@Angle(radians, _) =>
      Some(numerical.Real(numberToField(radians).x.make(inner.Radian)))
    case l@NatLog(x) =>
      Some(numerical.Real(numberToField(x).x.make(inner.NatLog)))
    case _ =>
      None // XXX v should be an Expression in this case (but expressions are not known in this package).
  }

  /**
    * Extractor method to convert a `Valuable` instance into an `Option` containing its corresponding `Field` representation.
    * This allows for safe pattern matching and handling of `Valuable` objects that may or may not be convertible to a `Field`.
    *
    * TODO we should move this method to the companion object of `Eager`.
    *
    * @param v the `Valuable` instance to be converted into an `Option[Field]`
    * @return `Some(Field)` if the conversion is successful, or `None` if it fails
    */
  def unapply(v: Valuable): Option[numerical.Field] =
    valuableToMaybeField(v)

  /**
    * Converts a given string into a `Valuable` representation.
    * This method allows implicit conversion from `String` to `Valuable`.
    *
    * @param w the input string to be converted into a `Valuable`.
    * @return a `Valuable` instance parsed from the provided string.
    */
  implicit def toValuable(w: String): Valuable = Eager(w)

  /**
    * Implicit object `LoggableValuable` provides a `Loggable` implementation for the `Valuable` type.
    * This allows `Valuable` instances to be formatted as strings suitable for logging purposes.
    *
    * The implementation utilizes the `toString` method of the `Valuable` instance
    * to generate the log representation.
    */
  implicit object LoggableValuable extends Loggable[Valuable] {
    def toLog(t: Valuable): String = t.toString
  }

  /**
    * Converts a `Number` into a corresponding `Field` representation.
    * The input number is matched against various cases to determine its specific type
    * (e.g., RationalNumber, Real, WholeNumber) and is subsequently converted.
    * If the conversion is not supported for the given `Number` type, a `AlgebraException` is thrown.
    *
    * @param number the `Number` to be converted into a `Field`. It can represent different
    *               numerical types such as RationalNumber, algebra.Real, or WholeNumber.
    * @throws AlgebraException if the input `Number` cannot be converted into a `Field`.
    */
  private def numberToField(number: Number) = number match {
    case RationalNumber(r, _) =>
      rationalToField(r, PureNumber)
    case algebra.Real(x, fo) =>
      numerical.Real(FuzzyNumber(Value.fromDouble(Some(x)), PureNumber, fo))
    case WholeNumber(x) =>
      numerical.Real(ExactNumber(Value.fromRational(Rational(x)), PureNumber))
    case _ =>
      throw AlgebraException(s"Valuable.numberToField: Cannot convert $number to a Field")
  }

  /**
    * Converts a `Rational` value into a `Field` representation, taking into account a specific scaling factor.
    *
    * @param rational the `Rational` number to be converted into a `Field`.
    * @param factor   the scaling `Factor` to be applied to the conversion.
    * @return a `Real` representation of the `Rational` number scaled by the given `Factor`.
    */
  private def rationalToField(rational: Rational, factor: Factor) = numerical.Real(ExactNumber(Value.fromRational(rational), factor))

  /**
    * Converts an integer value into a `Field` representation, encapsulated as a `Real`,
    * based on the provided multiplication factor.
    *
    * @param x      the integer value to be converted into a `Field`.
    * @param factor the factor to be applied in the `Real` representation.
    * @return a `Real` representing the converted integer value.
    */
  private def intToField(x: Int, factor: Factor) = numerical.Real(ExactNumber(Value.fromInt(x), factor))

}

/**
  * Represents a trait for performing dyadic operations on instances of the `Eager` type.
  *
  * This trait provides methods for equality checks, fuzzy comparisons, ordering, and arithmetic operations.
  *
  * NOTE that compare, fuzzyCompare, and sum are not defined in this trait, have two parameters where one would be sufficient (and probably better).
  */
trait DyadicOps {
  /**
    * Compares this `Eager` instance with another `Eager` instance for equality.
    *
    * @param that the `Eager` instance to be compared against.
    * @return a `Try[Boolean]` indicating whether the two `Eager` instances are equal.
    *         A successful result contains `true` if they are equal, or `false` if not.
    *         If the comparison cannot be performed, a failure with an appropriate exception is returned.
    */
  def eqv(that: Eager): Try[Boolean]

  /**
    * Determines if the current `Eager` instance is approximately equal to another `Eager` instance 
    * within a specified tolerance.
    *
    * @param p    the tolerance level as a `Double`. A smaller value indicates stricter equality requirements.
    * @param that the `Eager` instance to compare against.
    * @return a `Try[Boolean]` indicating the result of the fuzzy equality comparison:
    *         - `true` if the two instances are approximately equal within the tolerance `p`.
    *         - `false` otherwise.
    *         If the comparison cannot be performed, the `Try` contains a failure with an appropriate exception.
    */
  def fuzzyEqv(p: Double)(that: Eager): Try[Boolean]

  /**
    * Compares two instances of the `Eager` type and returns the result of the comparison.
    *
    * @param x the first `Eager` instance to compare
    * @param y the second `Eager` instance to compare
    * @return a `Try[Int]` containing the result of the comparison, where a negative value indicates that `x` is less than `y`, zero indicates equality, and a positive value indicates
    *         that `x` is greater than `y`
    */
  def compare(x: Eager, y: Eager): Try[Int]

  /**
    * Compares two instances of `Eager` using a fuzzy comparison approach with a specified tolerance.
    * The comparison determines their relative ordering.
    *
    * @param p the tolerance level as a `Double`; smaller values indicate stricter thresholds for comparison.
    * @param x the first `Eager` instance to compare.
    * @param y the second `Eager` instance to compare.
    * @return a `Try[Int]` indicating the result of the comparison:
    *         - A negative integer if `x` is less than `y`.
    *         - Zero if `x` is approximately equal to `y` within the specified tolerance `p`.
    *         - A positive integer if `x` is greater than `y`.
    *         - A `Failure` containing an exception if the comparison cannot be performed.
    */
  def fuzzyCompare(p: Double)(x: Eager, y: Eager): Try[Int]

  /**
    * Computes the sum of two values wrapped in the Eager type and returns the result as a Try containing an Eager value.
    *
    * @param x the first operand of type Eager
    * @param y the second operand of type Eager
    * @return a Try containing the result of the sum operation as an Eager value if successful, or a failure if an error occurs
    */
  def sum(x: Eager, y: Eager): Try[Eager]
}
