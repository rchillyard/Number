/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.core

import com.phasmidsoftware.flog.Loggable
import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.eager.{Angle, Complex, Eager, InversePower, Monotone, Nat, NatLog, Number, RationalNumber, Real, WholeNumber}
import com.phasmidsoftware.number.algebra.util.AlgebraException
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
trait Valuable extends Renderable with Numeric with Zeroable with Exactitude with Normalizable[Valuable] with TypeSafe {

  /**
    * Converts the current `Lazy` instance into an `Eager` instance by forcing
    * the resolution or evaluation of its underlying value.
    *
    * The `materialize` method ensures that the computation of the value represented
    * by this `Lazy` instance is completed and encapsulated within an appropriate
    * `Eager` representation. This is particularly useful in scenarios where
    * deferred computation needs to be explicitly resolved prior to further processing.
    *
    * @return the materialized `Eager` instance representing the resolved value
    */
  def materialize: Eager

  /**
    * Obtains the product of this `Valuable` instance and another `Valuable` instance.
    * The behavior of the method is determined by whether the instances are of type `Lazy` or `Eager`.
    * If the provided instances do not conform to the expected types, an `AlgebraException` is thrown.
    * CONSIDER supporting `sum` also.
    *
    * CONSIDER using lazify
    *
    * @param other the other `Valuable` instance to compute the product with
    * @return a `Valuable` instance representing the product of this and the other instance
    * @note throws AlgebraException if the computation involves unsupported `Valuable` types
    */
  def product(other: Valuable): Valuable = (this, other) match {
    case (a: Lazy, b: Lazy) =>
      a.multiply(b)
    case (a: Lazy, b: Eager) =>
      a.multiply(a.unit(b))
    case (b: Eager, a: Lazy) =>
      a.multiply(a.unit(b))
    case (a: Eager, b: Eager) =>
      a.multiply(b).get // NOTE this should never fail. Famous last words!
    case _ => // NOTE this can never happen since the only two subtypes of Valuable are Eager and Lazy.
      throw AlgebraException(s"product: expected Lazy/Eager values but got $this and $other")
  }

  /**
    * Attempts to retrieve a factor based on the provided context.
    * This method evaluates whether there is an applicable factor within the given context.
    *
    * @param context the context in which the factor is evaluated.
    * @return an optional `Factor` if one qualifies under the provided context; otherwise, `None`.
    */
  def maybeFactor(context: Context): Option[Factor]

  /**
    * Casts this `Valuable` instance into a `Monotone` type if it already is a `Monotone`.
    * If the instance is not a `Monotone`, an `AlgebraException` is thrown.
    * CONSIDER returning Option[Monotone] instead.
    *
    * @return the current instance as a `Monotone`
    * @note Throws [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the instance is not of type `Monotone`
    */
  def asMonotone: Monotone = this match
    case m: Monotone => m
    case _ => throw AlgebraException(s"asMonotone: expected Monotone value but got $this")

  /**
    * Attempts to cast this instance to a `Number`.
    * If the instance is of type `Number`, it is wrapped in an `Option` and returned.
    * Otherwise, returns `None`.
    *
    * @return an `Option` containing the instance as a `Number` if applicable, or `None` if not.
    */
  def asNumber: Option[Number] = this match
    case n: Number => Some(n)
    case _ => None
}

/**
  * The `Lazy` trait extends the `Valuable` interface and represents an abstraction
  * for objects that are lazy in their evaluation or resolution. This trait provides
  * methods for simplification, materialization, and normalization of the underlying value.
  *
  * The key distinction of `Lazy` implementations is their support for deferred computation,
  * whereby full resolution or evaluation may not be required unless explicitly invoked.
  */
trait Lazy extends Valuable {

  /**
    * Simplifies the given expression or computation and returns a lazy evaluation result.
    *
    * @return a lazy evaluation result representing the simplified expression or computation
    */
  def simplify: Lazy

  /**
    * Normalizes the current `Lazy` instance to its simplest `Valuable` equivalent.
    * The method first attempts to simplify the instance using the `simplify` method.
    * If the simplified result is exact, the method materializes it and normalizes the resulting value.
    * Otherwise, the simplified value is returned as is.
    *
    * @return the simplest `Valuable` representation of this value after normalization
    */
  def normalize: Valuable = {
    val simplified = simplify
    if (simplified.isExact)
      simplified.materialize.normalize
    else
      simplified
  }

  /**
    * Multiplies this `Lazy` instance with another `Lazy` instance, producing
    * a new `Lazy` result that represents the product of the two inputs.
    *
    * CONSIDER we should probably orivude an `add` method as well.
    *
    * @param other the `Lazy` instance to multiply with this instance
    * @return a new `Lazy` instance representing the product of the two inputs
    */
  def multiply(other: Lazy): Lazy

  /**
    * Transforms an instance of the `Eager` type into a `Lazy` instance, allowing for deferred
    * computation or evaluation.
    * NOTE that this method ignores `this`.
    *
    * @param x an instance of `Eager` that is to be converted into a `Lazy` representation
    * @return a `Lazy` instance representing the deferred computation or evaluation of the input
    */
  def unit(x: Eager): Lazy
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
}

/**
  * Object `Valuable` provides utility methods and implicit conversions related to the `Valuable` trait,
  * enabling parsing and conversion of strings to `Valuable` representations.
  */
object Valuable {

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
    case Real(x, fo) =>
      Some(numerical.Real(FuzzyNumber(Value.fromDouble(Some(x)), PureNumber, fo)))
    case q: Q =>
      Some(rationalToField(q.toRational, PureNumber))
    case a@Angle(radians, _) =>
      Some(numerical.Real(numberToField(radians).x.make(inner.Radian)))
    case l@NatLog(x) =>
      Some(numerical.Real(numberToField(x).x.make(inner.NatLog)))
    case InversePower(n, x) =>
      Some(numberToField(x).power(ExactNumber(Value.fromRational(Rational(n).invert), PureNumber)))
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
    * Implicitly converts a `Valuable` instance into its string representation.
    * This is achieved by invoking the `render` method of the `Valuable` object.
    *
    * @param v the `Valuable` instance to be converted into a `String`.
    * @return the string representation of the provided `Valuable` object, as rendered by its `render` method.
    */
  implicit def toString(v: Valuable): String = v.render

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
    * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the input `Number` cannot be converted into a `Field`.
    */
  private def numberToField(number: Number) = number match {
    case RationalNumber(r, _) =>
      rationalToField(r, PureNumber)
    case Real(x, fo) =>
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
