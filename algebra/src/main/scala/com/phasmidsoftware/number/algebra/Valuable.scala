/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra

import com.phasmidsoftware.flog.Loggable
import com.phasmidsoftware.number.algebra.misc.{FP, MaybeNumeric, Renderable}
import com.phasmidsoftware.number.core.algebraic.Algebraic
import com.phasmidsoftware.number.core.inner.{Factor, PureNumber, Rational, Value}
import com.phasmidsoftware.number.core.numerical.{ExactNumber, Field, FuzzyNumber, NumberException, NumberExceptionWithCause, Real}
import com.phasmidsoftware.number.core.{inner, numerical}
import com.phasmidsoftware.number.parse.NumberParser
import com.phasmidsoftware.number.{algebra, core}
import scala.language.implicitConversions
import scala.util.{Failure, Success}

/**
  * A trait representing an object that is in some sense numerical and has a value (or possibly more than one value).
  * `Valuable` does not define an order because a sub-class may not be comparable, for example, a complex number.
  *
  * The properties exposed by this trait are: `isExact`, `approximation`, `maybeDouble`.
  *
  * NOTE: this trait has the same name as the `Valuable` typeclass in the `com.phasmidsoftware.number` package,
  * but it is not the same thing.
  */
trait Valuable extends Renderable with MaybeNumeric {

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
  // CONSIDER moving these to Eager
  lazy val zero: Eager = Number.zero
  lazy val one: Eager = Number.one
  lazy val minusOne: Eager = Number.minusOne
  lazy val two: Eager = Scalar(2)
  lazy val half: Eager = RationalNumber(Rational.half)
  lazy val ten: Eager = Scalar(10)
  lazy val pi: Eager = Angle.pi
  lazy val piBy2: Eager = Angle.piBy2
  lazy val piBy4: Eager = Angle.piBy4
  lazy val e: Eager = NatLog.e
  lazy val infinity: Eager = RationalNumber(Rational.infinity)
  lazy val negInfinity: Eager = RationalNumber(Rational.negInfinity)
  lazy val root2: Eager = InversePower(2, 2)
  lazy val root3: Eager = InversePower(2, 3)

  /**
    * TODO change the type of the input to `Eager`.
    *
    * Converts a `Valuable` instance into a `Field` representation.
    * If the conversion fails, it recovers by throwing a `NumberException`
    * with an appropriate error message indicating the failure.
    *
    * @param v the `Valuable` instance to be converted into a `Field`.
    *          This is expected to represent a numerical value.
    * @return the `Field` representation of the input `Valuable`.
    *         If conversion is not possible, a `NumberException` is thrown.
    */
  def valuableToField(v: Valuable): Field =
    FP.recover(valuableToMaybeField(v))(NumberException(s"ExpressionFunction:valuableToField: Cannot convert $v to a Field"))

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
      Some(Real(numberToField(radians).x.make(inner.Radian)))
    case l@NatLog(x) =>
      Some(Real(numberToField(x).x.make(inner.NatLog)))
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
    * If the conversion is not supported for the given `Number` type, a `NumberException` is thrown.
    *
    * @param number the `Number` to be converted into a `Field`. It can represent different
    *               numerical types such as RationalNumber, algebra.Real, or WholeNumber.
    * @throws NumberException if the input `Number` cannot be converted into a `Field`.
    */
  private def numberToField(number: Number) = number match {
    case RationalNumber(r, _) =>
      rationalToField(r, PureNumber)
    case algebra.Real(x, fo) =>
      numerical.Real(FuzzyNumber(Value.fromDouble(Some(x)), PureNumber, fo))
    case WholeNumber(x) =>
      Real(ExactNumber(Value.fromRational(Rational(x.toBigInt)), PureNumber))
    case _ =>
      throw NumberException(s"Valuable.numberToField: Cannot convert $number to a Field")
  }

  /**
    * Converts a `Rational` value into a `Field` representation, taking into account a specific scaling factor.
    *
    * @param rational the `Rational` number to be converted into a `Field`.
    * @param factor   the scaling `Factor` to be applied to the conversion.
    * @return a `Real` representation of the `Rational` number scaled by the given `Factor`.
    */
  private def rationalToField(rational: Rational, factor: Factor) = Real(ExactNumber(Value.fromRational(rational), factor))

  /**
    * Converts an integer value into a `Field` representation, encapsulated as a `Real`,
    * based on the provided multiplication factor.
    *
    * @param x      the integer value to be converted into a `Field`.
    * @param factor the factor to be applied in the `Real` representation.
    * @return a `Real` representing the converted integer value.
    */
  private def intToField(x: Int, factor: Factor) = Real(ExactNumber(Value.fromInt(x), factor))

}

/**
  * Trait `Eager` extends `Valuable` and is used to represent entities that evaluate their values eagerly.
  * That's to say, `Valuable` objects that do not extend `Expression`.
  * At present, `Eager` is extended by `Structure, Complex`, and `Nat`.
  *
  * Unlike lazy evaluation, eager evaluation computes and stores the value immediately when the entity is created
  * or instantiated. This behavior can be useful in scenarios where prompt computation is essential, and
  * deferred or lazy evaluation may introduce undesired complexities or delays.
  *
  * `Eager` does not introduce additional properties or methods but serves as a marker trait
  * that confirms the eager nature of an extending type.
  */
trait Eager extends Valuable

/**
  * The `Eager` object provides factory methods to create instances of `Valuable` entities
  * that are evaluated eagerly. These entities can represent numerical values parsed from
  * strings, long integers, or specific types of mathematical fields.
  */
object Eager {

  /**
    * Parses the given string into a `Valuable` representation. If the string cannot be parsed
    * into a valid `Number`, an exception is thrown.
    *
    * @param str the input string representing a numerical value.
    * @return a `Valuable` representation of the parsed `Number`.
    * @throws NumberExceptionWithCause if parsing the string fails.
    */
  def apply(str: String): Eager =
    NumberParser.parseNumber(str) match {
      case Success(number) =>
        Scalar(number)
      case Failure(exception) =>
        throw NumberExceptionWithCause("Valuable.apply", exception)
    }

  /**
    * Creates a `Valuable` instance representing the given long value.
    *
    * @param x the input value of type `Long` to be wrapped in a `Valuable` representation.
    * @return a `Valuable` object corresponding to the input value.
    */
  def apply(x: Long): Eager = WholeNumber(x)

  /**
    * Creates a `Valuable` instance based on the given `Field`.
    * If the `Field` is a `Real` object, it converts it into a `Scalar` representation.
    * Otherwise, it throws an `IllegalArgumentException`.
    *
    * @param field the input field to be converted into a `Valuable`. It is expected
    *              to be of type `com.phasmidsoftware.number.core.Real`.
    * @return a `Valuable` representation of the input `Field` as a `Scalar`.
    * @throws IllegalArgumentException if the provided `Field` is not of type `Real`.
    */
  def apply(field: numerical.Field): Eager =
    field match {
      case numerical.Real(n) =>
        Scalar(n)
      case c: numerical.Complex =>
        Complex(c)
      case a: Algebraic =>
        throw NumberException(s"Valuable.apply: Algebraic not yet implemented: $field")
    }

}
