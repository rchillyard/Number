/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra

import com.phasmidsoftware.flog.Loggable
import com.phasmidsoftware.number.algebra.misc.Renderable
import com.phasmidsoftware.number.core
import com.phasmidsoftware.number.core.algebraic.Algebraic
import com.phasmidsoftware.number.core.inner.{Factor, Rational}
import com.phasmidsoftware.number.core.{Constants, NumberException, NumberExceptionWithCause}
import com.phasmidsoftware.number.parse.NumberParser
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/**
  * A trait representing an object that is in some sense numerical and has a value (or possibly more than one value).
  * `Valuable` does not define an order because a sub-class may not be comparable, for example, a complex number.
  *
  * The properties exposed by this trait are: `isExact`, `approximation`, `maybeDouble`.
  *
  * NOTE: this trait has the same name as the `Valuable` typeclass in the `com.phasmidsoftware.number` package,
  * but it is not the same thing.
  */
trait Valuable extends Renderable {

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
    * If this `Valuable` is exact, it returns the exact value as a `Double`.
    * Otherwise, it returns `None`.
    * NOTE: do NOT implement this method to return a Double for a fuzzy Real--only for exact numbers.
    *
    * @return Some(x) where x is a Double if this is exact, else None.
    */
  def maybeDouble: Option[Double]

  /**
    * Optionally retrieves a factor associated with this `Valuable` if one exists (this is a Scalar).
    *
    * Factors are components or divisors related to the numerical value represented 
    * by this `Valuable`. If no such factor exists or is applicable, the result will 
    * be `None`.
    *
    * @return an `Option` containing the `Factor` if available, otherwise `None`.
    */
  def maybeFactor: Option[Factor]
}

/**
  * Object `Valuable` provides utility methods and implicit conversions related to the `Valuable` trait,
  * enabling parsing and conversion of strings to `Valuable` representations.
  */
object Valuable {
  lazy val zero: Valuable = Number.zero
  lazy val one: Valuable = Number.one
  lazy val minusOne: Valuable = Number.minusOne
  lazy val two: Valuable = Scalar(2)
  //  lazy val half: Valuable = RationalNumber(Rational.half)
  lazy val ten: Valuable = Scalar(10)
  //  lazy val pi: Valuable = Angle.pi
//  lazy val piBy2: Valuable = Angle.piBy2
//  lazy val piBy4: Valuable = Angle.piBy4
//  lazy val e: Valuable = NatLog.e
//  lazy val infinity: Valuable = RationalNumber.zero.inverse
//  lazy val negInfinity: Valuable = RationalNumber(Rational.negInfinity)
  lazy val root2: Valuable = Valuable(Constants.root2) // TODO use Root constants
  lazy val root3: Valuable = Valuable(Constants.root3) // TODO use Root constants

  /**
    * Parses the given string into a `Valuable` representation. If the string cannot be parsed
    * into a valid `Number`, an exception is thrown.
    *
    * @param str the input string representing a numerical value.
    * @return a `Valuable` representation of the parsed `Number`.
    * @throws NumberExceptionWithCause if parsing the string fails.
    */
  def apply(str: String): Valuable =
    NumberParser.parseNumber(str) match {
      case Success(number) =>
        Scalar(number)
      case Failure(exception) =>
        throw NumberExceptionWithCause("Valuable.apply", exception)
    }

  def apply(x: Long): Valuable = WholeNumber(x)

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
  def apply(field: core.Field): Valuable =
    field match {
      case core.Real(n) =>
        Scalar(n)
      case c: core.Complex =>
        Complex(c)
      case a: Algebraic =>
        throw new NumberException(s"Valuable.apply: Algebraic not yet implemented: $field")
    }
//
//  /**
//    * Extractor method to convert a `Valuable` instance into an `Option` containing its corresponding `Field` representation.
//    * This allows for safe pattern matching and handling of `Valuable` objects that may or may not be convertible to a `Field`.
//    *
//    * @param v the `Valuable` instance to be converted into an `Option[Field]`
//    * @return `Some(Field)` if the conversion is successful, or `None` if it fails
//    */
//  def unapply(v: Valuable): Option[core.Field] =
//    Try(valuableToField(v)).toOption

  /**
    * Converts a given string into a `Valuable` representation.
    * This method allows implicit conversion from `String` to `Valuable`.
    *
    * @param w the input string to be converted into a `Valuable`.
    * @return a `Valuable` instance parsed from the provided string.
    */
  implicit def toValuable(w: String): Valuable = apply(w)

  implicit object LoggableValuable extends Loggable[Valuable] {
    def toLog(t: Valuable): String = t.toString
  }
}