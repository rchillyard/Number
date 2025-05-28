/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.inner

import com.phasmidsoftware.number.core.NumberException
import com.phasmidsoftware.number.core.inner.Render.renderValue
import com.phasmidsoftware.number.misc.FP._
import scala.util._

/**
  * This object provides methods related to the type Value.
  */
object Value {

  /**
    * Represents a predefined `Value` wrapping the integer `1` as a `Right`.
    */
  val one: Value = Right(1)

  /**
    * Represents a predefined constant `Value` instance associated with the numeric value zero.
    * The `Value` is constructed as a `Right` type containing the integer `0`.
    */
  val zero: Value = Right(0)

  /**
    * Convert an Int to a Value.
    *
    * @param x an Int.
    * @return a Value.
    */
  def fromInt(x: Int): Value = Right(x)

  /**
    * Parses a given string and attempts to convert it into a Value.
    * The method first tries to interpret the string as an integer, then as a Rational,
    * and finally as a decimal (Double).
    *
    * NOTE this is only used by unit tests at present.
    *
    * If the conversion is successful and matches the original string, it returns the corresponding Value.
    * Otherwise, it defaults to parsing the string as a Double.
    *
    * @param w the string to be converted into a Value.
    * @return the resulting Value that corresponds to the supplied string.
    */
  def fromString(w: String): Value = {
    val maybeIntValue: Option[Value] = w.toIntOption map fromInt
    val maybeRationalValue = Rational.parse(w).toOption map fromRational
    maybeIntValue orElse maybeRationalValue match {
      case Some(v) if stringsMatch(v, w) =>
        v
      case Some(v) if renderValue(v)._2 =>
        v
      case _ =>
        fromOptionDouble(w.toDoubleOption)
    }
  }

  /**
    * Converts a Rational number into a Value.
    *
    * @param x the Rational number to be converted.
    * @return a Value, which is either a Right containing an Int if the Rational can be precisely represented as an Int,
    *         or a Left containing the original Rational if not.
    */
  def fromRational(x: Rational): Value = x match {
    case Rational(n, Rational.bigOne) if n <= Int.MaxValue && n >= Int.MinValue =>
      Right(n.toInt)
    case _ =>
      Left(Right(x))
  }

  /**
    * Convert an Option[Double] to a Value.
    *
    * As currently defined, this saves 49 failures in unit tests
    *
    * @param xo a Double.
    * @return a Value.
    */
  def fromDouble(xo: Option[Double]): Value = fromOptionDouble(xo)
//    xo match {
//    case Some(Double.PositiveInfinity) =>
//      fromRational(Rational.infinity)
//    case Some(Double.NegativeInfinity) =>
//      fromRational(Rational.negInfinity)
//    case Some(Double.NaN) =>
//      fromRational(Rational.NaN)
//    case Some(x) =>
//      val maybeRational: Option[Rational] = createExact(x).toOption
//      maybeRational.map(fromRational).getOrElse(fromOptionDouble(xo))
//    case None =>
//      fromNothing()
//  }

  /**
    * Method to (optionally) convert a Value into a Double.
    *
    * @param value the Value to be represented as a Double.
    * @return Some(x) where x is a Double if the conversion was possible, otherwise None.
    */
  def maybeDouble(value: Value): Option[Double] =
    optionMap(value)(_.toDouble, x => optionMap(x)(_.toDouble, identity))

  /**
    * Determines whether the given `Value` represents the numeric value zero.
    * It doesn't matter whether the `Value` is a `Double`, `Int` or a `Rational`.
    *
    * @param value the Value to be checked.
    * @return true if the Value represents zero, otherwise false.
    */
  def isZero(value: Value): Boolean =
    maybeDouble(value).contains(0.0)

  /**
    * Compares two `Value` instances to check if they are equal based
    * on specific transformations and operations.
    *
    * @param v1 the nominal `Value` against which `v` is compared.
    * @param v2 the `Value` to be compared with the `nominalValue`.
    * @return `true` if the transformed and composed values result in equality
    *         after performing operations, otherwise `false`.
    */
  def isEqual(v1: Value, v2: Value): Boolean =
    (for {
      x <- Operations.doTransformValueMonadic(v1)(MonadicOperationNegate.functions)
      y <- Operations.doComposeValueDyadic(x, v2)(DyadicOperationPlus.functions) if isZero(y)
    } yield true) getOrElse false

  /**
    * An optional Rational that corresponds to the value of this Number (but ignoring the factor).
    * A Double value is not converted to a Rational since, if it could be done exactly, it already would have been.
    * CONSIDER using query
    */
  def maybeRational(value: Value): Option[Rational] = {
    import com.phasmidsoftware.number.misc.Converters._
    val ry = tryMap(value)(tryF(Rational.apply), x => tryMap(x)(identityTry, fail("no Double=>Rational conversion")))
    ry.toOption
  }

  /**
    * Determines if the given Value can be exactly represented as a Rational.
    * Although a Double value is not necessarily inexact, if it could been converted exactly to a Rational,
    * it would have been converted already.
    *
    * @param value the Value to be checked.
    * @return true if the Value can be exactly represented as a Rational, otherwise false.
    */
  def isExact(value: Value): Boolean =
    maybeRational(value).isDefined

  /**
    * Scales a `Value` by the given `Rational` factor.
    *
    * The method attempts to scale a `Value` based on its internal type (e.g., Rational or Double).
    * If the `Value` can be represented as a `Rational`, it is multiplied by the given scaling factor.
    * Otherwise, if it can be represented as a `Double`, the scaling is applied in the `Double` domain.
    *
    * @param x     the `Rational` factor by which the `Value` is scaled.
    * @param value the `Value` to be scaled.
    * @return a new scaled `Value`. The result is based on either the Rational or Double representation of the input value.
    */
  def scaleRational(x: Rational)(value: Value): Value =
    maybeRational(value) map (r => fromRational(r * x)) getOrElse fromDouble(maybeDouble(value) map (d => x.toDouble * d))

  /**
    * An optional Int that corresponds to the value of this Number (but ignoring the factor).
    *
    * CONSIDER using query
    */
  def maybeInt(value: Value): Option[Int] = {
    val xToZy0: Option[Double] => Try[Int] = {
      case Some(n) if Math.round(n) == n =>
        if (n <= Int.MaxValue && n >= Int.MinValue)
          Try(n.toInt)
        else
          Failure(NumberException(s"double $n cannot be represented as an Int"))
      case Some(n) =>
        Failure(NumberException(s"toInt: $n is not integral"))
      case None =>
        Failure(new NoSuchElementException())
    }
    import com.phasmidsoftware.number.misc.Converters._
    val xToZy1: Either[Option[Double], Rational] => Try[Int] = y => tryMap(y)(tryF(y => y.toInt), xToZy0)
    tryMap(value)(identityTry, xToZy1).toOption
  }

  /**
    * Method to get the sign of a Value.
    *
    * @param value the value whose sign we need.
    * @return an Int.
    */
  def signum(value: Value): Int = value match {
    case Right(x) =>
      x.sign
    case Left(Right(x)) =>
      x.signum
    case Left(Left(Some(x))) =>
      x.sign.toInt
    case _ =>
      0
  }

  /**
    * Method to get the sign of a Value.
    *
    * @param value the value whose sign we need.
    * @return an Int.
    */
  def abs(value: Value): Value = value match {
    case Right(x) =>
      Right(math.abs(x))
    case Left(Right(x)) =>
      Left(Right(x.abs))
    case Left(Left(Some(x))) =>
      Left(Left(Some(math.abs(x))))
    case _ =>
      fromNothing()
  }

  /**
    * Negates the given Value.
    *
    * @param value the Value to be negated.
    * @return a Value representing the negation of the given Value.
    */
  def negate(value: Value): Value = value match {
    case Right(Integer.MIN_VALUE) =>
      Left(Right(Rational(BigInt(Integer.MIN_VALUE))))
    case Right(0) =>
      value
    case Right(x) =>
      Right(-x)
    case Left(Right(x)) =>
      Left(Right(-x))
    case Left(Left(Some(x))) =>
      Left(Left(Some(-x)))
    case _ =>
      fromNothing()
  }

  /**
    * Conditionally negates the provided `Value` based on the boolean parameter `neg`.
    * If `neg` is true, the `Value` is negated using the `negate` method.
    * Otherwise, the original `Value` is returned unchanged.
    *
    * @param neg   a Boolean indicating whether the `Value` should be negated.
    * @param value the `Value` to be conditionally negated.
    * @return the negated `Value` if `neg` is true, otherwise the original `Value`.
    */
  def negateConditional(neg: Boolean)(value: Value): Value =
    if (neg)
      negate(value)
    else
      value

  /**
    * Computes the inverse of the given Value, if possible.
    *
    * For numeric values, this method attempts to determine their inverse.
    * If the input Value is a Double, None is returned.
    *
    * @param value the Value to invert.
    * @return an Option containing the inverted Value, or None if the inversion
    *         cannot be performed.
    */
  def inverse(value: Value): Option[Value] = value match {
    case Right(1) =>
      Some(value)
    case Right(x) =>
      Some(Left(Right(Rational(1, x))))
    case Left(Right(x)) =>
      val inverted = x.invert
      if (inverted.isInteger)
        Some(Right(inverted.toInt))
      else
        Some(Left(Right(inverted)))
    case Left(Left(Some(_))) =>
      None
    case _ =>
      None
  }

  /**
    * Method to scale the given Double according to the provided scaling factors.
    *
    * @param x       the Double to be scaled.
    * @param fThis   the value of the factor that x is associated with.
    * @param fResult the value of the factor that the result will be associated with.
    * @return a Double which will be paired with fResult.
    */
  def scaleDouble(x: Double, fThis: Double, fResult: Double): Double =
    x * fThis / fResult

  /**
    * Method to render a Value as a String.
    *
    * @param v the value to be rendered.
    * @return a String.
    */
  def valueToString(v: Value, exact: Boolean = true): String =
    (renderValue(v, exact), exact) match {
      case ((x, true), _) =>
        x
      case ((x, _), false) =>
        x
      case ((x, _), _) =>
        x + "*"
  }

  /**
    * Unapply method which will yield an optional array.
    * If there is only one item returned, it is an Int;
    * if there are two items returned, they are null followed by a Rational;
    * if there are three items returned, they are two nulls followed by a Double;
    * otherwise, if the given value is not defined, then None is returned.
    *
    * @param v the value.
    * @return Some(List[Int](x)) or Some(List[Rational](null,x)) or Some(List[Double](null,null,x)) or None.
    */
  def unapplySeq(v: Value): Option[List[Any]] = v match {
    case Right(x) =>
      Some(List(x))
    case Left(Right(x)) =>
      Some(List(null, x))
    case Left(Left(Some(x))) =>
      Some(List(null, null, x))
    case _ =>
      None
  }

  /**
    * Compares a given `Value` and a `String` to determine if they match under specific conditions.
    *
    * The method first converts the `Value` into a `String` using `valueToString`
    * and compares it to the supplied `String`.
    * Special cases are handled, such as treating "½" as equal to "1/2".
    *
    * @param v the `Value` to be compared.
    * @param w the `String` to be compared against.
    * @return true if the `Value` and the `String` match based on the specified logic, otherwise false.
    */
  private def stringsMatch(v: Value, w: String) =
    (valueToString(v), w) match {
      case (x, y) if x == y =>
        true
      case ("½", "1/2") =>
        true
      case _ =>
        false
    }

  /**
    * Private method to convert an `Option[Double]` into a `Value` representation.
    * Applications should use `fromDouble`.
    *
    * @param xo an `Option` containing a `Double` value or `None`.
    * @return a `Value` representing the wrapped `Option[Double]` in its `Left` structure.
    */
  private def fromOptionDouble(xo: Option[Double]): Value =
    Left(Left(xo))

  /**
    * Convert nothing to an invalid Value.
    *
    * TESTME
    *
    * @return a Value.
    */
  private def fromNothing(): Value =
    fromOptionDouble(None)
}
