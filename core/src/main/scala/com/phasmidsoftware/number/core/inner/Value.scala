/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.inner

import com.phasmidsoftware.number.core.inner.Operations.doComposeValueDyadic
import com.phasmidsoftware.number.core.inner.Render.renderValue
import com.phasmidsoftware.number.core.misc.FP._
import com.phasmidsoftware.number.core.numerical.CoreException
import java.lang
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
    * Represents the value `2` as a `Value` type,
    * where `Right(2)` signifies a valid integer representation
    * wrapped in a `Right`, indicating successful creation as a `Value`.
    */
  val two: Value = Right(2)

  /**
    * A pre-defined constant `Value` representing one-half.
    * This `Value` is derived from the `Rational` representation of half (1/2).
    *
    * It is created using the `fromRational` method to ensure an appropriate `Value`
    * representation of the rational number one-half.
    */
  val half: Value = fromRational(Rational.half)

  /**
    * Convert an Int to a Value.
    *
    * @param x an Int.
    * @return a Value.
    */
  def fromInt(x: Int): Value = Right(x)

  /**
    * Converts a Rational number into a Value.
    *
    * @param x the Rational number to be converted.
    * @return a Value, which is either a Right containing an Int if the Rational can be precisely represented as an Int,
    *         or a Left containing the original Rational if not.
    */
  def fromRational(x: Rational): Value = x match {
    case Rational(n, Rational.bigOne) if n <= Int.MaxValue && n >= Int.MinValue => Right(n.toInt)
    case _ => Left(Right(x))
  }

  /**
    * Convert an Option[Double] to a Value.
    *
    * @param xo a Double.
    * @return a Value.
    */
  def fromDouble(xo: Option[Double]): Value = Left(Left(xo))

  /**
    * Convert nothing to an invalid Value.
    *
    * TESTME
    *
    * @return a Value.
    */
  private def fromNothing(): Value = Left(Left(None))

  /**
    * Method to (optionally) convert a Value into a Double.
    *
    * @param value the Value to be represented as a Double.
    * @return Some(x) where x is a Double if the conversion was possible, otherwise None.
    */
  def maybeDouble(value: Value): Option[Double] = optionMap(value)(_.toDouble, x => optionMap(x)(_.toDouble, identity))

  /**
    * Converts a `Value` into an optional `java.lang.Number`. This method attempts to map the given `Value`
    * to a corresponding `Number` type in the following order:
    * - If the `Value` can be represented as an `Int`, the result will be wrapped as a `java.lang.Number`.
    * - If the `Value` is a `Rational` that can be accurately represented as a `Double`, it is converted and wrapped as a `java.lang.Number`.
    * - If the `Value` is already a `Double`, it will be directly wrapped as a `java.lang.Number`.
    * - If none of the above conversions are applicable, `None` is returned.
    *
    * CONSIDER using other methods from this class to get an appropriate Double.
    *
    * @param x the `Value` to be converted.
    * @return an `Option` containing the `java.lang.Number` representation of the given `Value`, or `None`
    *         if conversion is not possible.
    */
  def asJavaNumber(x: Value): Option[lang.Number] =
    Value.maybeInt(x).map(i => i.asInstanceOf[lang.Number]) orElse
        Value.maybeRational(x).flatMap(r => if (r.isExactDouble) Some(r.toDouble.asInstanceOf[lang.Number]) else None) orElse
        // NOTE the following should only work if the (relative) error bounds are smaller than the double-precision error bound
        Value.maybeDouble(x).map(d => d.asInstanceOf[lang.Number]) orElse
        None

  /**
    * Determines whether the given `Value` represents the numeric value zero.
    * It doesn't matter whether the `Value` is a `Double`, `Int` or a `Rational`.
    *
    * @param value the Value to be checked.
    * @return true if the Value represents zero, otherwise false.
    */
  def isZero(value: Value): Boolean = maybeDouble(value).contains(0.0)

  /**
    * Compares two `Value` instances to check if they are equal based
    * on specific transformations and operations.
    *
    * @param v1 the nominal `Value` against which `v` is compared.
    * @param v2 the `Value` to be compared with the `nominalValue`.
    * @return `true` if the transformed and composed values result in equality
    *         after performing operations, otherwise `false`.
    */
  def isEqual(v1: Value, v2: Value): Boolean = {
    // First, check to see if v1.equals(v2) returns true
    v1 == v2 ||
        // Next, check to see if v1 + v2 = 0
        ((for {
          x <- Operations.doTransformValueMonadic(v1)(MonadicOperationNegate.functions)
          y <- Operations.doComposeValueDyadic(x, v2)(DyadicOperationPlus.functions) if isZero(y)
        } yield true) getOrElse false)
  }

  /**
    * An optional Rational that corresponds to the value of this Number (but ignoring the factor).
    * A Double value is not converted to a Rational since, if it could be done exactly, it already would have been.
    * CONSIDER using query
    */
  def maybeRational(value: Value): Option[Rational] = {
    import com.phasmidsoftware.number.core.misc.Converters._
    val ry = tryMap(value)(tryF(Rational.apply), x => tryMap(x)(identityTry, fail("no Double=>Rational conversion")))
    ry.toOption
  }

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
      case Some(n) if Math.round(n) == n => if (n <= Int.MaxValue && n >= Int.MinValue) Try(n.toInt)
      else Failure(CoreException(s"double $n cannot be represented as an Int"))
      case Some(n) => Failure(CoreException(s"toInt: $n is not integral"))
      case None => Failure(new NoSuchElementException())
    }
    import com.phasmidsoftware.number.core.misc.Converters._
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
    case Right(x) => x.sign
    case Left(Right(x)) => x.signum
    case Left(Left(Some(x))) => x.sign.toInt
    case _ => 0
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
    * Adds two `Value` instances and returns the result.
    *
    * The method attempts to perform addition by considering various combinations
    * of `Value` types. If the addition cannot be performed, an invalid `Value`
    * is returned.
    *
    * @param v1 the first `Value` to be added
    * @param v2 the second `Value` to be added
    * @return a `Value` representing the sum of `v1` and `v2`
    */
  def add(v1: Value, v2: Value): Value =
    doComposeValueDyadic(v1, v2)(DyadicOperationPlus.functions) getOrElse fromNothing()

  /**
    * Multiplies two `Value` instances and returns the result.
    *
    * This method attempts to perform multiplication by considering various combinations
    * of `Value` types. If the multiplication cannot be performed, an invalid `Value`
    * is returned.
    *
    * @param v1 the first `Value` to be multiplied
    * @param v2 the second `Value` to be multiplied
    * @return a `Value` representing the product of `v1` and `v2`
    */
  def multiply(v1: Value, v2: Value): Value =
    doComposeValueDyadic(v1, v2)(DyadicOperationTimes.functions) getOrElse fromNothing()

  /**
    * Negates the given Value.
    *
    * @param value the Value to be negated.
    * @return a Value representing the negation of the given Value.
    */
  def negate(value: Value): Value = value match {
    case Right(Integer.MIN_VALUE) => Left(Right(Rational(BigInt(Integer.MIN_VALUE))))
    case Right(0) => value
    case Right(x) => Right(-x)
    case Left(Right(x)) => Left(Right(-x))
    case Left(Left(Some(x))) => Left(Left(Some(-x)))
    case _ => fromNothing()
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
  def negateConditional(neg: Boolean)(value: Value): Value = if (neg) negate(value) else value

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
    case Right(1) => Some(value)
    case Right(x) => Some(Left(Right(Rational(1, x))))
    case Left(Right(x)) =>
      val inverted = x.invert
      if (inverted.isInteger) Some(Right(inverted.toInt)) else Some(Left(Right(inverted)))
    case Left(Left(Some(_))) => None
    case _ => None
  }

  /**
    * Method to scale the given Double according to the provided scaling factors.
    *
    * @param x       the Double to be scaled.
    * @param fThis   the value of the factor that x is associated with.
    * @param fResult the value of the factor that the result will be associated with.
    * @return a Double which will be paired with fResult.
    */
  def scaleDouble(x: Double, fThis: Double, fResult: Double): Double = x * fThis / fResult

  /**
    * Converts a given `Value` into its string representation, with options to skip certain outputs or use exact formatting.
    *
    * @param v       the `Value` to be converted into a string.
    * @param skipOne a Boolean indicating whether to skip returning the string representation if the value equals "1"
    *                (only when `exact` is true).
    * @param exact   a Boolean (default is true) specifying whether the string representation should be exact
    *                or adjusted with additional formatting (e.g., appending a "*").
    * @return a `String` representing the `Value`, formatted according to the given parameters.
    */
  def valueToString(v: Value, skipOne: Boolean, exact: Boolean = true): String = (renderValue(v, exact), exact) match {
    case (("1", true), _) if skipOne => ""
    case ((x, true), _) => x
    case ((x, _), false) => x
    case (x, false) => x.toString() + "*"
    // Needs a wildcard case
  }

  /**
    * A variable-element `unapply` method which will yield an optional array.
    * If there is only one item returned, it is an Int;
    * if there are two items returned, they are null followed by a Rational;
    * if there are three items returned, they are two nulls followed by a Double;
    * otherwise, if the given value is not defined, then `None` is returned.
    *
    * @param v the value.
    * @return `Some(List[Int](x))` or `Some(List[Rational](null,x))` or `Some(List[Double](null,null,x))` or `None`.
    */
  @annotation.nowarn("msg=infer-any")
  def unapplySeq(v: Value): Option[List[Any]] = {
    val result = v match {
      case Right(x) => Some(List(x))
      case Left(Right(x)) => Some(List(null, x))
      case Left(Left(Some(x))) => Some(List(null, null, x))
      case _ => None
    }
    result
  }
}
