/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.FP._
import com.phasmidsoftware.number.core.Render.renderValue
import scala.util._

/**
  * This object provides methods related to the type Value.
  */
object Value {

  /**
    * Convert an Int to a Value.
    *
    * @param x an Int.
    * @return a Value.
    */
  def fromInt(x: Int): Value = Right(x)

  /**
    * Convert a Rational to a Value.
    *
    * @param x a Rational.
    * @return a Value.
    */
  def fromRational(x: Rational): Value = Left(Right(x))

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
    * An optional Rational that corresponds to the value of this Number (but ignoring the factor).
    * A Double value is not converted to a Rational since, if it could be done exactly, it already would have been.
    * CONSIDER using query
    */
  def maybeRational(value: Value): Option[Rational] = {
    import Converters._
    val ry = tryMap(value)(tryF(Rational.apply), x => tryMap(x)(identityTry, fail("no Double=>Rational conversion")))
    ry.toOption
  }

  /**
    * An optional Int that corresponds to the value of this Number (but ignoring the factor).
    *
    * CONSIDER using query
    */
  def maybeInt(value: Value): Option[Int] = {
    val xToZy0: Option[Double] => Try[Int] = {
      case Some(n) if Math.round(n) == n => if (n <= Int.MaxValue && n >= Int.MinValue) Try(n.toInt)
      else Failure(NumberException(s"double $n cannot be represented as an Int"))
      case Some(n) => Failure(NumberException(s"toInt: $n is not integral"))
      case None => Failure(new NoSuchElementException())
    }
    import Converters._
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
    case Right(x) => Right(math.abs(x))
    case Left(Right(x)) => Left(Right(x.abs))
    case Left(Left(Some(x))) => Left(Left(Some(math.abs(x))))
    case _ => fromNothing()
  }

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
    * Method to render a Value as a String.
    *
    * @param v the value to be rendered.
    * @return a String.
    */
  def valueToString(v: Value, exact: Boolean = true): String = (renderValue(v, exact), exact) match {
    case ((x, true), _) => x
    case ((x, _), false) => x
    case (x, false) => x.toString() + "*"
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
