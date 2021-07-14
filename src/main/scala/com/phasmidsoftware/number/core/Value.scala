package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.FP.optionMap

import scala.math.BigInt
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
    * @return a Value.
    */
  def fromNothing(): Value = Left(Left(None))
}

sealed trait Factor {
  val value: Double

  def +(other: Factor): Option[Factor]
}

sealed abstract class NonScalarFactor extends Factor {
  def +(other: Factor): Option[Factor] = other match {
    case Scalar => if (this != E) Some(this) else None
    case E => if (this == E) Some(this) else None
    case _ => throw NumberException("cannot add Pi factors together")
  }
}

case object Scalar extends Factor {
  val value: Double = 1

  override def toString: String = ""

  def +(other: Factor): Option[Factor] = other match {
    case E => None
    case _ => Some(other)
  }

}

/**
  * This factor is primarily used for rotation by an angle.
  * The range of such values is 0 thru 2pi.
  */
case object Pi extends NonScalarFactor {
  val value: Double = Math.PI

  override def toString: String = Factor.sPi
}

/**
  * This factor essentially provides log/exponent arithmetic.
  *
  * A number in factor E will evaluate as e raised to that power.
  * So, it is the natural log of a scalar value.
  *
  * Thus the range of such values is any positive number.
  */
case object E extends NonScalarFactor {
  val value: Double = Math.E

  override def toString: String = Factor.sE
}

object Factor {
  val sE = "\uD835\uDF00"
  val sPi = "\uD835\uDED1"
  val sPiAlt0 = "pi"
  val sPiAlt1 = "Pi"
  val sPiAlt2 = "PI"

  def apply(w: String): Factor = w match {
    case `sPi` | `sPiAlt0` | `sPiAlt1` | `sPiAlt2` => Pi
    case `sE` => E
    case _ => Scalar
  }
}

object Render {
  def renderInt(x: Int): (String, Boolean) = (x.toString, true)

  def renderBigInt(x: BigInt): (String, Boolean) = (x.toString, true)

  def renderRational(x: Rational): (String, Boolean) = (x.toString, true)

  def renderDouble(x: Double): (String, Boolean) = (x.toString, false)

  def renderValue(v: Value): (String, Boolean) =
    optionMap[Either[Option[Double], Rational], Int, (String, Boolean)](v)(y => renderInt(y), x => optionMap[Option[Double], Rational, (String, Boolean)](x)(y => renderRational(y), {
      case Some(n) => Some(renderDouble(n))
      case None => None
    })).getOrElse(("<undefined>", true))
}

