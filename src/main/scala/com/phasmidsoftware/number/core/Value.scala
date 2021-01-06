package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.misc.FP.optionMap

import scala.language.implicitConversions
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
    * Convert a BigInt to a Value.
    *
    * @param x a BigInt.
    * @return a Value.
    */
  def fromBigInt(x: BigInt): Value = Left(Right(x))

  /**
    * Convert a Rational to a Value.
    *
    * @param x a Rational.
    * @return a Value.
    */
  def fromRational(x: Rational): Value = Left(Left(Right(x)))

  /**
    * Convert an Option[Double] to a Value.
    *
    * @param xo a Double.
    * @return a Value.
    */
  def fromDouble(xo: Option[Double]): Value = Left(Left(Left(xo)))

  /**
    * Convert nothing to an invalid Value.
    *
    * @return a Value.
    */
  def fromNothing(): Value = Left(Left(Left(None)))
}

sealed trait Factor {
  def value: Double

  def +(other: Factor): Factor
}

sealed abstract class NonScalarFactor extends Factor {
  def +(other: Factor): Factor = other match {
    case Scalar => this
    case _ => throw NumberException("cannot add non-Scalar factors together")
  }
}

case object Scalar extends Factor {
  override def value: Double = 1

  override def toString: String = ""

  def +(other: Factor): Factor = other
}

case object Pi extends NonScalarFactor {
  def value: Double = Math.PI

  override def toString: String = Factor.sPi
}

case object E extends NonScalarFactor {
  def value: Double = Math.E

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
    optionMap[Either[Either[Option[Double], Rational], BigInt], Int, (String, Boolean)](v)(y => renderInt(y), x => optionMap[Either[Option[Double], Rational], BigInt, (String, Boolean)](x)(y => renderBigInt(y), x => optionMap[Option[Double], Rational, (String, Boolean)](x)(y => renderRational(y), {
      case Some(n) => Some(renderDouble(n))
      case None => None
    }))).getOrElse(("<undefined>", true))
}

