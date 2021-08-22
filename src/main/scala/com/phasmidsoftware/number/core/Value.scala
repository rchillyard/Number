package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.FP.optionMap
import com.phasmidsoftware.number.core.Render.renderValue

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

  /**
    * Method to (optionally) convert a Value into a Double.
    *
    * @param value the Value to be represented as a Double.
    * @return Some(x) where x is a Double if the conversion was possible, otherwise None.
    */
  def maybeDouble(value: Value): Option[Double] = optionMap(value)(_.toDouble, x => optionMap(x)(_.toDouble, identity))

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
  def valueToString(v: Value): String = renderValue(v) match {
    case (x, true) => x
    case (x, false) => x + "..."
  }

}

sealed trait Factor {
  /**
    * A value which can be used to convert a value associated with this Factor to a different Factor.
    */
  val value: Double

  /**
    * A method to combine this Factor with another Factor and, if they are compatible, to return Some(factor).
    *
    * @param other the other factor.
    * @return Some(f) if the factors are compatible, otherwise None.
    */
  def +(other: Factor): Option[Factor]

  /**
    * Convert a value x from this factor to f if possible, using simple scaling.
    * If the factors are incompatible, then None will be returned.
    *
    * @param x the value to be converted.
    * @param f the factor of the result.
    * @return an optional Value which, given factor f, represents the same quantity as x given this.
    */
  def convert(x: Value, f: Factor): Option[Value]

  /**
    * Method to render a Value in the context of this Factor.
    *
    * @param x the Value.
    * @return a String.
    */
  def render(x: Value): String
}

/**
  * Trait to define a Factor which is a scaled version of a pure number.
  */
sealed trait PureNumber extends Factor {
  // NOTE duplicate of Logarithmic
  def +(other: Factor): Option[Factor] = other match {
    case Scalar => if (this != NatLog) Some(this) else None // TODO impossible
    case NatLog => if (this == NatLog) Some(this) else None // TODO impossible
    case _ => throw NumberException("cannot add Radian factors together")
  }

  /**
    * Convert a value x from this factor to f if possible, using simple scaling.
    * If the factors are incompatible, then a Failure will be returned.
    *
    * @param v the value to be converted.
    * @param f the factor of the result.
    * @return a Try of Value which, given factor f, represents the same quantity as x given this.
    */
  def convert(v: Value, f: Factor): Option[Value] = f match {
    case PureNumber(z) =>
      Some(Value.fromDouble(Value.maybeDouble(v) map (x => Value.scaleDouble(x, this.value, z))))
    case _ => None
  }
}

object PureNumber {
  def unapply(arg: PureNumber): Option[Double] = Some(arg.value)
}

/**
  * Trait to define a Factor which is a scaled version of the natural log.
  */
sealed trait Logarithmic extends Factor {
  def +(other: Factor): Option[Factor] = other match {
    case Scalar => if (this != NatLog) Some(this) else None
    case NatLog => if (this == NatLog) Some(this) else None
    case _ => throw NumberException("cannot add Radian factors together")
  }

  /**
    * Convert a value x from this factor to f if possible, using simple scaling.
    * If the factors are incompatible, then a Failure will be returned.
    *
    * @param v the value to be converted.
    * @param f the factor of the result.
    * @return a Try of Value which, given factor f, represents the same quantity as x given this.
    */
  def convert(v: Value, f: Factor): Option[Value] = f match {
    case Logarithmic(z) =>
      Some(Value.fromDouble(Value.maybeDouble(v) map (x => Value.scaleDouble(x, this.value, z))))
    case _ => None
  }

  def render(x: Value): String

  def asPower(v: Value, f: String): String = v match {
    case Right(1) => f
    case Right(2) => f + "\u00B2"
    case Right(3) => f + "\u00B3"
    case Right(x) if x > 3 & x < 10 => f + Logarithmic.incrementUnicode("\u2070", 0, x)
    case Left(Right(r)) if r * 2 == Rational.one => "\u221A" + f
    case Left(Right(r)) if r * 3 == Rational.one => "\u221B" + f
    case Left(Right(r)) if r * 4 == Rational.one => "\u221C" + f
    case _ => f + "^" + Value.valueToString(v)
  }


}

object Logarithmic {
  def unapply(arg: Logarithmic): Option[Double] = Some(arg.value)

  private def incrementUnicode(str: String, index: Int, x: Int): String = {
    val chars: Array[Char] = str.toArray
    chars.update(index, (chars(index) + x).toChar)
    new String(chars)
  }

}

case object Scalar extends PureNumber {
  val value: Double = 1

  override def toString: String = ""

  /**
    * TODO check the logic here. Don't think we really need this method defined here.
    *
    * @param other the other factor.
    * @return Some(f) if the factors are compatible, otherwise None.
    */
  override def +(other: Factor): Option[Factor] = other match {
    case NatLog => None
    case _ => Some(other)
  }

  def render(x: Value): String = x.toString
}

/**
  * This factor is primarily used for rotation by an angle.
  *
  * A number x with factor Radian (theoretically) evaluates to e raised to the power ix.
  * So, you could think of it as essentially a shorthand for writing both cosine and sine.
  * NOTE however that, unlike, with the factor NatLog, we currently do not treat Radian values in quite this way in the code.
  *
  * CONSIDER implementing the Radian factor conversions in a manner similar to that of NatLog.
  * This would entail conversion from a single number to a pair of Doubles when going from Radian to Scalar.
  * We could do that using a Complex number but I'd rather do it as a 2-tuple of Doubles.
  * Perhaps the whole idea of our Complex implementation is misguided (although it does allow us to represent
  * complex numbers in Polar form since, in that case, the real part is a Scalar number, and the imaginary part is coded in
  * with factor Radian).
  *
  * The range of such values is 0 thru 2pi.
  */
case object Radian extends PureNumber {
  val value: Double = Math.PI

  override def toString: String = Factor.sPi

  def render(x: Value): String = renderValue(x) + Factor.sPi
}

/**
  * This factor essentially provides log/exponent arithmetic.
  *
  * NOTE: A number in factor NatLog will evaluate as e raised to that power.
  * So, it is the natural log of a scalar value.
  *
  * Thus the range of such values is any positive number.
  */
case object NatLog extends Logarithmic {
  val value: Double = 1.0

  override def toString: String = Factor.sE

  def render(x: Value): String = asPower(x, Factor.sE)
}

case object Log2 extends Logarithmic {
  val value: Double = math.log(2)

  override def toString: String = "log2"

  def render(x: Value): String = asPower(x, "2")
}

object Factor {
  val sE = "\uD835\uDF00"
  val sPi = "\uD835\uDED1"
  val sPiAlt0 = "pi"
  val sPiAlt1 = "Radian"
  val sPiAlt2 = "PI"

  def apply(w: String): Factor = w match {
    case `sPi` | `sPiAlt0` | `sPiAlt1` | `sPiAlt2` => Radian
    case `sE` => NatLog
    case _ => Scalar
  }
}

object Render {
  def renderInt(x: Int): (String, Boolean) = (x.toString, true)

  def renderBigInt(x: BigInt): (String, Boolean) = (x.toString, true)

  def renderRational(x: Rational): (String, Boolean) = (x.toString, true)

  def renderDouble(x: Double): (String, Boolean) = (x.toString, false)

  /**
    * Method to render a Value.
    *
    * CONSIDER this doesn't belong here.
    *
    * @param v the Value to be rendered.
    * @return a tuple of String and Boolean.
    */
  def renderValue(v: Value): (String, Boolean) =
    optionMap[Either[Option[Double], Rational], Int, (String, Boolean)](v)(y => renderInt(y), x => optionMap[Option[Double], Rational, (String, Boolean)](x)(y => renderRational(y), {
      case Some(n) => Some(renderDouble(n))
      case None => None
    })).getOrElse(("<undefined>", true))
}

