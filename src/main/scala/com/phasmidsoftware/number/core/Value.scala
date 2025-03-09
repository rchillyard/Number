/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.FP._
import com.phasmidsoftware.number.core.Operations.doComposeValueDyadic
import com.phasmidsoftware.number.core.Render.renderValue
import com.phasmidsoftware.number.core.Value.{fromDouble, scaleDouble, valueToString}

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

sealed trait Factor {
  /**
    * A value which can be used to convert a value associated with this Factor to a different Factor.
    */
  val value: Double

  /**
   * Determines if the current factor operates additively.
   *
   * @return true if the factor is additive; false otherwise.
   */
  def isAdditive: Boolean

  /**
   * Determines if the current factor satisfies certain conditions within the given context.
   *
   * @param context the context in which the factor is evaluated.
   * @return a Boolean indicating whether the factor satisfies the specified conditions in the given context.
   */
  def isA(context: Context): Boolean

  /**
   * Convert a value x from this factor to f if possible, using the simplest possible mechanism.
    * If the factors are incompatible, then None will be returned.
    *
   * NOTE: only Scalar<->Scalar, Root<->Root or Logarithmic<->Logarithmic conversions can be effected.
    *
    * @param v the value to be converted.
    * @param f the factor of the result.
    * @return an optional Value which, given factor f, represents the same quantity as v given this.
    */
  def convert(v: Value, f: Factor): Option[Value]

  /**
    * Method to render a Value in the context of this Factor.
    *
    * @param x the Value.
    * @return a String.
    */
  def render(x: Value): String = render(valueToString(x))

  /**
    * Method to render a Value (which has already been converted to a String) in the context of this Factor.
    *
    * @param v a String representing the Value.
    * @return a String.
    */
  def render(v: String): String
}

/**
 * Trait to define a Factor which is a scalar (something that can be scaled by a pure number).
  */
sealed trait Scalar extends Factor {

  /**
   * Determines if the current factor operates additively.
   *
   * @return true if the factor is additive; false otherwise.
   */
  def isAdditive: Boolean = true

  /**
   * Determines if the current factor satisfies certain conditions within the given context.
   *
   * @param context the context in which the factor is evaluated.
   * @return a Boolean indicating whether the factor satisfies the specified conditions in the given context.
   */
  def isA(context: Context): Boolean = context.isEmpty || context.contains(this) || context.contains(PureNumber)

  /**
    * Convert a value x from this factor to f if possible, using simple scaling.
   * Result is defined only if f is a Scalar.
    *
    * @param v the value to be converted.
    * @param f the factor of the result.
    * @return a Try of Value which, given factor f, represents the same quantity as x given this.
    */
  def convert(v: Value, f: Factor): Option[Value] = f match {
    case Scalar(z) =>
      Some(fromDouble(Value.maybeDouble(v) map (x => scaleDouble(x, this.value, z))))
    case _ => None
  }
}

object Scalar {
  def unapply(arg: Scalar): Option[Double] = Some(arg.value)
}

/**
  * Trait to define a Factor which is a scaled version of the natural log.
  */
sealed trait Logarithmic extends Factor {
  val base: String

  /**
   * Determines if the current factor operates additively.
   *
   * @return false.
   */
  def isAdditive: Boolean = false

  /**
   * Determines if the current factor satisfies certain conditions within the given context.
   *
   * @param context the context in which the factor is evaluated.
   * @return a Boolean indicating whether the factor satisfies the specified conditions in the given context.
   */
  def isA(context: Context): Boolean = context.isEmpty || context.contains(this)

  /**
    * Convert a value x from this factor to f if possible, using simple scaling.
    * Result is defined only if f is Logarithmic.
    *
    * @param v the value to be converted.
    * @param f the factor of the result.
    * @return a Try of Value which, given factor f, represents the same quantity as x given this.
    */
  def convert(v: Value, f: Factor): Option[Value] = f match {
    case Logarithmic(z) => Some(fromDouble(Value.maybeDouble(v) map (x => scaleDouble(x, this.value, z))))
    case _ => None
  }

  def asPower(v: Value): String = v match {
    case Right(1) => base
    case Right(2) => base + "\u00B2"
    case Right(3) => base + "\u00B3"
    case Right(x) if x > 3 & x < 10 => base + Logarithmic.incrementUnicode("\u2070", 0, x)
    case Left(Right(r)) if r * 2 == Rational.one => "\u221A" + base
    case Left(Right(r)) if r * 3 == Rational.one => "\u221B" + base
    case Left(Right(r)) if r * 4 == Rational.one => "\u221C" + base
    case _ => base + "^" + valueToString(v)
  }

  def render(v: String): String = base + "^" + v
}

object Logarithmic {
  def unapply(arg: Logarithmic): Option[Double] = Some(arg.value)

  private def incrementUnicode(str: String, index: Int, x: Int): String = {
    val chars: Array[Char] = str.toArray
    chars.update(index, (chars(index) + x).toChar)
    new String(chars)
  }
}

/**
 * A sealed trait extending the `Factor` trait, representing a mathematical root-based factorization.
 *
 * A number with value x and factor Root(n) represents the nth root of x.
 * For example, ExactNumber(5, Root2) is the square root of 5, where Root2 is an alias for Root(2).
 *
 * This trait provides mechanisms to convert values associated with one root-based factor
 * to another, when such conversions are mathematically possible.
 */
sealed trait Root extends Factor {

  /**
   * Retrieves the root degree represented by this factor.
   *
   * The root degree indicates the mathematical nth root applicable to this factor.
   * For instance, a root degree of 2 represents a square root, a root degree of 3
   * represents a cube root, and so on.
   *
   * @return the integer value of the root degree.
   */
  def root: Int

  /**
    * A value which can be used to convert a value associated with this Factor to a different Factor.
    */
  val value: Double = root

  /**
   * Determines if the current factor operates additively.
   *
   * @return false.
   */
  def isAdditive: Boolean = false

  /**
   * Determines if the current factor satisfies certain conditions within the given context.
   *
   * @param context the context in which the factor is evaluated.
   * @return a Boolean indicating whether the factor satisfies the specified conditions in the given context.
   */
  def isA(context: Context): Boolean = context.isEmpty || context.contains(this)

  /**
   * Converts a given Value `v` from one Factor `this` to another Factor `f`, if possible.
   * Specifically handles cases where the target factor `f` is of type Root.
   *
   * @param v the value to be converted.
   * @param f the target factor for the conversion.
   * @return an optional Value which represents the same quantity as `v` under the factor `f`, or `None` if the
   *         conversion is not possible.
   */
  def convert(v: Value, f: Factor): Option[Value] = f match {
    case Root(z) =>
      val vo = doComposeValueDyadic(v, Number(z).specialize.value)(DyadicOperationPower.functions)
      vo flatMap (doComposeValueDyadic(_, Number(value).specialize.getInverse.value)(DyadicOperationPower.functions))
    case _ => None
  }
}

/**
 * Companion object for the `Root` trait, providing utility methods for working with `Root` instances.
 */
object Root {
  /**
   * Extractor method to retrieve the root degree from a `Root` instance.
   *
   * This method is used for pattern matching to extract the integer root degree
   * associated with the given `Root` object.
   *
   * @param arg the `Root` instance from which to extract the root degree.
   * @return an `Option` containing the root degree as an `Int`, or `None` if the extraction fails.
   */
  def unapply(arg: Root): Option[Int] = Some(arg.root)
}

/**
 * A case object representing the pure number 1.
 * `PureNumber` extends the `Scalar` trait and behaves as a unit scalar factor.
 *
 * This object encapsulates the value of 1, providing a representation of a pure scalar.
 * It includes functionality to render a string representation of a value.
 */
case object PureNumber extends Scalar {
  val value: Double = 1

  /**
   * Determines if the current factor satisfies certain conditions within the given context.
   *
   * @param context the context in which the factor is evaluated.
   * @return a Boolean indicating whether the factor satisfies the specified conditions in the given context.
   */
  override def isA(context: Context): Boolean = context.isEmpty || context.contains(this)

  override def toString: String = ""

  def render(x: String): String = x
}

/**
  * This factor is primarily used for rotation by an angle.
  *
  * A number x with factor Radian (theoretically) evaluates to e raised to the power ix.
  * So, you could think of it as essentially a shorthand for writing both cosine and sine.
  * NOTE however that, unlike, with the factor NatLog, we currently do not treat Radian values in quite this way in the code.
  *
  * CONSIDER implementing the Radian factor conversions in a manner similar to that of NatLog.
 * This would entail conversion from a single number to a pair of Doubles when going from Radian to PureNumber.
  * We could do that using a Complex number but I'd rather do it as a 2-tuple of Doubles.
  * Perhaps the whole idea of our Complex implementation is misguided (although it does allow us to represent
 * complex numbers in Polar form since, in that case, the real part is a PureNumber number, and the imaginary part is coded in
  * with factor Radian).
  *
  * The range of these values is 0 thru 2, which represents the radian values of 0 thru 2pi.
  */
case object Radian extends Scalar {
  val value: Double = Math.PI

  override def toString: String = Factor.sPi

  def render(x: String): String = x + Factor.sPi
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
  val base: String = Factor.sE

  val value: Double = 1.0

  override def toString: String = Factor.sE

  override def render(x: Value): String = asPower(x)
}

case object Log2 extends Logarithmic {
  val base: String = "2"

  val value: Double = math.log(2)

  override def toString: String = "log2"

  override def render(x: Value): String = asPower(x)
}

case object Log10 extends Logarithmic {
  val base: String = "10"

  val value: Double = math.log(10)

  override def toString: String = "log10"

  override def render(x: Value): String = asPower(x)
}

/**
  * This object represents the square root factor.
  */
case object Root2 extends Root {

  override def toString: String = "√"

  def render(x: String): String = s"√$x"

  def root: Int = 2
}

/**
 * This object represents the cube root factor.
  */
case object Root3 extends Root {

  override def toString: String = "³√"

  def render(x: String): String = s"³√$x"

  def root: Int = 3
}

/**
 * An object that provides a mechanism to map input strings to specific factors such as Radian, NatLog, or PureNumber.
 * It acts as a factory for creating instances of the `Factor` sealed trait.
 *
 * The object includes predefined constants representing symbols or keywords associated with certain factors.
 * These constants include mathematical symbols like 'ℯ' and 'π', as well as their textual alternatives.
 *
 * The `apply` method takes a string input and matches it to one of the predefined constants to return an appropriate `Factor` instance.
 *
 * Matching logic:
 * - Strings representing π (like "π", "pi", "Radian", "PI") will return the `Radian` factor.
 * - The symbol 'ℯ' will return the `NatLog` factor.
 * - All other strings default to the `PureNumber` factor.
 */
object Factor {
  val sE = "\uD835\uDF00"
  val sPi = "\uD835\uDED1"
  val sPiAlt0 = "pi"
  val sPiAlt1 = "Radian"
  val sPiAlt2 = "PI"

  def apply(w: String): Factor = w match {
    case `sPi` | `sPiAlt0` | `sPiAlt1` | `sPiAlt2` => Radian
    case `sE` => NatLog
    case _ => PureNumber
  }
}

object Render {
  private def renderInt(x: Int): (String, Boolean) = (x.toString, true)

  /**
    * Add exact parameter.
    *
    * @param x the Rational value to be rendered.
    * @return a String.
    */
  private def renderRational(x: Rational): (String, Boolean) = x.renderConditional(true)

  private def renderDouble(x: Double): (String, Boolean) = (x.toString, false)

  /**
    * Method to render a Value as a tuple of String and Boolean where the latter represents whether or not we were able
    * to render the value exactly or not.
    *
    * @param v the Value to be rendered.
   * @param exact true if the Value is exact.
    * @return a tuple of String and Boolean.
    *         If the value is an Int or a Rational, then the Boolean returned is true; otherwise, it's false.
    */
  def renderValue(v: Value, exact: Boolean = true): (String, Boolean) =
    optionMap(v)(
      y => renderInt(y),
      x => optionMap(x)(
        // TODO Issue #48
        y => if (exact) renderRational(y) else (y.renderApproximate(100, Some(16)).stripLeading(), false),
        {
          case Some(n) => Some(renderDouble(n))
          case None => None
        })
    ).getOrElse(("<undefined>", true))
}

