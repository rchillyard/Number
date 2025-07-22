/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.inner

import com.phasmidsoftware.number.core.inner.Factor.composeDyadic
import com.phasmidsoftware.number.core.inner.Operations.doComposeValueDyadic
import com.phasmidsoftware.number.core.inner.Rational.{cubeRoots, squareRoots, toIntOption}
import com.phasmidsoftware.number.core.inner.Value.{fromDouble, scaleDouble, valueToString}
import com.phasmidsoftware.number.core.{Field, Number, Real}
import com.phasmidsoftware.number.misc.FP._
import scala.language.implicitConversions
import scala.util._

/**
  * Represents a factor type used for evaluations, conversions, and rendering in a specific context.
  *
  * CONSIDER defining an imaginary (or Complex) subclass of Factor.
  * CONSIDER renaming this trait as Domain.
  *
  * This trait serves as a foundation for different kinds of factors and provides functionality such as:
  * - Determining whether the factor is additive.
  * - Checking if the factor meets certain conditions within a specific context.
  * - Performing conversions of values between compatible factors.
  * - Rendering values in the context of the factor.
  */
sealed trait Factor {
  /**
    * A value which can be used to convert a value associated with this Factor to a different Factor.
    * CONSIDER redefining this as a Number (which could be exact or fuzzy).
    */
  val value: Double

  /**
    * Determines whether the current factor can be augmented by the given factor.
    * CONSIDER do we need this method now that we have add(x, y, this)?
    *
    * @param f the factor to be checked for compatibility with addition.
    * @return true if the factors can be added; false otherwise.
    */
  def canAdd(f: Factor): Boolean =
    f == this

  /**
    * Determines whether the current factor can be multiplied by the given factor.
    * CONSIDER do we need this method now that we have multiply(x, y, this)?
    *
    * @param f the factor to be checked for compatibility with multiplication.
    * @return true if the factors can be multiplied; false otherwise.
    */
  def canMultiply(f: Factor): Boolean

  /**
    * Determines whether `this` factor can be raised by the given factor `f`.
    * CONSIDER do we need this method now that we have raise(x, y, this)?
    *
    * @param f the factor to be checked for compatibility with raising to a power.
    * @return true if the factors can be powered; false otherwise.
    */
  def canRaise(f: Factor, exponent: Field): Boolean = f match {
    case PureNumber =>
      true
    case Radian =>
      this == PureNumber // XXX CHECK this
    case NatLog =>
      this == PureNumber // XXX CHECK this
    case _ =>
      false
  }

  /**
    * Determines if the current factor operates additively.
    *
    * CONSIDER rewriting in terms of canAdd
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
    * Determines whether given `value` with this `Factor` can be rendered exactly.
    *
    * @return true if this `Factor` can be rendered exactly; false otherwise.
    */
  def canShow(value: Value): Boolean

  /**
    * Method to render a Value in the context of this Factor.
    *
    * @param x the Value.
    * @return a String.
    */
  def render(x: Value): String =
    render(valueToString(x))

  /**
    * Method to render a Value (which has already been converted to a String) in the context of this Factor.
    *
    * @param v a String representing the Value.
    * @return a String.
    */
  def render(v: String): String

  // CONSIDER adding the following methods for exact arithmetic,
  // or possibly with an additional member in the result defining any additional fuzz.

  /**
    * Computes the negation of the given value `x` within the context of this factor.
    *
    * @param x the value to negate
    * @return an optional `ProtoNumber` representing the negated value, or `None` if the operation is not valid
    */
  def negate(x: Value): Option[ProtoNumber]

  /**
    * Computes the multiplicative inverse of the given value if it exists.
    *
    * @param x the value to be inverted
    * @return an optional `ProtoNumber` representing the inverse of `x`,
    *         or `None` if the inverse cannot be computed
    */
  def invert(x: Value): Option[ProtoNumber]

  /**
    * Attempts to add two values together (each based on `this Factor`) and computes an appropriate factor for the result.
    *
    * @param x the first value
    * @param y the addend
    * @param f the factor associated with y
    * @return an optional tuple containing the resultant value and its factor
    */
  def add(x: Value, y: Value, f: Factor): Option[ProtoNumber]

  /**
    * Multiplies two values together and computes an appropriate factor for the result.
    *
    * @param x the first value
    * @param y the multiplicand
    * @param f the factor associated with y
    * @return an optional tuple containing the resultant value and its factor
    */
  def multiply(x: Value, y: Value, f: Factor): Option[ProtoNumber]

  /**
    * Raises x to the power of y and compute an appropriate factor for the result.
    * NOTE: for now, we only allow raising to a power of a PureNumber.
    * TODO: enable powers of Radian factor.
    *
    * @param x the first value
    * @param y the power
    * @param f the factor associated with y
    * @return an optional tuple containing the resultant value and its factor
    */
  def raise(x: Value, y: Value, f: Factor): Option[ProtoNumber] =
    (this, f) match {
      case (_, PureNumber) =>
        clean(doRaiseByPureNumber(x, y))
      case (_, Log2) =>
        for {
          z <- Log2.asPureValue(y)
          q <- clean(doRaiseByPureNumber(x, z))
        } yield q
      case (PureNumber, _) =>
        f.raise(y, x, this)
      case _ =>
        None
    }

  /**
    * Cleans a given optional ProtoNumber by potentially simplifying its root quantities
    * based on specific conditions and returning the modified result.
    *
    * @param po an optional ProtoNumber that may be simplified
    * @return an optional ProtoNumber, simplified if applicable, or `None` if input is `None`
    */
  def clean(po: Option[ProtoNumber]): Option[ProtoNumber] =
    po match {
      case Some(p@(v, f, zo)) =>
        // XXX simplify root quantities
        f match {
          case AnyRoot(k) if k == Rational.one =>
            Some((v, PureNumber, zo))
          case r@InversePower(k) if (k / 2).isInteger =>
            r.simplifyRoot(p, v, k, 2)
          case _ =>
            Some(p)
        }
      case None => None
    }


  /**
    * Raises the value `x` to the power of the value `y`.
    *
    * @param x the base value to be raised
    * @param y the exponent value (from a PureNumber)
    * @return an optional result of type `ProtoNumber`,
    *         representing the computed value if the operation is valid, or `None` otherwise
    */
  def doRaiseByPureNumber(x: Value, y: Value): Option[ProtoNumber]
}

/**
  * Trait to define a Factor which is a scalar (something that can be scaled by a pure number).
  */
sealed trait Scalar extends Factor {
  /**
    * Determines whether given `value` with this `Factor` can be rendered exactly.
    *
    * @return true (value is ignored).
    */
  def canShow(value: Value): Boolean = true

  /**
    * Determines whether the current factor can be multiplied by the given factor.
    *
    * @param f the factor to be checked for compatibility with multiplication.
    * @return true if the factors can be multiplied; false otherwise.
    */
  def canMultiply(f: Factor): Boolean = f match {
    case _: Scalar =>
      true
    case _ =>
      false
  }

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
  def isA(context: Context): Boolean =
    context.factorQualifies(this)

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
    case _ =>
      None
  }

  def negate(x: Value): Option[ProtoNumber] =
    for {v <- Operations.doTransformValueMonadic(x)(MonadicOperationNegate.functions)} yield (v, this, None)

  /**
    * Adds two values together, applying the specified factor, to compute a resultant value and factor.
    *
    * @param x the first value to be added
    * @param y the second value (addend) to be added
    * @param f the factor associated with the operation
    * @return an optional tuple containing the resultant value, its factor, and optional fuzziness,
    *         or None if the operation cannot be performed under the given factor
    */
  def add(x: Value, y: Value, f: Factor): Option[ProtoNumber] =
    for {v <- Factor.composeDyadic(x, y)(DyadicOperationPlus) if this == f} yield (v, f, None)

}

/**
  * Companion object for the `Scalar` trait, providing utility methods.
  *
  * The `unapply` method allows pattern matching on an instance of `Scalar`
  * to extract its underlying `Double` value.
  */
object Scalar {
  def unapply(arg: Scalar): Option[Double] = Some(arg.value)
}

/**
  * Trait to define a Factor which is a scaled version of the natural log.
  */
sealed trait Logarithmic extends Factor {
  /**
    * This is a `String` representation of the base of the logarithm used for this `Factor`.
    */
  val base: String

  /**
    * Determines whether given `value` with this `Factor` can be rendered exactly.
    *
    * @return true if this `Factor` can be rendered exactly; false otherwise.
    */
  def canShow(value: Value): Boolean =
    asPower(value).isDefined

  /**
    * Determines whether the current factor can be multiplied by the given factor.
    *
    * TODO implement me properly
    *
    * @param f the factor to be checked for compatibility with multiplication.
    * @return true if the factors can be multiplied; false otherwise.
    */
  def canMultiply(f: Factor): Boolean = false

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
  def isA(context: Context): Boolean =
    context.factorQualifies(this)

  /**
    * Convert a value x from this factor to f if possible, using simple scaling.
    * Result is defined only if f is Logarithmic.
    *
    * @param v the value to be converted.
    * @param f the factor of the result.
    * @return a Try of Value which, given factor f, represents the same quantity as x given this.
    */
  def convert(v: Value, f: Factor): Option[Value] = f match {
    case l@Logarithmic(_) =>
      Some(fromDouble(Value.maybeDouble(v) map (x => scaleDouble(x, this.value, l.value))))
    case _ =>
      None
  }

  /**
    * Converts a given value into a string representation of its corresponding power notation or root notation
    * based on specific conditions.
    *
    * @param v the value to be converted, which can represent an integer power or a rational root.
    * @return a string representing the value in power notation, root notation, or a generic exponential format.
    */
  def asPower(v: Value): Option[String] = v match {
    case Right(1) =>
      Some(base)
    case Right(2) =>
      Some(base + "\u00B2")
    case Right(3) =>
      Some(base + "\u00B3")
    case Right(x) if x > 3 & x < 10 =>
      Some(base + Logarithmic.incrementUnicode("\u2070", 0, x))
    case Left(Right(r)) if r * 2 == Rational.one =>
      Some("\u221A" + base)
    case Left(Right(r)) if r * 3 == Rational.one =>
      Some("\u221B" + base)
    case Left(Right(r)) if r * 4 == Rational.one =>
      Some("\u221C" + base)
    case Right(x) =>
      Some(base + "^" + x)
    case _ =>
      None
  }

  /**
    * Renders a string representation of the given value based on specific conditions.
    * If the value can be represented in power or root notation, it uses that representation;
    * otherwise, it falls back to the string representation of the value.
    *
    * @param x the value to be rendered into a string representation.
    * @return a string representation of the value, using power or root notation when applicable.
    */
  override def render(x: Value): String =
    asPower(x) getOrElse toString

  /**
    * Renders a string representation of the value based on the current base and the given input value.
    * CONSIDER why do we have a second signature for render?
    *
    * @param v the input value to be rendered, represented as a string
    * @return a string combining the base and the input value in a specific exponential format
    */
  def render(v: String): String = base + "^" + v

  /**
    * Inverts the given value by raising it to the power of -1, using a pure number factor.
    *
    * @param x the value to be inverted
    * @return an optional result of type `ProtoNumber`, representing the inverted value if the operation is valid, or `None` otherwise
    */
  def invert(x: Value): Option[ProtoNumber] =
    this.raise(x, Value.fromInt(-1), PureNumber)

  /**
    * Negates the given value and returns the result as an optional ProtoNumber.
    *
    * @param x the value to be negated
    * @return an optional ProtoNumber representing the negated value, or None if the operation is invalid
    */
  def negate(x: Value): Option[ProtoNumber] = None

  /**
    * Adds two values together and computes an appropriate factor for the result.
    *
    * @param x the first value
    * @param y the addend
    * @param f the factor associated with y
    * @return an optional tuple containing the resultant value and its factor
    */
  def add(x: Value, y: Value, f: Factor): Option[ProtoNumber] = None

  /**
    * Multiplies two values together and computes an appropriate factor for the result.
    * TODO implement for Log10
    *
    * @param x the first value
    * @param y the multiplicand
    * @param f the factor associated with y
    * @return an optional tuple containing the resultant value and its factor
    */
  def multiply(x: Value, y: Value, f: Factor): Option[ProtoNumber] = this match {
    case `f` =>
      clean(for {v <- Factor.composeDyadic(x, y)(DyadicOperationPlus)} yield (v, this, None))
    case Logarithmic("2") =>
      clean(for {
        a <- Value.maybeRational(x)
        b <- Value.maybeRational(y)
        log <- convertRationalToMaybeLog(b, "2", f)
      } yield (Value.fromRational(a + log), this, None))
    case _ =>
      None
  }

  /**
    * Converts a given Rational number to an optional logarithmic representation based on the provided base and factor.
    *
    * @param b    the input rational number to be converted.
    * @param base the base of the logarithm, such as "2".
    * @param f    the factor indicating whether the conversion is logarithmic or a pure number.
    * @return an Option containing the converted rational value if successful, or None otherwise.
    */
  private def convertRationalToMaybeLog(b: Rational, base: String, f: Factor): Option[Rational] = f match {
    case Logarithmic(`base`) =>
      Some(b)
    case PureNumber =>
      base match {
        case "2" =>
          b.maybeInt flatMap Rational.binaryLogs.get map Rational.apply
//        case "10" =>
//          Rational.base10Logs.get(b)
        case _ =>
          None // Not yet implemented
      }
  }

  /**
    * Raises the value `x` to the power of the value `y`.
    *
    * @param x the base value to be raised
    * @param y the exponent value (from a PureNumber)
    * @return an optional result of type `ProtoNumber`,
    *         representing the computed value if the operation is valid, or `None` otherwise
    */
  def doRaiseByPureNumber(x: Value, y: Value): Option[ProtoNumber] =
    for {v <- Factor.composeDyadic(x, y)(DyadicOperationTimes)} yield (v, this, None)
}

/**
  * Companion object for the `Logarithmic` class.
  *
  * This object provides utilities associated with the `Logarithmic` class, including
  * operations for pattern matching and internal functionality for string manipulation.
  */
object Logarithmic {
  /**
    * Extractor method for the `Logarithmic` trait that enables pattern matching to extract the inner value.
    *
    * @param arg the `Logarithmic` instance from which the value will be extracted.
    * @return an `Option` containing the extracted `Double` value if available, otherwise `None`.
    */
  def unapply(arg: Logarithmic): Option[String] = Some(arg.base)

  /**
    * Increments the Unicode value of the character at the specified index in the given string by the given amount.
    *
    * @param str   the input string whose character's Unicode value will be incremented
    * @param index the index of the character in the string to be incremented
    * @param x     the value to increment the character's Unicode value by
    * @return a new string with the character at the specified index incremented by the specified value
    */
  private def incrementUnicode(str: String, index: Int, x: Int): String = {
    val chars: Array[Char] = str.toArray
    chars.update(index, (chars(index) + x).toChar)
    new String(chars)
  }
}

/**
  * A sealed trait extending the `Factor` trait, representing a mathematical root-based factorization.
  *
  * This trait provides mechanisms to convert values associated with one root-based factor
  * to another, when such conversions are mathematically possible.
  */
sealed trait InversePower extends Factor {

  /**
    * Retrieves the root degree represented by this factor.
    *
    * The root degree indicates the mathematical nth root applicable to this factor.
    * For instance, a root degree of 2 represents a square root, a root degree of 3
    * represents a cube root, and so on.
    *
    * @return the integer value of the root degree.
    */
  def inversePower: Rational

  /**
    * A value which can be used to convert a value associated with this Factor to a different Factor.
    */
  val value: Double = inversePower.toDouble

  /**
    * Converts the given value into its root representation if applicable.
    *
    * @param value the Value object to be converted into its root representation.
    * @return an optional String representing the root form of the value, or None if the conversion is not applicable.
    */
  def asRoot(value: Value): Option[String]

  /**
    * Determines whether given `value` with this `Factor` can be rendered exactly.
    *
    * @return true if this `Factor` can be rendered exactly; false otherwise.
    */
  def canShow(value: Value): Boolean =
    asRoot(value).isDefined

  /**
    * Converts the object into its string representation.
    *
    * @return a string representation of the object
    */
  override def toString: String = s"^(${inversePower.invert})"

  /**
    * Generates a string representation of the cube root for a given input.
    *
    * @param x the input string to be represented with the cube root symbol
    * @return a string in the format ³√ followed by the input
    */
  def render(x: String): String = toString

  /**
    * Method to render a Value in the context of this Factor.
    *
    * @param x the Value.
    * @return a String.
    */
  override def render(x: Value): String =
    asImaginary(x) orElse asRoot(x) getOrElse convert(x, PureNumber).toString

  /**
    * Simplifies a root representation of a number based on the given parameters.
    * The method computes whether the provided value `v` can be converted into a simpler root form
    * using the provided `divisor` (e.g., square root for divisor 2, cube root for divisor 3).
    *
    * @param p       the prototype number containing initial value and metadata
    * @param v       the value to be simplified
    * @param r       the rational factor determining the type of root operation
    * @param divisor the root degree (e.g., 2 for square root, 3 for cube root)
    * @return an optional ProtoNumber containing the simplified root representation,
    *         or the original input if simplification is not applicable
    */
  def simplifyRoot(p: ProtoNumber, v: Value, r: Rational, divisor: Int): Option[ProtoNumber] = {
    val q: Option[Rational] =
      for {
        Rational(n, d) <- Value.maybeRational(v)
        if n.isValidInt
        i = n.toInt
        if d.isValidInt
        j = d.toInt
        k <- if (divisor == 2) squareRoots.get(i) else if (divisor == 3) cubeRoots.get(i) else None
        l <- if (divisor == 2) squareRoots.get(j) else if (divisor == 3) cubeRoots.get(j) else None
      } yield Rational(k, l)
    q match {
      case Some(x) =>
        val factor =
          (r, divisor) match {
            case (Rational.four, 2) => SquareRoot
            case (Rational.nine, 3) => CubeRoot
            case (Rational.two, 2) => PureNumber
            case (Rational.three, 3) => PureNumber
            case (_, 1) => AnyRoot(r) // NOTE: actually, this is impossible
          }
        clean(Some((Value.fromRational(x), factor, p._3)))
      case None =>
        (r, divisor) match {
          case (Rational.two, 2) => Some((v, SquareRoot, p._3))
          case (Rational.three, 3) => Some((v, CubeRoot, p._3))
          case _ => Some(p)
        }
    }
  }

  /**
    * Inverts the given value by raising it to the power of -1, using a pure number factor.
    *
    * @param x the value to be inverted
    * @return an optional result of type `ProtoNumber`,
    *         representing the inverted value if the operation is valid, or `None` otherwise
    */
  def invert(x: Value): Option[ProtoNumber] =
    this.raise(x, Value.fromInt(-1), PureNumber)

  /**
    * Negates the given value and computes an appropriate output tuple.
    * The computation may include a value, its associated factor, and an optional fuzziness.
    *
    * @param x the value to be negated
    * @return an optional tuple containing the negated value, its factor,
    *         and an optional fuzziness; `None` if the operation is not applicable
    */
  def negate(x: Value): Option[ProtoNumber] = None

  /**
    * Adds two values together and computes an appropriate factor for the result.
    *
    * @param x the first value
    * @param y the addend
    * @param f the factor associated with y
    * @return an optional tuple containing the resultant value and its factor
    */
  def add(x: Value, y: Value, f: Factor): Option[ProtoNumber] = None

  /**
    * Multiplies two values together and computes an appropriate factor for the result.
    *
    * @param x the first value
    * @param y the multiplicand
    * @param f the factor associated with y
    * @return an optional tuple containing the resultant value and its factor
    */
  def multiply(x: Value, y: Value, f: Factor): Option[ProtoNumber] =
    (this, f) match {
    case (Root(r), PureNumber) =>
      clean(for {
        a <- Value.maybeRational(x)
        b <- Value.maybeRational(y)
        z = a * (b ∧ r) if b.signum > 0
      } yield (Value.fromRational(z), this, None))
    case (Root(r), Root(s)) =>
      clean(multiplyRootValues(x, r, y, s))
    case (_, Log2) =>
      clean(for {
        z <- Log2.asPureValue(y)
        q <- multiply(x, z, PureNumber)
      } yield q)
    case _ =>
      None
  }

  /**
    * Multiplies the root values of two given `Value` objects based on their respective root degrees.
    *
    * This method computes the product of the rational representations of `x` and `y`, each raised
    * to their specified root degrees (`r` and `s` respectively). The result is wrapped with an associated
    * factor and optional additional information if the operation is valid. The method ensures that the
    * input values are positive and can be represented rationally.
    *
    * @param x the first value to be multiplied
    * @param r the root degree of the first value
    * @param y the second value to be multiplied
    * @param s the root degree of the second value
    * @return an optional tuple containing the resultant value, its associated factor, and optional metadata;
    *         `None` if the values are not positive or cannot be represented rationally
    */
  def multiplyRootValues(x: Value, r: Int, y: Value, s: Int): Option[ProtoNumber] =
    if (r == s && r >= 0 && Value.signum(x) > 0 && Value.signum(y) > 0)
      for {
        a <- Value.maybeRational(x)
        b <- Value.maybeRational(y)
      } yield (Value.fromRational(a * b), AnyRoot(r), None)
    else
      for {
        a <- Value.maybeRational(x)
        b <- Value.maybeRational(y)
        z = (a ∧ s) * (b ∧ r) if b.signum > 0 && a.signum > 0
      } yield (Value.fromRational(z), AnyRoot(r * s), None)

  /**
    * Raises the value `x` to the power of the value `y`.
    *
    * @param x the base value to be raised
    * @param y the exponent value (from a PureNumber)
    * @return an optional result of type `ProtoNumber`,
    *         representing the computed value if the operation is valid, or `None` otherwise
    */
  def doRaiseByPureNumber(x: Value, y: Value): Option[ProtoNumber] = this match {
    case Root(r) =>
      for {
        q: Rational <- Value.maybeRational(y)
        z = q / r
        i <- z.maybeInt
      } yield (x, AnyRoot(i), None)
  }

  /**
    * Converts a given Value into its imaginary representation, if applicable.
    *
    * The method checks if the provided `value` can be represented as an imaginary number
    * based on specific mathematical conditions, such as whether it is a negative integer
    * and has a corresponding square root.
    *
    * @param value the Value object to be evaluated and potentially converted into its
    *              imaginary representation.
    * @return an optional String representing the imaginary form of the value, or None if
    *         the value does not meet the conditions for conversion to an imaginary number.
    */
  def asImaginary(value: Value): Option[String] =
    (for {
      r <- Value.maybeRational(value)
      n <- toIntOption(r) if r.signum < 0 && r.isInteger
      x <- Rational.squareRoots.get(-n)
      s = if (x == 1) "" else x.toString
    } yield s) map ("i" + _)

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
  def isA(context: Context): Boolean =
    context.factorQualifies(this)
}

/**
  * Companion object for the `Root` trait, providing utility methods for working with `Root` instances.
  */
object InversePower {
  /**
    * Extractor method to retrieve the root degree from a `Root` instance.
    *
    * This method is used for pattern matching to extract the integer root degree
    * associated with the given `Root` object.
    *
    * @param arg the `Root` instance from which to extract the root degree.
    * @return an `Option` containing the root degree as an `Int`, or `None` if the extraction fails.
    */
  def unapply(arg: InversePower): Option[Rational] = Some(arg.inversePower)
}

/**
  * Represents an abstract base class for mathematical root operations by extending `InversePower`.
  *
  * A number with value `x` and factor `InversePower(n)` represents the `nth` root of `x` where `n` is a `Rational`.
  * For example, `ExactNumber(5, SquareRoot)` is the square root of 5, where `SquareRoot` is an alias for `Root(2)`.
  *
  * The `Root` class encapsulates the concept of a mathematical `nth` root and provides
  * methods to retrieve the degree of the root.
  * It is intended to be extended by concrete implementations that define specific behaviors
  * for inverse power computation.
  *
  * @constructor Creates a new instance of the `Root` class with the specified root degree.
  * @param root the integer value representing the nth root degree.
  */
abstract class Root(val root: Int) extends InversePower {

  /**
    * Retrieves the root degree represented by this factor in the form of a Rational.
    *
    * The root degree indicates the mathematical `nth` root applicable to this factor.
    * For instance, a root degree of 2 represents a square root, a root degree of 3
    * represents a cube root, and so on.
    *
    * @return the integer value of the root degree.
    */
  def inversePower: Rational = root

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
      val po = this.raise(v, Value.fromRational(Rational(z, root)), PureNumber)
      (po map {
        case (x, _, _) =>
          x
      }).orElse(for {
        q <- doComposeValueDyadic(v, Number(z).specialize.nominalValue)(DyadicOperationPower.functions)
        inverseV <- Value.inverse(v)
        _ = println(s"convert: $v, $f, $q, $inverseV")
        z <- doComposeValueDyadic(q, inverseV)(DyadicOperationPower.functions)
      } yield z)
    case _ =>
      None
  }

  /**
    * Determines whether the current factor can be raised by the given factor.
    *
    * @param f the factor to be checked for compatibility with raising to a power.
    * @return true if the factors can be powered; false otherwise.
    */
  override def canRaise(f: Factor, exponent: Field): Boolean = f match {
    case PureNumber =>
      exponent match {
        case Real(n: Number) =>
          n.isInteger && {
            val x = n.toInt.get
            x > 0 && x % root == 0
          }
        case _ =>
          false
      }
    case _ =>
      super.canRaise(f, exponent)
  }

  /**
    * Determines whether the current factor can be multiplied by the given factor.
    *
    * TODO check the logic here
    *
    * @param f the factor to be checked for compatibility with multiplication.
    * @return true if the factors can be multiplied; false otherwise.
    */
  def canMultiply(f: Factor): Boolean = f match {
    case x: Root =>
      x.root == this.root
    case _ =>
      false
  }

}

/**
  * Represents the companion object for the `Root` class.
  *
  * Provides utility functionality related to `Root`, including methods for
  * matching and extracting values from instances of the `Root` type.
  */
object Root {
  /**
    * Extracts the root integer value encapsulated in the given `Root` instance.
    *
    * @param r the `Root` instance from which the integer value is to be extracted.
    * @return an `Option` containing the root integer if extraction is successful, or `None` if not.
    */
  def unapply(r: Root): Option[Int] = Some(r.root)
}

/**
  * A case object representing the pure number 1.
  * `PureNumber` extends the `Scalar` trait and behaves as a unit scalar factor.
  *
  * CONSIDER expanding to include a DecimalNumber as well as a PureNumber (which logically should be represented in binary).
  * Just because we can represent somehing exactly in binary doesn't mean we can represent it in decimal (and vice versa).
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
  override def isA(context: Context): Boolean =
    context.factorQualifies(this)

  override def toString: String = ""

  /**
    * Renders the given string and returns it.
    *
    * @param x the input string to be rendered
    * @return the rendered string
    */
  def render(x: String): String = x

  /**
    * Computes the inverse of a given Value, leveraging the monadic transformation functions defined
    * in MonadicOperationInvert. This method allows for the calculation of an inverted number when valid.
    *
    * @param x the input Value to be inverted
    * @return an Option containing the resulting ProtoNumber if the inversion is successful, or None otherwise
    */
  def invert(x: Value): Option[ProtoNumber] =
    for {v <- Operations.doTransformValueMonadic(x)(MonadicOperationInvert.functions)} yield (v, this, None)

  /**
    * Multiplies two values together and computes an appropriate factor for the result.
    *
    * @param x the first value
    * @param y the multiplicand
    * @param f the factor associated with y
    * @return an optional tuple containing the resultant value and its factor
    */
  def multiply(x: Value, y: Value, f: Factor): Option[ProtoNumber] =
    if (this == f)
      clean(for {v <- Factor.composeDyadic(x, y)(DyadicOperationTimes)} yield (v, f, None))
    else
      f.multiply(y, x, this)

  /**
    * Raises the value `x` to the power of the value `y`.
    *
    * @param x the base value to be raised
    * @param y the exponent value (from a PureNumber)
    * @return an optional result of type `ProtoNumber`,
    *         representing the computed value if the operation is valid, or `None` otherwise
    */
  def doRaiseByPureNumber(x: Value, y: Value): Option[ProtoNumber] = {
    Value.inverse(y) flatMap Value.maybeRational match {
      case Some(Rational.two) =>
        clean(Some((x, SquareRoot, None)))
      case Some(Rational.three) =>
        clean(Some((x, CubeRoot, None)))
      case Some(r) if r > 1 =>
        clean(Some((x, AnyRoot(r), None)))
      case _ =>
        for {
          v <- Factor.composeDyadic(x, y)(DyadicOperationPower)
        } yield (v, this, None)
    }
  }

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
  /**
    * Represents the mathematical constant π (pi), approximately equal to 3.14159.
    * This value is commonly used in trigonometric, geometric, and other mathematical computations.
    */
  val value: Double = Math.PI

  /**
    * Determines whether `this` factor can be raised by the given factor `f`.
    *
    * @param f the factor to be checked for compatibility with raising to a power.
    * @return true if the factors can be powered; false otherwise.
    */
  override def canRaise(f: Factor, exponent: Field): Boolean =
    exponent.isZero || exponent.isUnity

  /**
    * Returns the string representation of the `Radian` factor.
    *
    * @return a string representation of the mathematical constant π.
    */
  override def toString: String = Factor.sPi

  /**
    * Renders a string by appending the mathematical symbol for pi (π).
    *
    * @param x the input string to which the mathematical symbol for pi will be appended
    * @return a new string consisting of the input string followed by the symbol for pi
    */
  def render(x: String): String = x + Factor.sPi


  /**
    * Inverts the given value by raising it to the power of -1, using a pure number factor.
    *
    * @param x the value to be inverted
    * @return None.
    */
  def invert(x: Value): Option[ProtoNumber] =
    None

  /**
    * Multiplies x with y provided that f is a PureNumber.
    *
    * @param x the first value
    * @param y the multiplicand
    * @param f the factor associated with y
    * @return an optional tuple containing the resultant value and its factor
    */
  def multiply(x: Value, y: Value, f: Factor): Option[ProtoNumber] =
    clean(for {v <- Factor.composeDyadic(x, y)(DyadicOperationTimes); if f == PureNumber} yield (v, this, None))

  /**
    * Raises the value `x` to the power of the value `y`.
    *
    * @param x the base value to be raised
    * @param y the exponent value (from a PureNumber)
    * @return an optional result of type `ProtoNumber`,
    *         representing the computed value if the operation is valid, or `None` otherwise
    */
  def doRaiseByPureNumber(x: Value, y: Value): Option[ProtoNumber] = None
}

/**
  * This factor essentially provides log/exponent arithmetic.
  *
  * NOTE: A number in factor NatLog will evaluate as e raised to that power.
  * So, the `value` is the natural log of the evaluation.
  *
  * Thus the range of such values is any positive number.
  */
case object NatLog extends Logarithmic {
  /**
    * Represents the base symbol used for the `NatLog` factor.
    *
    * The symbol is defined as the mathematical constant ℯ (e).
    * This constant is used to indicate the base of the natural logarithm
    * and is sourced from the `Factor` object.
    */
  val base: String = Factor.sE

  /**
    * Represents the default value for the `NatLog` factor.
    *
    * This value is used in conversions (it's the natural log of e, viz., 1.0).
    */
  val value: Double = 1.0

  override def toString: String = Factor.sE
}

/**
  * Object representing the logarithm to base 2.
  *
  * This object extends the `Logarithmic` trait and provides specific functionality
  * for dealing with logarithms to the base 2. It defines the base as "2" and computes the
  * logarithmic value (`math.log(2)`).
  *
  * Methods include:
  * - `toString`: Returns "log2" as the string representation.
  * - `render(x: Value)`: Converts the input value to a string representation based on the power notation
  * specific to the base 2.
  *
  * @see Logarithmic
  */
case object Log2 extends Logarithmic {
  /**
    * Represents the base of the logarithm as a string.
    *
    * For this specific implementation, the base is "2", which corresponds
    * to logarithms computed for base 2 (binary logarithms).
    */
  val base: String = "2"

  /**
    * Represents the default value for the `NatLog` factor.
    *
    * This value is used in conversions (it's the natural log of 2, approximately 0.69).
    */
  val value: Double = math.log(2)

  override def toString: String = "log2"

  /**
    * Converts the given `Value` into a pure integral value, if possible.
    * The method evaluates the input as a rational number, raises it to the power of 2,
    * and extracts its integer representation, if it is exact.
    *
    * @param y the `Value` to be evaluated and potentially converted to a pure integral value.
    * @return an `Option` containing the resulting integral `Value` if successful,
    *         otherwise `None`.
    */
  def asPureValue(y: Value): Option[Value] =
    for {
      v <- Value.maybeRational(y)
      p <- (Rational.two ^ v).toOption
      i <- p.maybeInt
    } yield Value.fromInt(i)
}
/**
  * Represents the logarithm with base 10.
  *
  * This object extends the `Logarithmic` trait and provides functionality for representing
  * and rendering logarithms of base 10.
  *
  * @constructor Creates an instance of `Log10`.
  */
case object Log10 extends Logarithmic {
  /**
    * Represents the base of the logarithm, which is set to "10".
    */
  val base: String = "10"

  /**
    * Represents the natural logarithm of 10.
    *
    * This value is a constant that evaluates to the natural logarithm (base e) of the number 10.
    */
  val value: Double = math.log(10)

  override def toString: String = "log10"
}

/**
  * This object represents the square root factor.
  */
case object SquareRoot extends Root(2) {

  override def toString: String = "√"

  /**
    * Renders the input string with a square root symbol.
    *
    * @param x the input string to be rendered
    * @return a string prefixed with the square root symbol
    */
  override def render(x: String): String = s"√$x"

  /**
    * Constructs an optional string representation of the current root object concatenated
    * with the string representation of the specified value.
    *
    * @param value the value to be represented as part of the root string.
    * @return an Option containing the concatenated string representation of the root
    *         and the value, or None if the string construction is unsuccessful.
    */
  def asRoot(value: Value): Option[String] = Some(s"$toString${valueToString(value)}")

  /**
    * Raises the value `x` to the power of the value `y`.
    *
    * TODO move this up into Root.
    *
    * @param x the base value to be raised
    * @param y the exponent value (from a PureNumber)
    * @return an optional result of type `ProtoNumber`,
    *         representing the computed value if the operation is valid, or `None` otherwise
    */
  override def doRaiseByPureNumber(x: Value, y: Value): Option[ProtoNumber] =
    Value.maybeInt(y) match {
      case Some(`root`) =>
        Some((x, PureNumber, None))
      case None =>
        for {
          p <- Value.maybeRational(y)
          z = p / root
          q <- composeDyadic(x, Value.fromRational(z.n))(DyadicOperationPower)
        } yield (q, AnyRoot(z.d), None)
      case _ =>
        None
    }

  /**
    * Attempts to add two `Value` instances under the influence of a given `Factor`.
    *
    * The method verifies whether the operation can be applied with the given factor
    * (e.g., a square root) and performs rational arithmetic to compute the result.
    * The operation ensures that the values can be combined via integer arithmetic
    * after factoring out common divisors and applies root simplifications where necessary.
    *
    * @param x the first value to be added
    * @param y the second value to be added
    * @param f the factor defining the context for the addition (e.g., square root)
    * @return an optional `ProtoNumber` that contains the resultant `Value`, its factor,
    *         and optional metadata; `None` if the operation cannot be performed
    */
  override def add(x: Value, y: Value, f: Factor): Option[ProtoNumber] =
    whenever(this == f)(
      for {
        a <- Value.maybeRational(x)
        b <- Value.maybeRational(y)
        h = a.d.gcd(b.d)
        p = a * h if p.isInteger
        q = b * h if q.isInteger
        g = p.n.gcd(q.n)
        z = p / g + q / g
        if h.isValidInt
        r <- Rational.squareRoots.get(h.toInt) // NOTE this is the only code that depends on this being SquareRoot
      } yield (Value.fromRational(z * g / r), f, None))

  /**
    * Provides an implicit conversion from an `Int` to an imaginary number representation.
    *
    * This class allows integers to be interpreted as imaginary numbers by appending `.i`
    * to an integer. The imaginary number is constructed using the square of the integer
    * negated, combined with the square root notation.
    *
    * @param x the integer to be converted into an imaginary number
    */
  implicit class IntToImaginary(x: Int) {
    /**
      * Converts an integer to its imaginary number representation.
      *
      * This method creates an imaginary number by negating the square of the integer
      * and associating it with the square root operator.
      *
      * @return a `Number` instance representing the imaginary number constructed
      *         from the integer.
      */
    def i: Number = Number().make(-x * x, SquareRoot)
  }
}

/**
  * This object represents the cube root factor.
  */
case object CubeRoot extends Root(3) {

  /**
    * Converts the object into its string representation.
    *
    * @return a string representation of the object
    */
  override def toString: String = "³√"

  /**
    * Generates a string representation of the cube root for a given input.
    *
    * @param x the input string to be represented with the cube root symbol
    * @return a string in the format ³√ followed by the input
    */
  override def render(x: String): String = s"³√$x"

  /**
    * Raises the value `x` to the power of the value `y`.
    *
    * @param x the base value to be raised
    * @param y the exponent value (from a PureNumber)
    * @return an optional result of type `ProtoNumber`,
    *         representing the computed value if the operation is valid, or `None` otherwise
    */
  override def doRaiseByPureNumber(x: Value, y: Value): Option[ProtoNumber] =
    y match {
      case Right(`root`) =>
        Some((x, PureNumber, None))
      case _ =>
        None
    }

  /**
    * Converts a given value into a string representation and appends it to the
    * root symbol.
    *
    * @param value the value to be converted into a string representation.
    * @return an optional string representing the root concatenated with the value.
    */
  def asRoot(value: Value): Option[String] =
    Some(s"$toString${valueToString(value)}")
}

/**
  * This object represents a general inverse power factor.
  */
case class AnyRoot(inversePower: Rational) extends InversePower {
  /**
    * Determines whether the current factor can be multiplied by the given factor.
    * CONSIDER do we need this method now that we have multiply(x, y, this)?
    *
    * @param f the factor to be checked for compatibility with multiplication.
    * @return true if the factors can be multiplied; false otherwise.
    */
  def canMultiply(f: Factor): Boolean =
    false // TODO figure this out

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
  def convert(v: Value, f: Factor): Option[Value] =
    None // TODO figure this out

  /**
    * Raises the value `x` to the power of the value `y`.
    *
    * @param x the base value to be raised
    * @param y the exponent value (from a PureNumber)
    * @return an optional result of type `ProtoNumber`,
    *         representing the computed value if the operation is valid, or `None` otherwise
    */
  override def doRaiseByPureNumber(x: Value, y: Value): Option[ProtoNumber] =
    Value.maybeRational(y) match {
      case Some(`inversePower`) =>
        Some((x, PureNumber, None))
      case Some(r) =>
        val z: Option[Value] = composeDyadic(x, y)(DyadicOperationPower)
        val factor = inversePower * r match {
          case Rational.two => SquareRoot
          case Rational.three => CubeRoot
          case Rational.one => PureNumber
          case _ => AnyRoot(inversePower * r)
        }
        clean(for {v <- z} yield (v, factor, None))
      case _ =>
        None
    }

  /**
    * Converts a given value into a string representation and appends it to the
    * root symbol.
    *
    * @param value the value to be converted into a string representation.
    * @return an optional string representing the root concatenated with the value.
    */
  def asRoot(value: Value): Option[String] =
    Some(s"${valueToString(value)}$toString")
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
  /**
    * Represents the Unicode character ℯ (𝜀), which is commonly used to denote Euler's number in mathematics.
    */
  val sE = "\uD835\uDF00"
  /**
    * Represents the symbol for π (pi) in unicode format.
    * This constant can be used to identify and match the mathematical symbol π
    * within the `Factor` class operations and logic.
    */
  val sPi = "\uD835\uDED1"
  /**
    * Alternate representation of the mathematical constant π as a string.
    * Used as one of the predefined constants within the `Factor` class
    * for identifying and handling π-related symbols.
    */
  val sPiAlt0 = "pi"
  /**
    * Represents an alternative designation for the mathematical constant π (pi) in radians.
    *
    * This string serves as a predefined constant within the `Factor` class,
    * used for matching and identifying inputs associated with the radian unit
    * when applying the `Factor` class's methods.
    */
  val sPiAlt1 = "Radian"
  /**
    * Represents an alternative string representation for the mathematical constant π (Pi).
    * This value is one of several predefined symbols within the `Factor` class
    * that correspond to π-related constants.
    */
  val sPiAlt2 = "PI"

  /**
    * Applies the given string to derive a corresponding `Factor` instance.
    *
    * The method matches the input string against predefined constants representing
    * specific factors.
    * Depending on the match, it returns an instance of either `Radian`, `NatLog`, or `PureNumber`.
    *
    * @param w the input string to match, which can represent a mathematical symbol or name.
    *          Supported values:
    *           - Strings representing π (e.g., `sPi`, `sPiAlt0`, `sPiAlt1`, `sPiAlt2`)
    *           - String representing ℯ (`sE`)
    *           - Any other string is treated as a pure number.
    * @return a `Factor` representing the input:
    *         - `Radian` if the input matches a π-related symbol.
    *         - `NatLog` if the input matches the ℯ symbol.
    *         - `PureNumber` otherwise.
    */
  def apply(w: String): Factor = w match {
    case `sPi` | `sPiAlt0` | `sPiAlt1` | `sPiAlt2` =>
      Radian
    case `sE` =>
      NatLog
    case _ =>
      PureNumber
  }

  /**
    * Composes two `Value` instances using the specified dyadic operation.
    * Evaluate a dyadic operator on this and other, using the various functions passed in.
    * NOTE: this and other must have been aligned by type so that they have the same structure.
    * This method is similar to its namesake in GeneralNumber (which, is soon going to be deprecated).
    *
    * This method internally uses the `Operations.doComposeValueDyadic` function,
    * which evaluates the given dyadic operation on the provided values, based on
    * the associated conversion functions.
    *
    * @param x  the first operand, a `Value`.
    * @param y  the second operand, a `Value`.
    * @param op the dyadic operation to be performed, containing the conversion functions.
    * @return an optional `Value` representing the result of the dyadic operation, or `None` if the operation fails.
    */
  def composeDyadic(x: Value, y: Value)(op: DyadicOperation): Option[Value] =
    Operations.doComposeValueDyadic(x, y)(op.functions)
}

/**
  * The `Render` object provides functionality to render various value types as a formatted string,
  * along with a flag indicating whether the value could be rendered exactly or approximately.
  */
object Render {

  /**
    * Method to render a Value as a tuple of String and Boolean where the latter represents whether we were able
    * to render the value exactly or not.
    *
    * @param v     the Value to be rendered.
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
          case Some(n) =>
            Some(renderDouble(n))
          case None =>
            None
        })
    ).getOrElse(("<undefined>", true))

  /**
    * Renders an integer as a tuple containing its string representation and a boolean flag indicating exact rendering.
    *
    * @param x the integer value to be rendered.
    * @return a tuple where the first element is the string representation of the integer and the second element is `true`,
    *         indicating it is rendered exactly.
    */
  private def renderInt(x: Int): (String, Boolean) =
    (x.toString, true)

  /**
    * Add exact parameter.
    *
    * @param x the Rational value to be rendered.
    * @return a String.
    */
  private def renderRational(x: Rational): (String, Boolean) =
    x.renderConditional(true)

  /**
    * Renders a `Double` value as a tuple containing its string representation
    * and a boolean flag indicating that it is not rendered exactly.
    *
    * @param x the `Double` value to be rendered.
    * @return a tuple where the first element is the string representation of the `Double`
    *         and the second element is always `false`, indicating it is not rendered exactly.
    */
  private def renderDouble(x: Double): (String, Boolean) =
    (x.toString, false)
}

