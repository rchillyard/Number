/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.FP._
import com.phasmidsoftware.number.core.Operations.doComposeValueDyadic
import com.phasmidsoftware.number.core.Rational.toIntOption
import com.phasmidsoftware.number.core.Value.{fromDouble, scaleDouble, valueToString}
import scala.util._

/**
  * Represents a factor type used for evaluations, conversions, and rendering in a specific context.
  *
  * CONSIDER defining an imaginary (or Complex) subclass of Factor.
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
    */
  val value: Double

  /**
    * Determines whether the current factor can be augmented by the given factor.
    *
    * @param f the factor to be checked for compatibility with addition.
    * @return true if the factors can be added; false otherwise.
    */
  def canAdd(f: Factor): Boolean =
    f == this

  /**
    * Determines whether the current factor can be multiplied by the given factor.
    *
    * @param f the factor to be checked for compatibility with multiplication.
    * @return true if the factors can be multiplied; false otherwise.
    */
  def canMultiply(f: Factor): Boolean

  /**
    * Determines whether `this` factor can be raised by the given factor `f`.
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
    case Logarithmic(z) =>
      Some(fromDouble(Value.maybeDouble(v) map (x => scaleDouble(x, this.value, z))))
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
  def unapply(arg: Logarithmic): Option[Double] = Some(arg.value)

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
  * A number with value x and factor Root(n) represents the nth root of x.
  * For example, ExactNumber(5, SquareRoot) is the square root of 5, where SquareRoot is an alias for Root(2).
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
    * Method to render a Value in the context of this Factor.
    *
    * @param x the Value.
    * @return a String.
    */
  override def render(x: Value): String =
    asImaginary(x) orElse asRoot(x) getOrElse convert(x, PureNumber).toString

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
      val vo = doComposeValueDyadic(v, Number(z).specialize.nominalValue)(DyadicOperationPower.functions)
      vo flatMap (doComposeValueDyadic(_, Number(value).specialize.getInverse.nominalValue)(DyadicOperationPower.functions))
    case _ =>
      None
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
  override def isA(context: Context): Boolean =
    context.factorQualifies(this)

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
case object SquareRoot extends Root {

  override def toString: String = "√"

  /**
    * Renders the input string with a square root symbol.
    *
    * @param x the input string to be rendered
    * @return a string prefixed with the square root symbol
    */
  def render(x: String): String = s"√$x"

  /**
    * Indicates that this Factor is the Square root.
    *
    * @return the integer value representing the root factor, which is 2
    */
  def root: Int = 2

  /**
    * Constructs an optional string representation of the current root object concatenated
    * with the string representation of the specified value.
    *
    * @param value the value to be represented as part of the root string.
    * @return an Option containing the concatenated string representation of the root
    *         and the value, or None if the string construction is unsuccessful.
    */
  def asRoot(value: Value): Option[String] = Some(s"$toString${valueToString(value)}")
}

/**
  * This object represents the cube root factor.
  */
case object CubeRoot extends Root {

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
  def render(x: String): String = s"³√$x"

  /**
    * Indicates that this Factor is the Cube root.
    *
    * @return the root value as an integer (3).
    */
  def root: Int = 3

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

