package com.phasmidsoftware.number.top

import com.phasmidsoftware.number.algebra.core.{Lazy, Renderable, Valuable}
import com.phasmidsoftware.number.algebra.eager
import com.phasmidsoftware.number.algebra.eager.*
import com.phasmidsoftware.number.algebra.util.LatexRenderer
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.dimensions.core.*
import com.phasmidsoftware.number.expression.expr.Expression
import com.phasmidsoftware.number.parse.{ParseError, UnitsParser}
import com.phasmidsoftware.number.top.Quantity.lazify

type PhysicalUnit[D <: Dimension] = com.phasmidsoftware.number.dimensions.core.Unit[D]

/**
  * Represents a measurable quantity defined by a numerical value and a physical unit.
  * The type parameter `D` specifies the dimension of the quantity.
  *
  * This case class is useful for performing unit-aware numerical operations and rendering
  * quantities in a human-readable format.
  *
  * @param value the numerical value of the quantity, which is represented as a `Valuable`
  * @param unit  the physical unit associated with the quantity, specified as a `PhysicalUnit[D]`
  */
case class Quantity[D <: Dimension](value: Valuable, unit: PhysicalUnit[D]) extends Renderable {
  /**
    * Renders the quantity as a string representation by combining the rendered value
    * and the unit's symbol.
    *
    * @return a string consisting of the value's rendered representation followed
    *         by the unit's symbol, separated by a space
    */
  def render: String = s"${value.render} ${unit.symbol}"

  /**
    * Convenience method for LaTeX rendering.
    * Delegates to toLatex function in companion object.
    */
  def renderLaTeX: String = Quantity.toLatex(this)

  /**
    * Converts this `Quantity` to an equivalent `Quantity` that has a different unit and returns the result as an `Option` of the converted quantity.
    * The conversion is based on the scale factors of the source and target units relative to the Standard International (SI) base units.
    * If the conversion is successful, a new `Quantity` instance is created with the converted value and target unit.
    *
    * @param targetUnit the physical unit to which this quantity should be converted.
    *                   The dimension of the target unit must correspond to the dimension of this quantity.
    *
    * @return an `Option` containing the converted `Quantity` if the operation is valid,
    *         or `None` if the conversion cannot be performed due to the absence of a valid numerical value.
    */
  def in(targetUnit: PhysicalUnit[D]): Option[Quantity[D]] =
    val scaleFactor = unit.toSI / targetUnit.toSI
    value.asNumber.map(n => Quantity((n * scaleFactor).normalize, targetUnit))

  /**
    * Multiplies this quantity with another quantity, resulting in a new quantity
    * whose dimension is the product of the dimensions of the two quantities.
    *
    * @param other the quantity to multiply with this quantity. The dimension of the
    *              other quantity must be of type `D2`, a subtype of `Dimension`.
    *
    * @return a new quantity representing the product of this quantity and the other quantity.
    *         The resulting quantity has a dimension defined as `MulDim[D, D2]`.
    */
  def *[D2 <: Dimension](other: Quantity[D2]): Quantity[MulDim[D, D2]] =
    Quantity(value.product(other.value), unit * other.unit)

  /**
    * Divides this quantity by another quantity, resulting in a new quantity whose dimension
    * is the quotient of the dimensions of the two quantities.
    *
    * @param other the quantity to divide by. The dimension of the other quantity must be
    *              of type `D2`, a subtype of `Dimension`.
    *
    * @return a new quantity representing the quotient of this quantity and the other quantity.
    *         The resulting quantity has a dimension defined as `DivDim[D, D2]`.
    */
  def /[D2 <: Dimension](other: Quantity[D2]): Quantity[DivDim[D, D2]] =
    Quantity(lazify(value) / lazify(other.value), unit / other.unit)

  /**
    * Computes the squared value of the quantity by multiplying it with itself.
    *
    * @return an `Option` containing the squared quantity if the operation is valid, or `None` if the operation cannot be performed.
    */
  def squared: Quantity[MulDim[D, D]] = this * this

  /**
    * Computes the multiplicative inverse of this quantity, resulting in a quantity
    * that represents one divided by the value of this quantity.
    *
    * @return an `Option` containing the inverted quantity if the operation is valid,
    *         or `None` if the operation cannot be performed.
    */
  def inverted: Quantity[DivDim[Dimensionless, D]] = Quantity.unity / this

  /**
    * Adds this quantity to another quantity of the same dimension.
    * If the units of both quantities are compatible, the resulting quantity is computed
    * by converting the `other` quantity into the same unit as `this` quantity (if necessary)
    * and summing their values.
    *
    * @param other the quantity to be added, which must have the same dimension as this quantity.
    * @return an `Option` containing the resulting quantity if the units are compatible,
    *         or `None` if the units are incompatible.
    */
  def +(other: Quantity[D]): Option[Quantity[D]] =
    if (this.unit == other.unit)
      (this.value.materialize, other.value.materialize) match {
        case (e: Number, f: Number) =>
          Some(Quantity(e + f, unit))
        case _ =>
          None
      }
    else
      other.in(this.unit) match {
        case Some(q) =>
          this + q
        case None =>
          None
      }
}

/**
  * Provides factory methods for creating instances of the `Quantity` class.
  *
  * Quantities represent measurable values associated with physical units
  * and are parameterized by their dimension type `D`, which defines the
  * specific physical property of the quantity (e.G., length, time, etc.).
  *
  * The companion object offers multiple overloaded `apply` methods for
  * constructing quantities using different types of numerical input.
  */
object Quantity {
  /**
    * Constructs a new `Quantity` instance with the specified value and unit.
    *
    * @param value the numerical value of the quantity
    * @param unit  the physical unit associated with the quantity
    * @return a `Quantity` instance representing the specified value and unit
    */
  def apply[D <: Dimension](value: Number, unit: PhysicalUnit[D]): Quantity[D] =
    new Quantity(value.normalize, unit)

  /**
    * Creates a quantity with a specified value and physical unit, where the value is provided as a rational number.
    *
    * @param value The rational value representing the magnitude of the quantity.
    * @param unit  The physical unit associated with the quantity.
    * @return A quantity with the specified value and unit.
    */
  def apply[D <: Dimension](value: Rational, unit: PhysicalUnit[D]): Quantity[D] =
    apply(RationalNumber(value), unit)

  /**
    * Constructs a `Quantity` instance with an integer value and a corresponding physical unit.
    *
    * @param value the integer value to be associated with the quantity
    * @param unit  the physical unit of the specified dimension associated with the quantity
    * @return a `Quantity` instance with the specified value and unit
    */
  def apply[D <: Dimension](value: Int, unit: PhysicalUnit[D]): Quantity[D] =
    apply(Rational(value), unit)

  /**
    * Constructs a `Quantity` instance from a `Double` value and a physical unit.
    *
    * The method internally converts the given `Double` value to a `Real`
    * representation and then utilizes the alternate `apply` method for `Real` values.
    *
    * @param value the numerical value of the quantity as a `Double`
    * @param unit  the physical unit associated with the quantity, of type `PhysicalUnit[D]`
    * @return a `Quantity[D]` instance representing the specified value and unit
    */
  def apply[D <: Dimension](value: Double, unit: PhysicalUnit[D]): Quantity[D] =
    apply(Real(value), unit)

  /**
    * Constructs a `Quantity` instance with the specified value and physical unit.
    *
    * This method interprets the provided value as a string and associates it
    * with the given physical unit to create a measurable quantity.
    *
    * @param value the string representation of the quantity's numerical value
    * @param unit  the physical unit associated with the quantity
    * @return a `Quantity` instance representing the specified value and unit
    */
  def apply[D <: Dimension](value: String, unit: PhysicalUnit[D]): Quantity[D] =
    apply(Eager(value), unit)

  /**
    * Constructs a `Quantity[Dimensionless]` instance using the provided `Valuable` value.
    *
    * This method associates the given `Valuable` numerical value with the `Dimensionless` unit,
    * creating a quantity that represents a dimensionless measurement.
    *
    * @param value the numerical value of type `Valuable` to be associated with the `Dimensionless` unit
    * @return a `Quantity[Dimensionless]` instance representing the specified value
    */
  def apply(value: Valuable): Quantity[Dimensionless] = Quantity(value, Dimensionless)

  /**
    * Constructs a `Quantity[Dimensionless]` instance using the provided `Rational` value.
    *
    * This method associates the given `Rational` numerical value with the `Dimensionless` unit,
    * creating a quantity that represents a dimensionless measurement.
    *
    * @param value the numerical value of type `Rational` to be associated with the `Dimensionless` unit
    * @return a `Quantity[Dimensionless]` instance representing the specified value
    */
  def apply(value: Rational): Quantity[Dimensionless] = Quantity(value, Dimensionless)

  /**
    * Creates a `Quantity` instance with the specified numerical value and physical unit.
    *
    * This method takes a lazy numerical value, which can be simplified, and a unit
    * specifying the dimension of the quantity, and constructs a measurable quantity
    * object of the given dimension.
    *
    * @param value the lazy numerical value representing the magnitude of the quantity
    * @param unit  the physical unit specifying the dimension of the quantity
    * @return a `Quantity` instance combining the specified value and unit
    */
  def apply[D <: Dimension](value: Lazy, unit: PhysicalUnit[D]): Quantity[D] =
    new Quantity[D](value, unit)

  /**
    * Constructs a `Quantity[Dimensionless]` instance with the given integer value.
    *
    * This method takes an integer value and associates it with the `Dimensionless` unit,
    * creating a quantity that represents a dimensionless measurement.
    *
    * @param value the integer value to be associated with the `Dimensionless` unit
    * @return a `Quantity[Dimensionless]` instance representing the specified value
    */
  def apply(value: Int): Quantity[Dimensionless] = Quantity(value, Dimensionless)

  /**
    * Constructs a `Quantity[Dimensionless]` instance from a string representation of a value.
    *
    * This method parses the input string to extract its numerical value and
    * associates it with the `Dimensionless` unit to create a measurable quantity.
    *
    * @param value the string representation of the numerical value to be parsed and associated
    *              with the `Dimensionless` unit
    *
    * @return a `Quantity[Dimensionless]` instance representing the numerical value as a
    *         dimensionless quantity
    */
  def apply(value: String): Quantity[Dimensionless] =
    Quantity(QuantityParser.doParseNumber(value), Dimensionless)
  /**
    * Represents the dimensionless unit quantity with a value of 1.
    *
    * This predefined quantity is associated with the `Dimensionless` unit
    * and is constructed with an integer value of 1. It can be used as a
    * constant for operations requiring a base unit in dimensional analysis.
    */
  val unity: Quantity[Dimensionless] = apply(1)

  /**
    * Converts a `Valuable` into an `Expression` while preserving lazy evaluation where applicable.
    *
    * If the input is of type `Eager`, it is wrapped into an `Expression`.
    * If the input is already an `Expression`, it is returned as is.
    *
    * @param valuable the input value of type `Valuable` to be transformed into an `Expression`
    * @return an `Expression` representing the given `Valuable`
    */
  def lazify(valuable: Valuable): Expression = valuable match {
    case e: Eager => Expression(e)
    case e: Expression => e
  }

  /**
    * Parses a numerical value and a unit into a `Quantity` instance.
    *
    * This method takes a `Valuable` numerical value and a `String` representing a unit.
    * It first attempts to parse the unit string and, if successful, constructs a `Quantity`
    * using the provided value and the parsed unit. If parsing the unit fails,
    * it returns a `Left` containing the `ParseError`.
    * NOTE this method is a flexible means of constructing a `Quantity`, 
    * but it suffers from the lack of type safety.
    *
    * @param value the numerical value of type `Valuable` to be combined with the unit.
    * @param unit  a `String` representing the physical unit to be parsed and used with the value.
    * @return an `Either` containing a `ParseError` if the unit string failed parsing,
    *         or a `Quantity[?]` instance representing the parsed numerical value and unit.
    */
  def parse(value: Valuable, unit: String): Either[ParseError, Quantity[?]] =
    UnitsParser.parse(unit) match {
      case Left(s) => Left(s)
      case Right(u) => Right(Quantity(value, u))
    }

  /**
    * Parses a string representation of a quantity into a `Quantity` instance.
    *
    * This method interprets the input string, which should include a numerical
    * value and an optional physical unit, and attempts to construct a measurable
    * quantity. If parsing fails, it returns a `Failure` containing a `UnitsParserException`.
    *
    * NOTE this method is the most flexible means of constructing a `Quantity`, 
    * but it suffers from the lack of type safety.
    *
    * @param quantity the string representation of the quantity to be parsed. It
    *                 is expected to include a numerical value followed optionally
    *                 by a unit.
    *
    * @return a `Try` containing the parsed `Quantity[?]` if the input is successfully
    *         parsed, or a `Failure` with a `UnitsParserException` if an error occurs.
    */
  def parse(quantity: String): Either[ParseError, Quantity[?]] =
    QuantityParser.parse(quantity)

  /**
    * Render a quantity in LaTeX format.
    * This is the definitive LaTeX rendering function.
    *
    * @param quantity the quantity to render
    * @return LaTeX string representation
    */
  def toLatex[D <: Dimension](quantity: Quantity[D]): String = {
    val valueLatex = quantity.value match {
      case e: Eager =>
        summon[LatexRenderer[Eager]].toLatex(e)
      case l: Lazy =>
        l.materialize match {
          case e: Eager => summon[LatexRenderer[Eager]].toLatex(e)
        }
    }

    s"$valueLatex\\,${com.phasmidsoftware.number.dimensions.core.toLatex(quantity.unit)}"
  }
}

/**
  * Represents an exception specific to quantity-related errors.
  *
  * The `QuantityException` is used to signal issues or invalid operations
  * related to quantities within the application. It extends the base `Exception`
  * class and includes a customizable error message to provide additional context
  * about the exception's cause.
  *
  * @param message Detailed message describing the nature of the quantity-related error.
  */
case class QuantityException(message: String) extends Exception(message)