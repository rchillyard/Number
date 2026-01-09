package com.phasmidsoftware.number.top

import com.phasmidsoftware.number.algebra.core.{Lazy, Renderable, Valuable}
import com.phasmidsoftware.number.algebra.eager.{ExactNumber, Number, RationalNumber, Real}
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.dimensions.core.*

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

  // Convert to a different unit of the same dimension
  def in(targetUnit: PhysicalUnit[D]): Option[Quantity[D]] =
    val scaleFactor = unit.toSI / targetUnit.toSI
    value.asNumber.map(n => Quantity((n * scaleFactor).normalize, targetUnit))

  // Arithmetic operations
  def *[D2 <: Dimension](other: Quantity[D2]): Option[Quantity[MulDim[D, D2]]] =
    for {
      v1 <- value.asNumber
      v2 <- other.value.asNumber
    } yield Quantity(v1 * v2, unit * other.unit)

  def /[D2 <: Dimension](other: Quantity[D2]): Option[Quantity[DivDim[D, D2]]] =
    for {
      v1 <- value.asNumber
      case (v2: ExactNumber) <- other.value.asNumber
    } yield Quantity(v1 / v2, unit / other.unit)
}

object Quantity {
  /**
    * Constructs a new `Quantity` instance with the specified value and unit.
    *
    * @param value the numerical value of the quantity
    * @param unit  the physical unit associated with the quantity
    * @return a `Quantity` instance representing the specified value and unit
    */
  def apply[D <: Dimension](value: Number, unit: PhysicalUnit[D]): Quantity[D] = new Quantity(value.normalize, unit)

  /**
    * Creates a quantity with a specified value and physical unit, where the value is provided as a rational number.
    *
    * @param value The rational value representing the magnitude of the quantity.
    * @param unit  The physical unit associated with the quantity.
    * @return A quantity with the specified value and unit.
    */
  def apply[D <: Dimension](value: Rational, unit: PhysicalUnit[D]): Quantity[D] = apply(RationalNumber(value), unit)

  /**
    * Constructs a `Quantity` instance with an integer value and a corresponding physical unit.
    *
    * @param value the integer value to be associated with the quantity
    * @param unit  the physical unit of the specified dimension associated with the quantity
    * @return a `Quantity` instance with the specified value and unit
    */
  def apply[D <: Dimension](value: Int, unit: PhysicalUnit[D]): Quantity[D] = apply(Rational(value), unit)

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
  def apply[D <: Dimension](value: Double, unit: PhysicalUnit[D]): Quantity[D] = apply(Real(value), unit)

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
  def apply[D <: Dimension](value: Lazy, unit: PhysicalUnit[D]): Quantity[D] = new Quantity[D](value.simplify, unit)
}