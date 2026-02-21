package com.phasmidsoftware.number.dimensions.core

import com.phasmidsoftware.number.algebra.eager.*
import com.phasmidsoftware.number.core.inner.Rational

/**
  * Trait representing a unit of measurement.
  * Units combine a dimension with a scale factor relative to SI base units.
  */
trait Unit[D <: Dimension] {
  /**
    * Converts the current unit of measurement to its equivalent in Standard International (SI) base units.
    *
    * The conversion takes into account the unit's scale factor, aligning its value with the
    * corresponding SI base unit representation.
    *
    * @return an `ExactNumber` representing the converted value of the unit in SI base units.
    */
  def toSI: ExactNumber

  /**
    * Retrieves the symbol representing this unit of measurement.
    *
    * The symbol is typically a short string (e.g., "kg" for kilograms, "m" for meters)
    * that uniquely identifies the unit in accordance with standard conventions or custom definitions.
    *
    * @return a string containing the unit's symbol.
    */
  def symbol: String

  /**
    * Computes the composite symbol for this unit of measurement based on its dimensional structure.
    *
    * The composite symbol is a representation derived from the dimensional exponents
    * encapsulated by the associated dimension witness. It combines the base dimensions
    * into a simplified or standardized form, reflecting the unit's fundamental structure.
    *
    * @return a string representing the composite symbol of the unit.
    */
  def compositeSymbol: String = dimensionWitness.toCompositeSymbol

  /**
    * Renames the symbol representing this unit of measurement.
    *
    * This method updates the current symbol of the unit to the provided value,
    * allowing for a custom or alternative representation of the unit.
    *
    * @param s a string representing the new symbol for the unit
    * @return the updated unit object with the new symbol applied
    */
  def rename(s: String): Unit[D]

  /**
    * Retrieves the (run-time) dimension witness associated with this unit of measurement.
    *
    * The dimension witness encapsulates the dimensional exponents in the seven
    * SI base dimensions (mass, length, time, electric current, thermodynamic temperature,
    * amount of substance, and luminous intensity) and allows for deriving composite symbols
    * that reflect the structure of the unit.
    *
    * @return the dimension witness of this unit, representing its fundamental dimensional structure.
    */
  def dimensionWitness: DimensionWitness

  /**
    * Compares this unit of measurement to another object for equality.
    *
    * Equality is determined based on the dimension witness and the conversion factor to
    * Standard International (SI) base units. The unit's symbolic representation is not
    * considered during comparison.
    *
    * @param obj the object to compare with this unit
    * @return true if the specified object is a unit and has the same dimension witness
    *         and SI conversion factor as this unit; false otherwise
    */
  override def equals(obj: Any): Boolean = obj match {
    case that: Unit[?] =>
      this.dimensionWitness == that.dimensionWitness &&
        this.toSI == that.toSI
    case _ => false
  }

  /**
    * Computes the hash code for this unit of measurement.
    *
    * The hash code is derived from the dimension witness and the conversion factor to
    * Standard International (SI) base units, ensuring consistency with the `equals` method.
    *
    * @return an integer representing the hash code of this unit, based on its dimension witness and SI conversion factor.
    */
  override def hashCode(): Int =
    (dimensionWitness, toSI).hashCode()
}

/**
  * Represents a unit of measurement that is part of the International System of Units (SI).
  *
  * @tparam D the dimension type associated with this unit, which must extend `Dimension`
  */
trait SIUnit[D <: Dimension] extends Unit[D] {
  lazy val toSI: ExactNumber = WholeNumber.one

  /**
    * Renames the symbol representing this SI unit of measurement.
    *
    * This method is not supported for `SIUnit` and will throw a `DimensionsException`
    * if invoked.
    *
    * @param s a string representing the new symbol for the unit
    * @return this method does not return normally; it always throws a `DimensionsException`
    *         indicating that renaming is not allowed for `SIUnit`
    */
  def rename(s: String): Unit[D] =
    throw DimensionsException("Cannot rename SIUnit")
}

/**
  * A composite unit formed by multiplying two units.
  * The resulting dimension is the product of the component dimensions.
  */
case class ProductUnit[D <: Dimension](
                                        left: Unit[?],
                                        right: Unit[?],
                                        symbol: String
                                      ) extends Unit[D]:

  /**
    * Converts the product of two units to its equivalent in Standard International (SI) base units.
    *
    * The conversion is achieved by multiplying the SI representations of the left and right component units.
    * If the resulting product cannot be expressed as an `ExactNumber`, a `DimensionsException` is thrown.
    *
    * @return An `ExactNumber` representing the product of the two units in SI base units.
    * @note Throws DimensionsException if the product of the converted components cannot be represented as an `ExactNumber`.
    */
  lazy val toSI: ExactNumber =
    left.toSI * right.toSI match {
      case r: ExactNumber =>
        r
      case x =>
        throw DimensionsException(s"cannot determine a conversion factor for $left * $right that is an ExactNumber: got $x")
    }

  /**
    * Computes the composite symbol for the product of two units.
    *
    * The composite symbol is derived by concatenating the composite symbols
    * of the left and right component units, separated by a centered dot (`·`).
    *
    * @return a string representing the composite symbol of the product unit.
    */
  override def compositeSymbol: String =
    s"${left.compositeSymbol}·${right.compositeSymbol}"

  /**
    * Renames the symbol representing this unit of measurement.
    *
    * This method updates the current symbol of the unit to the provided value,
    * allowing for a custom or alternative representation of the unit.
    *
    * @param name the new symbol to replace the current symbol of the unit
    * @return a new unit instance with the updated symbol
    */
  def rename(name: String): Unit[D] = copy(symbol = name)

  /**
    * Computes the composite dimension witness for the product of two units.
    *
    * This method derives the dimension witness by summing the dimensional exponents
    * of the left and right component units in their respective seven SI base dimensions
    * (mass, length, time, electric current, thermodynamic temperature, amount of substance,
    * and luminous intensity).
    *
    * @return A `DimensionWitness` representing the combined dimensional structure of the
    *         product unit.
    *         Throws `DimensionsException` if the dimension witnesses of the components
    *         cannot be combined.
    */
  lazy val dimensionWitness: DimensionWitness =
    (left.dimensionWitness, right.dimensionWitness) match {
      case (BaseDimWitness(m1, l1, t1, i1, θ1, n1, j1), BaseDimWitness(m2, l2, t2, i2, θ2, n2, j2)) =>
        BaseDimWitness(m1 + m2, l1 + l2, t1 + t2, i1 + i2, θ1 + θ2, n1 + n2, j1 + j2)
      case _ =>
        throw DimensionsException("Invalid dimension witness for ProductUnit")
    }

/**
  * A composite unit formed by dividing two units.
  * The resulting dimension is the quotient of the component dimensions.
  */
case class QuotientUnit[D <: Dimension](
                                         numerator: Unit[?],
                                         denominator: Unit[?],
                                         symbol: String
                                       ) extends Unit[D]:

  /**
    * Converts the quotient of the numerator and denominator units into their equivalent value
    * in Standard International (SI) base units.
    *
    * The method performs the following steps:
    * - Converts both the numerator and denominator units to their SI equivalents using their `toSI` methods.
    * - Divides the SI representation of the numerator by the SI representation of the denominator.
    * - Ensures the result is an `ExactNumber`. If not, it throws a `DimensionsException`.
    *
    * @return an `ExactNumber` representing the quotient of the numerator and denominator in SI base units.
    * @note Throws DimensionsException if the conversion to a rational number fails.
    */
  lazy val toSI: ExactNumber =
    numerator.toSI / denominator.toSI match {
      case r: ExactNumber =>
        r
      case x =>
        throw DimensionsException(s"cannot determine a conversion factor for $numerator / $denominator that is an ExactNumber: got $x")
  }

  /**
    * Computes the composite symbol of the quotient unit by combining the composite symbols
    * of its numerator and denominator.
    *
    * The composite symbol is represented as a fraction, where the numerator's composite symbol
    * appears above the denominator's composite symbol. This provides a readable and standardized
    * form for representing the quotient unit.
    *
    * @return a string representation of the composite symbol in the format "numerator/denominator".
    */
  override def compositeSymbol: String =
    s"${numerator.compositeSymbol}/${denominator.compositeSymbol}"

  /**
    * Renames the symbol representing this unit of measurement.
    *
    * This method updates the symbol of the unit to the provided value, allowing for a custom or alternative representation.
    *
    * @param name the new symbol to be assigned to the unit
    * @return a new instance of the unit with the updated symbol
    */
  def rename(name: String): Unit[D] = copy(symbol = name)

  /**
    * Computes the dimension witness for the current `QuotientUnit`.
    *
    * This method derives the dimensional structure of the quotient by subtracting
    * the dimensions of the denominator from those of the numerator.
    * If the dimension witnesses for the numerator or denominator are invalid,
    * a `DimensionsException` is thrown.
    *
    * @return a `DimensionWitness` representing the resultant dimensional structure of the quotient unit.
    * @throws DimensionsException if the numerator or denominator do not have valid dimension witnesses.
    */
  lazy val dimensionWitness: DimensionWitness =
    (numerator.dimensionWitness, denominator.dimensionWitness) match {
      case (BaseDimWitness(m1, l1, t1, i1, θ1, n1, j1), BaseDimWitness(m2, l2, t2, i2, θ2, n2, j2)) =>
        BaseDimWitness(m1 - m2, l1 - l2, t1 - t2, i1 - i2, θ1 - θ2, n1 - n2, j1 - j2)
      case _ =>
        throw DimensionsException("Invalid dimension witness for QuotientUnit")
    }

/**
  * A unit raised to an integer power.
  * The resulting dimension is the base dimension raised to that power.
  */
case class PowerUnit[D <: Dimension](
                                      base: Unit[?],
                                      power: Rational,
                                      symbol: String
                                    ) extends Unit[D]:

  /**
    * Converts the power unit into its equivalent value in the SI system of units.
    * The calculation is performed by raising the SI representation of the base unit
    * to the power specified by this `PowerUnit`.
    *
    * @return The exact numerical representation of the SI value of this unit as an `ExactNumber`.
    *         If the conversion cannot be performed, an exception is thrown.
    *
    * @note Throws DimensionsException if there is an error during conversion to the
    *       exact rational number representation.
    */
  lazy val toSI: ExactNumber =
    base.toSI.pow(RationalNumber(power)) match {
      case Some(e: ExactNumber) =>
        e
      case None =>
        throw DimensionsException("PowerUnit.toSI: cannot evaluate toSI")
    }

  /**
    * Computes the composite symbol for this `PowerUnit` based on its base unit and power.
    *
    * The composite symbol is derived by taking the composite symbol of the base unit
    * and appending the power as an exponent. This provides a human-readable representation
    * that reflects the dimensional structure of the `PowerUnit`.
    *
    * @return a string representing the composite symbol of the `PowerUnit`,
    *         formatted as "(baseCompositeSymbol)^power".
    */
  override def compositeSymbol: String =
    s"(${base.compositeSymbol})^$power"

  def rename(name: String): Unit[D] = copy(symbol = name)

  lazy val dimensionWitness: DimensionWitness =
    base.dimensionWitness match {
      case d@BaseDimWitness(_, _, _, _, _, _, _) =>
        d pow power
      case _ =>
        throw DimensionsException("Invalid dimension witness for PowerUnit")
    }

/**
  * Represents a unit of measurement that is scaled relative to some base unit.
  *
  * This class defines a scaled version of a base unit, using a specified scale factor
  * and name for the scaled unit. It extends the `Unit` trait, tying the scaled unit
  * to a specific physical dimension and providing the scale factor relative to the
  * SI base unit.
  *
  * @tparam D The physical dimension associated with this unit.
  * @param base  The base unit this scaled unit derives from.
  * @param scale The scale factor relative to the base unit.
  * @param symbol The name or symbol of the scaled unit.
  */
case class ScaledUnit[D <: Dimension](
                                       base: Unit[?],
                                       scale: Rational,
                                       symbol: String
                                     ) extends Unit[D]:
  /**
    * Converts the scaled unit to its equivalent value in Standard International (SI) base units.
    *
    * This method multiplies the base unit's equivalent SI value by the scale factor
    * of the current scaled unit, obtaining the exact representation in SI base units.
    *
    * @return an `ExactNumber` representing the value of the scaled unit in SI base units.
    */
  lazy val toSI: ExactNumber = base.toSI * scale

  /**
    * Constructs the composite symbol for the unit, incorporating its scale factor and
    * the composite symbol of its base unit.
    *
    * If the scale factor is different from one, it is included as a prefix to the composite
    * symbol of the base unit, formatted as a fraction if necessary.
    *
    * @return The composite symbol for the unit, combining the scale factor and the base unit's composite symbol.
    */
  override lazy val compositeSymbol: String =
    if (scale != Rational.one) {
      val scaleStr = if (scale.d == 1) scale.n.toString else s"(${scale.n}/${scale.d})"
      s"${scaleStr}·${base.compositeSymbol}"
    }
    else
      base.compositeSymbol

  def rename(name: String): Unit[D] = copy(symbol = name)

  /** Inherit dimension from base unit */
  lazy val dimensionWitness: DimensionWitness = base.dimensionWitness

private def powerSymbol[D <: Dimension](base: Unit[D], power: Rational) = {
  val baseSymbol = base match {
    case _: ProductUnit[?] | _: QuotientUnit[?] =>
      s"(${base.symbol})"
    case _ =>
      base.symbol
  }
  if power == Rational(2) then s"$baseSymbol²"
  else if power == Rational(3) then s"$baseSymbol³"
  else if power == Rational(-1) then s"$baseSymbol⁻¹"
  else if power == Rational(-2) then s"$baseSymbol⁻²"
  else if power == Rational(-3) then s"$baseSymbol⁻³"
  else s"$baseSymbol^$power"
}

/**
  * Extension methods to enable algebraic composition of units.
  */
extension [D1 <: Dimension](u1: Unit[D1])
  /**
    * Multiply two units to create a composite unit.
    * The dimension of the result is the product of the input dimensions.
    *
    * Example: Kilogram * Meter gives a unit with dimension [M¹L¹]
    */
  def *[D2 <: Dimension](u2: Unit[D2]): Unit[MulDim[D1, D2]] =
    ProductUnit[MulDim[D1, D2]](u1, u2, s"${u1.symbol}·${u2.symbol}")

  /**
    * Divide two units to create a composite unit.
    * The dimension of the result is the quotient of the input dimensions.
    *
    * Example: Meter / Second gives a unit with dimension [L¹T⁻¹] (velocity)
    */
  def /[D2 <: Dimension](u2: Unit[D2]): Unit[DivDim[D1, D2]] =
    QuotientUnit[DivDim[D1, D2]](u1, u2, s"${u1.symbol}/${u2.symbol}")

  /**
    * Squares the current unit.
    * Example: Meter.squared gives a unit with dimension [L²] (area)
    */
  def squared: Unit[PowDim[D1, Two]] =
    PowerUnit[PowDim[D1, Two]](u1, Rational(2), powerSymbol(u1, 2))

  /**
    * Cubes the current unit.
    * Example: Meter.cubed gives a unit with dimension [L³] (volume)
    */
  def cubed: Unit[PowDim[D1, TRat[3, 1]]] =
    PowerUnit[PowDim[D1, TRat[3, 1]]](u1, Rational(3), powerSymbol(u1, 3))

  /**
    * Takes the square root of the current unit.
    * Example: SquareMeter.sqrt gives a unit with dimension [L] (length)
    */
  def sqrt: Unit[PowDim[D1, Half]] =
    PowerUnit[PowDim[D1, Half]](u1, Rational.half, powerSymbol(u1, Rational.half))

  /**
    * Inverts the current unit (raises to power -1).
    * Example: Second.invert gives a unit with dimension [T⁻¹] (frequency)
    */
  def invert: Unit[PowDim[D1, MinusOne]] =
    PowerUnit[PowDim[D1, MinusOne]](u1, Rational(-1), powerSymbol(u1, -1))

  /**
    * Scales the unit by the given rational factor and assigns an optional name to the resulting unit.
    *
    * @param scale The rational factor by which the unit is scaled.
    * @param name  An optional name for the scaled unit, default is an empty string.
    * @return A scaled unit of type `Unit[D1]`.
    */
  def scaled(scale: Rational, name: String): Unit[D1] =
    ScaledUnit[D1](u1, scale, name)

  /**
    * Scales a unit by a given integer factor and assigns a name to the resulting unit.
    *
    * @param scale The Int factor by which the unit is scaled.
    * @param name  The name assigned to the scaled unit.
    * @return A scaled unit of type `Unit[D1]`.
    */
  def scaled(scale: Int, name: String): Unit[D1] =
    scaled(Rational(scale), name)

/**
  * Extension method to render units in LaTeX format
  */
extension [D <: Dimension](unit: Unit[D])
  def renderLaTeX: String = toLatex(unit)

/**
  * Represents the dimensionless unit in the International System of Units (SI).
  *
  * This is a special unit that denotes quantities without any associated physical dimension.
  * Examples of dimensionless quantities include pure numbers, ratios, angles measured in radians, etc.
  *
  * Extends `SIUnit` and provides a specific implementation for dimensionless quantities.
  */
case object Dimensionless extends SIUnit[Dimensionless] {
  lazy val symbol: String = ""

  lazy val dimensionWitness: DimensionWitness = DimensionWitness.dimensionless

}

// ============================================================================
// SI Base Units
// ============================================================================

/**
  * Represents the meter, the base SI unit of length.
  *
  * The meter is a fundamental unit used in the International System of Units (SI) for measuring
  * length or distance. It serves as the standard unit for evaluating a wide variety of physical
  * quantities related to length, such as dimensions, distances, and spatial measurements.
  *
  * This object extends the `SIUnit` trait with the `Length` dimension, ensuring compatibility
  * with SI-based unit systems. The symbol for the meter is "m", and the corresponding dimension
  * witness is provided as part of this object's definition.
  *
  * @see DimensionWitness.length for the length dimension's representation in the dimension-witness system
  */
case object Meter extends SIUnit[Length] {
  lazy val symbol: String = "m"

  lazy val dimensionWitness: DimensionWitness = DimensionWitness.length
}

/**
  * Represents the SI base unit for mass.
  *
  * This object corresponds to the kilogram, the fundamental unit of mass
  * in the International System of Units (SI). It extends the `SIUnit`
  * trait specific to the `Mass` dimension, providing the symbol and
  * dimension witness for this unit.
  *
  * @see SIUnit
  * @see DimensionWitness
  */
case object Kilogram extends SIUnit[Mass] {
  lazy val symbol: String = "kg"

  lazy val dimensionWitness: DimensionWitness = DimensionWitness.mass
}

/**
  * Represents the second time unit in the International System of Units (SI).
  * A second is the base unit of time, symbolized by "s".
  * It is associated with the time dimension.
  */
case object Second extends SIUnit[Time] {
  lazy val symbol: String = "s"

  lazy val dimensionWitness: DimensionWitness = DimensionWitness.time
}

/**
  * Represents the SI base unit of electric current, ampere (A).
  *
  * Ampere is a fundamental unit in the International System of Units (SI),
  * representing the dimension of electric current.
  *
  * This object provides the symbol for the ampere unit and a witness associating
  * it with its corresponding dimension. The dimension of ampere is characterized
  * by the tuple `[0, 0, 0, 1, 0, 0, 0]`, indicating no contribution from mass,
  * length, time, temperature, amount of substance, or luminous intensity,
  * and a single unit contribution from electric current.
  */
case object Ampere extends SIUnit[BaseDim[Zero, Zero, Zero, One, Zero, Zero, Zero]] {
  lazy val symbol: String = "A"

  lazy val dimensionWitness: DimensionWitness = DimensionWitness.current
}

/**
  * Represents the Kelvin unit of thermodynamic temperature in the International System of Units (SI).
  *
  * The Kelvin is the base SI unit for temperature measurement, symbolized by "K". It is defined using
  * the `BaseDim` type, where the exponent for thermodynamic temperature (Θ) is `One` and all
  * other exponents are `Zero`.
  *
  * Provides methods to retrieve its symbol and dimension witness representation.
  *
  * Inherits from the `SIUnit` trait.
  */
case object Kelvin extends SIUnit[BaseDim[Zero, Zero, Zero, Zero, One, Zero, Zero]] {
  lazy val symbol: String = "K"

  lazy val dimensionWitness: DimensionWitness = DimensionWitness.temperature
}

/**
  * Represents the mole, the SI base unit of the amount of substance.
  *
  * The mole is the SI unit used to measure the amount of a substance.
  * It is defined as the amount of a substance that contains as many
  * elementary entities (atoms, molecules, ions, etc.) as there are
  * atoms in 0.012 kilograms of carbon-12.
  *
  * @see SIUnit
  * @see BaseDim
  */
case object Mole extends SIUnit[BaseDim[Zero, Zero, Zero, Zero, Zero, One, Zero]] {
  lazy val symbol: String = "mol"

  lazy val dimensionWitness: DimensionWitness = DimensionWitness.amount
}

/**
  * Represents the candela (cd), the SI base unit of luminous intensity.
  *
  * The candela measures the luminous intensity of a light source in a
  * particular direction, aligned with human perception of brightness.
  *
  * This is the SI unit associated specifically with the `luminosity`
  * dimension, represented by the exponents (0, 0, 0, 0, 0, 0, 1) in the
  * seven-dimensional SI base unit system.
  *
  * The symbol for the candela is "cd".
  */
case object Candela extends SIUnit[BaseDim[Zero, Zero, Zero, Zero, Zero, Zero, One]] {
  lazy val symbol: String = "cd"

  lazy val dimensionWitness: DimensionWitness = DimensionWitness.luminosity
}

// ============================================================================
// Common Derived SI Units
// ============================================================================

/**
  * Represents the unit of area in square meters, derived by squaring the base unit of length (Meter).
  * SquareMeter is a unit of the dimension [L²] (area) in the International System of Units (SI).
  */
val SquareMeter: Unit[Area] = Meter.squared

/**
  * Represents the SI-derived unit of volume (`m³`), corresponding to a cubic meter.
  * This unit is created by cubing the `Meter` unit of length, resulting in a dimension
  * with the exponent 3 applied to the length dimension, i.e., [L³].
  */
val CubicMeter: Unit[Volume] = Meter.cubed

/**
  * Represents the unit of velocity in meters per second (m/s).
  * Combines the SI base unit of length (Meter) and time (Second) to create a derived unit
  * with a dimensional representation of [L¹T⁻¹].
  *
  * @see Meter for the SI unit of length
  * @see Second for the SI unit of time
  * @see / for unit division and creation of composite units
  */
val MetersPerSecond: Unit[Velocity] = Meter / Second

/**
  * Represents the unit of acceleration, defined as meters per second squared (m/s²),
  * using the SI base units for length (Meter) and time (Second).
  *
  * Combines the `Meter` unit with the squared inverse of the `Second` unit
  * using the composite unit operation `/`.
  *
  * The dimension of `Acceleration` can be expressed as [L¹T⁻²], where:
  * - L corresponds to Length (meter)
  * - T corresponds to Time (second)
  *
  * Example: When an object's velocity changes with time, its rate of change
  * is expressed in this unit as acceleration.
  */
val MetersPerSecondSquared: Unit[Acceleration] = Meter / Second.squared

/**
  * Represents the Newton unit of force, which is a derived SI unit
  * defined as \( 1 \, N = 1 \, kg \cdot m/s^2 \).
  *
  * This object provides the symbol and dimension witness for the Newton unit.
  */
case object Newton extends SIUnit[Force] {
  lazy val symbol: String = "N"

  lazy val dimensionWitness: DimensionWitness = DimensionWitness.force
}

/**
  * Represents the SI unit for energy, known as the joule (J).
  *
  * The joule is the derived unit of energy in the International System of Units (SI).
  * It is defined as the energy transferred when a force of one newton is applied over a distance of one meter.
  *
  * This object is a concrete implementation of the `SIUnit` trait, specifically for the dimension of energy.
  */
case object Joule extends SIUnit[Energy] {
  lazy val symbol: String = "J"

  lazy val dimensionWitness: DimensionWitness = DimensionWitness.energy
}

/**
  * Represents the Watt unit of measurement, the SI unit for power.
  *
  * Watt is defined as one joule per second and measures the rate of energy transfer.
  * It is primarily used in the context of electricity, mechanics, and thermodynamics.
  *
  * This object is part of the SI unit hierarchy and extends `SIUnit` with the `Power` dimension.
  *
  * @see SIUnit
  * @see DimensionWitness
  */
case object Watt extends SIUnit[Power] {
  lazy val symbol: String = "W"

  lazy val dimensionWitness: DimensionWitness = DimensionWitness.power
}

case object Weber extends SIUnit[MagneticFlux] {
  lazy val symbol: String = "Wb"

  lazy val dimensionWitness: DimensionWitness = DimensionWitness.magneticFlux
}

case object Henry extends SIUnit[Inductance] {
  lazy val symbol: String = "H"

  lazy val dimensionWitness: DimensionWitness = DimensionWitness.inductance
}

case object Tesla extends SIUnit[MagneticFluxDensity] {
  lazy val symbol: String = "T"

  lazy val dimensionWitness: DimensionWitness = DimensionWitness.magneticFluxDensity
}

/**
  * Represents the Pascal unit of pressure in the International System of Units (SI).
  *
  * The Pascal is the SI unit for measuring pressure, defined as one newton per square meter.
  * It is a derived unit with the dimension of force (mass × acceleration) per unit area.
  *
  * This object provides the unit symbol ("Pa") and an associated dimension witness for pressure.
  *
  * @define symbol           The symbol for the Pascal unit, "Pa".
  * @define dimensionWitness The dimension corresponding to pressure: mass × length⁻¹ × time⁻².
  */
case object Pascal extends SIUnit[Pressure] {
  lazy val symbol: String = "Pa"

  lazy val dimensionWitness: DimensionWitness = DimensionWitness.pressure
}

/**
  * Represents the Hertz (Hz), the SI unit of frequency.
  *
  * The Hertz is defined as the reciprocal of seconds (s⁻¹) and is used
  * to measure the frequency of periodic phenomena, such as vibrations
  * and waves.
  *
  * This object provides the symbol for the unit and a dimension witness
  * for frequency.
  */
case object Hertz extends SIUnit[Frequency] {
  lazy val symbol: String = "Hz"

  lazy val dimensionWitness: DimensionWitness = DimensionWitness.frequency
}

/**
  * Represents the Coulomb (C), the SI unit of electric charge.
  *
  * The symbol for the Coulomb is "C". This unit corresponds to the derived SI quantity
  * of electric charge as defined by the International System of Units (SI).
  */
case object Coulomb extends SIUnit[Charge] {
  lazy val symbol: String = "C"

  lazy val dimensionWitness: DimensionWitness = DimensionWitness.charge
}

/**
  * Represents the unit of electric potential, commonly known as voltage, in the International System of Units (SI).
  *
  * This object is a concrete instance of the `SIUnit` trait with the dimension type `Voltage`.
  * The `Volt` is used to measure the potential difference or electric pressure that drives the flow of electric charge in a circuit.
  *
  * @see SIUnit
  * @see DimensionWitness.voltage
  */
case object Volt extends SIUnit[Voltage] {
  lazy val symbol: String = "V"

  lazy val dimensionWitness: DimensionWitness = DimensionWitness.voltage
}

/**
  * Represents the SI unit of capacitance, the Farad (F).
  *
  * The Farad is the derived unit of electric capacitance in the International System of Units (SI).
  * It is defined as the capacitance of a capacitor in which a charge of one coulomb produces a
  * potential difference of one volt.
  *
  * This object provides a symbol for the unit and a dimension witness indicating the
  * dimensional analysis of capacitance.
  */
case object Farad extends SIUnit[Capacitance] {
  lazy val symbol: String = "F"

  lazy val dimensionWitness: DimensionWitness = DimensionWitness.capacitance
}

/**
  * Represents the ohm (Ω), the SI unit of electrical resistance.
  *
  * The ohm is defined as the resistance between two points of a conductor
  * when a constant potential difference of one volt across these points
  * produces a current of one ampere, provided the conductor is not the
  * seat of any electromotive force.
  *
  * This object extends the `SIUnit` trait for the dimension of resistance.
  *
  * @see [[SIUnit]]
  * @see [[DimensionWitness.resistance]]
  */
case object Ohm extends SIUnit[Resistance] {
  lazy val symbol: String = "Ω"

  lazy val dimensionWitness: DimensionWitness = DimensionWitness.resistance
}

// ============================================================================
// Common Non-SI Length Units
// ============================================================================

/** Kilometer */
val Kilometer: Unit[Length] = Meter.scaled(Rational(1000), "km")

/** Centimeter */
val Centimeter: Unit[Length] = Meter.scaled(Rational(1, 100), "cm")

/** Millimeter */
val Millimeter: Unit[Length] = Meter.scaled(Rational(1, 1000), "mm")

/** Inch */
val Inch: Unit[Length] = Meter.scaled(Rational(254, 10000), "in")

/** Foot */
val Foot: Unit[Length] = Inch.scaled(Rational(12), "ft")

/** Yard */
val Yard: Unit[Length] = Foot.scaled(Rational(3), "yd")

/** Mile */
val Mile: Unit[Length] = Yard.scaled(Rational(1760), "mi")

// ============================================================================
// Common Non-SI Mass Units
// ============================================================================

/** Gram */
val Gram: Unit[Mass] = Kilogram.scaled(Rational(1, 1000), "g")

/** Pound */
val Pound: Unit[Mass] = Kilogram.scaled(Rational(45359237, 100000000), "lb")

/** Ounce */
val Ounce: Unit[Mass] = Pound.scaled(Rational(1, 16), "oz")

// ============================================================================
// Common Non-SI Time Units
// ============================================================================

/** Minute */
val Minute: Unit[Time] = Second.scaled(Rational(60), "min")

/** Hour */
val Hour: Unit[Time] = Minute.scaled(Rational(60), "h")

/** Day */
val Day: Unit[Time] = Hour.scaled(Rational(24), "d")

// ============================================================================
// Angular Measures (dimensionless but semantically distinct)
// ============================================================================

/** Radian - SI unit of angle (dimensionless) */
case object Radian extends SIUnit[com.phasmidsoftware.number.dimensions.core.Angle] {
  lazy val symbol: String = "rad"

  lazy val dimensionWitness: DimensionWitness = DimensionWitness.dimensionless
}

// ============================================================================
// Common Derived Non-SI Units
// ============================================================================

/** Liter - unit of volume */
val Liter: Unit[Volume] = CubicMeter.scaled(Rational(1, 1000), "L")

/** Kilometer per hour - unit of velocity */
val KilometerPerHour: Unit[Velocity] = MetersPerSecond.scaled(Rational(5, 18), "km/h")

/** Mile per hour - unit of velocity */
val MilePerHour: Unit[Velocity] = MetersPerSecond.scaled(Rational(1397, 3125), "mph")

// ============================================================================
// Unit Registry
// ============================================================================

// Registry of known unit symbols
val unitRegistry: Map[String, Unit[?]] = buildRegistry(
  // Base SI units
  Meter, Kilogram, Second, Ampere, Kelvin, Mole, Candela,
  // Named derived SI units
  Newton, Joule, Watt, Pascal, Hertz, Coulomb, Volt, Ohm, Tesla, Henry, Weber, Farad,
  // Scaled units
  Kilometer, Centimeter, Millimeter, Inch, Foot, Yard, Mile,
  Gram, Pound, Ounce,
  Minute, Hour, Day,
  Radian, // Keep - named unit with "rad" symbol
  // Dimensionless,  // Optional - has empty symbol so won't match anyway
  Liter, KilometerPerHour, MilePerHour,
  // Named composite units
  CompositeUnits.Chain, CompositeUnits.LightSecond, CompositeUnits.C,
  CompositeUnits.Hectometer, CompositeUnits.Hectare, CompositeUnits.Milliliter,
  CompositeUnits.GallonImp
)
/**
  * Build a registry mapping unit symbols (both regular and composite) to unit instances.
  */
def buildRegistry(units: Unit[?]*): Map[String, Unit[?]] = {
  units.flatMap { u =>
    val entries = scala.collection.mutable.ListBuffer((u.symbol, u))

    // Only add composite symbol for SIUnits (named derived units like Newton)
    u match {
      case _: SIUnit[?] =>
        val comp = u.compositeSymbol
        if (comp != u.symbol && comp != "1") {
          entries += ((comp, u))
        }
      case _ => // Don't register composite symbols for ProductUnit, QuotientUnit, etc.
    }

    entries.toSeq
  }.toMap
}

// ============================================================================
// LaTeX Rendering
// ============================================================================

/**
  * Render a physical unit in LaTeX format.
  * This is the definitive LaTeX rendering function.
  *
  * @param unit the unit to render
  * @return LaTeX string representation
  */
def toLatex[D <: Dimension](unit: Unit[D]): String = unit match {
  case u: SIUnit[D] => s"\\text{${u.symbol}}"

  case u: ProductUnit[D] =>
    s"${toLatex(u.left)}\\cdot${toLatex(u.right)}"

  case u: QuotientUnit[D] =>
    s"\\frac{${toLatex(u.numerator)}}{${toLatex(u.denominator)}}"

  case u: PowerUnit[D] =>
    val baseLatex = u.base match {
      case _: ProductUnit[?] | _: QuotientUnit[?] =>
        s"\\left(${toLatex(u.base)}\\right)"
      case _ =>
        toLatex(u.base)
    }

    val exponent = u.power match {
      case r if r == Rational(2) => "{2}"
      case r if r == Rational(3) => "{3}"
      case r if r == Rational(-1) => "{-1}"
      case r if r == Rational(-2) => "{-2}"
      case r if r == Rational(-3) => "{-3}"
      case r if r == Rational(1, 2) => "{\\frac{1}{2}}"
      case r if r.d == 1 => s"{${r.n}}"
      case r => s"{\\tfrac{${r.n}}{${r.d}}}"
    }

    s"$baseLatex^$exponent"

  case u: ScaledUnit[D] =>
    s"\\text{${u.symbol}}"

  case _ =>
    s"\\text{${unit.symbol}}" // fallback for any other unit types
}

/**
  * Commonly used composite units defined for convenience.
  * Users can also create their own composite units using the * and / operators.
  *
  * Note: These are provided as alternatives to the predefined units like
  * MeterPerSecond, Newton, etc. The advantage of composite units is that
  * they can be created on-the-fly for any combination.
  *
  * For definitions of Imperial Units, see [[https://www.legislation.gov.uk/uksi/1995/1804/schedule/made]]
  */
object CompositeUnits:

  // Length
  val Chain: Unit[Length] = Yard.scaled(22, "ch")
  val LightSecond: Unit[Length] = Meter.scaled(299792458, "ls")

  // Velocity
  val C: Unit[Velocity] = LightSecond / Second

  // Area
  val Hectometer: Unit[Length] = Meter.scaled(100, "hm")
  val Hectare: Unit[Area] = (Hectometer * Hectometer).rename("ha")

  // Volume
  val Milliliter: Unit[Volume] = Liter.scaled(Rational(1000).invert, "ml")
  val GallonImp: Unit[Volume] = Liter.scaled(Rational(454609, 10000), "gal")

  // Gravitation
  val Kepler: Unit[DivDim[DivDim[Volume, Mass], PowDim[Time, Two]]] =
    CubicMeter / Kilogram / Second.squared

  // For Planck's constant
  val JouleSecond: Unit[EnergyTime] = Joule * Second

  // For Boltzmann's constant
  val JoulePerKelvin: Unit[EnergyPerTemperature] = Joule / Kelvin

  // For Electromagnetism
  val HenryPerMeter: Unit[Permeability] = Henry / Meter
  val FaradPerMeter: Unit[Permittivity] = Farad / Meter

  // For Thermodynamics
  val blackBoxRadiationUnits: Unit[DivDim[Power, MulDim[Area, PowDim[PowDim[Temperature, Two], Two]]]] =
    Watt / (Meter.squared * Kelvin.squared.squared)
  val gasConstantUnits: Unit[DivDim[Energy, MulDim[Amount, Temperature]]] =
    Joule / (Mole * Kelvin)
