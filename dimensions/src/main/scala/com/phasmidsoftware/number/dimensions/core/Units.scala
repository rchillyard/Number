package com.phasmidsoftware.number.dimensions.core

import com.phasmidsoftware.number.algebra.eager.*
import com.phasmidsoftware.number.algebra.util.FP
import com.phasmidsoftware.number.core.inner.Rational

/**
  * Trait representing a unit of measurement.
  * Units combine a dimension with a scale factor relative to SI base units.
  */
trait Unit[D <: Dimension] {
  /** The scale factor relative to the SI base unit */
  def toSI: ExactNumber

  /** The symbol for this unit */
  def symbol: String
}

/**
  * Represents a unit of measurement that is part of the International System of Units (SI).
  *
  * @tparam D the dimension type associated with this unit, which must extend `Dimension`
  */
trait SIUnit[D <: Dimension] extends Unit[D] {
  /** The scale factor relative to the SI base unit */
  def toSI: ExactNumber = WholeNumber.one
}

/**
  * A composite unit formed by multiplying two units.
  * The resulting dimension is the product of the component dimensions.
  */
case class ProductUnit[D <: Dimension](
                                        left: Unit[?],
                                        right: Unit[?]
                                      ) extends Unit[D]:

  def toSI: ExactNumber = left.toSI * right.toSI match {
    case r: ExactNumber => r
    case x => throw DimensionsException(s"cannot convert $left * $right to a RationalNumber")
  }

  def symbol: String = s"${left.symbol}·${right.symbol}"

/**
  * A composite unit formed by dividing two units.
  * The resulting dimension is the quotient of the component dimensions.
  */
case class QuotientUnit[D <: Dimension](
                                         numerator: Unit[?],
                                         denominator: Unit[?]
                                       ) extends Unit[D]:

  def toSI: ExactNumber = numerator.toSI / denominator.toSI match {
    case r: ExactNumber => r
    case x => throw DimensionsException(s"cannot convert $numerator / $denominator to a RationalNumber: got $x")
  }

  def symbol: String = s"${numerator.symbol}/${denominator.symbol}"

/**
  * A unit raised to an integer power.
  * The resulting dimension is the base dimension raised to that power.
  */
case class PowerUnit[D <: Dimension](
                                      base: Unit[?],
                                      power: Rational
                                    ) extends Unit[D]:

  def toSI: ExactNumber =
    FP.recover(base.toSI.pow(RationalNumber(power)))(DimensionsException("PowerUnit.toSI: cannot convert to RationalNumber:"))

  def symbol: String =
    val baseSymbol = base match {
      case _: ProductUnit[?] | _: QuotientUnit[?] => s"(${base.symbol})"
      case _ => base.symbol
    }

    if power == Rational(2) then s"$baseSymbol²"
    else if power == Rational(3) then s"$baseSymbol³"
    else if power == Rational(-1) then s"$baseSymbol⁻¹"
    else if power == Rational(-2) then s"$baseSymbol⁻²"
    else if power == Rational(-3) then s"$baseSymbol⁻³"
    else s"$baseSymbol^$power"

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
  * @param name  The name or symbol of the scaled unit.
  */
case class ScaledUnit[D <: Dimension](
                                       base: Unit[?],
                                       scale: Rational,
                                       name: String
                                     ) extends Unit[D]:
  /** The scale factor relative to the SI base unit */
  def toSI: ExactNumber = base.toSI * scale

  /** The symbol for this unit */
  def symbol: String = name

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
    ProductUnit[MulDim[D1, D2]](u1, u2)

  /**
    * Divide two units to create a composite unit.
    * The dimension of the result is the quotient of the input dimensions.
    *
    * Example: Meter / Second gives a unit with dimension [L¹T⁻¹] (velocity)
    */
  def /[D2 <: Dimension](u2: Unit[D2]): Unit[DivDim[D1, D2]] =
    QuotientUnit[DivDim[D1, D2]](u1, u2)

  /**
    * Squares the current unit.
    * Example: Meter.squared gives a unit with dimension [L²] (area)
    */
  def squared: Unit[PowDim[D1, Two]] =
    PowerUnit[PowDim[D1, Two]](u1, Rational(2))

  /**
    * Cubes the current unit.
    * Example: Meter.cubed gives a unit with dimension [L³] (volume)
    */
  def cubed: Unit[PowDim[D1, TRat[3, 1]]] =
    PowerUnit[PowDim[D1, TRat[3, 1]]](u1, Rational(3))

  /**
    * Takes the square root of the current unit.
    * Example: SquareMeter.sqrt gives a unit with dimension [L] (length)
    */
  def sqrt: Unit[PowDim[D1, Half]] =
    PowerUnit[PowDim[D1, Half]](u1, Rational(1, 2))

  /**
    * Inverts the current unit (raises to power -1).
    * Example: Second.invert gives a unit with dimension [T⁻¹] (frequency)
    */
  def invert: Unit[PowDim[D1, MinusOne]] =
    PowerUnit[PowDim[D1, MinusOne]](u1, Rational(-1))

  /**
    * Scales the unit by the given rational factor and assigns an optional name to the resulting unit.
    *
    * @param scale The rational factor by which the unit is scaled.
    * @param name  An optional name for the scaled unit, default is an empty string.
    * @return A scaled unit of type `Unit[D1]`.
    */
  def scaled(scale: Rational, name: String = ""): Unit[D1] =
    ScaledUnit[D1](u1, scale, name)

  /**
    * Scales a unit by a given integer factor and assigns a name to the resulting unit.
    *
    * @param scale The integer factor by which the unit is scaled.
    * @param name  The name assigned to the scaled unit.
    * @return A scaled unit of type `Unit[D1]`.
    */
  def scaled(scale: Int, name: String): Unit[D1] =
    scaled(Rational(scale), name)

/**
  * Special dimension for angles (dimensionless but semantically distinct)
  */
type Angle = BaseDim[Zero, Zero, Zero, Zero, Zero, Zero, Zero]

// ============================================================================
// SI Base Units
// ============================================================================

/** Meter - SI base unit of length */
case object Meter extends SIUnit[Length] {
  def symbol: String = "m"
}

/** Kilogram - SI base unit of mass */
case object Kilogram extends SIUnit[Mass] {
  def symbol: String = "kg"
}

/** Second - SI base unit of time */
case object Second extends SIUnit[Time] {
  def symbol: String = "s"
}

/** Ampere - SI base unit of electric current */
case object Ampere extends SIUnit[BaseDim[Zero, Zero, Zero, One, Zero, Zero, Zero]] {
  def symbol: String = "A"
}

/** Kelvin - SI base unit of thermodynamic temperature */
case object Kelvin extends SIUnit[BaseDim[Zero, Zero, Zero, Zero, One, Zero, Zero]] {
  def symbol: String = "K"
}

/** Mole - SI base unit of amount of substance */
case object Mole extends SIUnit[BaseDim[Zero, Zero, Zero, Zero, Zero, One, Zero]] {
  def symbol: String = "mol"
}

/** Candela - SI base unit of luminous intensity */
case object Candela extends SIUnit[BaseDim[Zero, Zero, Zero, Zero, Zero, Zero, One]] {
  def symbol: String = "cd"
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

/** Newton - unit of force (kg⋅m/s²) */
case object Newton extends SIUnit[Force] {
  def symbol: String = "N"
}

/** Joule - unit of energy (N⋅m) */
case object Joule extends SIUnit[Energy] {
  def symbol: String = "J"
}

case object Watt extends SIUnit[Power] {
  def symbol: String = "W"
}

case object Pascal extends SIUnit[Pressure] {
  def symbol: String = "Pa"
}

case object Hertz extends SIUnit[Frequency] {
  def symbol: String = "Hz"
}

case object Coulomb extends SIUnit[Charge] {
  def symbol: String = "C"
}

case object Volt extends SIUnit[Voltage] {
  def symbol: String = "V"
}

case object Ohm extends SIUnit[Resistance] {
  def symbol: String = "Ω"
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
case object Radian extends SIUnit[Angle] {
  def symbol: String = "rad"
}

///** Degree - unit of angle (π/180 radians) */
//case object Degree extends Unit[Angle] {
//  def toSI: Real = Angle.pi.scale(Rational(180).invert).convert(Real.one).getOrElse(Real(0))
//
//  def symbol: String = "°"
//}

// ============================================================================
// Common Derived Non-SI Units
// ============================================================================

/** Liter - unit of volume */
val Liter: Unit[Volume] = CubicMeter.scaled(Rational(1, 1000), "L")

/** Kilometer per hour - unit of velocity */
val KilometerPerHour: Unit[Velocity] = MetersPerSecond.scaled(Rational(5, 18), "km/h")

/** Mile per hour - unit of velocity */
val MilePerHour: Unit[Velocity] = MetersPerSecond.scaled(Rational(1397, 3125), "mph")

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
  * Extension method to render units in LaTeX format
  */
extension [D <: Dimension](unit: Unit[D])
  def renderLaTeX: String = toLatex(unit)


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
  val Chain: Unit[Length] = Yard.scaled(22, "chain")
  val LightSecond: Unit[Length] = Meter.scaled(299792458, "ls")

  // Velocity
  val C: Unit[Velocity] = LightSecond / Second

  // Area
  val Hectometer: Unit[Length] = Meter.scaled(Rational(100))
  val Hectare: Unit[Area] = Hectometer * Hectometer

  // Volume
  val Milliliter: Unit[Volume] = Liter.scaled(Rational(1000).invert)
  val GallonImp: Unit[Volume] = Liter.scaled(Rational(454609, 10000))