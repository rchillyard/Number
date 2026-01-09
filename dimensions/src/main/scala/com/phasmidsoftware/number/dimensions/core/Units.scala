package com.phasmidsoftware.number.dimensions.core

import com.phasmidsoftware.number.algebra.eager.*
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
    case r: RationalNumber => r
    case _ => throw DimensionsException(s"cannot convert $left * $right to a RationalNumber")
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
    case r: RationalNumber => r
    case _ => throw DimensionsException(s"cannot convert $numerator / $denominator to a RationalNumber")
  }

  def symbol: String = s"${numerator.symbol}/${denominator.symbol}"

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
    * Scales the unit by the given rational factor and assigns an optional name to the resulting unit.
    *
    * @param scale The rational factor by which the unit is scaled.
    * @param name  An optional name for the scaled unit, default is an empty string.
    * @return A scaled unit of type `Unit[D1]`.
    */
  def scaled(scale: Rational, name: String = ""): Unit[D1] = 
    ScaledUnit[D1](u1, scale, name)

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

/** Square meter - unit of area */
case object SquareMeter extends SIUnit[Area] {
  def symbol: String = "m²"
}

/** Cubic meter - unit of volume */
case object CubicMeter extends SIUnit[Volume] {
  def symbol: String = "m³"
}

/** Meter per second - unit of velocity */
case object MeterPerSecond extends SIUnit[Velocity] {
  def symbol: String = "m/s"
}

/** Meter per second squared - unit of acceleration */
case object MeterPerSecondSquared extends SIUnit[Acceleration] {
  def symbol: String = "m/s²"
}

/** Newton - unit of force (kg⋅m/s²) */
case object Newton extends SIUnit[Force] {
  def symbol: String = "N"
}

/** Joule - unit of energy (N⋅m) */
case object Joule extends SIUnit[Energy] {
  def symbol: String = "J"
}

/** Watt - unit of power (J/s) */
type Power = BaseDim[One, Two, TRat[-3, 1], Zero, Zero, Zero, Zero]

case object Watt extends SIUnit[Power] {
  def symbol: String = "W"
}

/** Pascal - unit of pressure (N/m²) */
type Pressure = BaseDim[One, TRat[-1, 1], TRat[-2, 1], Zero, Zero, Zero, Zero]

case object Pascal extends SIUnit[Pressure] {
  def symbol: String = "Pa"
}

/** Hertz - unit of frequency (1/s) */
type Frequency = BaseDim[Zero, Zero, MinusOne, Zero, Zero, Zero, Zero]

case object Hertz extends SIUnit[Frequency] {
  def symbol: String = "Hz"
}

/** Coulomb - unit of electric charge (A⋅s) */
type Charge = BaseDim[Zero, Zero, One, One, Zero, Zero, Zero]

case object Coulomb extends SIUnit[Charge] {
  def symbol: String = "C"
}

/** Volt - unit of electric potential (W/A) */
type Voltage = BaseDim[One, Two, TRat[-3, 1], MinusOne, Zero, Zero, Zero]

case object Volt extends SIUnit[Voltage] {
  def symbol: String = "V"
}

/** Ohm - unit of resistance (V/A) */
type Resistance = BaseDim[One, Two, TRat[-3, 1], TRat[-2, 1], Zero, Zero, Zero]

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
val KilometerPerHour: Unit[Velocity] = MeterPerSecond.scaled(Rational(5, 18), "km/h")

/** Mile per hour - unit of velocity */
val MilePerHour: Unit[Velocity] = MeterPerSecond.scaled(Rational(1397, 3125), "mph")

/**
  * Commonly used composite units defined for convenience.
  * Users can also create their own composite units using the * and / operators.
  *
  * Note: These are provided as alternatives to the predefined units like
  * MeterPerSecond, Newton, etc. The advantage of composite units is that
  * they can be created on-the-fly for any combination.
  */
object CompositeUnits:
  // Velocity
  val MetersPerSecond: Unit[Velocity] = Meter / Second
  val KilometersPerHour: Unit[Velocity] = Kilometer / Hour
  val MilesPerHour: Unit[Velocity] = Mile / Hour

  // Acceleration
  val MetersPerSecondSquared: Unit[Acceleration] = Meter / (Second * Second)

  // Force
  val Newtons: Unit[Force] = Kilogram * Meter / (Second * Second)

  // Energy
  val Joules: Unit[Energy] = Kilogram * (Meter / Second) * (Meter / Second)

  // Area
  val SquareMeters: Unit[Area] = Meter * Meter
  val SquareKilometers: Unit[Area] = Kilometer * Kilometer

  // Volume
  val CubicMeters: Unit[Volume] = Meter * Meter * Meter