package com.phasmidsoftware.number.dimensions.core

import com.phasmidsoftware.number.algebra.eager.*
import com.phasmidsoftware.number.core.inner.Rational

/**
  * Trait representing a unit of measurement.
  * Units combine a dimension with a scale factor relative to SI base units.
  */
trait Unit[D <: Dimension] {
  /** The scale factor relative to the SI base unit */
  def toSI: Scalar

  /** The symbol for this unit */
  def symbol: String
}

trait SIUnit[D <: Dimension] extends Unit[D] {
  /** The scale factor relative to the SI base unit */
  def toSI: Scalar = WholeNumber.one
}
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
case object Kilometer extends Unit[Length] {
  def toSI: WholeNumber = 1000

  def symbol: String = "km"
}

/** Centimeter */
case object Centimeter extends Unit[Length] {
  def toSI: RationalNumber = RationalNumber(1, 100)

  def symbol: String = "cm"
}

/** Millimeter */
case object Millimeter extends Unit[Length] {
  def toSI: RationalNumber = RationalNumber(1, 1000)

  def symbol: String = "mm"
}

/** Inch */
case object Inch extends Unit[Length] {
  def toSI: RationalNumber = RationalNumber(254, 10000)

  def symbol: String = "in"
}

/** Foot */
case object Foot extends Unit[Length] {
  def toSI: RationalNumber = Inch.toSI * Rational(12)

  def symbol: String = "ft"
}

/** Yard */
case object Yard extends Unit[Length] {
  def toSI: RationalNumber = Foot.toSI * Rational(3)

  def symbol: String = "yd"
}

/** Mile */
case object Mile extends Unit[Length] {
  def toSI: RationalNumber = Yard.toSI * Rational(1760)

  def symbol: String = "mi"
}

// ============================================================================
// Common Non-SI Mass Units
// ============================================================================

/** Gram */
case object Gram extends Unit[Mass] {
  def toSI: RationalNumber = RationalNumber(1, 1000)

  def symbol: String = "g"
}

/** Pound */
case object Pound extends Unit[Mass] {
  def toSI: RationalNumber = RationalNumber(45359237, 100000000)

  def symbol: String = "lb"
}

/** Ounce */
case object Ounce extends Unit[Mass] {
  def toSI: Scalar = Pound.toSI.scale(Rational(16).invert)

  def symbol: String = "oz"
}

// ============================================================================
// Common Non-SI Time Units
// ============================================================================

/** Minute */
case object Minute extends Unit[Time] {
  def toSI: WholeNumber = 60

  def symbol: String = "min"
}

/** Hour */
case object Hour extends Unit[Time] {
  def toSI: WholeNumber = 60 * Minute.toSI

  def symbol: String = "h"
}

/** Day */
case object Day extends Unit[Time] {
  def toSI: WholeNumber = 24 * Hour.toSI

  def symbol: String = "d"
}

// ============================================================================
// Angular Measures (dimensionless but semantically distinct)
// ============================================================================

/** Radian - SI unit of angle (dimensionless) */
case object Radian extends SIUnit[Angle] {
  def symbol: String = "rad"
}

/** Degree - unit of angle (π/180 radians) */
case object Degree extends Unit[Angle] {
  def toSI: Real = Angle.pi.scale(Rational(180).invert).convert(Real.one).getOrElse(Real(0))

  def symbol: String = "°"
}

// ============================================================================
// Common Derived Non-SI Units
// ============================================================================

/** Liter - unit of volume */
case object Liter extends Unit[Volume] {
  def toSI: RationalNumber = RationalNumber(1, 1000)

  def symbol: String = "L"
}

/** Kilometer per hour - unit of velocity */
case object KilometerPerHour extends Unit[Velocity] {
  def toSI: RationalNumber = RationalNumber(5, 18)

  def symbol: String = "km/h"
}

/** Mile per hour - unit of velocity */
case object MilePerHour extends Unit[Velocity] {
  def toSI: Scalar = Mile.toSI * Hour.toSI.toRational.invert

  def symbol: String = "mph"
}