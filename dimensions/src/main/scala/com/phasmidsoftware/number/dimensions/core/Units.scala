package com.phasmidsoftware.number.dimensions.core

/**
  * Trait representing a unit of measurement.
  * Units combine a dimension with a scale factor relative to SI base units.
  */
trait Unit[D <: Dimension] {
  /** The scale factor relative to the SI base unit */
  def toSI: Double

  /** The symbol for this unit */
  def symbol: String
}

/**
  * Special dimension for angles (dimensionless but semantically distinct)
  */
type Angle = BaseDim[Zero, Zero, Zero, Zero, Zero, Zero, Zero]

// ============================================================================
// SI Base Units
// ============================================================================

/** Meter - SI base unit of length */
case object Meter extends Unit[Length] {
  def toSI: Double = 1.0

  def symbol: String = "m"
}

/** Kilogram - SI base unit of mass */
case object Kilogram extends Unit[Mass] {
  def toSI: Double = 1.0

  def symbol: String = "kg"
}

/** Second - SI base unit of time */
case object Second extends Unit[Time] {
  def toSI: Double = 1.0

  def symbol: String = "s"
}

/** Ampere - SI base unit of electric current */
case object Ampere extends Unit[BaseDim[Zero, Zero, Zero, One, Zero, Zero, Zero]] {
  def toSI: Double = 1.0

  def symbol: String = "A"
}

/** Kelvin - SI base unit of thermodynamic temperature */
case object Kelvin extends Unit[BaseDim[Zero, Zero, Zero, Zero, One, Zero, Zero]] {
  def toSI: Double = 1.0

  def symbol: String = "K"
}

/** Mole - SI base unit of amount of substance */
case object Mole extends Unit[BaseDim[Zero, Zero, Zero, Zero, Zero, One, Zero]] {
  def toSI: Double = 1.0

  def symbol: String = "mol"
}

/** Candela - SI base unit of luminous intensity */
case object Candela extends Unit[BaseDim[Zero, Zero, Zero, Zero, Zero, Zero, One]] {
  def toSI: Double = 1.0

  def symbol: String = "cd"
}

// ============================================================================
// Common Derived SI Units
// ============================================================================

/** Square meter - unit of area */
case object SquareMeter extends Unit[Area] {
  def toSI: Double = 1.0

  def symbol: String = "m²"
}

/** Cubic meter - unit of volume */
case object CubicMeter extends Unit[Volume] {
  def toSI: Double = 1.0

  def symbol: String = "m³"
}

/** Meter per second - unit of velocity */
case object MeterPerSecond extends Unit[Velocity] {
  def toSI: Double = 1.0

  def symbol: String = "m/s"
}

/** Meter per second squared - unit of acceleration */
case object MeterPerSecondSquared extends Unit[Acceleration] {
  def toSI: Double = 1.0

  def symbol: String = "m/s²"
}

/** Newton - unit of force (kg⋅m/s²) */
case object Newton extends Unit[Force] {
  def toSI: Double = 1.0

  def symbol: String = "N"
}

/** Joule - unit of energy (N⋅m) */
case object Joule extends Unit[Energy] {
  def toSI: Double = 1.0

  def symbol: String = "J"
}

/** Watt - unit of power (J/s) */
type Power = BaseDim[One, Two, TRat[-3, 1], Zero, Zero, Zero, Zero]

case object Watt extends Unit[Power] {
  def toSI: Double = 1.0

  def symbol: String = "W"
}

/** Pascal - unit of pressure (N/m²) */
type Pressure = BaseDim[One, TRat[-1, 1], TRat[-2, 1], Zero, Zero, Zero, Zero]

case object Pascal extends Unit[Pressure] {
  def toSI: Double = 1.0

  def symbol: String = "Pa"
}

/** Hertz - unit of frequency (1/s) */
type Frequency = BaseDim[Zero, Zero, MinusOne, Zero, Zero, Zero, Zero]

case object Hertz extends Unit[Frequency] {
  def toSI: Double = 1.0

  def symbol: String = "Hz"
}

/** Coulomb - unit of electric charge (A⋅s) */
type Charge = BaseDim[Zero, Zero, One, One, Zero, Zero, Zero]

case object Coulomb extends Unit[Charge] {
  def toSI: Double = 1.0

  def symbol: String = "C"
}

/** Volt - unit of electric potential (W/A) */
type Voltage = BaseDim[One, Two, TRat[-3, 1], MinusOne, Zero, Zero, Zero]

case object Volt extends Unit[Voltage] {
  def toSI: Double = 1.0

  def symbol: String = "V"
}

/** Ohm - unit of resistance (V/A) */
type Resistance = BaseDim[One, Two, TRat[-3, 1], TRat[-2, 1], Zero, Zero, Zero]

case object Ohm extends Unit[Resistance] {
  def toSI: Double = 1.0

  def symbol: String = "Ω"
}

// ============================================================================
// Common Non-SI Length Units
// ============================================================================

/** Kilometer */
case object Kilometer extends Unit[Length] {
  def toSI: Double = 1000.0

  def symbol: String = "km"
}

/** Centimeter */
case object Centimeter extends Unit[Length] {
  def toSI: Double = 0.01

  def symbol: String = "cm"
}

/** Millimeter */
case object Millimeter extends Unit[Length] {
  def toSI: Double = 0.001

  def symbol: String = "mm"
}

/** Inch */
case object Inch extends Unit[Length] {
  def toSI: Double = 0.0254

  def symbol: String = "in"
}

/** Foot */
case object Foot extends Unit[Length] {
  def toSI: Double = 0.3048

  def symbol: String = "ft"
}

/** Yard */
case object Yard extends Unit[Length] {
  def toSI: Double = 0.9144

  def symbol: String = "yd"
}

/** Mile */
case object Mile extends Unit[Length] {
  def toSI: Double = 1609.344

  def symbol: String = "mi"
}

// ============================================================================
// Common Non-SI Mass Units
// ============================================================================

/** Gram */
case object Gram extends Unit[Mass] {
  def toSI: Double = 0.001

  def symbol: String = "g"
}

/** Pound */
case object Pound extends Unit[Mass] {
  def toSI: Double = 0.45359237

  def symbol: String = "lb"
}

/** Ounce */
case object Ounce extends Unit[Mass] {
  def toSI: Double = 0.028349523125

  def symbol: String = "oz"
}

// ============================================================================
// Common Non-SI Time Units
// ============================================================================

/** Minute */
case object Minute extends Unit[Time] {
  def toSI: Double = 60.0

  def symbol: String = "min"
}

/** Hour */
case object Hour extends Unit[Time] {
  def toSI: Double = 3600.0

  def symbol: String = "h"
}

/** Day */
case object Day extends Unit[Time] {
  def toSI: Double = 86400.0

  def symbol: String = "d"
}

// ============================================================================
// Angular Measures (dimensionless but semantically distinct)
// ============================================================================

/** Radian - SI unit of angle (dimensionless) */
case object Radian extends Unit[Angle] {
  def toSI: Double = 1.0

  def symbol: String = "rad"
}

/** Degree - unit of angle (π/180 radians) */
case object Degree extends Unit[Angle] {
  def toSI: Double = Math.PI / 180.0

  def symbol: String = "°"
}

/** Gradian - unit of angle (π/200 radians) */
case object Gradian extends Unit[Angle] {
  def toSI: Double = Math.PI / 200.0

  def symbol: String = "grad"
}

/** Turn - full rotation (2π radians) */
case object Turn extends Unit[Angle] {
  def toSI: Double = 2.0 * Math.PI

  def symbol: String = "tr"
}

// ============================================================================
// Common Derived Non-SI Units
// ============================================================================

/** Liter - unit of volume */
case object Liter extends Unit[Volume] {
  def toSI: Double = 0.001

  def symbol: String = "L"
}

/** Kilometer per hour - unit of velocity */
case object KilometerPerHour extends Unit[Velocity] {
  def toSI: Double = 1000.0 / 3600.0

  def symbol: String = "km/h"
}

/** Mile per hour - unit of velocity */
case object MilePerHour extends Unit[Velocity] {
  def toSI: Double = 0.44704

  def symbol: String = "mph"
}