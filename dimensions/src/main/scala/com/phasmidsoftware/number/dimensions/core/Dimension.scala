package com.phasmidsoftware.number.dimensions.core

/**
  * Trait representing physical dimensions using SI base units.
  */
trait Dimension

/**
  * Seven-tuple of rational exponents representing the seven SI base units:
  * - M: Mass (kilogram)
  * - L: Length (meter)
  * - T: Time (second)
  * - I: Electric current (ampere)
  * - Θ: Thermodynamic temperature (kelvin)
  * - N: Amount of substance (mole)
  * - J: Luminous intensity (candela)
  */
case class BaseDim[
  M <: TRational,
  L <: TRational,
  T <: TRational,
  I <: TRational,
  Θ <: TRational,
  N <: TRational,
  J <: TRational
]() extends Dimension

// Common rational values
type Zero = TRat[0, 1]
type One = TRat[1, 1]
type Half = TRat[1, 2]
type Two = TRat[2, 1]
type MinusOne = TRat[-1, 1]

// Fundamental dimensions as type aliases
type Dimensionless = BaseDim[Zero, Zero, Zero, Zero, Zero, Zero, Zero]
type Mass = BaseDim[One, Zero, Zero, Zero, Zero, Zero, Zero]
type Length = BaseDim[Zero, One, Zero, Zero, Zero, Zero, Zero]
type Time = BaseDim[Zero, Zero, One, Zero, Zero, Zero, Zero]
type Current = BaseDim[Zero, Zero, Zero, One, Zero, Zero, Zero]
type Temperature = BaseDim[Zero, Zero, Zero, Zero, One, Zero, Zero]
type Amount = BaseDim[Zero, Zero, Zero, Zero, Zero, One, Zero]
type LuminousIntensity = BaseDim[Zero, Zero, Zero, Zero, Zero, Zero, One]

// Other common dimensions as type aliases
/** Area: Distance times Distance */
type Area = MulDim[Length, Length]
/** Volume: Area times Distance */
type Volume = MulDim[Area, Length]
/** SquareRoot of Length: SquareRoot(Distance) */
type SqrtLength = BaseDim[Zero, Half, Zero, Zero, Zero, Zero, Zero]
/** Velocity: Length over Time */
type Velocity = DivDim[Length, Time]
/** Acceleration: Velocity over Time */
type Acceleration = DivDim[Velocity, Time]
/** Force: Acceleration times Mass */
type Force = MulDim[Acceleration, Mass]
/** Energy: Force times Length */
type Energy = MulDim[Force, Length]
/** Pressure (Stress): Force over Area */
type Pressure = DivDim[Force, Area]
/** Power: Energy over Time */
type Power = DivDim[Energy, Time]
/** Charge: Current times Time */
type Charge = MulDim[Current, Time]
/** Voltage: Power over Current */
type Voltage = DivDim[Power, Current]
/** Resistance: Voltage over Current */
type Resistance = DivDim[Voltage, Current]
/** Frequency: Dimensionless over Time */
type Frequency = DivDim[Dimensionless, Time]

/**
  * Multiply two dimensions by adding their corresponding exponents.
  */
type MulDim[D1 <: Dimension, D2 <: Dimension] <: Dimension = (D1, D2) match {
  case (BaseDim[m1, l1, t1, i1, θ1, n1, j1], BaseDim[m2, l2, t2, i2, θ2, n2, j2]) =>
  BaseDim[
    AddTRat[m1, m2],
    AddTRat[l1, l2],
    AddTRat[t1, t2],
    AddTRat[i1, i2],
    AddTRat[θ1, θ2],
    AddTRat[n1, n2],
    AddTRat[j1, j2]
  ]
}

/**
  * Divide two dimensions by subtracting their corresponding exponents.
  */
type DivDim[D1 <: Dimension, D2 <: Dimension] <: Dimension = (D1, D2) match {
  case (BaseDim[m1, l1, t1, i1, θ1, n1, j1], BaseDim[m2, l2, t2, i2, θ2, n2, j2]) =>
  BaseDim[
    SubTRat[m1, m2],
    SubTRat[l1, l2],
    SubTRat[t1, t2],
    SubTRat[i1, i2],
    SubTRat[θ1, θ2],
    SubTRat[n1, n2],
    SubTRat[j1, j2]
  ]
}

/**
  * Raise a dimension to a power by multiplying all exponents by that power.
  */
type PowDim[D <: Dimension, E <: TRational] <: Dimension = D match {
  case BaseDim[m, l, t, i, θ, n, j] =>
  BaseDim[
    MulTRat[m, E],
    MulTRat[l, E],
    MulTRat[t, E],
    MulTRat[i, E],
    MulTRat[θ, E],
    MulTRat[n, E],
    MulTRat[j, E]
  ]
}