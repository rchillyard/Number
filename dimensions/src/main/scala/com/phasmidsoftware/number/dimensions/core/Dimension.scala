package com.phasmidsoftware.number.dimensions.core

import com.phasmidsoftware.number.core.inner.Rational

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

/**
  * Represents a dimensionless quantity, defined as a special case of the `BaseDim` type
  * where all seven SI base unit exponents (Mass, Length, Time, Electric current,
  * Thermodynamic temperature, Amount of substance, and Luminous intensity) are zero.
  */
type Dimensionless = BaseDim[Zero, Zero, Zero, Zero, Zero, Zero, Zero]
/**
  * Represents the physical dimension of mass in the SI base unit system.
  * It is defined as a specialization of `BaseDim` with the exponent `1` for
  * mass (kilograms) and `0` for all other base units.
  */
type Mass = BaseDim[One, Zero, Zero, Zero, Zero, Zero, Zero]
/**
  * Represents the dimension of length in terms of the seven SI base units.
  *
  * This type is expressed using the `BaseDim` seven-tuple type of rational exponents,
  * where the tuple is specified as:
  * - M: Zero (mass)
  * - L: One (length)
  * - T: Zero (time)
  * - I: Zero (electric current)
  * - Θ: Zero (thermodynamic temperature)
  * - N: Zero (amount of substance)
  * - J: Zero (luminous intensity)
  *
  * The `Length` type specifically corresponds to a dimensional quantity with a unit
  * of meters in the SI system.
  */
type Length = BaseDim[Zero, One, Zero, Zero, Zero, Zero, Zero]
/**
  * Alias for a dimension representing time in the SI unit system.
  *
  * Time is one of the seven base physical quantities in the International System of Units (SI).
  * It is represented as a `BaseDim` with a numerator of `1` for the time (T) component and `0` for all other components.
  */
type Time = BaseDim[Zero, Zero, One, Zero, Zero, Zero, Zero]
/**
  * Represents the dimension of electric current in terms of the seven SI base units.
  *
  * This type alias defines the dimensional exponents for electric current, where only
  * the exponent for the electric current (I) is non-zero (set to `One`), and the exponents
  * for all other base units (M, L, T, Θ, N, J) are zero (`Zero`).
  *
  * - M: 0 (Mass)
  * - L: 0 (Length)
  * - T: 0 (Time)
  * - I: 1 (Electric current)
  * - Θ: 0 (Thermodynamic temperature)
  * - N: 0 (Amount of substance)
  * - J: 0 (Luminous intensity)
  */
type Current = BaseDim[Zero, Zero, Zero, One, Zero, Zero, Zero]
/**
  * Type alias representing the dimension of thermodynamic temperature in the SI base unit system.
  *
  * This type is a specialization of `BaseDim` with the following exponents:
  * - M (Mass): 0
  * - L (Length): 0
  * - T (Time): 0
  * - I (Electric current): 0
  * - Θ (Thermodynamic temperature): 1
  * - N (Amount of substance): 0
  * - J (Luminous intensity): 0
  *
  * It corresponds to the SI base unit of temperature, measured in kelvins (K).
  */
type Temperature = BaseDim[Zero, Zero, Zero, Zero, One, Zero, Zero]
/**
  * Represents the dimension for the amount of substance in the SI base units.
  *
  * Defined as a specialization of `BaseDim` where the sixth component (N: Amount of substance)
  * is one, and all other components are zero. This corresponds to the mole (the SI unit for
  * the amount of substance).
  */
type Amount = BaseDim[Zero, Zero, Zero, Zero, Zero, One, Zero]
/**
  * Represents the SI base dimension of luminous intensity, measured in candelas.
  *
  * This type is defined as a specialization of the `BaseDim` type with the luminous
  * intensity component (`J`) set to one, and all other base dimensions set to zero.
  */
type LuminousIntensity = BaseDim[Zero, Zero, Zero, Zero, Zero, Zero, One]

/**
  * Special dimension for angles (dimensionless but semantically distinct)
  */
type Angle = BaseDim[Zero, Zero, Zero, Zero, Zero, Zero, Zero]

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

type MagneticFlux = MulDim[Voltage, Time]

type MagneticFluxDensity = DivDim[MagneticFlux, Area]

type Inductance = DivDim[MagneticFlux, Current]

type Permeability = DivDim[Inductance, Length]

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

// In Dimension.scala - add runtime witness

trait DimensionWitness {
  def toCompositeSymbol: String
}

case class BaseDimWitness(
                           m: Rational, // Mass
                           l: Rational, // Length
                           t: Rational, // Time
                           i: Rational, // Current
                           θ: Rational, // Temperature
                           n: Rational, // Amount
                           j: Rational // Luminosity
                         ) extends DimensionWitness {

  def *(other: BaseDimWitness): BaseDimWitness = BaseDimWitness(
    m + other.m,
    l + other.l,
    t + other.t,
    i + other.i,
    θ + other.θ,
    n + other.n,
    j + other.j
  )

  def /(other: BaseDimWitness): BaseDimWitness = BaseDimWitness(
    m - other.m,
    l - other.l,
    t - other.t,
    i - other.i,
    θ - other.θ,
    n - other.n,
    j - other.j
  )

  def pow(exp: Rational): BaseDimWitness = BaseDimWitness(
    m * exp,
    l * exp,
    t * exp,
    i * exp,
    θ * exp,
    n * exp,
    j * exp
  )

  def toCompositeSymbol: String = {
    val baseUnits = Seq(
      ("kg", m),
      ("m", l),
      ("s", t),
      ("A", i),
      ("K", θ),
      ("mol", n),
      ("cd", j)
    ).filter(_._2 != Rational.zero)

    if (baseUnits.isEmpty) return "1" // Dimensionless

    val (positive, negative) = baseUnits.partition(_._2 > Rational.zero)

    val posStr = positive.map { case (sym, exp) => formatUnit(sym, exp) }.mkString("·")
    val negStr = negative.map { case (sym, exp) => formatUnit(sym, -exp) }.mkString("·")

    (posStr, negStr) match {
      case (pos, "") => pos
      case ("", neg) => s"1/$neg"
      case (pos, neg) => s"$pos/$neg"
    }
  }

  private def formatUnit(sym: String, exp: Rational): String = {
    exp match {
      case r if r == Rational.one => sym
      case r if r == Rational(2) => s"$sym²"
      case r if r == Rational(3) => s"$sym³"
      case r if r.d == 1 => s"$sym^${r.n}"
      case r => s"$sym^(${r.n}/${r.d})"
    }
  }
}

object DimensionWitness {
  // Helper to create witnesses for base dimensions
  val dimensionless = BaseDimWitness(0, 0, 0, 0, 0, 0, 0)
  val mass = BaseDimWitness(1, 0, 0, 0, 0, 0, 0)
  val length = BaseDimWitness(0, 1, 0, 0, 0, 0, 0)
  val time = BaseDimWitness(0, 0, 1, 0, 0, 0, 0)
  val current = BaseDimWitness(0, 0, 0, 1, 0, 0, 0)
  val temperature = BaseDimWitness(0, 0, 0, 0, 1, 0, 0)
  val amount = BaseDimWitness(0, 0, 0, 0, 0, 1, 0)
  val luminosity = BaseDimWitness(0, 0, 0, 0, 0, 0, 1)

  // Derived dimensions
  val area: BaseDimWitness = length * length
  val volume: BaseDimWitness = area * length
  val velocity: BaseDimWitness = length / time
  val acceleration: BaseDimWitness = velocity / time
  val force: BaseDimWitness = acceleration * mass
  val energy: BaseDimWitness = force * length
  val power: BaseDimWitness = energy / time
  val pressure: BaseDimWitness = force / area
  val charge: BaseDimWitness = current * time
  val voltage: BaseDimWitness = power / current
  val resistance: BaseDimWitness = voltage / current
  val magneticFlux: BaseDimWitness = voltage * time
  val magneticFluxDensity: BaseDimWitness = magneticFlux / area
  val frequency: BaseDimWitness = dimensionless / time
  val inductance: BaseDimWitness = magneticFlux / area
  val permeability: BaseDimWitness = inductance / length
}