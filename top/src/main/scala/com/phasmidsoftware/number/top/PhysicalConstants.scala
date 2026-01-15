package com.phasmidsoftware.number.top

import com.phasmidsoftware.number.algebra.eager.Angle
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.dimensions.core.*
import com.phasmidsoftware.number.dimensions.core.CompositeUnits.{JouleSecond, Kepler}
import com.phasmidsoftware.number.expression.expr.∅

/**
  * The `PhysicalConstants` object serves as a collection of fundamental physical constants,
  * including both exact (defined by the SI system) and measured values (with uncertainties).
  * Each constant is encapsulated as a `Quantity` with its respective dimension and unit.
  *
  * These constants are essential for a wide range of scientific and engineering calculations
  * in disciplines such as electromagnetism, thermodynamics, quantum mechanics, and relativity.
  */
object PhysicalConstants {

  // ============================================================================
  // EXACT Constants (by SI definition as of 2019 revision)
  // ============================================================================

  /**
    * Represents the speed of light in vacuum, defined as an immutable quantity
    * with a value of 299,792,458 meters per second.
    *
    * This value is a physical constant used in various scientific calculations,
    * including those related to relativity, electromagnetism, and quantum mechanics.
    *
    * Defined in the context of the `PhysicalConstants` class, this quantity is of type
    * `Quantity[Velocity]`, where `Velocity` is a dimension representing speed.
    *
    * The unit of measurement is `MetersPerSecond`, which precisely correlates
    * to the SI standard for velocity.
    */
  val c: Quantity[Velocity] = Quantity(299792458, MetersPerSecond)

  /**
    * Represents the Planck constant, a fundamental physical constant used in quantum mechanics.
    * Defined as a measurable quantity with a value of 6.62607015 × 10⁻³⁴ Joule-seconds (J·s).
    *
    * This constant is expressed as a `Quantity` with the dimension of energy multiplied by time (`EnergyTime`).
    * It is utilized in calculations involving energy, frequency, and photon behavior.
    */
  lazy val h: Quantity[EnergyTime] = Quantity("6.6260701500E-34", JouleSecond) // NOTE trailing "00" is to force exactitude

  /** Reduced Planck constant: ℏ = h/(2π) (exact, derived from h) */
  lazy val HBar: Quantity[EnergyTime] = h / Quantity(Angle.twoPi)

  /**
    * Represents the elementary charge, a physical constant that quantifies the charge
    * of a single proton or the magnitude of the charge of a single electron.
    *
    * This constant is expressed as a `Quantity` with a value of "1.602176634E-19"
    * and is measured in the SI unit of charge, the Coulomb (C).
    *
    * The elementary charge is a fundamental parameter in physics and is integral
    * to the study of electromagnetism, quantum mechanics, and other physical theories.
    */
  lazy val e = Quantity("1.60217663400E-19", Coulomb) // NOTE trailing "00" is to force exactitude

  /**
    * The Boltzmann constant, denoted as `k`, is a physical constant connecting the average kinetic energy
    * of particles in a gas with the temperature of the gas. Its value is exactly 1.380649 × 10⁻²³ J/K.
    *
    * @note The constant is represented as a `Quantity` with the unit of energy per temperature (Joule/Kelvin).
    */
  lazy val k: Quantity[EnergyPerTemperature] = Quantity("1.38064900E-23", Joule / Kelvin) // NOTE trailing "00" is to force exactitude

  /**
    * Represents the Avogadro constant (N_A), which is the proportionality factor that
    * relates the number of constituent particles (usually atoms or molecules) in a
    * sample to the amount of substance in moles.
    *
    * Its value is defined as 6.02214076 × 10^23 (exactly), and its unit is the reciprocal of the mole.
    *
    * Defined as a quantity with the dimensionless unit divided by the mole (Dimensionless / Mole).
    *
    * @see Quantity
    * @see Dimensionless
    * @see Mole
    */
  lazy val N_A: Quantity[DivDim[Dimensionless, Amount]] =
    Quantity("6.0221407600E23", Dimensionless / Mole) // NOTE trailing "00" is to force exactitude

  // ============================================================================
  // MEASURED Constants (with uncertainty)
  // ============================================================================

  /** Gravitational constant: G = 6.67430(15) × 10⁻¹¹ m³/(kg·s²)
    * Relative uncertainty: 2.2 × 10⁻⁵
    * One of the least precisely known constants!
    */
  lazy val G: Quantity[DivDim[DivDim[Volume, Mass], PowDim[Time, Two]]] =
    Quantity("6.67430(15)E-11", Kepler)

  /** Fine structure constant: α = 7.2973525693(11) × 10⁻³ (dimensionless)
    * Relative uncertainty: 1.5 × 10⁻¹⁰
    */
  lazy val alpha =
    Quantity("7.2973525693(11)e-3", Dimensionless)

  /** Rydberg constant: R∞ = 10973731.568160(21) m⁻¹
    * Relative uncertainty: 1.9 × 10⁻¹²
    */
  lazy val R_inf: Quantity[PowDim[Length, MinusOne]] =
    Quantity("10973731.568160(21)", Meter.invert)

  /** Electron mass: mₑ = 9.1093837015(28) × 10⁻³¹ kg
    * Relative uncertainty: 3.0 × 10⁻¹⁰
    */
  lazy val m_e =
    Quantity("9.1093837015(28)e-31", Kilogram)

  /** Proton mass: mₚ = 1.67262192369(51) × 10⁻²⁷ kg
    * Relative uncertainty: 3.1 × 10⁻¹⁰
    */
  lazy val m_p =
    Quantity("1.67262192369(51)E-27", Kilogram)

  /** Vacuum permittivity: ε₀ = 8.8541878128(13) × 10⁻¹² F/m
    * Derived from: ε₀ = 1/(μ₀c²)
    * Relative uncertainty: 1.5 × 10⁻¹⁰
    */
  lazy val epsilon_0: Quantity[Permittivity] =
    Quantity("8.8541878128(13)E-12", Farad / Meter)

  /** Vacuum permeability: μ₀ = 1.25663706212(19) × 10⁻⁶ H/m
    * Previously exact (4π × 10⁻⁷), now measured
    * Relative uncertainty: 1.5 × 10⁻¹⁰
    */
  lazy val mu_0: Quantity[Permeability] =
    Quantity("1.25663706212(19)E-6", Henry / Meter)

  /** Stefan-Boltzmann constant: σ = 5.670374419... × 10⁻⁸ W/(m²·K⁴)
    * Derived from: σ = (2π⁵k⁴)/(15h³c²)
    * Exact in SI 2019 (derived from exact constants)
    * NOTE at present our library does not allow us to express π⁵ as an exact number.
    */
  lazy val sigma =
    Quantity("5.670374419184429453970996731889230876059E-8", Watt / (Meter.squared * Kelvin.squared.squared))

  /** Gas constant: R = k·Nₐ
    * Derived from: R = k·Nₐ
    * Exact (product of exact constants)
    */
  lazy val R: Quantity[DivDim[EnergyPerTemperature, Amount]] = k * N_A
}

object PhysicalConstantsConventional {

  import com.phasmidsoftware.number.algebra.eager.Angle

  // ============================================================================
  // Pre-2019 Exact Values (for reference/compatibility)
  // ============================================================================

  /** Conventional value of μ₀ (pre-2019): exactly 4π × 10⁻⁷ H/m
    *
    * This was the exact defined value before the 2019 SI redefinition.
    * Use this if you need exact rational arithmetic or are working with
    * pre-2019 calculations.
    */
  lazy val mu_0 = Quantity(∅ * Angle(4) * Rational(10000000).invert, Henry / Meter)

  /** Conventional value of ε₀ (pre-2019): derived exactly from μ₀ and c
    *
    * ε₀ = 1/(μ₀c²) = 1/(4π × 10⁻⁷ × (299792458)²)
    * ≈ 8.8541878176... × 10⁻¹² F/m (exact with exact μ₀)
    */
  lazy val epsilon_0: Quantity[DivDim[Dimensionless, MulDim[MulDim[Velocity, Velocity], DivDim[Inductance, Length]]]] =
    (PhysicalConstants.c.squared * mu_0).inverted
}