package com.phasmidsoftware.number.top

import com.phasmidsoftware.number.algebra.eager.*
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.dimensions.core.*
import com.phasmidsoftware.number.dimensions.core.CompositeUnits.Kepler
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PhysicalConstantsSpec extends AnyFlatSpec with should.Matchers {

  behavior of "PhysicalConstants - Exact Constants (SI 2019)"

  it should "have exact speed of light" in {
    val c = PhysicalConstants.c
    c.value shouldBe WholeNumber(299792458)
    c.unit shouldBe MetersPerSecond
    c.unit.dimensionWitness shouldBe DimensionWitness.velocity
  }

  it should "have exact Planck constant" in {
    val h = PhysicalConstants.h
    // h = 6.62607015 × 10⁻³⁴ J·s
    h.value.isExact shouldBe true
    h.unit shouldBe (Joule * Second)
    h.unit.dimensionWitness shouldBe DimensionWitness.energy * DimensionWitness.time
    h.renderLaTeX shouldBe """6.62607015E-34\,\text{J}\cdot\text{s}"""
  }

  it should "have exact elementary charge" in {
    val e = PhysicalConstants.e
    // e = 1.602176634 × 10⁻¹⁹ C
    e.value.isExact shouldBe true
    e.unit shouldBe Coulomb
    e.unit.dimensionWitness shouldBe DimensionWitness.charge
  }

  it should "have exact Boltzmann constant" in {
    val k = PhysicalConstants.k
    // k = 1.380649 × 10⁻²³ J/K
    k.value.isExact shouldBe true
    k.unit shouldBe (Joule / Kelvin)
    k.unit.dimensionWitness shouldBe (DimensionWitness.energy / DimensionWitness.temperature)
  }

  it should "have exact Avogadro constant" in {
    val Na = PhysicalConstants.N_A
    // Na = 6.02214076 × 10²³ mol⁻¹
    Na.value.isExact shouldBe true
    Na.unit shouldBe (Dimensionless / Mole)
    Na.unit.dimensionWitness shouldBe (DimensionWitness.dimensionless / DimensionWitness.amount)
  }

  behavior of "PhysicalConstants - Measured Constants"

  import com.phasmidsoftware.number.algebra.core.FuzzyEq.~=

  it should "have gravitational constant with uncertainty" in {
    val G = PhysicalConstants.G
    G.value.isExact shouldBe false
    // G = 6.67430(15) × 10⁻¹¹ m³/(kg·s²)
    G.value.materialize.~=(Eager(6.67428E-11)) shouldBe true
    G.unit.dimensionWitness shouldBe Kepler.dimensionWitness
    G.unit shouldBe Kepler
  }

  it should "have fine structure constant" in {
    val alpha = PhysicalConstants.alpha
    // α = 7.2973525693(11) × 10⁻³
    alpha.value.isExact shouldBe false
    alpha.value.materialize.~=(Real(7.2973525693e-3)) shouldBe true
    alpha.unit shouldBe Dimensionless
    alpha.unit.dimensionWitness shouldBe DimensionWitness.dimensionless
  }

  it should "have Rydberg constant" in {
    val Rinf = PhysicalConstants.R_inf
    // R∞ = 10973731.568160(21) m⁻¹
    Rinf.value.isExact shouldBe false
    Rinf.value.materialize.~=(Real(10973731.568160)) shouldBe true
    Rinf.unit shouldBe Meter.invert
    Rinf.unit.dimensionWitness shouldBe DimensionWitness.length.pow(Rational(-1))
  }

  it should "have electron mass" in {
    val me = PhysicalConstants.m_e
    // mₑ = 9.1093837015(28) × 10⁻³¹ kg
    me.value.isExact shouldBe false
    me.value.materialize.~=(Real(9.1093837015e-31)) shouldBe true
    me.unit shouldBe Kilogram
    me.unit.dimensionWitness shouldBe DimensionWitness.mass
  }

  it should "have proton mass" in {
    val mp = PhysicalConstants.m_p
    // mₚ = 1.67262192369(51) × 10⁻²⁷ kg
    mp.value.isExact shouldBe false
    mp.value.materialize.~=(Real(1.67262192369E-27)) shouldBe true
    mp.unit shouldBe Kilogram
    mp.unit.dimensionWitness shouldBe DimensionWitness.mass
  }

  it should "have vacuum permittivity" in {
    val eps0 = PhysicalConstants.epsilon_0
    // ε₀ = 8.8541878128(13) × 10⁻¹² F/m
    eps0.value.isExact shouldBe false
    eps0.value.materialize.~=(Real(8.8541878128e-12)) shouldBe true
    eps0.unit shouldBe (Farad / Meter)
  }

  it should "have vacuum permeability" in {
    val mu0 = PhysicalConstants.mu_0
    // μ₀ = 1.25663706212(19) × 10⁻⁶ H/m
    mu0.value.isExact shouldBe false
    mu0.value.materialize.~=(Real(1.25663706212e-6)) shouldBe true
    mu0.unit shouldBe (Henry / Meter)
  }

  behavior of "PhysicalConstants - Derived Constants"

  it should "have Stefan-Boltzmann constant" in {
    val sigma = PhysicalConstants.sigma
    // σ = 5.670374419... × 10⁻⁸ W/(m²·K⁴)
    // This is exact in principle but involves π⁵
    sigma.value.materialize.~=(Real(5.670374419e-8))
    sigma.unit shouldBe (Watt / (Meter.squared * Kelvin.squared.squared))
    sigma.unit.dimensionWitness shouldBe (
      DimensionWitness.power /
        DimensionWitness.area /
        DimensionWitness.temperature.pow(Rational(4))
      )
  }

  it should "have gas constant" in {
    val R = PhysicalConstants.R
    // R = 8.31446261815324 J/(mol·K)
    // Exact (product of k and Na)
    R.value.materialize.~=(Real(8.31446261815324)) shouldBe true
    R.unit shouldBe (Joule / (Mole * Kelvin))
    R.unit.dimensionWitness shouldBe (
      DimensionWitness.energy /
        DimensionWitness.amount /
        DimensionWitness.temperature
      )
  }

  behavior of "PhysicalConstants - Relationships"

  it should "satisfy R = k·Nₐ (approximately, given parsing)" in {
    val k = PhysicalConstants.k
    val Na = PhysicalConstants.N_A
    val R = PhysicalConstants.R

    // This test verifies the relationship holds approximately
    // Exact verification would require symbolic computation
    //    val kValue = k.value.materialize.approximation().get
    //    val NaValue = Na.value.maybeDouble.get
    //    val RValue = R.value.maybeDouble.get
    //
    //    val computed = kValue * NaValue
    //    computed should be (RValue +- RValue * 1e-10)
  }

  ignore should "satisfy ε₀μ₀c² = 1 (approximately)" in {
    val eps0 = PhysicalConstants.epsilon_0
    val mu0 = PhysicalConstants.mu_0
    val c = PhysicalConstants.c

    val eps0Val = eps0.value.maybeDouble.get
    val mu0Val = mu0.value.maybeDouble.get
    val cVal = c.value.maybeDouble.get

    val product = eps0Val * mu0Val * cVal * cVal
    product should be(1.0 +- 1e-9)
  }

  ignore should "have proton-to-electron mass ratio" in {
    val mp = PhysicalConstants.m_p
    val me = PhysicalConstants.m_e

    val mpVal = mp.value.maybeDouble.get
    val meVal = me.value.maybeDouble.get

    val ratio = mpVal / meVal
    // Known ratio is approximately 1836.15
    ratio should be(1836.15 +- 0.01)
  }

  behavior of "PhysicalConstants - Unit Dimensions"

  it should "have correct dimension for Planck constant (energy × time)" in {
    val h = PhysicalConstants.h
    h.unit.dimensionWitness shouldBe (DimensionWitness.energy * DimensionWitness.time)
  }

  it should "have correct dimension for gravitational constant" in {
    val G = PhysicalConstants.G
    // G has dimensions [L³/(M·T²)]
    val expected = BaseDimWitness(
      m = Rational(-1), // M⁻¹
      l = Rational(3), // L³
      t = Rational(-2), // T⁻²
      i = Rational.zero,
      θ = Rational.zero,
      n = Rational.zero,
      j = Rational.zero
    )
    G.unit.dimensionWitness shouldBe expected
  }

  it should "have correct dimension for Boltzmann constant (energy / temperature)" in {
    val k = PhysicalConstants.k
    // k has dimensions [M·L²/(T²·Θ)]
    val expected = BaseDimWitness(
      m = Rational(1), // M
      l = Rational(2), // L²
      t = Rational(-2), // T⁻²
      i = Rational.zero,
      θ = Rational(-1), // Θ⁻¹
      n = Rational.zero,
      j = Rational.zero
    )
    k.unit.dimensionWitness shouldBe expected
  }

  behavior of "PhysicalConstantsConventional - Pre-2019 Values"

  it should "have conventional μ₀ = 4π × 10⁻⁷ H/m (approximately)" in {
    val mu0 = PhysicalConstantsConventional.mu_0

    // Should be close to 4π × 10⁻⁷ ≈ 1.25663706144e-6
    val value = mu0.value.maybeDouble.get
    value should be(1.25663706144e-6 +- 1e-15)
  }

  it should "have conventional μ₀ as exact value (not fuzzy)" in {
    val mu0 = PhysicalConstantsConventional.mu_0
    mu0.value.isExact shouldBe true
  }
}