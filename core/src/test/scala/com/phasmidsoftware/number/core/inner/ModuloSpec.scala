/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.inner

import com.phasmidsoftware.number.core.inner.Rational
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModuloSpec extends AnyFlatSpec with Matchers {

  import Modulo.*

  behavior of "Modulo[Int]"

  it should "compute positive modulo correctly" in {
    val m = summon[Modulo[Int]]
    m.mod(7, 3) shouldBe 1
    m.mod(10, 4) shouldBe 2
  }

  it should "compute negative modulo correctly (always non-negative result)" in {
    val m = summon[Modulo[Int]]
    m.mod(-7, 3) shouldBe 2 // -7 mod 3 = 2 (not -1)
    m.mod(-10, 4) shouldBe 2 // -10 mod 4 = 2 (not -2)
  }

  it should "handle exact multiples" in {
    val m = summon[Modulo[Int]]
    m.mod(9, 3) shouldBe 0
    m.mod(-9, 3) shouldBe 0
  }

  behavior of "Modulo[Double]"

  it should "compute positive modulo correctly" in {
    val m = summon[Modulo[Double]]
    m.mod(7.5, 3.0) shouldBe 1.5
  }

  it should "compute negative modulo correctly" in {
    val m = summon[Modulo[Double]]
    m.mod(-7.5, 3.0) shouldBe 1.5
  }

  behavior of "Modulo[Rational]"

  it should "compute positive modulo correctly" in {
    val m = summon[Modulo[Rational]]
    m.mod(Rational(7, 2), Rational(3)) shouldBe Rational(1, 2)
  }

  it should "compute negative modulo correctly" in {
    val m = summon[Modulo[Rational]]
    val result = m.mod(Rational(-7, 2), Rational(3))
    result shouldBe Rational(5, 2) // -3.5 mod 3 = 2.5
  }

  it should "handle exact multiples" in {
    val m = summon[Modulo[Rational]]
    m.mod(Rational(9), Rational(3)) shouldBe Rational(0)
  }

  it should "preserve exactness" in {
    val m = summon[Modulo[Rational]]
    val result = m.mod(Rational(22, 7), Rational(2))
    result.isWhole shouldBe false // Should remain a fraction
  }

  behavior of "normalize with number boundary behavior (default)"

  it should "keep values within range unchanged" in {
    normalize(5, 0, 10, inclusive = false) shouldBe 5
    normalize(0, 0, 10, inclusive = false) shouldBe 0
    normalize(9, 0, 10, inclusive = false) shouldBe 9
  }

  it should "exclude max when inclusive = false" in {
    normalize(10, 0, 10, inclusive = false) shouldBe 0 // max wraps to min
    normalize(15, 0, 10, inclusive = false) shouldBe 5
  }

  it should "include max when inclusive = true" in {
    normalize(10, 0, 10, inclusive = true) shouldBe 10
    normalize(15, 0, 10, inclusive = true) shouldBe 5
  }

  it should "wrap values above max back into range" in {
    normalize(15, 0, 10, inclusive = false) shouldBe 5
    normalize(25, 0, 10, inclusive = false) shouldBe 5
  }

  it should "wrap values below min back into range" in {
    normalize(-5, 0, 10, inclusive = false) shouldBe 5
    normalize(-15, 0, 10, inclusive = false) shouldBe 5
  }

  it should "handle negative ranges" in {
    normalize(5, -10, 0, inclusive = false) shouldBe -5
    normalize(-15, -10, 0, inclusive = false) shouldBe -5
  }

  it should "handle ranges not starting at zero" in {
    normalize(15, 5, 10, inclusive = false) shouldBe 5 // wraps to min
    normalize(2, 5, 10, inclusive = false) shouldBe 7
  }

  it should "prefer min for exact multiples (number behavior)" in {
    // Value = 3, range [-1, 1), size = 2
    // 3 mod 2 = 1, shifted back gives 0, which is min behavior
    normalize(3, -1, 1, inclusive = false) shouldBe -1
  }

  it should "handle min = max" in {
    normalize(5, 10, 10, inclusive = false) shouldBe 10
  }

  it should "handle very large values efficiently" in {
    val large = 1000000000
    val result = normalize(large, 0, 10, inclusive = false)
    result shouldBe 0 // 10^9 mod 10 = 0, stays at min
  }

  behavior of "normalize with angle boundary behavior"

  it should "prefer max for angles (π over -π)" in {
    import Modulo.AngleBoundaryBehavior.given
    // With angle boundary behavior, exact multiples prefer max
    val result = normalize(Rational(3), Rational(-1), Rational(1), inclusive = false)
    result shouldBe Rational(1) // 3π wraps to π (max), not -π (min)
  }

  it should "treat min and max as equivalent for angles" in {
    import Modulo.AngleBoundaryBehavior.given
    // Landing on min (-π) should convert to max (π) for angles
    val result = normalize(Rational(-3), Rational(-1), Rational(1), inclusive = false)
    result shouldBe Rational(1) // -3π wraps to π (not -π, due to circular equivalence)
  }

  it should "handle small angle values" in {
    import Modulo.AngleBoundaryBehavior.given
    val result = normalize(Rational(1, 4), Rational(-1), Rational(1), inclusive = false)
    result shouldBe Rational(1, 4)
  }

  it should "handle angle normalization with inclusive = true" in {
    import Modulo.AngleBoundaryBehavior.given
    // With inclusive = true, both -π and π are valid
    // But due to circular equivalence and preferMax, we still prefer π
    val result = normalize(Rational(3), Rational(-1), Rational(1), inclusive = true)
    result shouldBe Rational(1)
  }

  behavior of "normalize for Rational values"

  it should "preserve exactness for rational values" in {
    import Modulo.AngleBoundaryBehavior.given
    val input = Rational(22, 7)
    val result = normalize(input, Rational(0), Rational(10), inclusive = false)
    result.isWhole shouldBe false
  }

  it should "preserve exactness for wrapped rational values" in {
    import Modulo.AngleBoundaryBehavior.given
    val input = Rational(31, 7)
    val result = normalize(input, Rational(0), Rational(3), inclusive = false)
    result.isWhole shouldBe false
  }

  it should "handle μ₀ value correctly" in {
    import Modulo.AngleBoundaryBehavior.given
    // μ₀ = 4π/10^7 as a multiple of π = 1/2500000
    val mu0 = Rational(1, 2500000)
    val result = normalize(mu0, Rational(-1), Rational(1), inclusive = false)
    result shouldBe mu0 // Should stay unchanged as it's in range
  }

  it should "handle very small ranges" in {
    import Modulo.NumberBoundaryBehavior.given
    val result = normalize(Rational(5, 2), Rational(0), Rational(1, 10), inclusive = false)
    result shouldBe Rational(0) // 2.5 mod 0.1 = 0
  }

  behavior of "BoundaryBehavior typeclass"

  it should "provide number behavior by default" in {
    val bb = summon[BoundaryBehavior[Int]]
    bb.preferMax shouldBe false
    bb.circularEquivalent shouldBe false
  }

  it should "provide angle behavior when explicitly imported" in {
    import Modulo.AngleBoundaryBehavior.given
    val bb = summon[BoundaryBehavior[Rational]]
    bb.preferMax shouldBe true
    bb.circularEquivalent shouldBe true
  }

  behavior of "Modulo operations"

  it should "support plus operation" in {
    val m = summon[Modulo[Int]]
    m.plus(5, 3) shouldBe 8
  }

  it should "support minus operation" in {
    val m = summon[Modulo[Int]]
    m.minus(5, 3) shouldBe 2
  }

  it should "support compare operation" in {
    val m = summon[Modulo[Int]]
    m.compare(5, 3) should be > 0
    m.compare(3, 5) should be < 0
    m.compare(5, 5) shouldBe 0
  }

  it should "provide zero value" in {
    val m = summon[Modulo[Int]]
    m.zero shouldBe 0
  }
}
