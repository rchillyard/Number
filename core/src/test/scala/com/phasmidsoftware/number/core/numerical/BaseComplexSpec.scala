/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.core.numerical

import com.phasmidsoftware.number.core.inner.{NatLog, Radian, Rational}
import com.phasmidsoftware.number.core.numerical.Number.negate
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BaseComplexSpec extends AnyFlatSpec with Matchers {

  behavior of "BaseComplex trigonometric functions"

  // Helper method to check approximate equality for complex numbers
  def approxEqual(a: Field, b: Field, tolerance: Double = 1e-10): Boolean = {
    val diff = a.asComplex `add` (b.asComplex.unary_-)
    diff.asComplex.modulus.toNominalDouble.exists(d => Math.abs(d) < tolerance)
  }

  // Test sin function
  it should "compute sin(0) = 0" in {
    val z = ComplexCartesian(Number.zero, Number.zero)
    val result = z.sin
    result.asComplex.modulus.toNominalDouble.get should be < 1e-10
  }

  it should "compute sin(π/2) = 1" in {
    val z = ComplexCartesian(Number.pi `doMultiply` Number(Rational.half), Number.zero)
    val result = z.sin
    result match {
      case Real(x) => x.toNominalDouble.get should be(1.0 +- 1e-10)
      case c: Complex =>
        // Should be close to 1 + 0i
        val cartesian = Complex.convertToCartesian(c)
        cartesian.real.toNominalDouble.get should be(1.0 +- 1e-10)
        cartesian.imag.toNominalDouble.get should be < 1e-10
    }
  }

  it should "compute sin(π) ≈ 0" in {
    val z = ComplexCartesian(Number.pi, Number.zero)
    val result = z.sin
    result.asComplex.modulus.toNominalDouble.get should be < 1e-10
  }

  it should "compute sin(i) = i·sinh(1)" in {
    val z = ComplexCartesian(Number.zero, Number.one)
    val result = z.sin
    // sin(i) = i·sinh(1) ≈ i·1.175201...
    result match {
      case c: Complex =>
        val cartesian = Complex.convertToCartesian(c)
        cartesian.real.toNominalDouble.get should be < 1e-10
        Math.abs(cartesian.imag.toNominalDouble.get) should be(1.1752 +- 0.001)
    }
  }

  it should "handle this simple test" in {
    val z = ComplexCartesian(Number.zero, Number.one) // just i
    val angle = z.imag.make(Radian)
    val result = z.exp // should be e^(i) = cos(1) + i*sin(1)
    result match {
      case c: Complex =>
        val cartesian = Complex.convertToCartesian(c)
        cartesian.real.toNominalDouble.get should be(Math.cos(1) +- 0.001)
        cartesian.imag.toNominalDouble.get should be(Math.sin(1) +- 0.001)
    }
  }

  it should "compute sin of complex number z = 1 + i" in {
    val z = ComplexCartesian(Number.one, Number.one)
    val result = z.sin
    // sin(1+i) ≈ 1.2985 + 0.6350i
    result match {
      case c: Complex =>
        val cartesian = Complex.convertToCartesian(c)
        cartesian.real.toNominalDouble.get should be(1.2985 +- 0.001)
        cartesian.imag.toNominalDouble.get should be(0.6350 +- 0.001)
    }
  }

  // Test cos function
  it should "compute cos(0) = 1" in {
    val z = ComplexCartesian(Number.zero, Number.zero)
    val result = z.cos
    result match {
      case Real(x) => x.toNominalDouble.get should be(1.0 +- 1e-10)
      case c: Complex =>
        val cartesian = Complex.convertToCartesian(c)
        cartesian.real.toNominalDouble.get should be(1.0 +- 1e-10)
        cartesian.imag.toNominalDouble.get should be < 1e-10
    }
  }

  it should "compute cos(π/2) ≈ 0" in {
    val z = ComplexCartesian(Number.pi `doMultiply` Number(Rational.half), Number.zero)
    val result = z.cos
    result.asComplex.modulus.toNominalDouble.get should be < 1e-10
  }

  it should "compute cos(π) = -1" in {
    val z = ComplexCartesian(Number.pi, Number.zero)
    val result = z.cos
    result match {
      case Real(x) => x.toNominalDouble.get should be(-1.0 +- 1e-10)
      case c: Complex =>
        val cartesian = Complex.convertToCartesian(c)
        cartesian.real.toNominalDouble.get should be(-1.0 +- 1e-10)
        cartesian.imag.toNominalDouble.get should be < 1e-10
    }
  }

  it should "compute cos(i) = cosh(1)" in {
    val z = ComplexCartesian(Number.zero, Number.one)
    val result = z.cos
    // cos(i) = cosh(1) ≈ 1.543080...
    result match {
      case Real(x) => x.toNominalDouble.get should be(1.5431 +- 0.001)
      case c: Complex =>
        val cartesian = Complex.convertToCartesian(c)
        cartesian.real.toNominalDouble.get should be(1.5431 +- 0.001)
        cartesian.imag.toNominalDouble.get should be < 1e-10
    }
  }

  it should "compute cos of complex number z = 1 + i" in {
    val z = ComplexCartesian(Number.one, Number.one)
    val result = z.cos
    // cos(1+i) ≈ 0.8337 - 0.9889i
    result match {
      case c: Complex =>
        val cartesian = Complex.convertToCartesian(c)
        cartesian.real.toNominalDouble.get should be(0.8337 +- 0.001)
        cartesian.imag.toNominalDouble.get should be(-0.9889 +- 0.001)
    }
  }

  // Test tan function
  it should "compute tan(0) = 0" in {
    val z = ComplexCartesian(Number.zero, Number.zero)
    val result = z.tan
    result.asComplex.modulus.toNominalDouble.get should be < 1e-10
  }

  it should "compute tan(π/4) ≈ 1" in {
    val z = ComplexCartesian(Number.pi `doMultiply` Number(Rational(1, 4)), Number.zero)
    val result = z.tan
    result match {
      case Real(x) => x.toNominalDouble.get should be(1.0 +- 1e-10)
      case c: Complex =>
        val cartesian = Complex.convertToCartesian(c)
        cartesian.real.toNominalDouble.get should be(1.0 +- 1e-10)
        cartesian.imag.toNominalDouble.get should be < 1e-10
    }
  }

  it should "satisfy trigonometric identity sin²(z) + cos²(z) = 1" in {
    val z = ComplexCartesian(Number(Rational(1, 2)), Number(Rational(3, 4)))
    val sinZ = z.sin.asComplex
    val cosZ = z.cos.asComplex
    val sin2 = sinZ `multiply` sinZ
    val cos2 = cosZ `multiply` cosZ
    val sum = sin2 `add` cos2

    // The sum should be very close to 1
    sum match {
      case Real(x) => x.toNominalDouble.get should be(1.0 +- 1e-8)
      case c: Complex =>
        val cartesian = Complex.convertToCartesian(c)
        cartesian.real.toNominalDouble.get should be(1.0 +- 1e-8)
        cartesian.imag.toNominalDouble.get should be < 1e-8
    }
  }

  it should "satisfy tan(z) = sin(z) / cos(z)" in {
    val z = ComplexCartesian(Number(Rational(1, 3)), Number(Rational(2, 5)))
    val tanZ = z.tan
    val sinZ = z.sin
    val cosZ = z.cos
    val sinOverCos = sinZ `divide` cosZ

    // tan(z) should equal sin(z)/cos(z)
    val diff = tanZ.asComplex `add` (sinOverCos.asComplex.unary_-)
    diff.asComplex.modulus.toNominalDouble.get should be < 1e-8
  }

  // Test atan function (atan2 variant)
  it should "compute atan2(1, 1) = π/4" in {
    val x = ComplexCartesian(Number.one, Number.zero)
    val y = Real(Number.one)
    val result = x.atan(y)
    result match {
      case Real(angle) =>
        angle.factor shouldBe Radian
        angle.toNominalDouble.get should be(0.25 +- 1e-10)
    }
  }

  it should "compute atan2(1, 0) = π/2" in {
    val x = ComplexCartesian(Number.zero, Number.zero)
    val y = Real(Number.one)
    val result = x.atan(y)
    result match {
      case Real(angle) =>
        angle.toNominalDouble.get should be(0.5 +- 1e-10)
    }
  }

  it should "compute atan2(0, 1) = 0" in {
    val x = ComplexCartesian(Number.one, Number.zero)
    val y = Real(Number.zero)
    val result = x.atan(y)
    result match {
      case Real(angle) =>
        angle.toNominalDouble.get should be < 1e-10
    }
  }

  it should "compute atan2(-1, -1) = -3π/4" in {
    val x = ComplexCartesian(Number.negOne, Number.zero)
    val y = Real(Number.negOne)
    val result = x.atan(y)
    result match {
      case Real(angle) =>
        angle.toNominalDouble.get should be(-3.0 / 4.0 +- 1e-10)
    }
  }

  // Test exp function
  it should "compute exp(0) = 1" in {
    val z = ComplexCartesian(Number.zero, Number.zero)
    val result = z.exp
    result match {
      case Real(x) => x.toNominalDouble.get should be(1.0 +- 1e-10)
      case c: Complex =>
        c.modulus.toNominalDouble.get should be(1.0 +- 1e-10)
        Complex.convertToCartesian(c).real.toNominalDouble.get should be(1.0 +- 1e-10)
    }
  }

  it should "compute exp(1) = e" in {
    val z = ComplexCartesian(Number.one, Number.zero)
    val result = z.exp
    result match {
      case Real(x) =>
        x.toNominalDouble.get should be(1.0 +- 1e-10)
        x.factor should be(NatLog)
    }
  }

  it should "compute exp(iπ) = -1 (Euler's identity)" in {
    val z = ComplexCartesian(Number.zero, Number.pi)
    val result = z.exp
    // exp(iπ) = -1
    result match {
      case Real(x) => x.toNominalDouble.get should be(-1.0 +- 1e-10)
      case c: Complex =>
        val cartesian = Complex.convertToCartesian(c)
        cartesian.real.toNominalDouble.get should be(-1.0 +- 1e-10)
        cartesian.imag.toNominalDouble.get should be < 1e-10
    }
  }

  it should "compute exp(iπ/2) = i" in {
    val z = ComplexCartesian(Number.zero, Number.pi `doMultiply` Number(Rational.half))
    val result = z.exp
    // exp(iπ/2) = i
    result match {
      case c: Complex =>
        val cartesian = Complex.convertToCartesian(c)
        cartesian.real.toNominalDouble.get should be < 1e-10
        cartesian.imag.toNominalDouble.get should be(1.0 +- 1e-10)
    }
  }

  it should "compute exp(1 + i)" in {
    val z = ComplexCartesian(Number.one, Number.one)
    val result = z.exp
    // exp(1+i) = e·exp(i) = e·(cos(1) + i·sin(1)) ≈ 1.4687 + 2.2874i
    result match {
      case c: Complex =>
        val cartesian = Complex.convertToCartesian(c)
        cartesian.real.toNominalDouble.get should be(1.4687 +- 0.001)
        cartesian.imag.toNominalDouble.get should be(2.2874 +- 0.001)
    }
  }

  it should "satisfy exp(z1 + z2) = exp(z1) · exp(z2)" in {
    val z1 = ComplexCartesian(Number(Rational(1, 2)), Number(Rational(1, 3)))
    val z2 = ComplexCartesian(Number(Rational(2, 3)), Number(Rational(1, 4)))

    val expZ1 = z1.exp
    println(s"expZ1 = $expZ1")
    val expZ2 = z2.exp
    println(s"expZ2 = $expZ2")
    val expZ1Z2 = expZ1 `multiply` expZ2
    println(s"expZ1Z2 = $expZ1Z2")
    val expSum = (z1 `add` z2).asComplex.exp
    println(s"expSum = $expSum")
    val expProduct = expZ1.asComplex `multiply` expZ2
    println(s"expProduct = $expProduct")

    val diff = expSum.asComplex `add` (expProduct.asComplex.unary_-)
    diff.asComplex.modulus.toNominalDouble.get should be < 1e-8
  }

  behavior of "BaseComplex basic operations"

  it should "add two complex numbers correctly" in {
    val z1 = ComplexCartesian(Number(3), Number(4))
    val z2 = ComplexCartesian(Number(1), Number(2))
    val result = z1 `add` z2
    result match {
      case ComplexCartesian(x, y) =>
        x.toNominalDouble.get should be(4.0)
        y.toNominalDouble.get should be(6.0)
    }
  }

  it should "multiply two complex numbers correctly" in {
    val z1 = ComplexCartesian(Number(2), Number(3))
    val z2 = ComplexCartesian(Number(4), Number(5))
    // (2+3i)(4+5i) = 8 + 10i + 12i + 15i² = 8 + 22i - 15 = -7 + 22i
    val result = z1 `multiply` z2
    result match {
      case ComplexCartesian(x, y) =>
        x.toNominalDouble.get should be(-7.0)
        y.toNominalDouble.get should be(22.0)
    }
  }

  it should "compute modulus of complex number" in {
    val z = ComplexCartesian(Number(3), Number(4))
    val mod = z.modulus
    mod.toNominalDouble.get should be(5.0 +- 1e-10)
  }
  it should "compute abs of complex number" in {
    val z = ComplexCartesian(Number(3), Number(4))
    val mod = z.abs
    mod.asNumber.flatMap(_.toNominalDouble).get should be(5.0 +- 1e-10)
  }

  it should "compute conjugate correctly" in {
    val z = ComplexCartesian(Number(3), Number(4))
    val conj = z.conjugate
    conj match {
      case ComplexCartesian(x, y) =>
        x.toNominalDouble.get should be(3.0)
        y.toNominalDouble.get should be(-4.0)
    }
  }

  it should "compute inverse correctly" in {
    val z = ComplexCartesian(Number(3), Number(4))
    val inv = z.invert
    // 1/(3+4i) = (3-4i)/25 = 3/25 - 4i/25
    inv match {
      case c: Complex =>
        val cartesian = Complex.convertToCartesian(c)
        cartesian.real.toNominalDouble.get should be(3.0 / 25.0 +- 1e-10)
        cartesian.imag.toNominalDouble.get should be(-4.0 / 25.0 +- 1e-10)
    }
  }

  it should "compute inverse correctly for polar" in {
    val z = ComplexPolar(Number(2), Number.piBy2)
    val inv = z.invert
    inv match {
      case ComplexPolar(r, theta, 1) =>
        r shouldBe Number.half
        theta shouldBe negate(Number.piBy2)
    }
  }

  it should "compute square correctly" in {
    val z = ComplexCartesian(Number(1), Number(1))
    // (1+i)² = 1 + 2i + i² = 1 + 2i - 1 = 2i
    val sq = z.square
    sq match {
      case ComplexCartesian(x, y) =>
        x.toNominalDouble.get should be < 1e-10
        y.toNominalDouble.get should be(2.0 +- 1e-10)
    }
  }

  it should "satisfy z · z̄ = |z|²" in {
    val z = ComplexCartesian(Number(3), Number(4))
    val conj = z.conjugate
    val product = z `multiply` conj
    val modSquared = z.modulusSquared

    product match {
      case Real(x) => x.toNominalDouble.get should be(modSquared.toNominalDouble.get +- 1e-10)
      case ComplexCartesian(x, y) =>
        x.toNominalDouble.get should be(modSquared.toNominalDouble.get +- 1e-10)
        y.toNominalDouble.get should be < 1e-10
    }
  }

  behavior of "ComplexPolar operations"

  it should "convert between Cartesian and Polar correctly" in {
    val cartesian = ComplexCartesian(Number(1), Number(1))
    val polar = Complex.convertToPolar(cartesian)
    val backToCartesian = Complex.convertToCartesian(polar)

    backToCartesian match {
      case ComplexCartesian(x, y) =>
        x.toNominalDouble.get should be(1.0 +- 1e-10)
        y.toNominalDouble.get should be(1.0 +- 1e-10)
    }
  }
  it should "compute abs of complex number" in {
    val z = ComplexPolar(Number(3), Number.zeroR)
    val mod = z.abs
    mod.asNumber.flatMap(_.toNominalDouble).get should be(3.0 +- 1e-10)
  }

  it should "multiply in polar form correctly" in {
    val z1 = ComplexPolar(Number(2), Number(Rational.half, Radian)) // 2·e^(iπ/2)
    val z2 = ComplexPolar(Number(3), Number(Rational(1, 3), Radian)) // 3·e^(iπ/3)
    // Product should be 6·e^(i·(π/2+π/3)) = 6·e^(i·5π/6)
    val product = z1 `multiply` z2
    product match {
      case ComplexPolar(r, theta, _) =>
        r.toNominalDouble.get should be(6.0 +- 1e-10)
        theta.toNominalDouble.get should be((0.5 + 1.0 / 3.0) +- 1e-10)
        theta.factor should be(Radian)
    }
  }

  it should "compute power in polar form correctly" in {
    val z = ComplexPolar(Number(2), Number(Rational.half, Radian))
    val squared = z.power(Number(2))
    // (2·e^(iπ/2))² = 4·e^(iπ)
    squared match {
      case ComplexPolar(r, theta, _) =>
        r.toNominalDouble.get should be(4.0 +- 1e-10)
        theta.toNominalDouble.get should be(1.0 +- 1e-10)
        theta.factor should be(Radian)
    }
  }

  it should "compute ln of polar form correctly (exact)" in {
    val z = ComplexPolar(Number.e, Number.pi) // e·e^(iπ) = -e (exact)
    val lnZ = z.ln
    // ln(-e) = ln(e) + iπ = 1 + iπ
    lnZ match {
      case ComplexCartesian(x, y) =>
        x shouldBe Number.one
        y shouldBe Number.pi
    }
  }

  it should "compute ln of polar form correctly (fuzzy)" in {
    val z = ComplexPolar(Number(Math.E), Number.pi) // e·e^(iπ) = -e (fuzzy)
    val lnZ = z.ln
    // ln(-e) = ln(e) + iπ ≈ 1 + iπ
    lnZ match {
      case ComplexCartesian(x, y) =>
        x.toNominalDouble.get should be(1.0 +- 1e-10)
        y shouldBe Number.pi // angle stays exact even if magnitude is fuzzy
    }
  }

  behavior of "BaseComplex edge cases"

  it should "handle zero correctly" in {
    val zero = ComplexCartesian(Number.zero, Number.zero)
    zero.isZero should be(true)
    zero.modulus.isZero should be(true)
  }

  it should "identify real numbers" in {
    val real = ComplexCartesian(Number(5), Number.zero)
    real.isReal should be(true)
    real.asNumber should be(Some(Number(5)))
  }

  it should "identify imaginary numbers" in {
    val imag = ComplexCartesian(Number.zero, Number(3))
    imag.isImaginary should be(true)
  }

  it should "handle unity correctly" in {
    val unity1 = ComplexPolar(Number.one, Number.zeroR)
    unity1.isUnity should be(true)

    val unity2 = ComplexCartesian(Number.one, Number.zero)
    unity2.asComplex.isUnity should be(true)
  }

  it should "normalize correctly" in {
    val z = ComplexCartesian(Number(3), Number.zero)
    val normalized = z.normalize
    normalized match {
      case Real(x) => x.toNominalDouble.get should be(3.0)
      case _ => fail("Should normalize to Real")
    }
  }

  behavior of "Exponential and logarithm relationship"

  it should "satisfy ln(exp(z)) = z for small z" in {
    val z = ComplexCartesian(Number(Rational(1, 2)), Number(Rational(1, 3)))
    val expZ = z.exp.asComplex
    val lnExpZ = expZ.ln

    val diff = lnExpZ.asComplex `add` (z.unary_-)
    diff.asComplex.modulus.toNominalDouble.get should be < 1e-8
  }

  it should "satisfy exp(ln(z)) = z for positive real z" in {
    val z = ComplexCartesian(Number(2), Number.zero)
    val lnZ = z.asComplex.ln
    val expLnZ = lnZ.asComplex.exp

    val diff = expLnZ.asComplex `add` (z.unary_-)
    diff.asComplex.modulus.toNominalDouble.get should be < 1e-8
  }
}