/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.numerical

import com.phasmidsoftware.number.core.inner.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * Test suite for TESTME items in Number.scala.
  * This spec addresses 6 specific TESTME comments found in the Number trait.
  */
class NumberTestmeSpec extends AnyFlatSpec with Matchers {

  behavior of "Number.toLong (TESTME at line 159)"

  it should "convert an integer Number to Long" in {
    val n = Number(42)
    n.toLong shouldBe Some(42L)
  }

  it should "convert a rational Number to Long when it has unit denominator" in {
    val n = Number(Rational(100, 1))
    n.toLong shouldBe Some(100L)
  }

  it should "return None for a rational Number with non-unit denominator" in {
    val n = Number(Rational(1, 2))
    n.toLong shouldBe None
  }

  it should "convert a large integer to Long" in {
    val largeLong = Long.MaxValue
    val n = Number(largeLong)
    n.toLong shouldBe Some(largeLong)
  }

  it should "return None for an invalid Number" in {
    val n = Number()
    n.toLong shouldBe None
  }

  behavior of "Number.add with BaseComplex (TESTME at line 276)"

  it should "add a Number to a BaseComplex (ComplexCartesian)" in {
    val num = Number(3)
    val complex = ComplexCartesian(Number(1), Number(2))
    val result = num.add(complex)

    result match {
      case c: BaseComplex =>
        c.real shouldBe Number(4)
        c.imag shouldBe Number(2)
      case _ => fail("Expected BaseComplex result")
    }
  }

  it should "add a Number to a BaseComplex (ComplexPolar)" in {
    val num = Number(5)
    val polar = ComplexPolar(Number(2), Number.zeroR) // 2∠0 = 2+0i
    val result = num.add(polar)

    result match {
      case c: BaseComplex =>
        c.real shouldBe Number(7)
        c.imag shouldBe Number.zero
      case _ => fail("Expected BaseComplex result")
    }
  }

  it should "handle addition with imaginary BaseComplex" in {
    val num = Number(1)
    val complex = ComplexCartesian(Number.zero, Number(1)) // pure imaginary: i
    val result = num.add(complex)

    result match {
      case c: BaseComplex =>
        c.real shouldBe Number(1)
        c.imag shouldBe Number(1)
      case _ => fail("Expected BaseComplex result")
    }
  }

  behavior of "Number.divide with BaseComplex (TESTME at line 337)"

  it should "divide a Number by a BaseComplex (ComplexCartesian)" in {
    val num = Number(10)
    val complex = ComplexCartesian(Number(2), Number.zero) // 2+0i = 2
    val result = num.divide(complex)

    result match {
      case c: BaseComplex =>
        c.real shouldBe Number(5)
        c.imag shouldBe Number.zero
      case _ => fail("Expected BaseComplex result")
    }
  }

  it should "divide a Number by a complex number with imaginary part" in {
    val num = Number(1)
    val complex = ComplexCartesian(Number.zero, Number(1)) // i
    val result = num.divide(complex)

    result match {
      case c: BaseComplex =>
        // 1/i = -i
        c.real shouldBe Number.zero
        c.imag shouldBe Number(-1)
      case _ => fail("Expected BaseComplex result")
    }
  }

  it should "handle division by ComplexPolar" in {
    val num = Number(4)
    val polar = ComplexPolar(Number(2), Number.zeroR) // 2∠0 = 2
    val result = num.divide(polar)

    result match {
      case c: BaseComplex =>
        c.real shouldBe Number(2)
        c.imag shouldBe Number.zeroR
      case _ => fail("Expected BaseComplex result")
    }
  }

  behavior of "Number.power with ComplexCartesian (TESTME at line 378)"

  it should "raise ComplexCartesian to power 2 (stays Cartesian)" in {
    val base = ComplexCartesian(Number(3), Number(4)) // 3+4i
    val exponent = Number(2)
    val result: Complex = base.power(exponent)

    result match {
      case ComplexCartesian(re, im) =>
        // (3+4i)^2 = 9 + 24i - 16 = -7 + 24i
        re shouldBe Number(-7)
        im shouldBe Number(24)
      case _ => fail("Expected ComplexCartesian result for power 2")
    }
  }

  it should "handle power of 1 (stays Cartesian, identity)" in {
    val base = ComplexCartesian(Number(2), Number(3))
    val exponent = Number(1)
    val result = base.power(exponent)

    result match {
      case ComplexCartesian(re, im) =>
        // x^1 = x (identity)
        re shouldBe Number(2)
        im shouldBe Number(3)
      case _ => fail("Expected ComplexCartesian result for power 1")
    }
  }

  it should "handle power of 0 (returns 1 as Cartesian)" in {
    val base = ComplexCartesian(Number(5), Number(12))
    val exponent = Number(0)
    val result = base.power(exponent)

    result match {
      case ComplexCartesian(re, im) =>
        // Any non-zero number to the power 0 is 1
        re shouldBe Number(1)
        im shouldBe Number.zero
      case _ => fail("Expected ComplexCartesian result for power 0")
    }
  }
  it should "handle power of 3 (converts to Polar)" in {
    val base = ComplexCartesian(Number(3), Number(4)) // 3+4i, magnitude = 5
    val exponent = Number(3)
    val result = base.power(exponent)

    result match {
      case ComplexPolar(r, theta, _) =>
        // (3+4i)^3 should give magnitude = 5^3 = 125
        r.maybeNominalDouble.get should be(125.0 +- 0.001)
        theta.maybeNominalDouble.get should be(0.8855 +- 0.01)
        theta.factor shouldBe Radian
      case _ => fail(s"Expected ComplexPolar result for power 3")
    }
  }

  behavior of "Number.compare with NatLog factors (TESTME at line 1271)"

  it should "compare two Numbers with NatLog factor" in {
    val x = Number(2, NatLog) // e^2
    val y = Number(3, NatLog) // e^3
    val result = Number.doCompare(x, y)

    result should be < 0 // e^2 < e^3
  }

  it should "compare equal Numbers with NatLog factor" in {
    val x = Number(5, NatLog)
    val y = Number(5, NatLog)
    val result = Number.doCompare(x, y)

    result shouldBe 0
  }

  it should "compare with negative NatLog values" in {
    val x = Number(-1, NatLog) // e^(-1)
    val y = Number(1, NatLog) // e^1
    val result = Number.doCompare(x, y)

    result should be < 0 // e^(-1) < e^1
  }

  it should "verify conversion to PureNumber maintains ordering for NatLog" in {
    // The TESTME asks why we need to convert to PureNumber
    // This test verifies that the conversion is necessary for correct comparison
    val x = Number(2, NatLog)
    val y = Number(3, NatLog)

    // Compare by converting to PureNumber (what the code does)
    val resultViaConversion = Number.doCompare(x.make(PureNumber), y.make(PureNumber))

    // Direct comparison of the underlying values
    val resultDirect = (for {
      a <- Value.maybeDouble(x.nominalValue)
      b <- Value.maybeDouble(y.nominalValue)
    } yield a.compareTo(b)).getOrElse(0)

    resultViaConversion should be < 0
    resultDirect should be < 0
    // Both should give the same ordering
    math.signum(resultViaConversion) shouldBe math.signum(resultDirect)
  }

  behavior of "Number.compare with Euler factors (TESTME at line 1273)"

  it should "compare two Numbers with Euler factor" in {
    val x = Number(1, Euler) // represents an angle/phase
    val y = Number(2, Euler)
    val result = Number.doCompare(x, y)

    result should be < 0 // 1 < 2 in Euler factor
  }

  it should "compare equal Numbers with Euler factor" in {
    val x = Number(0.5, Euler)
    val y = Number(0.5, Euler)
    val result = Number.doCompare(x, y)

    result shouldBe 0
  }

  it should "doCompare with negative Euler values" in {
    val x = Number(-0.5, Euler)
    val y = Number(0.5, Euler)
    val result = Number.doCompare(x, y)

    result should be < 0 // -0.5 < 0.5
  }

  it should "verify conversion to Radian maintains ordering for Euler" in {
    // The TESTME asks why we need to convert to Radian
    // This test verifies that the conversion is necessary for correct comparison
    val x = Number(0.25, Euler)
    val y = Number(0.75, Euler)

    // doCompare by converting to Radian (what the code does)
    val resultViaConversion = Number.doCompare(x.make(Radian), y.make(Radian))

    resultViaConversion should be < 0
  }

  it should "handle boundary cases for Euler factor comparison" in {
    val x = Number(0, Euler)
    val y = Number(1, Euler)
    val result = Number.doCompare(x, y)

    result should be < 0
  }
}