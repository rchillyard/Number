/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.misc

import org.scalatest.flatspec
import org.scalatest.matchers.should

import scala.util.{Failure, Success, Try}

/**
  * Tests for [[MathFunctions]].
  *
  * Two principles are used throughout, and they are worth stating because they are what make
  * these tests portable:
  *
  *   1. Where a function substitutes an exact value (`exp(0)`, `exp(1)`, `log(1)`, `log(E)`,
  *      `sin(0)`, `sin(Pi/2)`, `cos(0)`, `cos(Pi/2)`), the test asserts that exact value with
  *      `shouldBe`. That is the whole point of the substitution and it holds on every platform.
  *   2. Where a function falls through to `math.exp`/`log`/`sin`/`cos`, the test asserts
  *      equality with the *same* library call rather than with a decimal literal. `math.exp(2)`
  *      may differ by one unit in the last place between an aarch64 Mac and an x86-64 container;
  *      `MathFunctions.exp(2) == Try(math.exp(2))` is true on both. Writing the literal is what
  *      made the two `LazyNumberFuzzySpec` tests platform-dependent.
  */
class MathFunctionsSpec extends flatspec.AnyFlatSpec with should.Matchers {

  behavior of "exp"

  it should "be exactly 1 for 0" in {
    MathFunctions.exp(0.0) shouldBe Success(1.0)
  }

  it should "be exactly 1 for negative zero" in {
    // -0.0 == 0.0, so the substitution applies; math.exp(-0.0) is also exactly 1.0.
    MathFunctions.exp(-0.0) shouldBe Success(1.0)
  }

  it should "be exactly Math.E for 1" in {
    // The substitution that makes this platform-independent: math.exp(1.0) is one ulp above
    // Math.E on aarch64 and equal to it on x86-64.
    MathFunctions.exp(1.0) shouldBe Success(math.E)
  }

  it should "agree with math.exp for a value it does not substitute" in {
    MathFunctions.exp(2.0) shouldBe Success(math.exp(2.0))
    MathFunctions.exp(-1.0) shouldBe Success(math.exp(-1.0))
    MathFunctions.exp(0.5) shouldBe Success(math.exp(0.5))
  }

  it should "overflow to positive infinity rather than failing" in {
    MathFunctions.exp(1000.0) shouldBe Success(Double.PositiveInfinity)
  }

  it should "underflow to zero for a large negative argument" in {
    MathFunctions.exp(-1000.0) shouldBe Success(0.0)
  }

  behavior of "log"

  it should "be exactly 0 for 1" in {
    MathFunctions.log(1.0) shouldBe Success(0.0)
  }

  it should "be exactly 1 for Math.E" in {
    MathFunctions.log(math.E) shouldBe Success(1.0)
  }

  it should "agree with math.log for a value it does not substitute" in {
    MathFunctions.log(10.0) shouldBe Success(math.log(10.0))
    MathFunctions.log(0.5) shouldBe Success(math.log(0.5))
  }

  it should "give negative infinity for 0" in {
    MathFunctions.log(0.0) shouldBe Success(Double.NegativeInfinity)
  }

  it should "fail for a negative argument rather than returning NaN" in {
    // math.log returns NaN instead of throwing, so a bare Try would give Success(NaN) and leave
    // every caller to test for it. The domain check turns that into a reportable failure.
    val result: Try[Double] = MathFunctions.log(-1.0)
    result match {
      case Success(d) => fail(s"expected a Failure but got Success($d)")
      case Failure(e) => e shouldBe a[MathFunctionsException]
    }
  }

  it should "round-trip with exp" in {
    val values = Seq(0.5, 1.0, 2.0, 10.0)
    for (x <- values) {
      val roundTripped = for {
        e <- MathFunctions.exp(x)
        l <- MathFunctions.log(e)
      } yield l
      roundTripped.get shouldBe x +- 1E-12
    }
  }

  behavior of "sin"

  it should "be exactly 0 for 0" in {
    MathFunctions.sin(0.0) shouldBe Success(0.0)
  }

  it should "be exactly 1 for Pi/2" in {
    // 2 * (Pi/2) is exactly math.Pi, because multiplication by two is exact in binary floating
    // point, so the substitution fires. math.sin(Pi/2) happens also to be exactly 1.0.
    MathFunctions.sin(math.Pi / 2) shouldBe Success(1.0)
  }

  it should "agree with math.sin for a value it does not substitute" in {
    MathFunctions.sin(1.0) shouldBe Success(math.sin(1.0))
  }

  it should "not pretend that sin(Pi) is exactly 0" in {
    // Documents a gap in the substitution rather than a defect: matching on 2*x catches Pi/2
    // but not Pi, so this returns math.sin(Pi), which is 1.2246467991473532E-16.
    MathFunctions.sin(math.Pi) shouldBe Success(math.sin(math.Pi))
    MathFunctions.sin(math.Pi).get should not be 0.0
  }

  it should "be exactly -1 for -Pi/2" in {
    // The negation branch reaches the Pi/2 substitution through -x, so this is exact too.
    MathFunctions.sin(-math.Pi / 2) shouldBe Success(-1.0)
  }

  it should "be an odd function" in {
    MathFunctions.sin(-1.0) shouldBe Success(-math.sin(1.0))
    MathFunctions.sin(-0.5) shouldBe Success(-math.sin(0.5))
  }

  behavior of "cos"

  it should "be exactly 1 for 0" in {
    MathFunctions.cos(0.0) shouldBe Success(1.0)
  }

  it should "be exactly 0 for Pi/2" in {
    MathFunctions.cos(math.Pi / 2) shouldBe Success(0.0)
  }

  it should "agree with math.cos for a value it does not substitute" in {
    MathFunctions.cos(1.0) shouldBe Success(math.cos(1.0))
    MathFunctions.cos(math.Pi) shouldBe Success(-1.0)
  }

  it should "be an even function, including at the substituted points" in {
    // Before the negation branch was added, cos(Pi/2) was exactly 0.0 while cos(-Pi/2) was
    // 6.123233995736766E-17, because -math.Pi matched none of the exact cases.
    MathFunctions.cos(-math.Pi / 2) shouldBe Success(0.0)
    MathFunctions.cos(-math.Pi / 2) shouldBe MathFunctions.cos(math.Pi / 2)
    MathFunctions.cos(-0.0) shouldBe Success(1.0)
  }

  it should "agree with math.cos for ordinary negative arguments" in {
    MathFunctions.cos(-1.0) shouldBe Success(math.cos(-1.0))
  }
}
