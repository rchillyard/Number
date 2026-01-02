/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.inner

import com.phasmidsoftware.number.core.inner.Rational.pi_5000

/**
  * @author scalaprof
  */
class RationalFuncSpec extends flatspec.AnyFlatSpec with should.Matchers with PrivateMethodTester {

  behavior of "renderApproximate"
  // TODO the following works but is very slow.
  it should "work with one parameter (pi)" taggedAs Slow in {
    pi_5000.renderApproximate(2) shouldBe " 3"
    pi_5000.renderApproximate(3) shouldBe "3.1"
    pi_5000.renderApproximate(4) shouldBe "3.14"
    pi_5000.renderApproximate(5) shouldBe "3.142"
    pi_5000.renderApproximate(6) shouldBe "3.1416"
    pi_5000.renderApproximate(7) shouldBe "3.14159"
    pi_5000.renderApproximate(8) shouldBe "3.141593"
    pi_5000.renderApproximate(9) shouldBe "3.1415927"
    pi_5000.renderApproximate(10) shouldBe "3.14159265"
  }
  // TODO the following works but is very slow.
  it should "work with two parameters (pi)" taggedAs Slow in {
    pi_5000.renderApproximate(2, Some(0)) shouldBe " 3"
    pi_5000.renderApproximate(3, Some(1)) shouldBe "3.1"
    pi_5000.renderApproximate(4, Some(2)) shouldBe "3.14"
    pi_5000.renderApproximate(5, Some(2)) shouldBe " 3.14"
    pi_5000.renderApproximate(6, Some(2)) shouldBe "  3.14"
    pi_5000.renderApproximate(7, Some(2)) shouldBe "   3.14"
    pi_5000.renderApproximate(8, Some(2)) shouldBe "    3.14"
    pi_5000.renderApproximate(9, Some(2)) shouldBe "     3.14"
    pi_5000.renderApproximate(10, Some(2)) shouldBe "      3.14"
  }

  behavior of "approximateAny"
  // NOTE: this test works but it is very slow. It should be checked from time to time.
  it should "work for 3.1416" taggedAs Slow in {
    Rational.approximateAny(3.1416) shouldBe Rational(3141600355L, 1000000113)
  }
}
