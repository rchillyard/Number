/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.misc

import com.phasmidsoftware.number.core.misc.Factorial
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.{an, convertToAnyShouldWrapper}

class FactorialSpec extends AnyFlatSpec {

  behavior of "Factorial"

  it should "apply successfully" in {
    Factorial(0) shouldBe BigInt(1)
    Factorial(1) shouldBe BigInt(1)
    Factorial(2) shouldBe BigInt(2)
    Factorial(3) shouldBe BigInt(6)
    Factorial(4) shouldBe BigInt(24)
    Factorial(5) shouldBe BigInt(120)
    Factorial(6) shouldBe BigInt(720)
  }

  it should "fail on negative numbers" in {
    an[IllegalArgumentException] should be thrownBy Factorial(-1)
  }

}
