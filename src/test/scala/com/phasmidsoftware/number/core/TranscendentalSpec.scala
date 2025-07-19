/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.expression._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TranscendentalSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Transcendental"

  it should "evaluate pi" in {
    Pi.evaluate shouldBe Some(Constants.pi)
  }
  it should "evaluate e" in {
    E.evaluate shouldBe Some(Constants.e)
  }
  it should "evaluate l2" in {
    L2.evaluate shouldBe None
  }
  it should "expression pi" in {
    Pi.expression shouldEqual ConstPi
  }
  it should "expression e" in {
    E.expression shouldEqual ConstE
  }
  it should "expression l2" in {
    L2.expression shouldEqual Two.log
  }

  it should "function 1" in {
    val result = Pi.function(Sine).expression.simplify
    val expected = Expression.zero
    result shouldEqual expected
  }

  it should "function 2" in {
    val natLog2 = L2
    val result = natLog2.function(Exp).expression.simplify
    val expected = Expression.two
    result shouldEqual expected
  }

  it should "render" in {
    Pi.render shouldBe "\uDED1"
    E.render shouldBe "\uD835\uDF00"
    L2.render shouldBe "l2"
  }

}
