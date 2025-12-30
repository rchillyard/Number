/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.eager.{RationalNumber, WholeNumber}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class LiteralSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Literal"

  it should "create from WholeNumber" in {
    val lit = Literal(WholeNumber(5))
    lit.value shouldBe WholeNumber(5)
  }

  it should "create from RationalNumber" in {
    val lit = Literal(RationalNumber(3, 2))
    lit.value shouldBe RationalNumber(3, 2)
  }

  it should "normalize to its underlying number" in {
    Literal(WholeNumber(5)).normalize shouldBe WholeNumber(5)
    Literal(RationalNumber(4, 2)).normalize shouldBe WholeNumber(2)
    Literal(RationalNumber(3, 2)).normalize shouldBe RationalNumber(3, 2)
  }

  it should "render correctly" in {
    Literal(WholeNumber(5)).render shouldBe "5"
    Literal(RationalNumber(3, 2)).render shouldBe "1.5"
  }

  it should "materialize to Some of its number" in {
    Literal(WholeNumber(5)).materialize shouldBe WholeNumber(5)
    Literal(RationalNumber(3, 2)).materialize shouldBe RationalNumber(3, 2)
  }

  it should "have correct isAtomic" in {
    Literal(WholeNumber(5)).isAtomic shouldBe true
  }
}