/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.inner.{PureNumber, Rational, Value}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ExactNumberSpec extends AnyFlatSpec with should.Matchers {

  behavior of "ExactNumber"

  it should "isProbablyZero" in {
    Number.zero.isProbablyZero(0.5) shouldBe true
    Number("0.00100").isProbablyZero(0.5) shouldBe false
  }

  it should "isSame" in {

  }

  it should "signum" in {

  }

  it should "compare" in {

  }

  it should "isZero" in {

  }

  it should "isExact" in {

  }

  it should "doAdd" in {

  }

  it should "scale" in {

  }

  it should "doPower" in {

  }

  it should "factor" in {

  }

  it should "render" in {
    Number.pi.render shouldBe "\uD835\uDED1"
    Number.pi.makeNegative.render shouldBe "-\uD835\uDED1"
  }

  it should "make" in {

  }

  it should "doMultiply" in {

  }

  it should "simplify" in {

  }

  it should "nominalValue" in {

  }

  it should "apply" in {

  }

  it should "succeed with log" in {
    ExactNumber.log(ExactNumber(1), ExactNumber(2)) shouldBe Some(Number.zero)
    ExactNumber.log(ExactNumber(1024), ExactNumber(2)) shouldBe Some(Number(10))
    ExactNumber.log(ExactNumber(Value.fromRational(Rational(1024).invert), PureNumber), ExactNumber(2)) shouldBe Some(Number(-10))
    ExactNumber.log(ExactNumber(1000), ExactNumber(10)) shouldBe Some(Number(3))
    val thou: ExactNumber = Number("0.00100").asInstanceOf[ExactNumber]
    ExactNumber.log(thou, ExactNumber(10)) shouldBe Some(Number(-3))
  }

  it should "fail with log" in {
    ExactNumber.log(ExactNumber(2), Number.e.asInstanceOf[ExactNumber]) shouldBe None
    ExactNumber.log(ExactNumber(1024), ExactNumber(10)) shouldBe None
  }

}
