/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class InversePowerSpec extends AnyFlatSpec with should.Matchers {

  behavior of "InversePowerSpec"

  it should "compareTo" in {

  }

  it should "convert" in {

  }

  it should "toDouble" in {

  }

  it should "signum" in {

  }

  it should "approximation" in {

  }

  it should "asJavaNumber" in {

  }

  it should "maybeDouble" in {

  }

  it should "$times" in {

  }

  it should "$div" in {

  }

  it should "compare" in {

  }

  it should "doScaleDouble" in {

  }

  it should "isZero" in {

  }

  it should "isExact" in {

  }

  it should "maybeFactor" in {

  }

  it should "render" in {
    Valuable.root2.render shouldBe "√2"
    Valuable.root3.render shouldBe "√3"
    InversePower(3, 2).render shouldBe "³√2"
  }

  it should "doScale" in {

  }

  it should "one" in {

  }

  it should "toRational" in {

  }

  it should "doScaleInt" in {

  }

  it should "showInversePower" in {

  }

}
