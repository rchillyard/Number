package com.phasmidsoftware.number.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class FactorSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Factor"

  it should "convert" in {

  }

  it should "isAdditive" in {

  }

  it should "isA" in {
    PureNumber.isA(RestrictedContext(PureNumber)) shouldBe true
    PureNumber.isA(AnyContext) shouldBe true
  }

  it should "value" in {

  }

  it should "render" in {

  }

  it should "sPiAlt2" in {

  }

  it should "sPiAlt1" in {

  }

  it should "sPiAlt0" in {

  }

  it should "apply" in {

  }

  it should "sPi" in {

  }

  it should "sE" in {

  }

}
