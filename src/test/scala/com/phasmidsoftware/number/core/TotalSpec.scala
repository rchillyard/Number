package com.phasmidsoftware.number.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TotalSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Total"

  it should "isExactInContext" in {
    val target = Total(List(Constants.one, Constants.one))
    target.isExactInContext(Some(Scalar)) shouldBe true
  }

  it should "context" in {
    val target = Total(List(Constants.one, Constants.one))
    target.context shouldBe Some(Scalar)
  }

  it should "render" in {
    val target = Total(List(Constants.one, Constants.one))
    target.toString shouldBe "1+1"
    target.render shouldBe "2"
  }

  it should "depth" in {
    val target = Total(List(Constants.one, Constants.one))
    target.depth shouldBe 2

  }

  it should "evaluate" in {
    val target = Total(List(Constants.one, Constants.one))
    target.evaluate shouldBe Constants.two
  }

}
