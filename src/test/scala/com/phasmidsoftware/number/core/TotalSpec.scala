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

  it should "evaluate 1" in {
    val target = Total(List(Constants.one, Constants.one))
    target.evaluate shouldBe Constants.two
  }

  it should "evaluate 2" in {
    val target = Total(List(Constants.one, Constants.two, Constants.minusOne))
    target.evaluate shouldBe Constants.two
  }

  it should "evaluate 3" in {
    import com.phasmidsoftware.number.core.Expression.ExpressionOps
    val target = Total(List(One * MinusOne, Two + One, MinusOne * 5, Constants.two))
    target.evaluate shouldBe Constants.minusOne
  }

}
