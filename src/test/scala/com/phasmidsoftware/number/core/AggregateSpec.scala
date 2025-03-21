package com.phasmidsoftware.number.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class AggregateSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Aggregate"

  it should "isExactInContext" in {
    val target = Aggregate.total(Constants.one, Constants.one)
    target.isExactInContext(Some(PureNumber)) shouldBe true
  }

  it should "context" in {
    val target = Aggregate.total(Constants.one, Constants.one)
    target.context shouldBe Some(PureNumber)
  }

  it should "render" in {
    val target = Aggregate.total(Constants.one, Constants.one)
    target.toString shouldBe "1+1"
    target.render shouldBe "2"
  }

  it should "depth" in {
    val target = Aggregate.total(Constants.one, Constants.one)
    target.depth shouldBe 2
  }

  it should "evaluate 1" in {
    val target = Aggregate.total(Constants.one, Constants.one)
    target.evaluateAsIs shouldBe Constants.two
  }

  it should "evaluate 2" in {
    val target = Aggregate.total(Constants.one, Constants.two, Constants.minusOne)
    target.evaluateAsIs shouldBe Constants.two
  }

  it should "evaluate 3" in {
    import com.phasmidsoftware.number.core.Expression.ExpressionOps
    val target = Aggregate.total(One * MinusOne, Two + One, MinusOne * 5, Constants.two)
    target.evaluateAsIs shouldBe Constants.minusOne
  }

  val em: ExpressionMatchers = Expression.em

  it should "simplifier 1" in {
    import com.phasmidsoftware.number.core.Expression.ExpressionOps
    val target = Aggregate.total(Two * MinusOne, Two + One, MinusOne * 5, Constants.two)
    val result = em.simplifier(target)
    result.successful shouldBe true
    result match {
      case em.Match(expression) => expression shouldBe Literal(Real(-2))
    }
  }

}
