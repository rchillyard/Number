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
    target.evaluate shouldBe Constants.two
  }

  it should "evaluate 2" in {
    val target = Aggregate.total(Constants.one, Constants.two, Constants.minusOne)
    target.evaluate shouldBe Constants.two
  }

  it should "evaluate 3" in {
    import com.phasmidsoftware.number.core.Expression.ExpressionOps
    val target = Aggregate.total(One * MinusOne, Two + One, MinusOne * 5, Constants.two)
    target.evaluate shouldBe Constants.minusOne
  }

  val em: ExpressionMatchers = Expression.em

  it should "simplifier 1" in {
    import com.phasmidsoftware.number.core.Expression.ExpressionOps
    val target = Aggregate.total(Two * MinusOne, Two + One, MinusOne * 5, Constants.two)
    val result = em.simplifier(target)
    result.successful shouldBe true
    result match {
      case em.Match(expression) => expression shouldBe BiFunction(Literal(3), Literal(-5), Sum)
        val value1 = result flatMap (em.evaluator(None))
        value1 shouldBe em.Match(Real(-2))
    }
  }

}
