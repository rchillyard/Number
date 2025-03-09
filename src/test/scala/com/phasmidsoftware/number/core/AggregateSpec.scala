package com.phasmidsoftware.number.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class AggregateSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Aggregate"

  it should "isExactInContext" in {
    val target = Aggregate(Constants.one, Constants.one)
    target.isExactInContext(Some(PureNumber)) shouldBe true
  }

  it should "context" in {
    val target = Aggregate(Constants.one, Constants.one)
    target.context shouldBe Some(PureNumber)
  }

  it should "render" in {
    val target = Aggregate(Constants.one, Constants.one)
    target.toString shouldBe "1+1"
    target.render shouldBe "2"
  }

  it should "depth" in {
    val target = Aggregate(Constants.one, Constants.one)
    target.depth shouldBe 2
  }

  it should "evaluate 1" in {
    val target = Aggregate(Constants.one, Constants.one)
    target.evaluate(None) shouldBe Constants.two
  }

  it should "evaluate 2" in {
    val target = Aggregate(Constants.one, Constants.two, Constants.minusOne)
    target.evaluate(None) shouldBe Constants.two
  }

  it should "evaluate 3" in {
    import com.phasmidsoftware.number.core.Expression.ExpressionOps
    val target = Aggregate(One * MinusOne, Two + One, MinusOne * 5, Constants.two)
    target.evaluate(None) shouldBe Constants.minusOne
  }

  val em: ExpressionMatchers = Expression.em

  it should "simplify 1" in {
    import com.phasmidsoftware.number.core.Expression.ExpressionOps
    val target = Aggregate(Two * MinusOne, Two + One, MinusOne * 5, Constants.two)
    val result = em.simplifier(target)
    result shouldBe em.Match(Aggregate(Sum, List(Literal(3), Literal(-5))))
    val value1 = result flatMap (em.evaluator(None))
    value1 shouldBe em.Match(Real(-2))
  }

}
