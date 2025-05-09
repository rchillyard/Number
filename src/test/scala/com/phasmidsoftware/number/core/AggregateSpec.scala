package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Constants.root5
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class AggregateSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Aggregate"

  it should "render" in {
    val target = Aggregate.total(Constants.one, Constants.one)
    target.materialize shouldBe Real(2)
    target.toString shouldBe "Aggregate{+,1,1}"
    target.render shouldBe "2"
  }

  it should "depth" in {
    val target = Aggregate.total(Constants.one, Constants.one)
    target.depth shouldBe 2
  }

  it should "evaluate 1" in {
    val target = Aggregate.total(Constants.one, Constants.one)
    target.evaluateAsIs shouldBe Some(Constants.two)
  }

  it should "evaluate 2" in {
    val target = Aggregate.total(Constants.one, Constants.two, Constants.minusOne)
    target.evaluateAsIs shouldBe Some(Constants.two)
  }

  it should "evaluate 3" in {
    import com.phasmidsoftware.number.core.Expression.ExpressionOps
    val target = Aggregate.total(One * MinusOne, Two + One, MinusOne * 5, Constants.two)
    target.evaluateAsIs shouldBe Some(Constants.minusOne)
  }

  val em: ExpressionMatchers = Expression.em

  it should "simplifyComponents {2 * -1}+{2 + 1}+{-1 * 5}+2" in {
    import com.phasmidsoftware.number.core.Expression.ExpressionOps
    val target = Aggregate.total(Two * MinusOne, Two + One, MinusOne * 5, Constants.two)
    target.simplifyComponents(target) shouldBe em.Match(Aggregate.total(Literal(-2), Literal(3), Literal(-5), Literal(2)))
    target.simplifyConstant(target) shouldBe em.Match(Literal(-2))
  }

  //  FIXME infinite recursion
  ignore should "simplify {2 * -1}+{2 + 1}+{-1 * 5}+2" in {
    import com.phasmidsoftware.number.core.Expression.ExpressionOps
    val target = Aggregate.total(Two * MinusOne, Two + One, MinusOne * 5, Constants.two)
    target.simplify shouldBe Literal(-2)
  }

  ignore should "simplifier 2" in {
    val target = Aggregate.total(One, root5)
    val result = em.matchCompositeAndSimplify(target)
    result.successful shouldBe false
  }

}
