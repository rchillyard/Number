package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.eager.{Eager, WholeNumber}
import com.phasmidsoftware.number.core.inner.Rational
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

/**
  * This is based on AggregateSpec from the core module.
  */
class AggregateSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Aggregate"
  it should "render" in {
    val target = Aggregate.total(Literal(WholeNumber.one), Literal(WholeNumber.one))
    target.materialize shouldBe WholeNumber.two
    target.toString shouldBe "Aggregate{+,1,1}"
    target.render shouldBe "2"
  }
  it should "depth" in {
    val target = Aggregate.total(WholeNumber.one, WholeNumber.one)
    target.depth shouldBe 2
  }
  it should "evaluate 1" in {
    val target = Aggregate.total(WholeNumber.one, WholeNumber.one)
    target.evaluateAsIs shouldBe Some(WholeNumber.two)
  }
  it should "evaluate 2" in {
    val target = Aggregate.total(WholeNumber.one, WholeNumber.two, WholeNumber.minusOne)
    target.evaluateAsIs shouldBe Some(WholeNumber.two)
  }
  it should "evaluate 3" in {
    val target = Aggregate.total(One * MinusOne, Two + One, MinusOne * 5, WholeNumber.two)
    target.evaluateAsIs shouldBe Some(WholeNumber.minusOne)
  }
  val em: ExpressionMatchers = Expression.em
  it should "simplifyComponents {2 * -1}+{2 + 1}+{-1 * 5}+2" in {
    val target = Aggregate.total(Two * MinusOne, Two + One, MinusOne * 5, WholeNumber.two)
    target.simplifyComponents(target) shouldBe em.Match(Aggregate.total(-2, 3, -5, 2))
    target.simplifyConstant(target) shouldBe em.Match(Literal(-2))
  }
  it should "simplify {2 * -1}+{2 + 1}+{-1 * 5}+2" in {
    val target = Aggregate.total(Two * MinusOne, Two + One, MinusOne * 5, WholeNumber.two)
    target.simplify shouldBe Literal(-2)
  }
  it should "simplify 3 -5 +2" in {
    val target = Aggregate.total(Literal(3), Literal(-5), Literal(2))
    target.simplify shouldBe Literal(0)
  }
  it should "simplify 3 * 1/5 * 5/3" in {
    val target = Aggregate.product(Literal(3), Literal(Rational(5)).reciprocal, Literal(Rational(5, 3)))
    target.simplify shouldBe Literal(1)
  }
}
