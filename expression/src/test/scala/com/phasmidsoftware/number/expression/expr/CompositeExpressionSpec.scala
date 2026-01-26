/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.eager.{Eager, WholeNumber}
//import com.phasmidsoftware.number.expression.expr.ExpressionHelper.math
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class CompositeExpressionSpec extends AnyFlatSpec with should.Matchers {

  behavior of "CompositeExpression"

  it should "terms" in {

  }

  it should "simplifyExact 1" in {
    val x1 = Eager.one
    val x2 = Eager.pi
    val e = BiFunction(Literal(x1), Literal(x2), Sum)
    e.simplifyExact(e).successful shouldBe false
  }
  // NOTE Test case for Issue #142
  it should "simplifyExact 2" in {
    Expression("sin(𝛑) * -1") match {
      case expression: CompositeExpression =>
        val simplified = expression.simplifyExact(expression)
        simplified.successful shouldBe true
        simplified.get shouldBe Literal(WholeNumber(0), Some("0")) // NOTE this should be Zero, not Literal(WholeNumber(0))
      case x =>
        fail(s"expected CompositeExpression, got $x")
    }
  }
  it should "simplifyExact 3" in {
    val e: CompositeExpression = ((Expression(3) :+ 5) * (7 - 2)).asInstanceOf[CompositeExpression]
    val m = e.simplifyExact(e)
    m.successful shouldBe true
    val expected = Expression(40)
    m.get should matchPattern { case `expected` => }
  }

  it should "simplifyOperands" in {

  }

  it should "render" in {

  }

  it should "maybeDouble" in {

  }

  it should "simplifyIdentities" in {

  }

  it should "simplifyStructural" in {

  }

  it should "simplifyConstant" in {

  }

  it should "isAtomic" in {

  }

  it should "apply" in {

  }

  it should "create" in {

  }

}
