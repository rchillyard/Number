/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.Valuable
import com.phasmidsoftware.number.expression.expr.ExpressionHelper.math
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class CompositeExpressionSpec extends AnyFlatSpec with should.Matchers {

  behavior of "CompositeExpression"

  it should "terms" in {

  }

  it should "simplifyExact 1" in {
    val x1 = Valuable.one
    val x2 = Valuable.pi
    val e = BiFunction(Literal(x1), Literal(x2), Sum)
    e.simplifyExact(e).successful shouldBe false
  }
  // TODO Issue #139
  ignore should "simplifyExact 2" in {
    val x1 = Valuable.one
    val x2 = Valuable.pi
    val e = math"sin(𝛑) * -1".asInstanceOf[CompositeExpression]
    e.simplifyExact(e).successful shouldBe false
  }
  it should "simplifyExact 3" in {
    val e: CompositeExpression = ((Expression(3) + 5) * (7 - 2)).asInstanceOf[CompositeExpression]
    val m = e.simplifyExact(e)
    m.successful shouldBe true
    val expected = Expression(40)
    m.get should matchPattern { case `expected` => }
  }

  it should "simplifyComponents" in {

  }

  it should "render" in {

  }

  it should "maybeDouble" in {

  }

  it should "simplifyTrivial" in {

  }

  it should "simplifyComposite" in {

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
