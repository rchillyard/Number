// src/test/scala/com/phasmidsoftware/number/expression/ExpressionMatchers.scala
package com.phasmidsoftware.number.expression.expr

import org.scalatest.matchers.{MatchResult, Matcher, should}

trait ExpressionMatcherPatterns {
  self: should.Matchers =>
  def simplifyAs(expected: Expression): Matcher[Expression] =
    (left: Expression) => MatchResult(
      left.simplify == expected,
      s"${left.render} did not simplify to ${expected.render}, got ${left.simplify.render}",
      s"${left.render} simplified to ${expected.render}"
    )
}