package com.phasmidsoftware.number.core

class ExpressionMatchers extends Matchers {

  def value: Matcher[Expression, Number] = {
    case Literal(x) => Match(x)
    case x@ExactNumber(_, _) => Match(x)
    case x@FuzzyNumber(_, _, _) => Match(x)
    case x@(Zero | MinusOne | One) => Match(x.materialize)
    case x => Miss(x)
  }

  def matchValue(x: Number): Matcher[Expression, Number] = e => value(e) match {
    case v@Match(`x`) => v
    case Match(_) => Miss(e)
    case v@Miss(_) => v
  }

}
