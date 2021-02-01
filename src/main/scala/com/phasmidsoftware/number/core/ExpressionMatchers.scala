package com.phasmidsoftware.number.core

class ExpressionMatchers extends Matchers {

  case class DyadicTriple(f: ExpressionBiFunction, l: Expression, r: Expression)

  case class MonadicDuple(f: ExpressionFunction, x: Expression)

  /**
    * Matcher which matches on Expressions that directly represent Numbers.
    *
    * @return a Matcher[Expression, Number].
    */
  def value: Matcher[Expression, Number] = {
    case Literal(x) => Match(x)
    case x@ExactNumber(_, _) => Match(x)
    case x@FuzzyNumber(_, _, _) => Match(x)
    case x@(Zero | MinusOne | One) => Match(x.materialize)
    case x => Miss(x)
  }

  /**
    * Matcher to match a specific Number.
    *
    * @param x the Number to match.
    * @return a Matcher[Number, Number] which matches only on x.
    */
  def matchNumber(x: Number): Matcher[Number, Number] = {
    case `x` => Match(x)
    case e => Miss(e)
  }

  /**
    * Matcher which matches on Expressions that directly represents a specific given Number.
    *
    * @param x the Number to match.
    * @return a Matcher[Expression, Number].
    */
  def matchValue(x: Number): Matcher[Expression, Number] = value & matchNumber(x)

  def matchBiFunction: Matcher[Expression, DyadicTriple] = {
    case BiFunction(a, b, f) => Match(DyadicTriple(f, a, b))
    case e => Miss(e)
  }

  //
  //  /**
  //    * Matcher which matches on Expression that directly represents a specific given Number.
  //    *
  //    * @param x the Number to match.
  //    * @return a Matcher[Expression, Number].
  //    */
  //  def complementaryExpressions: Matcher[Expression, Expression] = e => matchBiFunction(e) match {
  //    case Match(DyadicTriple(f, a, b)) =>
  //  }

}
