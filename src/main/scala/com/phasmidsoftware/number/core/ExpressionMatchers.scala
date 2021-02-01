package com.phasmidsoftware.number.core

class ExpressionMatchers extends Matchers {

  case class DyadicTriple(f: ExpressionBiFunction, l: Expression, r: Expression)

  case class MonadicDuple(f: ExpressionFunction, x: Expression)

  def ExpressionMatcher[R](f: Expression => MatchResult[R]): ExpressionMatcher[R] = (t: Expression) => f(t)

  abstract class ExpressionMatcher[R] extends Matcher[Expression, R] {
  }

  /**
    * Matcher which matches on Expressions that directly represent Numbers.
    *
    * @return a Matcher[Expression, Number].
    */
  def value: ExpressionMatcher[Number] = {
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
  def matchValue(x: Number): ExpressionMatcher[Number] = ExpressionMatcher(value & matchNumber(x))

  def matchBiFunction: ExpressionMatcher[DyadicTriple] = {
    case BiFunction(a, b, f) => Match(DyadicTriple(f, a, b))
    case e => Miss(e)
  }

  //  /**
  //    * Matcher which matches on Expression that directly represents a specific given Number.
  //    *
  //    * @param x the Number to match.
  //    * @return a Matcher[Expression, Number].
  //    */
  //    def complementaryExpressions: Matcher[Expression, Expression] = e => matchBiFunction(e) match {
  //      case Match(DyadicTriple(f, a, b)) =>
  //    }

}
