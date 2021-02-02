package com.phasmidsoftware.number.core

/**
  * Matchers whose input is generally an Expression.
  */
class ExpressionMatchers extends Matchers {

  case class DyadicTriple(f: ExpressionBiFunction, l: Expression, r: Expression)

  case class MonadicDuple(f: ExpressionFunction, x: Expression)

  /**
    * Abstract class ExpressionMatcher which extends Matcher where input is always an Expression.
    *
    * @tparam R the MatchResult type.
    */
  abstract class ExpressionMatcher[R] extends Matcher[Expression, R]

  /**
    * Method to create an ExpressionMatcher.
    *
    * @param f a function Expression => MatchResult[R]
    * @tparam R the MatchResult type.
    * @return a Matcher[Expression, R] which is also an ExpressionMatcher[R].
    */
  def ExpressionMatcher[R](f: Expression => MatchResult[R]): ExpressionMatcher[R] = (e: Expression) => f(e)

  def matcherToExpressionMatcher[R](m: Matcher[Expression, R]): ExpressionMatcher[R] = (e: Expression) => m(e)

  def always: ExpressionMatcher[Expression] = ExpressionMatcher(super.always)

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

  def matchDyadicTriple(fm: Matcher[ExpressionBiFunction, ExpressionBiFunction], lm: ExpressionMatcher[Expression], rm: ExpressionMatcher[Expression]): Matcher[DyadicTriple, (ExpressionBiFunction, Expression, Expression)] =
    matchProduct3All(fm, lm, rm)(DyadicTriple)

//  def matchBiFunctionByName(name: String): ExpressionMatcher[Number] = {
  //    val matcher1: ExpressionMatcher[DyadicTriple] = matchBiFunction
  //    val functionMatcher: Matcher[ExpressionBiFunction, ExpressionBiFunction] = matchBiFunctionByName("+")
  //    val expressionMatcher = matcherToExpressionMatcher(matchBiFunctionByName("*") ^^ (Expression(_)))
//    val matcher2: Matcher[DyadicTriple, (ExpressionBiFunction, Expression, Expression)] =
//      matchDyadicTriple(functionMatcher, always, expressionMatcher)
//    val result: ExpressionMatcher[(ExpressionBiFunction, Expression, Expression)] = ExpressionMatcher(matcher1 & matcher2)
//    ExpressionMatcher(result ^^ { case (f,x,y) => BiFunction(x,y,f) .materialize})
//  }

//  def matchExpressionBiFunction: Matcher[ExpressionBiFunction,ExpressionBiFunction] = always

//  def matchExpressionBiFunctionByName(name: String): Matcher[ExpressionBiFunction,ExpressionBiFunction] = havingSame[ExpressionBiFunction,String](matches(name))(_.name)

  //    /**
  //      * Matcher which matches on Expression that directly represents a specific given Number.
  //      *
  //      * @param x the Number to match.
  //      * @return a Matcher[Expression, Number].
  //      */
  //      def complementaryExpressions: Matcher[Expression, Expression] = e => matchBiFunction(e) match {
  //        case Match(DyadicTriple(f, a, b)) =>f
  //      }

}
