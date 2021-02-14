package com.phasmidsoftware.number.core

import scala.language.implicitConversions

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
  abstract class ExpressionMatcher[+R] extends Matcher[Expression, R]

  implicit def matcherConverter[R](x: Matcher[Expression, R]): ExpressionMatcher[R] = ExpressionMatcher(x)

  /**
    * Method to create an ExpressionMatcher.
    *
    * @param f a function Expression => MatchResult[R]
    * @tparam R the MatchResult type.
    * @return a Matcher[Expression, R] which is also an ExpressionMatcher[R].
    */
  def ExpressionMatcher[R](f: Expression => MatchResult[R]): ExpressionMatcher[R] = (e: Expression) => f(e)

  def simplifier: ExpressionMatcher[Number] = biFunctionSimplifier | functionSimplifier | materializer

  def biFunctionSimplifier: ExpressionMatcher[Number] = matchBiFunction & (matchCasePlus | matchCaseTimes) & materializer

  def matchFiveElementsA(f: String, h: String, c: Expression): Matcher[DyadicTriple, Expression] =
    Matcher {
      case d@DyadicTriple(f1, l1, r1) if f1.name == f =>
        r1 match {
          case BiFunction(`c`, e, ExpressionBiFunction(_, `h`)) if e.simplify == c => Match(Number.zero)
          case BiFunction(e, `c`, ExpressionBiFunction(_, `h`)) if e.simplify == c => Match(Number.zero)
          case _ => Miss(d)
        }
    }

  def matchCasePlus: Matcher[DyadicTriple, Expression] = matchFiveElementsA("+", "*", Number(-1))

  def matchCaseTimes: Matcher[DyadicTriple, Expression] = matchFiveElementsA("*", "^", Number(-1))

  def functionSimplifier: ExpressionMatcher[Number] = matchFunction & matchMonadicDuple(always, ExpressionMatcher(always)) & materializer

  def materializer: ExpressionMatcher[Number] = ExpressionMatcher(e => Match(e.materialize))

  /**
    * This is the Matcher which should be applied to the root of an Expression in order to yield a numeric value that is,
    * ideally, optimized.
    *
    * @return
    */
  //  def evaluator: ExpressionMatcher[Number] = ExpressionMatcher(value | simplifier | materializer)

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
  def matchValue(x: Number): ExpressionMatcher[Number] = value & matchNumber(x)

  def matchFunction: ExpressionMatcher[MonadicDuple] = {
    case Function(x, f) => Match(MonadicDuple(f, x))
    case e => Miss(e)
  }

  def matchBiFunction: ExpressionMatcher[DyadicTriple] = {
    case BiFunction(a, b, f) => Match(DyadicTriple(f, a, b))
    case e => Miss(e)
  }

  // CONSIDER does this really make sense? We end up extracting just the expression, providing that the function matches OK.
  def matchMonadicDuple(fm: Matcher[ExpressionFunction, ExpressionFunction], om: ExpressionMatcher[Expression]): Matcher[MonadicDuple, Expression] =
    from2(fm ~> om)(MonadicDuple)

  //  from2Alt(fm ~> om)(MonadicDuple.unapply)

  def matchDyadicTriple(fm: Matcher[ExpressionBiFunction, ExpressionBiFunction], lm: ExpressionMatcher[Expression], rm: ExpressionMatcher[Expression]): Matcher[DyadicTriple, (ExpressionBiFunction, Expression, Expression)] =
    matchProduct3All(fm, lm, rm)(DyadicTriple)

  //  def matchBiFunctionByNa
  //  me(name: String): ExpressionMatcher[Number] = {
  //      val matcher1: ExpressionMatcher[DyadicTriple] = matchBiFunction
  //      val functionMatcher: Matcher[ExpressionBiFunction, ExpressionBiFunction] = matchBiFunctionByName("+")
  //      val expressionMatcher = ExpressionMatcher(matchBiFunctionByName("*") ^^ (Expression(_)))
  //    val matcher2: Matcher[DyadicTriple, (ExpressionBiFunction, Expression, Expression)] =
  //      matchDyadicTriple(functionMatcher, ExpressionMatcher(always[Expression]), expressionMatcher)
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
