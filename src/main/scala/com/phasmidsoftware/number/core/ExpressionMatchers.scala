package com.phasmidsoftware.number.core

import scala.language.implicitConversions

/**
  * Matchers whose input is generally an Expression.
  */
class ExpressionMatchers(implicit val matchLogger: MatchLogger) extends Matchers {

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

  def simplifier: ExpressionMatcher[Expression] = (biFunctionSimplifier | functionSimplifier) :| "simplifier"

  def biFunctionSimplifier: ExpressionMatcher[Expression] = matchBiFunction & (matchCasePlus | matchCaseTimes) :| "biFunctionSimplifier"

  case class Branches(l: Expression, r: Expression)

  def matchDyadicBranches(f: ExpressionBiFunction): Matcher[DyadicTriple, (Expression, Expression)] =
    LoggingMatcher(s"matchDyadicBranches: $f") {
      case DyadicTriple(`f`, l1, r1) => Match((l1, r1))
      case x => Miss("matchDyadicBranches", x)
    }

  def matchDyadicBranch(h: ExpressionBiFunction, c: Expression, r: Expression): Matcher[(Expression, Expression), Expression] =
    LoggingMatcher(s"matchDyadicBranch: $h") {
      // TODO clean this up
      case (b, BiFunction(`c`, e, `h`))
        if e.simplify == b.simplify =>
        Match(r)
      case (BiFunction(`c`, e, `h`), b)
        if e.simplify == b.simplify =>
        Match(r)
      case (b, BiFunction(e, `c`, `h`))
        if e.simplify == b.simplify =>
        Match(r)
      case (BiFunction(e, `c`, `h`), b)
        if e.simplify == b.simplify =>
        Match(r)
      case x =>
        Miss("matchDyadicBranch", x)
    }

  def matchCasePlus: Matcher[DyadicTriple, Expression] = matchDyadicBranches(Sum) & matchDyadicBranch(Product, Number(-1), Number.zero) :| "matchCasePlus"

  def matchCaseTimes: Matcher[DyadicTriple, Expression] = matchDyadicBranches(Product) & matchDyadicBranch(Power, Number(-1), Number.zero) :| "matchCaseTimes"

  def functionSimplifier: ExpressionMatcher[Expression] = matchFunction & matchMonadicDuple(always, ExpressionMatcher(always)) :| "functionSimplifier"

  def materializer: ExpressionMatcher[Number] = ExpressionMatcher(e => Match(e.materialize)) :| "materializer"

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
    case x => Miss("value", x)
  }

  /**
    * Matcher to match a specific Number.
    *
    * @param x the Number to match.
    * @return a Matcher[Number, Number] which matches only on x.
    */
  def matchNumber(x: Number): Matcher[Number, Number] = {
    case `x` => Match(x)
    case e => Miss("matchNumber", e)
  }

  /**
    * Matcher which matches on Expressions that directly represents a specific given Number.
    *
    * @param x the Number to match.
    * @return a Matcher[Expression, Number].
    */
  def matchValue(x: Number): ExpressionMatcher[Number] = value & matchNumber(x) :| "matchValue"

  def matchFunction: ExpressionMatcher[MonadicDuple] = {
    case Function(x, f) => Match(MonadicDuple(f, x))
    case e => Miss("matchFunction", e)
  }

  def matchBiFunction: ExpressionMatcher[DyadicTriple] = {
    case BiFunction(a, b, f) => Match(DyadicTriple(f, a, b))
    case e => Miss("matchBiFunction", e)
  }

  // CONSIDER does this really make sense? We end up extracting just the expression, providing that the function matches OK.
  def matchMonadicDuple(fm: Matcher[ExpressionFunction, ExpressionFunction], om: ExpressionMatcher[Expression]): Matcher[MonadicDuple, Expression] =
    from2(fm ~> om)(MonadicDuple) :| "matchMonadicDuple"

  //  from2Alt(fm ~> om)(MonadicDuple.unapply)

  def matchDyadicTriple(fm: Matcher[ExpressionBiFunction, ExpressionBiFunction], lm: ExpressionMatcher[Expression], rm: ExpressionMatcher[Expression]): Matcher[DyadicTriple, (ExpressionBiFunction, Expression, Expression)] =
    matchProduct3All(fm, lm, rm)(DyadicTriple) :| "matchMonadicDuple"
}
