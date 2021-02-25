package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.matchers.{MatchLogger, Matchers}

import scala.language.implicitConversions

/**
  * Matchers whose input is generally an Expression.
  *
  * These Matchers are used to simplify (lazy) Expressions before those Expressions get evaluated,
  * thus sometimes avoiding loss of precision.
  */
class ExpressionMatchers(implicit val matchLogger: MatchLogger) extends Matchers {

  /**
    * Abstract class ExpressionMatcher which extends Matcher where input is always an Expression.
    *
    * @tparam R the MatchResult type.
    */
  abstract class ExpressionMatcher[+R] extends Matcher[Expression, R]

  /**
    * Implicit method to convert a Matcher[Expression, R] into an ExpressionMatcher[R].
    *
    * @param m a Matcher[Expression, R].
    * @tparam R the result type.
    * @return ExpressionMatcher[R]
    */
  implicit def matcherConverter[R](m: Matcher[Expression, R]): ExpressionMatcher[R] = ExpressionMatcher(m)

  /**
    * Method to create an ExpressionMatcher.
    *
    * @param f a function Expression => MatchResult[R]
    * @tparam R the MatchResult type.
    * @return a Matcher[Expression, R] which is also an ExpressionMatcher[R].
    */
  def ExpressionMatcher[R](f: Expression => MatchResult[R]): ExpressionMatcher[R] = (e: Expression) => f(e)

  /**
    * Matcher which either successfully simplifies the input expression, or evaluates it,
    * by means of the evaluator Matcher (below).
    *
    * @return an ExpressionMatcher[Expression].
    */
  def materializer: ExpressionMatcher[Expression] = (simplifier | evaluator) :| "materializer"

  /**
    * Matcher which materializes (in the expression sense) the given Expression.
    *
    * @return an ExpressionMatcher[Number].
    */
  def evaluator: ExpressionMatcher[Number] = lift[Expression, Number](t => t.materialize)

  /**
    * Method to match an Expression and replace it with a simplified expression.
    *
    * @return an ExpressionMatcher[Expression].
    */
  def simplifier: ExpressionMatcher[Expression] = (biFunctionSimplifier | functionSimplifier) :| "simplifier"

  /**
    * Method to match an Expression with is a BiFunction and replace it with a simplified expression.
    *
    * @return an ExpressionMatcher[Expression].
    */
  def biFunctionSimplifier: ExpressionMatcher[Expression] = matchBiFunction & (matchSimplifyPlus | matchSimplifyTimes | matchGatherer(Sum) | matchGatherer(Product) | matchGatherer(Power)) :| "biFunctionSimplifier"

  /**
    * Method to match an Expression with is a Function and replace it with a simplified expression.
    *
    * @return an ExpressionMatcher[Expression].
    */
  def functionSimplifier: ExpressionMatcher[Expression] =
    matchFunction & matchMonadicDuple(always, ExpressionMatcher(always)) :| "functionSimplifier"

  /**
    * Matcher which takes a DyadicTriple on + and, if appropriate, simplifies it to an Expression.
    * In particular, we simplify this following expression (in RPN) to 0:
    * x -1 * x +
    *
    * NOTE that the * operator in the following will invert the order of the incoming tuple if required.
    *
    * @return a Matcher[DyadicTriple, Expression]
    */
  def matchSimplifyPlus: Matcher[DyadicTriple, Expression] =
    matchDyadicBranches(Sum) & *(matchBiFunctionConstantResult(Product, Number(-1), Number.zero)) :| "matchSimplifyPlus"

  /**
    * Matcher which takes a DyadicTriple on * and, if appropriate, simplifies it to an Expression.
    * In particular, we simplify this following expression (in RPN) to 1:
    * x -1 power x *
    *
    * NOTE that the * operator in the following will invert the order of the incoming tuple if required.
    *
    * @return a Matcher[DyadicTriple, Expression]
    */
  def matchSimplifyTimes: Matcher[DyadicTriple, Expression] =
    matchDyadicBranches(Product) & *(matchBiFunctionConstantResult(Power, Number(-1), Number.one)) :| "matchSimplifyTimes"

  /**
    * Matcher which takes a specific ExpressionBiFunction h (such as Sum, Product, ...), a specific (typically, constant) Expression c,
    * and a specific result value, r (also an Expression).
    *
    * @param f the function to be matched.
    * @param c the constant to be matched.
    * @param r the result value (to be returned in the appropriate MatchResult).
    * @return a Matcher[(Expression, Expression), Expression].
    */
  def matchBiFunctionConstantResult(f: ExpressionBiFunction, c: Expression, r: Expression): Matcher[(Expression, Expression), Expression] =
    matchEitherDyadic & matchExpressionBiFunction(f) & matchAndSubstituteDyadicExpressions(c, r) :| s"matchBiFunctionConstantResult($f, $c, $r)"

  /**
    * Matcher of ((x, y), b) to r where b, x, y, r are all Expressions.
    * If c matches y, then b must match x.
    * If c matches x, then b must match y.
    *
    * TODO rewrite this in terms of Matchers.
    *
    * @param c an Expression which must match either of
    * @param r an Expression which will be returned on a successful match.
    * @return r assuming matching succeeds.
    */
  def matchAndSubstituteDyadicExpressions(c: Expression, r: Expression): Matcher[((Expression, Expression), Expression), Expression] =
    LoggingMatcher(s"matchAndSubstituteDyadicExpressions: $c, $r") {
      case ((x, y), b) if b.materialize == x.materialize && y.materialize == c.materialize => Match(r)
      case ((y, x), b) if b.materialize == x.materialize && y.materialize == c.materialize => Match(r)
    }

  /**
    * Matcher which takes a DyadicTriple on * and, if appropriate, simplifies it to an Expression.
    * In particular, we simplify this following expression (in RPN) to 1:
    * x -1 power x *
    *
    * NOTE that the * operator in the following will invert the order of the incoming tuple if required.
    *
    * @return a Matcher[DyadicTriple, Expression]
    */
  def matchGatherer(f: ExpressionBiFunction): Matcher[DyadicTriple, Expression] =
    matchDyadicBranches(f) & *(matchBiFunctionRepeated(f)) & gatherer(f) :| "matchGatherer"

  /**
    * Matcher which takes a specific ExpressionBiFunction h (such as Sum, Product, ...), a specific (typically, constant) Expression c,
    * and a specific result value, r (also an Expression).
    *
    * @param f the function to be matched.
    * @return a Matcher[(Expression, Expression), Expression].
    */
  def matchBiFunctionRepeated(f: ExpressionBiFunction): Matcher[(Expression, Expression), ((Expression, Expression), Expression)] =
    matchEitherDyadic & matchExpressionBiFunction(f) :| s"matchBiFunctionRepeated($f)"

  /**
    * Matcher which takes a tuple of BiFunction and Expression and returns an ((Expression, Expression), Expression).
    * The BiFunction is matched according to matchDyadicFunction(h) while the Expression is passed on unchanged.
    *
    * @param h an ExpressionBiFunction
    * @return
    */
  def matchExpressionBiFunction(h: ExpressionBiFunction): Matcher[(BiFunction, Expression), ((Expression, Expression), Expression)] =
  // CONSIDER why do we have to spell out the types just so we can add a logger here?
    filter2_0[BiFunction, Expression, (Expression, Expression)](matchDyadicFunction(h)) :| "matchExpressionBiFunction"

  /**
    * Matcher which takes a BiFunction and returns a tuple of two Expressions.
    * It succeeds if h matches the function of BiFunction.
    *
    * @param h an ExpressionBiFunction which must match the function of the input.
    * @return a tuple of two Expressions representing the first and second parameters of the BiFunction.
    */
  def matchDyadicFunction(h: ExpressionBiFunction): Matcher[BiFunction, (Expression, Expression)] =
    LoggingMatcher(s"matchDyadicFunction($h)") {
      case BiFunction(x, y, `h`) => Match(x -> y)
      case x => Miss(s"matchDyadicFunction($h)", x)
    }

  /**
    * Matcher which takes a tuple of Expressions and matches either element to a BiFunction and the remains as is.
    *
    * @return a tuple of BiFunction and Expression.
    */
  def matchEitherDyadic: Matcher[(Expression, Expression), (BiFunction, Expression)] =
    *(matchBiFunctionExpression) :| "matchEitherDyadic"

  /**
    * Matcher which takes a tuple of Expressions and matches the first to a BiFunction and leaves the other as is.
    *
    * @return a tuple of BiFunction and Expression.
    */
  def matchBiFunctionExpression: Matcher[(Expression, Expression), (BiFunction, Expression)] = LoggingMatcher("matchBiFunctionExpression") {
    case (f@BiFunction(_, _, _), x) => Match(f -> x)
    case z => Miss("matchBiFunctionExpression", z)
  }

  /**
    * Matcher of DyadicTriple to (Expression,Expression) which succeeds if the function of the DyadicTriple
    * matches f.
    *
    * NOTE: the result of this match is a tuple in the same order as the second and third elements of the DyadicTriple.
    * But, recall, that the ordering of those is essentially totally arbitrary. So, we will want to follow this
    * matcher with swapIfNecessary.
    *
    * @param f an ExpressionBiFunction
    * @return a tuple of left expression, right expression (from the DyadicTriple input).
    */
  def matchDyadicBranches(f: ExpressionBiFunction): Matcher[DyadicTriple, (Expression, Expression)] =
    LoggingMatcher(s"matchDyadicBranches: $f") {
      case DyadicTriple(`f`, l1, r1) => Match((l1, r1))
      case x => Miss("matchDyadicBranches", x)
    }

  /**
    * Matcher which takes a pair of Expressions and another Expression and combines them according to the parameter f.
    *
    * @param f Sum, Product of Power.
    * @return Matcher[((Expression, Expression), Expression), Expression].
    */
  def gatherer(f: ExpressionBiFunction): Matcher[((Expression, Expression), Expression), Expression] = Matcher {
    case ((x, y), z) => combineGather(f, x, y, z)
    case t => Miss("gatherer", t)
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
    case x => Miss("value", x)
  }

  /**
    * Matcher which matches on Expressions that directly represents a specific given Number.
    *
    * @param x the Number to match.
    * @return a Matcher[Expression, Number].
    */
  def matchValue(x: Number): ExpressionMatcher[Number] = value & matchNumber(x) :| "matchValue"

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
    * Matcher which matches a Function and results in a MonadicDuple.
    *
    * @return ExpressionMatcher[MonadicDuple]
    */
  def matchFunction: ExpressionMatcher[MonadicDuple] = {
    case Function(x, f) => Match(MonadicDuple(f, x))
    case e => Miss("matchFunction", e)
  }

  /**
    * Matcher which matches a BiFunction and results in a DyadicTriple.
    *
    * @return ExpressionMatcher[DyadicTriple]
    */
  def matchBiFunction: ExpressionMatcher[DyadicTriple] = {
    case BiFunction(a, b, f) => Match(DyadicTriple(f, a, b))
    case e => Miss("matchBiFunction", e)
  }

  // CONSIDER does this really make sense? We end up extracting just the expression, providing that the function matches OK.
  def matchMonadicDuple(fm: Matcher[ExpressionFunction, ExpressionFunction], om: ExpressionMatcher[Expression]): Matcher[MonadicDuple, Expression] =
    from2(fm ~> om)(MonadicDuple) :| "matchMonadicDuple"

  /**
    * Named Product to represent a BiFunction.
    *
    * CONSIDER do we really need this? Can't we do just fine with Tuples?
    *
    * @param f an ExpressionBiFunction.
    * @param l an Expression.
    * @param r an Expression.
    */
  case class DyadicTriple(f: ExpressionBiFunction, l: Expression, r: Expression)

  /**
    * Named Product to represent a Function.
    *
    * CONSIDER do we really need this? Can't we do just fine with Tuples?
    *
    * @param f an ExpressionBiFunction.
    * @param x an Expression.
    */
  case class MonadicDuple(f: ExpressionFunction, x: Expression)

  private def combineGather(f: ExpressionBiFunction, x: Expression, y: Expression, z: Expression) = f match {
    case Power =>
      (y * z).materialize.toInt match {
        case Some(p) => Match((x ^ p).materialize)
        case None => Miss(s"combine $f, $x, $y, $z missed", Number.zero) // NOTE this value is artificial
      }
    case Product =>
      Match((x * y * z).materialize)
    case Sum =>
      import com.phasmidsoftware.number.core.Expression.ExpressionOps
      Match((x + y + z).materialize)
  }


}
