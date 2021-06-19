package com.phasmidsoftware.number.core

import com.phasmidsoftware.matchers.{LogLevel, MatchLogger, ~}
import com.phasmidsoftware.number.matchers._

import scala.language.implicitConversions

/**
  * Matchers whose input is generally an Expression.
  *
  * These Matchers are used to simplify (lazy) Expressions before those Expressions get evaluated,
  * thus sometimes avoiding loss of precision.
  */
class ExpressionMatchers(implicit val ll: LogLevel, val matchLogger: MatchLogger) extends MatchersExtras {

  /**
    * Abstract class ExpressionMatcher which extends Matcher where input is always an Expression.
    *
    * @tparam R the MatchResult type.
    */
  abstract class ExpressionMatcher[+R] extends Matcher[Expression, R]

  /**
    * Implicit method to convert a Matcher[Expression, R] into an ExpressionMatcher[R].
    *
    * CONSIDER if we used a type alias for ExpressionMatcher, would we still need this?
    *
    * @param m a Matcher[Expression, R].
    * @tparam R the result type.
    * @return ExpressionMatcher[R]
    */
  implicit def matcherConverter[R](m: Matcher[Expression, R]): ExpressionMatcher[R] = ExpressionMatcher(m)

  /**
    * Type alias for the kind of ExpressionMatcher which results in a possibly different Expression.
    */
  type Transformer = ExpressionMatcher[Expression]

  /**
    * Type alias for a pair of expressions (purpose of this is solely for brevity).
    */
  type Expressions = Expression ~ Expression

  /**
    * Type alias for a monadic duple (purpose of this is solely for brevity).
    */
  type MonadicDuple = ExpressionFunction ~ Expression

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
    * @return an Transformer.
    */
  def materializer: Transformer = simplifier :| "simplifier"

  /**
    * Matcher which materializes (in the expression sense) the given Expression.
    *
    * @return an ExpressionMatcher[Number].
    */
  def evaluator: ExpressionMatcher[Number] = lift[Expression, Number](t => t.materialize)

  /**
    * Method to match an Expression and replace it with a simplified expression.
    * However, it will accept the replacement only if the replacement has an exact value.
    *
    * @return an Transformer.
    */
  def simplifier: Transformer = simpler(biFunctionSimplifier | functionSimplifier)

  /**
    * Method to match an Expression which is a BiFunction and replace it with a simplified expression.
    *
    * @return an Transformer.
    */
  def biFunctionSimplifier: Transformer =
    matchBiFunction & (matchSimplifyPlus | matchSimplifyPlusIdentity | matchSimplifyTimes | matchSimplifyTimesIdentity | matchSimplifyPowerIdentity | matchGatherer(Sum) | matchGatherer(Product) | matchGatherer(Power) | distributor) :| "biFunctionSimplifier"

  /**
    * Method to match an Expression which is a Function and replace it with a simplified expression.
    *
    * @return an Transformer.
    */
  def functionSimplifier: Transformer =
    matchFunction & matchMonadicDuple(always, ExpressionMatcher(always)) :| "functionSimplifier"

  /**
    * Type alias for a dyadic triple (purpose of this is solely for brevity).
    */
  type DyadicTriple = ExpressionBiFunction ~ Expression ~ Expression

  /**
    * This simplification will succeed if an expression is of one of the following forms:
    * (a + b) * c will become a * c + b * c; or
    * (a * b) power c will become a power c * b power c.
    *
    * @return Matcher[DyadicTriple, Expression]
    */
  def distributor: Matcher[DyadicTriple, Expression] =
    (matchDyadicBranches(Product) & *(distributeSum) | matchDyadicBranches(Power) & distributeProduct) :| "distributor"

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
    * Matcher which takes a DyadicTriple on + and, if appropriate, simplifies it to an Expression.
    * In particular, we simplify this following expression to x:  x + 0 or 0 + x.
    *
    * @return a Matcher[DyadicTriple, Expression]
    */
  def matchSimplifyPlusIdentity: Matcher[DyadicTriple, Expression] =
    matchDyadicBranches(Sum) & *(matchAndEliminateIdentity(Sum)) :| "matchSimplifyPlusIdentity"

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
    * Matcher which takes a DyadicTriple on + and, if appropriate, simplifies it to an Expression.
    * In particular, we simplify this following expression to x:  x * 1 or 1 * x.
    *
    * @return a Matcher[DyadicTriple, Expression]
    */
  def matchSimplifyTimesIdentity: Matcher[DyadicTriple, Expression] =
    matchDyadicBranches(Product) & *(matchAndEliminateIdentity(Product)) :| "matchSimplifyTimesIdentity"

  /**
    * Matcher which takes a DyadicTriple on + and, if appropriate, simplifies it to an Expression.
    * In particular, we simplify this following expression to x:  x to the power of 1.
    * And furthermore, we simplify this following expression to 1:  1 to the power of x.
    *
    * @return a Matcher[DyadicTriple, Expression]
    */
  def matchSimplifyPowerIdentity: Matcher[DyadicTriple, Expression] =
    matchDyadicBranches(Power) & matchAndEliminateIdentity(Power) :| "matchSimplifyPowerIdentity"

  /**
    * Matcher which simplifies a specific ExpressionBiFunction f (such as Sum, Product, ...), a specific (typically, constant) Expression c,
    * and a specific result value, r (also an Expression).
    *
    * For example, if p = matchBiFunctionConstantResult(Product, negativeOne, zero)
    * then p(one ~ BiFunction(one, negativeOne, Product)) will evaluate to zero.
    *
    * @param f the function to be matched.
    * @param c the constant to be matched.
    * @param r the result value (to be returned in the appropriate MatchResult).
    * @return a Matcher[Expressions, Expression].
    */
  def matchBiFunctionConstantResult(f: ExpressionBiFunction, c: Expression, r: Expression): Matcher[Expressions, Expression] =
  // CONSIDER replacing true with f.commutes
    matchEitherDyadic(true) & matchExpressionBiFunction(f) & matchAndSubstituteDyadicExpressions(c, r) :| s"matchBiFunctionConstantResult($f, $c, $r)"

  /**
    * Matcher which takes a specific ExpressionBiFunction f (such as Sum, Product, ...),
    * and determines whether a term/factor is an identity which can be eliminated.
    *
    * @param f the ExpressionBiFunction which will determine if the input Expressions is an identity.
    * @return a Matcher[Expressions, Expression].
    */
  def matchAndEliminateIdentity(f: ExpressionBiFunction): Matcher[Expressions, Expression] = Matcher {
    case x ~ y => eliminateIdentity(f, x, y)
    case t => Miss("matchAndEliminateIdentity", t)
  }

  /**
    * Matcher of x ~ y ~ b to r where b, x, y, r are all Expressions.
    * If c matches y, then b must match x.
    * If c matches x, then b must match y.
    *
    * TODO rewrite this in terms of Matchers.
    *
    * @param c an Expression which must match either of
    * @param r an Expression which will be returned on a successful match.
    * @return r assuming matching succeeds.
    */
  def matchAndSubstituteDyadicExpressions(c: Expression, r: Expression): Matcher[Expressions ~ Expression, Expression] =
    namedMatcher(s"matchAndSubstituteDyadicExpressions: $c, $r") {
      case x ~ y ~ b if b.materialize == x.materialize && y.materialize == c.materialize => Match(r)
      case y ~ x ~ b if b.materialize == x.materialize && y.materialize == c.materialize => Match(r)
      // NOTE non-match will be caught.
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
    matchDyadicBranches(f) & *(matchBiFunctionRepeated(f), f.commutes) & gatherer(f) :| "matchGatherer"

  /**
    * Matcher which takes a specific ExpressionBiFunction (such as Sum, Product, ...) and returns a Matcher
    * of type Expressions => Expressions ~ Expression.
    *
    * @param f the function to be matched.
    * @return a Matcher[Expressions, Expressions ~ Expression].
    */
  def matchBiFunctionRepeated(f: ExpressionBiFunction): Matcher[Expressions, Expressions ~ Expression] =
    matchEitherDyadic(f.commutes) & matchExpressionBiFunction(f) :| s"matchBiFunctionRepeated($f)"

  /**
    * Matcher which takes a BiFunction ~ Expression and returns an Expressions ~ Expression.
    * The BiFunction is matched according to matchDyadicFunction(h) while the Expression is passed on unchanged.
    *
    * @param f an ExpressionBiFunction
    * @return a Matcher[BiFunction ~ Expression, Expressions ~ Expression].
    */
  def matchExpressionBiFunction(f: ExpressionBiFunction): Matcher[BiFunction ~ Expression, Expressions ~ Expression] =
  // CONSIDER why do we have to spell out the types just so we can add a logger here?
    filter2_0[BiFunction, Expression, Expressions](matchDyadicFunction(f)) :| "matchExpressionBiFunction"

  /**
    * Matcher which takes a BiFunction and returns a ~ of two Expressions.
    * It succeeds if h matches the function of BiFunction.
    * The resulting MatchResult is made up of two Expressions representing the first and second parameters of the BiFunction.
    *
    * @param h an ExpressionBiFunction which must match the function of the input.
    * @return a Matcher[BiFunction, Expressions].
    */
  def matchDyadicFunction(h: ExpressionBiFunction): Matcher[BiFunction, Expressions] =
    namedMatcher(s"matchDyadicFunction($h)") {
      case BiFunction(x, y, `h`) => Match(x) ~ Match(y)
      case x => Miss(s"matchDyadicFunction($h)", x)
    }

  /**
    * Matcher which takes a ~ of Expressions and matches either element to a BiFunction and the other as is.
    *
    * @param flip whether or not to allow flipping the input members of the incoming ~.
    * @return a BiFunction ~ Expression.
    */
  def matchEitherDyadic(flip: Boolean): Matcher[Expressions, BiFunction ~ Expression] =
    *(matchBiFunctionExpression, flip) :| "matchEitherDyadic"

  /**
    * Matcher which takes a ~ of Expressions and matches the first to a BiFunction and leaves the other as is.
    *
    * @return a BiFunction ~ Expression.
    */
  def matchBiFunctionExpression: Matcher[Expressions, BiFunction ~ Expression] =
    namedMatcher("matchBiFunctionExpression") {
      case (f: BiFunction) ~ x => Match(f) ~ Match(x)
      case z => Miss("matchBiFunctionExpression", z)
    }

  /**
    * Matcher of DyadicTriple to Expressions which succeeds if the ExpressionBiFunction of the DyadicTriple
    * matches f.
    *
    * NOTE: the result of this match is a ~ in the same order as the second and third elements of the DyadicTriple.
    * But, recall, that the ordering of those is essentially totally arbitrary. So, we will want to follow this
    * matcher with swapIfNecessary.
    *
    * @param f an ExpressionBiFunction
    * @return a Matcher[DyadicTriple, Expressions].
    */
  def matchDyadicBranches(f: ExpressionBiFunction): Matcher[DyadicTriple, Expressions] =
    namedMatcher(s"matchDyadicBranches: $f") {
      case `f` ~ l1 ~ r1 => Match(l1) ~ Match(r1)
      case x => Miss("matchDyadicBranches", x)
    }

  /**
    * Do the distribution of the form: (a + b) * c
    *
    * @return a Matcher[Expressions, Expression]
    */
  def distributeSum: Matcher[Expressions, Expression] = Matcher {
    case x ~ y => x match {
      case BiFunction(a, b, Sum) => y match {
        case BiFunction(c, d, Sum) => Match(BiFunction((simplifyProduct(a, c) plus simplifyProduct(a, d)).simplify, (simplifyProduct(b, c) plus simplifyProduct(b, d)).simplify, Sum).simplify)
        case _ => Match(BiFunction((a * y).simplify, (b * y).simplify, Sum).simplify)
      }
      case t => Miss("distributeSum", t)
    }
  }

  def simplifyProduct(u: Expression, v: Expression): Expression =
    (u * v).simplify

  /**
    * Do the distribution of the form: (a * b) power c
    *
    * @return a Matcher[Expressions, Expression]
    */
  def distributeProduct: Matcher[Expressions, Expression] = Matcher {
    case x ~ y => x match {
      case BiFunction(a, b, Product) => Match(BiFunction((a ^ y).simplify, (b ^ y).simplify, Product).simplify)
      case t => Miss("distributeProduct", t)
    }
  }

  /**
    * Matcher which takes a ~~ of Expressions and combines them according to the parameter f.
    *
    * @param f Sum, Product or Power.
    * @return Matcher[Expressions ~ Expression, Expression].
    */
  def gatherer(f: ExpressionBiFunction): Matcher[Expressions ~ Expression, Expression] = Matcher {
    case x ~ y ~ z => combineGather(f, x, y, z)
    case t => Miss("gatherer", t)
  }

  /**
    * Matcher which matches on Expressions that directly represent Numbers.
    *
    * @return a ExpressionMatcher[Number].
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
    case Function(x, f) => Match(f ~ x)
    case e => Miss("matchFunction", e)
  }

  /**
    * Matcher which matches a BiFunction and results in a DyadicTriple.
    *
    * @return ExpressionMatcher[DyadicTriple]
    */
  def matchBiFunction: ExpressionMatcher[DyadicTriple] = {
    case BiFunction(a, b, f) => Match(f ~ a ~ b)
    case e => Miss("matchBiFunction", e)
  }

  // CONSIDER does this really make sense? We end up extracting just the expression, providing that the function matches OK.
  def matchMonadicDuple(fm: Matcher[ExpressionFunction, ExpressionFunction], om: Transformer): Matcher[MonadicDuple, Expression] =
    fm ~> om :| "matchMonadicDuple"

  /**
    * Private method to return a MatchResult, given an ExpressionBiFunction, and three Expressions: x, y, and z.
    * If f is Power, then we will return a match, provided that y * z is an integer, of x to the power of y * z.
    * If f is Product, then we will return a match of x * y * z.
    * If f is Sum, then we will return a match of x + y + z.
    *
    * CONSIDER why do we only gather triples together? Shouldn't we do the same thing for duples?
    *
    * @param f an ExpressionBiFunction.
    * @param x an Expression.
    * @param y an Expression.
    * @param z an Expression.
    * @return a MatchResult[Number].
    */
  private def combineGather(f: ExpressionBiFunction, x: Expression, y: Expression, z: Expression): MatchResult[Expression] =
    f match {
      case Power =>
        (y * z).materialize match {
          case n@ExactNumber(_, _) =>
            Match(x ^ n)
          case _ =>
            Miss(s"combineGather $f, $x, $y, $z missed", Number.NaN)
        }
      case Product =>
        // CONSIDER implementing this for the other pairs: x, z and y, z.
        (x * y).materialize match {
          case n@ExactNumber(_, _) => Match(n * z)
          case _ => Miss(s"combineGather $f, $x, $y, $z missed", Number.NaN)
        }
      case Sum =>
        import com.phasmidsoftware.number.core.Expression.ExpressionOps // CONSIDER why do we need this only for "+"?
        (x + y).materialize match {
          case n@ExactNumber(_, _) => Match(n + z)
          case _ => Miss(s"combineGather $f, $x, $y, $z missed", Number.NaN)
        }
    }

  private def eliminateIdentity(f: ExpressionBiFunction, x: Expression, c: Expression): MatchResult[Expression] =
    (f, x.materialize, c.materialize) match {
      case (Power, _, Number.one) | (Product, _, Number.one) | (Sum, _, Number.zero) => Match(x)
      case (Power, Number.one, _) => Match(Number.one)
      case _ => Miss(s"eliminateIdentity $f, $x, $c missed", Number.NaN)
    }

  private def simpler(m: Transformer): Transformer = Matcher[Expression, Expression] {
    r =>
      m(r) match {
        case z@Match(x) =>
          if (x.isExact || x.depth < r.depth)
            z
          else
            Miss("simplified version is neither exact nor simpler", x)
        case _ =>
          Miss("simplifier", r)
      }
  }

  //noinspection ScalaUnusedSymbol
  // NOTE unused method but keeping it for now.
  private def exact: Transformer = filter[Expression] ( _.isExact )
}
