package com.phasmidsoftware.number.core

import com.phasmidsoftware.matchers.{MatchLogger, ~}
import com.phasmidsoftware.number.matchers._

import scala.language.implicitConversions

/**
  * Matchers whose input is generally an Expression.
  *
  * These Matchers are used to simplify (lazy) Expressions before those Expressions get evaluated,
  * thus sometimes avoiding loss of precision.
  */
class ExpressionMatchers(implicit val matchLogger: MatchLogger) extends MatchersExtras {

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
    * Type alias for a dyadic triple (purpose of this is solely for brevity).
    */
  type DyadicTriple = ExpressionBiFunction ~ Expression ~ Expression

  /**
    * Method to create an ExpressionMatcher.
    *
    * @param f a function Expression => MatchResult[R]
    * @tparam R the MatchResult type.
    * @return a Matcher[Expression, R] which is also an ExpressionMatcher[R].
    */
  def ExpressionMatcher[R](f: Expression => MatchResult[R]): ExpressionMatcher[R] = (e: Expression) => f(e)

  def simplifyExpression(x: Expression): Expression = exactFieldMaterializer(x) match {
    case Match(f) => Expression(f)
    case Miss(_, _) => x
  }

  def exactFieldMaterializer: ExpressionMatcher[Field] = Matcher[Expression, Field]("exactFieldMaterializer")(exactMaterialization(None))

  def exactMaterializer: ExpressionMatcher[Expression] = exactFieldMaterializer map (Expression(_))

  // CONSIDER rewrite this as a Matcher
  def exactMaterialization(maybeFactor: Option[Factor])(x: Expression): MatchResult[Field] =
    if (x.isExact(maybeFactor)) Match(x.evaluate)
    else Miss("exactMaterialization: non-exact", x)

  def simplifyAndEvaluate(x: Expression): Field =
    simplifier(x) match {
      case Match(e) => e.evaluate
      case _ => x.evaluate
    }

  /**
    * Method to match an Expression and replace it with a simplified expression.
    * However, it will accept the replacement only if the replacement has an exact value.
    *
    * @return an Transformer.
    */
  def simplifier: Transformer = ExpressionMatcher {
    case a@AtomicExpression(_) => Match(a)
    case b@BiFunction(_, _, _) => biFunctionSimplifier(b)
    case f@Function(_, _) => functionSimplifier(f)
  } :| "simplifier"

  /**
    * Method to simplify a BiFunction.
    *
    * CONSIDER inlining this (but is used for unit tests).
    *
    * @return a Transformer.
    */
  def biFunctionSimplifier: Transformer = (matchBiFunction & biFunctionMatcher) :| "biFunctionSimplifier"

  /**
    * Matcher which always succeeds.
    * If either operand can be simplified, then it will be.
    *
    * @return
    */
  def matchSimplifyDyadicTerms: Matcher[DyadicTriple, DyadicTriple] = Matcher("matchSimplifyDyadicTerms") {
    case f ~ x ~ y => Match(f ~ simplifyExpression(x) ~ simplifyExpression(y))
  }

  /**
    * Matcher which always succeeds.
    * If either operand can be simplified, then it will be.
    *
    * @return
    */
  def matchSimplifyDyadicTermsTwoLevels: Matcher[DyadicTriple, DyadicTriple] = Matcher("matchSimplifyDyadicTermsTwoLevels") {
    case f ~ BiFunction(g, w, x) ~ BiFunction(h, y, z) =>
      Match(f ~ simplifyExpression(BiFunction(g, w, x)) ~ simplifyExpression(BiFunction(h, y, z)))
    case f ~ BiFunction(g, w, x) ~ y =>
      Match(f ~ simplifyExpression(BiFunction(g, w, x)) ~ y)
    case f ~ y ~ BiFunction(g, w, x) =>
      Match(f ~ y ~ simplifyExpression(BiFunction(g, w, x)))
    // CONSIDER should we match Function?
    case f ~ y ~ z =>
      // CONSIDER allowing these single-level expressions through
      Miss("matchSimplifyDyadicTermsTwoLevels: depth is only one level", f ~ y ~ z)
  }

  /**
    * Matcher which always succeeds.
    * If either operand can be simplified, then it will be.
    *
    * @return
    */
  def matchSimplifyMonadicTerm: Matcher[MonadicDuple, MonadicDuple] = Matcher("matchSimplifyMonadicTerm") {
    case f ~ x => Match(f ~ simplifyExpression(x))
  }

  /**
    * Method to match an Expression which is a BiFunction and replace it with a simplified expression.
    *
    * NOTE: there may be some redundancies here.
    *
    * @return an Matcher[DyadicTriple, Expression].
    */
  def biFunctionMatcher: Matcher[DyadicTriple, Expression] = matchSimplifyDyadicTerms &
          (evaluateExactDyadicTriple | matchDyadicTrivial | matchDyadicTwoLevels | matchMultiLevels) :| "biFunctionMatcher"
  // NOTE: for now we keep these comments showing the old method.
  //    (matchSimplifyBiFunction)
  //  (matchSimplifyBiFunction | matchSimplifySum | matchSimplifyProduct | matchSimplifyPowerIdentity | matchGatherer(Sum) | matchGatherer(Product) | matchGatherer(Power) | distributor) :| "biFunctionMatcher"

  /**
    * This method defines a single Matcher which combines the various two-level matchers which can be applied to an Expression.
    *
    * @return a Matcher.
    */
  def matchDyadicTwoLevels: Matcher[DyadicTriple, Expression] = matchSimplifyDyadicTermsTwoLevels & (
          (matchTwoDyadicLevels & matchAndCollectTwoDyadicLevels) |
                  (matchTwoDyadicLevelsL & (matchAndCancelTwoDyadicLevelsL | matchAndCollectTwoDyadicLevelsL)) |
                  (matchTwoDyadicLevelsR & (matchAndCancelTwoDyadicLevelsR | matchAndCollectTwoDyadicLevelsR)) |
                  fail("twoLevel")
          ) :| "matchDyadicTwoLevels"

  def matchMultiLevels: Matcher[DyadicTriple, Expression] = {
    case f ~ x ~ y =>
      val result = matchMultiLevelsInner(f, Nil)(f ~ x ~ y)
      combineTerms(f, result, Miss("matchMultiLevelsInner", x))
  }

  /**
    * Method to match an Expression which is a Function and replace it with a simplified expression.
    *
    * @return an Transformer.
    */
  def functionSimplifier: Transformer =
    (matchFunction & ((matchSimplifyMonadicTerm & evaluateMonadicDuple) | (matchTwoMonadicLevels & matchAndCancelTwoMonadicLevels))) :| "functionSimplifier"
  //  (matchFunction & matchSimplifyMonadicTerm & matchMonadicDuple(always, ExpressionMatcher(always))) :| "functionSimplifier"

  def evaluateMonadicDuple: Matcher[MonadicDuple, Expression] = Matcher("evaluateMonadicDuple") {
    case f ~ x => exactMaterialization(None)(Function(x, f)) map (Literal(_)) // CONSIDER use exactMaterializer
  }

  def matchTwoMonadicLevels: Matcher[MonadicDuple, ExpressionFunction ~ MonadicDuple] = Matcher("matchTwoMonadicLevels") {
    case f ~ Function(x, g) => Match(f ~ (g ~ x))
  }

  def matchAndCancelTwoMonadicLevels: Matcher[ExpressionFunction ~ MonadicDuple, Expression] = Matcher("matchAndCancelTwoMonadicLevels") {
    case f ~ (g ~ x) if complementaryMonadic(f, g) => Match(x)
    case f ~ (g ~ x) => Miss("cannot combine two monadic levels", f ~ (g ~ x))
  }

  def matchDyadicTrivial: Matcher[DyadicTriple, Expression] = Matcher("matchDyadicTrivial") {
    case Product ~ Zero ~ _ => Match(Zero)
    case Product ~ _ ~ Zero => Match(Zero)
    case Power ~ _ ~ Zero => Match(One)
    case Power ~ One ~ _ => matchAndSimplify(One)
    case Sum ~ Zero ~ x => matchAndSimplify(x)
    case Sum ~ x ~ Zero => matchAndSimplify(x)
    case Product ~ One ~ x => matchAndSimplify(x)
    case Product ~ x ~ One => matchAndSimplify(x)
    case Power ~ x ~ One => matchAndSimplify(x)
  }

  def evaluateExactDyadicTriple: Matcher[DyadicTriple, Expression] = Matcher("evaluateExactDyadicTriple") {
    case f ~ x ~ y => replaceExactBiFunction(None)(BiFunction(x, y, f))
  }

  def matchTwoDyadicLevels: Matcher[DyadicTriple, ExpressionBiFunction ~ DyadicTriple ~ DyadicTriple] = Matcher("matchTwoDyadicLevels") {
    case f ~ BiFunction(w, x, g) ~ BiFunction(y, z, h) => Match(f ~ (g ~ w ~ x) ~ (h ~ y ~ z))
  }

  def matchTwoDyadicLevelsL: Matcher[DyadicTriple, ExpressionBiFunction ~ DyadicTriple ~ Expression] = Matcher("matchTwoDyadicLevelsL") {
    case f ~ BiFunction(x, y, g) ~ z => Match(f ~ (g ~ x ~ y) ~ z)
  }

  def matchTwoDyadicLevelsR: Matcher[DyadicTriple, ExpressionBiFunction ~ Expression ~ DyadicTriple] = Matcher("matchTwoDyadicLevelsR") {
    case f ~ z ~ BiFunction(x, y, g) => Match(f ~ z ~ (g ~ x ~ y))
  }

  private def matchAndSimplify(x: Expression): MatchResult[Expression] = Match(x) flatMap simplifier

  private def matchMultiLevelsInner(f: ExpressionBiFunction, matches: Seq[MatchResult[Expression]])(input: ExpressionBiFunction ~ Expression ~ Expression): Seq[MatchResult[Expression]] = input match {
    // TODO we need to check context, e.g.  Some(Scalar) if f is Sum. For Product, it's not a problem
    case `f` ~ BiFunction(a, b, `f`) ~ BiFunction(c, d, `f`) if f != Power =>
      matchMultiLevelsInner(f, matchMultiLevelsInner(f, matches)(f ~ a ~ b) ++ matches)(f ~ c ~ d)
    case `f` ~ x ~ BiFunction(a, b, `f`) if f != Power =>
      matchMultiLevelsInner(f, exactMaterializer(x) +: matches)(f ~ a ~ b)
    case `f` ~ BiFunction(a, b, `f`) ~ y if f != Power =>
      matchMultiLevelsInner(f, exactMaterializer(y) +: matches)(f ~ a ~ b)
    case `f` ~ x ~ y if f != Power => // NOTE: this is solely to terminate the recursion (we should have some other illegal ExpressionBiFunction to use).
      matchMultiLevelsInner(f, exactMaterializer(x) +: exactMaterializer(y) +: matches)(Power ~ Zero ~ Zero)
    case _ =>
      matches
  }

  /**
    * Collect equal terms (fuzzy terms since this method won't be called if all the terms are exact) from an expression of the following two types:
    *
    * w power x * y power z where w = y => x power (y + z)
    * w * x + y * z where w = y => w * (x + z)
    *
    * CONSIDER pulling the flatMap simplifier out of each case and putting after the result is known. Applies to similar methods.
    *
    * @param f the higher level function.
    * @param g the lower level function of the left branch.
    * @param w the left/left expression.
    * @param x the left/right expression.
    * @param h the lower level function of the right branch.
    * @param y the right/left expression.
    * @param z the right/right expression.
    * @return a Match of the simplified expression, or a Miss.
    */
  private def collectTermsDyadicTwoLevels(f: ExpressionBiFunction, g: ExpressionBiFunction, w: Expression, x: Expression, h: ExpressionBiFunction, y: Expression, z: Expression): MatchResult[Expression] = (f, g, h) match {
    case (Product, Power, Power) if w == y =>
      replaceAndSimplify(x, z, w, Sum, Power)
    case (Product, Product, Power) if x == y && z == MinusOne =>
      Match(w) // NOTE this is a special case and I'm not sure it really belongs here
    case (Sum, Product, Product) if w == y =>
      replaceAndSimplify(x, z, w, Sum, Product)
    case (Sum, Product, Product) if w == z =>
      replaceAndSimplify(x, y, w, Sum, Product)
    case (Sum, Product, Product) if x == y =>
      replaceAndSimplify(w, z, x, Sum, Product)
    case (Sum, Product, Product) if x == z =>
      replaceAndSimplify(w, y, x, Sum, Product)
    case (Product, Sum, Sum) =>
      val terms = cartesianProduct(w ~ x, y ~ z) map matchExpressionPair(Product)
      combineTerms(Sum, terms, Miss("collectTermsDyadicTwoLevels: no *++ terms to collect", f ~ g ~ w ~ x ~ h ~ y ~ z))
    case _ =>
      Miss("collectTermsDyadicTwoLevels: functions don't match", f ~ g ~ w ~ x ~ h ~ y ~ z)
  }

  private def replaceAndSimplify(a: Expression, b: Expression, c: Expression, f: ExpressionBiFunction, g: ExpressionBiFunction) =
    replaceExactBiFunction(Some(Scalar))(BiFunction(a, b, f)) map formBiFunction(c, g) flatMap simplifier

  private def combineAndSimplify(w: Expression, x: Expression, function: ExpressionBiFunction) =
    replaceAndSimplify(One, x, w, Sum, function)

  private def matchExpressionPair(function: ExpressionBiFunction)(e: Expressions): MatchResult[Expression] =
    matchAndReplacePair(function, e.l, e.r)

  private def matchAndReplacePair(function: ExpressionBiFunction, x: Expression, y: Expression) =
    replaceExactBiFunction(Some(Scalar))(BiFunction(x, y, function))

  private def cartesianProduct(p1: Expression ~ Expression, p2: Expression ~ Expression): Seq[Expressions] =
    Seq(p1.l ~ p2.l, p1.r ~ p2.l, p1.l ~ p2.r, p1.r ~ p2.r)

  /**
    * Collect equal terms (fuzzy terms since this method won't be called if all the terms are exact) from an expression of the following two types:
    *
    * w power x * y where w = y => w power (y + 1)
    * w x + y where w = y => w * (x + 1)
    *
    * @param f the higher level function.
    * @param g the lower level function of the left branch.
    * @param w the left/left expression.
    * @param x the left/right expression.
    * @param y the right/right expression.
    * @return a Match of the simplified expression, or a Miss.
    */
  private def collectTermsDyadicTwoLevelsL(f: ExpressionBiFunction, g: ExpressionBiFunction, w: Expression, x: Expression, y: Expression): MatchResult[Expression] = (f, g) match {
    case (Product, Power) if w == y =>
      combineAndSimplify(w, x, Power)
    case (Sum, Product) if w == y =>
      combineAndSimplify(w, x, Product)
    case (Sum, Product) if x == y =>
      combineAndSimplify(x, w, Product)
    case (Product, Sum) =>
      val terms = Seq(w ~ y, x ~ y) map matchExpressionPair(Product)
      combineTerms(Sum, terms, Miss("collectTermsDyadicTwoLevelsL: no *++ terms to collect", f ~ g ~ w ~ x ~ y))
    case _ =>
      Miss("collectTermsDyadicTwoLevelsL: functions don't match", f ~ g ~ w ~ x ~ y)
  }

  /**
    * Collect equal terms (fuzzy terms since this method won't be called if all the terms are exact) from an expression of the following two types:
    *
    * w * x power y where w = x => w power (y + 1)
    * w + x * y where w = x => w * (y + 1)
    *
    * CONSIDER can we merge this code with collectTermsDyadicTwoLevelsL?
    *
    * @param f the higher level function.
    * @param g the lower level function of the left branch.
    * @param w the left/left expression.
    * @param x the left/right expression.
    * @param y the right/right expression.
    * @return a Match of the simplified expression, or a Miss.
    */
  private def collectTermsDyadicTwoLevelsR(f: ExpressionBiFunction, g: ExpressionBiFunction, w: Expression, x: Expression, y: Expression): MatchResult[Expression] = (f, g) match {
    case (Product, Power) if w == x =>
      combineAndSimplify(w, y, Power) // TODO refactor using method, etc...
    case (Sum, Product) if w == x =>
      combineAndSimplify(w, y, Product)
    case (Sum, Product) if w == y =>
      combineAndSimplify(w, x, Product)
    case (Product, Sum) =>
      val terms = Seq(w ~ x, w ~ y) map matchExpressionPair(Product)
      combineTerms(Sum, terms, Miss("collectTermsDyadicTwoLevelsR: no *++ terms to collect", f ~ g ~ w ~ x ~ y))
    case _ =>
      Miss("collectTermsDyadicTwoLevelsR: functions don't match", f ~ g ~ w ~ x ~ y)
  }

  private def combineTerms(function: ExpressionBiFunction, terms: Seq[MatchResult[Expression]], miss: MatchResult[Expression]): MatchResult[Expression] = {
    val (good, bad) = terms partition (_.successful)
    if (good.size < 2) miss
    else {
      val start = function match {
        case Sum => Number.zero
        case Product => Number.one
        case _ => throw ExpressionException(s"combineTerms: function not supported: $function")
      }
      val goodAccumulation: Number = good.foldLeft(start)((accum, term) => term match {
        case Match(t) =>
          function match {
            case Sum => accum doAdd t
            case Product => accum doMultiply t
            case _ => throw ExpressionException(s"combineTerms: function not supported: $function")
          }
        case _ => accum
      })
      val result: Expression = bad.foldLeft(Expression(goodAccumulation))((accum, term) => term match {
        case Miss(_, t: Expression) =>
          function match {
            case Sum => accum plus t
            case Product => accum * t
            case _ => throw ExpressionException(s"combineTerms: function not supported: $function")
          }
        case _ => accum
      })
      Match(result) flatMap simplifier
    }
  }

  private def formBiFunction(x: Expression, function: ExpressionBiFunction): Expression => Expression = BiFunction(x, _, function)

  // TODO redefine as a Matcher
  private def effectivelyExactMaterialization(function: BiFunction): MatchResult[Expression] = function match {
    case BiFunction(a, b, f) => matchDyadicTwoLevels(f ~ a ~ b)
  }

  // TODO redefine as a Matcher
  private def replaceExactBiFunction(maybeFactor: Option[Factor])(function: BiFunction): MatchResult[Expression] =
    (exactMaterialization(maybeFactor)(function) map (Expression(_))) || effectivelyExactMaterialization(function)  // CONSIDER use exactMaterializer

  /**
    * @see #collectTermsDyadicTwoLevels
    *      * Collect equal terms (fuzzy terms since this method won't be called if all the terms are exact) from an expression of the following two types:
    *
    *      w power x * y power z where w = y => x power (y + z)
    *      w * x + y * z where w = y => w * (x + z)
    * @return a Match of the simplified expression, or a Miss.
    */
    // private
   def matchAndCollectTwoDyadicLevels: Matcher[ExpressionBiFunction ~ DyadicTriple ~ DyadicTriple, Expression] = Matcher("matchAndCollectTwoDyadicLevels") {
    case f ~ (g ~ w ~ x) ~ (h ~ y ~ z) => collectTermsDyadicTwoLevels(f, g, w, x, h, y, z)
  }

  /**
    * @see #collectTermsDyadicTwoLevels
    *      * Collect equal terms (fuzzy terms since this method won't be called if all the terms are exact) from an expression of the following two types:
    *
    *      w power x * y power z where w = y => x power (y + z)
    *      w * x + y * z where w = y => w * (x + z)
    * @return a Match of the simplified expression, or a Miss.
    */
  private def matchAndCollectTwoDyadicLevelsL: Matcher[ExpressionBiFunction ~ DyadicTriple ~ Expression, Expression] = Matcher("matchAndCancelTwoDyadicLevelsL") {
    case f ~ (g ~ w ~ x) ~ y => collectTermsDyadicTwoLevelsL(f, g, w, x, y)
  }

  /**
    * @see #collectTermsDyadicTwoLevels
    *      * Collect equal terms (fuzzy terms since this method won't be called if all the terms are exact) from an expression of the following two types:
    *
    *      w power x * y power z where w = y => x power (y + z)
    *      w * x + y * z where w = y => w * (x + z)
    * @return a Match of the simplified expression, or a Miss.
    */
  private def matchAndCollectTwoDyadicLevelsR: Matcher[ExpressionBiFunction ~ Expression ~ DyadicTriple, Expression] = Matcher("matchAndCancelTwoDyadicLevelsL") {
    case f ~ x ~ (g ~ y ~ z) => collectTermsDyadicTwoLevelsR(f, g, x, y, z)
  }

  /**
    * Take a common commutative function f and three expressions, for example x + y + z, and find any two exact expressions
    * which can be combined.
    * For example, if y and z are exact, we replace the parameters by x + (y + z) where the second term is an atomic exact expression.
    *
    * @param f the common commutative, associative function.
    * @param x an expression.
    * @param y an expression.
    * @param z an expression.
    * @return a match of a new expression or a miss of the originals.
    */
  private def combineExact(f: ExpressionBiFunction, x: Expression, y: Expression, z: Expression): MatchResult[Expression] = {
    // TODO this is OK for multiplication but additive terms need to check that they are in the same factor context.
    val list = List(x, y, z) map (x => exactMaterializer(x))
    combineTerms(f, list, Miss("nothing to combine", f ~ x ~ y ~ z))
  }

  private def matchAndCancelTwoDyadicLevelsL: Matcher[ExpressionBiFunction ~ DyadicTriple ~ Expression, Expression] = {
    case f ~ (g ~ x ~ y) ~ z if f == g && f != Power => combineExact(f, x, y, z)
    case f ~ (g ~ x ~ y) ~ z => associativeDyadic(f, g) match {
      case Some(d) => replaceExactBiFunction(Some(Scalar))(BiFunction(x, BiFunction(z, y, d), f))
      case None => Miss(s"cannot combine two dyadic levels ($f and $g do not associate)", x)
    }
  }

  private def matchAndCancelTwoDyadicLevelsR: Matcher[ExpressionBiFunction ~ Expression ~ DyadicTriple, Expression] = { // NOTE this layout doesn't work for power but it would work for other operators.
    case f ~ z ~ (g ~ x ~ y) if f == g && f != Power => combineExact(f, x, y, z)
    //    case f ~ z ~ (g ~ x ~ y) if associativeDyadic(f, g) => Match(x ^ (z * y))
    case x => Miss("matchAndCancelTwoDyadicLevelsR: cannot combine two dyadic levels", x)
  }

  private def complementaryMonadic(f: ExpressionFunction, g: ExpressionFunction) = (f, g) match {
    case (Exp, Log) => true
    case (Log, Exp) => true
    case _ => false
  }

  // CONSIDER inlining this
  private def associativeDyadic(f: ExpressionBiFunction, g: ExpressionBiFunction): Option[ExpressionBiFunction] = (f, g) match {
    case (Power, Power) => Some(Product)
    case _ => None
  }

  // OLD STUFF

  /**
    * Matcher which takes a DyadicTriple on + and, if appropriate, simplifies it to an Expression.
    * In particular, we simplify this following expression (in RPN) to 0:
    * x -1 * x
    * Also, we can simplify expressions with repeated adjacent Sum operators.
    *
    * NOTE that the * operator in the following will invert the order of the incoming tuple if required.
    *
    * @return a Matcher[DyadicTriple, Expression]
    */
  def matchSimplifyBiFunction: Matcher[DyadicTriple, Expression] = Matcher("matchSimplifyBiFunction") {
    case f ~ x ~ y if resultExact(f, x, y) => Match(Expression(f(x.materialize, y.materialize)))
  }

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
  // NOTE that the naming operator doesn't have any effect when all composition is via the & operator.
    (matchEitherDyadic(commutes = true) & matchExpressionBiFunction(f) & matchAndSubstituteDyadicExpressions(c, r)) :| s"matchBiFunctionConstantResult($f, $c, $r)"

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
    Matcher(s"matchAndSubstituteDyadicExpressions: $c, $r") {
      case x ~ y ~ b if b.materialize == x.materialize && y.materialize == c.materialize => Match(r)
      case y ~ x ~ b if b.materialize == x.materialize && y.materialize == c.materialize => Match(r)
      // NOTE non-match will be caught.
    }

  /**
    * Matcher which takes a BiFunction ~ Expression and returns an Expressions ~ Expression.
    * The BiFunction is matched according to matchDyadicFunction(h) while the Expression is passed on unchanged.
    *
    * @param f an ExpressionBiFunction
    * @return a Matcher[BiFunction ~ Expression, Expressions ~ Expression].
    */
  def matchExpressionBiFunction(f: ExpressionBiFunction): Matcher[BiFunction ~ Expression, Expressions ~ Expression] =
  // CONSIDER why do we have to spell out the types just so we can add a logger here?
    filter2_0[BiFunction, Expression, Expressions](matchDyadicFunction(f)) :| s"matchExpressionBiFunction: $f"

  /**
    * Matcher which takes a BiFunction and returns a ~ of two Expressions.
    * It succeeds if h matches the function of BiFunction.
    * The resulting MatchResult is made up of two Expressions representing the first and second parameters of the BiFunction.
    *
    * @param h an ExpressionBiFunction which must match the function of the input.
    * @return a Matcher[BiFunction, Expressions].
    */
  def matchDyadicFunction(h: ExpressionBiFunction): Matcher[BiFunction, Expressions] =
    Matcher(s"matchDyadicFunction($h)") {
      case BiFunction(x, y, `h`) => Match(x) ~ Match(y)
      case x => Miss(s"matchDyadicFunction($h)", x)
    }

  /**
    * Matcher which takes a ~ of Expressions and matches either element to a BiFunction and the other as is.
    *
    * @param commutes whether or not to allow flipping the input members of the incoming ~.
    * @return a BiFunction ~ Expression.
    */
  def matchEitherDyadic(commutes: Boolean): Matcher[Expressions, BiFunction ~ Expression] =
    *(matchBiFunctionExpression, commutes) :| "matchEitherDyadic"

  /**
    * Matcher which takes a ~ of Expressions and matches the first to a BiFunction and leaves the other as is.
    *
    * @return a BiFunction ~ Expression.
    */
  def matchBiFunctionExpression: Matcher[Expressions, BiFunction ~ Expression] =
    Matcher("matchBiFunctionExpression") {
      case (f: BiFunction) ~ x => Match(f) ~ Match(x)
      case z => Miss("matchBiFunctionExpression", z)
    }

  /**
    * Method to simplify a product of two expressions.
    *
    * CONSIDER making this private and testing it via PrivateMethodTester.
    *
    * @param u the first expression.
    * @param v the second expression.
    * @return a possibly simplified version of their product.
    */
  // TODO eliminate simplify
  def simplifyProduct(u: Expression, v: Expression): Expression = (u * v).simplify

  /**
    * Matcher which matches on Expressions that directly represent Numbers.
    *
    * @return an ExpressionMatcher[Field].
    */
  def value: ExpressionMatcher[Field] = {
    case Literal(x) => Match(x)
    case x@Number(_, _) => Match(x)
    case x: Constant => Match(x.materialize)
    case x => Miss("value", x)
  }

  /**
    * Matcher which matches on Expressions that directly represents a specific given Field.
    *
    * @param x the Number to match.
    * @return a Matcher[Expression, Number].
    */
  def matchValue(x: Field): ExpressionMatcher[Field] = (value & matchNumber(x)) :| s"matchValue($x)"

  /**
    * Matcher to match a specific Number.
    *
    * @param x the Number to match.
    * @return a Matcher[Field, Field] which matches only on x.
    */
  def matchNumber(x: Field): Matcher[Field, Field] =
    Matcher("matchNumber") {
      case `x` => Match(x)
      case e => Miss("matchNumber", e)
    }

  /**
    * Matcher which matches a Function and results in a MonadicDuple.
    *
    * @return ExpressionMatcher[MonadicDuple]
    */
  def matchFunction: ExpressionMatcher[MonadicDuple] = ExpressionMatcher {
    case Function(x, f) => Match(f ~ x)
    case e => Miss("matchFunction", e)
  }.named("matchFunction")

  /**
    * Matcher which matches a BiFunction and results in a DyadicTriple.
    *
    * @return ExpressionMatcher[DyadicTriple]
    */
  def matchBiFunction: ExpressionMatcher[DyadicTriple] = ExpressionMatcher {
    case BiFunction(a, b, f) => Match(f ~ a ~ b)
    case e => Miss("matchBiFunction", e)
  }.named("matchBiFunction")

  // CONSIDER the actual result check may not be significant.
  // CONSIDER we should use conditionallyExact to be faster.
  // NOTE this is no longer used
  private def resultExact(f: ExpressionBiFunction, x: Expression, y: Expression) =
    x.isExact(None) && y.isExact(None) && BiFunction(x, y, f).materialize.isExact(None)
  //&& f.isExact //&& BiFunction(x, y, f).conditionallyExact(f, x, y)

  val logger: MatchLogger = matchLogger
}
