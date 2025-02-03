/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.matchers.{MatchLogger, ~}
import com.phasmidsoftware.number.matchers._

import scala.annotation.tailrec
import scala.language.implicitConversions

/**
  * Matchers whose input is generally an Expression.
  *
  * These Matchers are used to simplify (lazy) Expressions before those Expressions get evaluated,
  * thus sometimes avoiding loss of precision.
  *
  * Rules for matching:
  * (1) All substitution is based on a successful match -- no match no substitution;
  * (2) The first match to try for any expression is the exactMaterializer which returns Match(Literal(value));
  * (2) Some matches return non-exact match results -- these should be passed to flatMap simplifier;
  * NOTE: do not pass anything to flatMap simplifier if it could possibly be the same as the input (else stack overflow).
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
  private type Transformer = ExpressionMatcher[Expression]

  /**
    * Type alias for a pair of expressions (purpose of this is solely for brevity).
    */
  private type Expressions = Expression ~ Expression

  /**
    * Type alias for a monadic duple (purpose of this is solely for brevity).
    */
  private type MonadicDuple = ExpressionFunction ~ Expression

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

  /**
   * Creates and returns an expression matcher that filters expressions based on the `isExact` property.
   *
   * @return An ExpressionMatcher that matches only expressions with the `isExact` property set to true.
   */
  def exactExpressionMatcher: ExpressionMatcher[Expression] = ExpressionMatcher[Expression](filter(_.isExact))

  /**
   * Matcher which tries to evaluate the input exactly as a Scalar and then wraps the result as an Expression.
    *
    * @return Matcher[Expression, Expression]
    */
  def exactMaterializer: ExpressionMatcher[Expression] =
    // TODO change None to Some(Scalar)
    exactFieldMaterializer(None) map (Expression(_))

  /**
    * Matcher which takes an optional Factor and, if the input Expression is exact, it returns a Match of the value.
    *
    * @param maybeFactor the (optional) context in which we want to evaluate this Expression.
    *                    if factor is None then, the result will depend solely on whether this is exact.
   *                     CONSIDER renaming all of these maybeFactor parameters as context.
    * @return a Matcher[Expression, Field]
    */
  def exactMaterialization(maybeFactor: Option[Factor]): ExpressionMatcher[Field] = {
    x =>
      if (x.isExactByFactor(maybeFactor))
        Match(x.evaluate)
      else
        Miss("exactMaterialization: non-exact", x)
  }

  /**
    * This method is called by materializer and therefore will return a Field, regardless of whether any simplifications
    * were possible.
    *
    * CONSIDER both cases should be normalized.
    *
    * @param x the Expression to be evaluated as a Field.
    * @return the value of the original Expression or a simplified version of the Expression.
    */
  def simplifyAndEvaluate(x: Expression): Field =
    simplifier(x) match {
      case Match(e) =>
        val result: Field = e.evaluate
        if (result.isExact)
          result
        else
          result.normalize
      case _ => x.evaluate
    }

  /**
    * Method to match an Expression and replace it with a simplified expression, according to the type of Expression.
    *
    * @return an Transformer.
    */
  def simplifier: Transformer = ExpressionMatcher {
    case a@AtomicExpression(_) => Match(a)
    case e => e match {
      case b@BiFunction(_, _, _) => biFunctionSimplifier(b)
      case f@Function(_, _) => functionSimplifier(f)
      case t@Total(_) => totalSimplifier(t)
    }
  } :| "simplifier"

  /**
    * Method to simplify a BiFunction if possible.
    * If the input is a BiFunction, it will be transformed into a DyadicTriple and passed to biFunctionMatcher.
    *
    * @return a Transformer.
    */
  def biFunctionSimplifier: Transformer = (matchBiFunction & biFunctionMatcher) :| "biFunctionSimplifier"

  /**
   * Method to simplify a Total if possible.
   * If the input is a Total, simplification is if all elements are exact and can be reduced to zero, one, or two elements.
   *
   * @return a Transformer.
   */
  def totalSimplifier: Transformer = (matchTotal & totalMatcher) :| "totalSimplifier"

  /**
   * Matcher which always succeeds.
   * If either operand can be simplified, then it will be.
   *
   * @return a Match of an exact expression.
   */
  private def matchSimplifyTotalTerms: Matcher[Expression, Expression] = Matcher("matchSimplifyTotalTerms") {
    case Total(xs) =>
      if (xs.forall(_.asNumber.isDefined)) {
        // NOTE we should handle the very rare cases where the final get fails
        val sorted = xs.sortBy(x => Math.abs(x.asNumber.get.toDouble.get))

        @tailrec
        def inner(es: Seq[Expression], first: Boolean): Seq[Expression] = {
          val zs: Seq[Expression] = es.grouped(2).foldLeft(Seq.empty[Expression]) {
            case (accum, Seq(x1, x2)) =>
              val q = x1 plus x2
              if (q.isExact) accum :+ q
              else accum :+ x1 :+ x2
            case (accum, Seq(x)) => accum :+ x
            case (_, Nil) => throw new NoSuchFieldError("matchSimplifyTotalTerms: inner: logic error")
          }
          if (zs.size < es.size)
            inner(zs, first)
          else if (first && zs.size > 2)
            inner(zs.tail, first = false)
          else
            zs
        }

        inner(sorted, true) match {
          case Nil => Match(Constants.zero)
          case x :: Nil => Match(x)
          case x :: y :: Nil => Match(x plus y)
          case _ => Miss("matchSimplifyTotalTerms: cannot be simplified", xs)
        }
      } else Miss("matchSimplifyTotalTerms: not all elements are numbers", xs)
  }

  /**
    * Matcher which always succeeds.
    * If either operand can be simplified, then it will be.
    *
    * @return a Match of an exact expression.
    */
  def matchSimplifyDyadicTerms: Matcher[DyadicTriple, Expression] = Matcher("matchSimplifyDyadicTerms") {
    case f ~ x ~ y => exactMaterializer(x).combine(evaluateExpressionBiFunction(f))(exactMaterializer(y))
  }

  /**
    * Matcher which succeeds if either of the BiFunction components of the input can be simplified to be exact.
    * If either operand can be simplified, then it will be.
    *
    * CONSIDER these matches would have been picked up before.
    *
    * @return a Matcher[DyadicTriple, Expression].
    */
  def matchSimplifyDyadicTermsTwoLevels: Matcher[DyadicTriple, Expression] = Matcher("matchSimplifyDyadicTermsTwoLevels") {
    case f ~ BiFunction(g, w, x) ~ BiFunction(h, y, z) =>
      matcher3(exactMaterializer(BiFunction(g, w, x)), exactMaterializer(BiFunction(h, y, z)), Match(f)).map(x => Expression(x.evaluate))
    case f ~ BiFunction(g, w, x) ~ y =>
      matcher3(exactMaterializer(BiFunction(g, w, x)), Match(y), Match(f)).map(x => Expression(x.evaluate))
    case f ~ y ~ BiFunction(g, w, x) =>
      matcher3(Match(y), exactMaterializer(BiFunction(g, w, x)), Match(f)).map(x => Expression(x.evaluate))
    // CONSIDER should we match Function?
    case f ~ y ~ z =>
      // CONSIDER allowing these single-level expressions through
      Miss("matchSimplifyDyadicTermsTwoLevels: depth is only one level", f ~ y ~ z)
  }

  /**
    * If the operand can be simplified, then it will be.
    *
    * @return a Matcher[MonadicDuple, MonadicDuple].
    */
  def matchSimplifyMonadicTerm: Matcher[MonadicDuple, MonadicDuple] = Matcher("matchSimplifyMonadicTerm") {
    case Cosine ~ x => Match(Sine) ~ (Match(x plus ConstPi / 2) flatMap simplifier)
    //    case Sine ~ x if (x*4).evaluate == Number.one => (x*4).evaluate match {
    //      case 1 =>
    //    }
    case f ~ x => Match(f) ~ exactMaterializer(x)
  }

  /**
    * Method to match an Expression which is a BiFunction and replace it with a simplified expression.
    *
    * NOTE: there may be some redundancies here.
    *
    * @return an Matcher[DyadicTriple, Expression].
    */
  def biFunctionMatcher: Matcher[DyadicTriple, Expression] =
    (matchSimplifyDyadicTerms | matchDyadicTrivial | evaluateExactDyadicTriple | matchDyadicTwoLevels | matchMultiLevels) :| "biFunctionMatcher"

  /**
   * Method to match an Expression which is a Total and replace it with a simplified expression.
   *
   * @return an Matcher[Expression, Expression].
   */
  def totalMatcher: Matcher[Expression, Expression] =
    matchSimplifyTotalTerms :| "totalMatcher"

  /**
    * This method defines a single Matcher which combines the various two-level matchers which can be applied to an Expression.
    *
    * @return a Matcher.
    */
  def matchDyadicTwoLevels: Matcher[DyadicTriple, Expression] = (matchSimplifyDyadicTermsTwoLevels |
      (matchTwoDyadicLevels & matchAndCollectTwoDyadicLevels) |
      (matchTwoDyadicLevelsL & (matchAndCancelTwoDyadicLevelsL | matchAndCollectTwoDyadicLevelsL)) |
      (matchTwoDyadicLevelsR & (matchAndCancelTwoDyadicLevelsR | matchAndCollectTwoDyadicLevelsR)) |
      fail("twoLevel")
      ) :| "matchDyadicTwoLevels"

  /**
    * Matcher which runs depth-first search on an Expression tree, as long as the functions are the same.
    * Then it collects the operands together and tries to combine any exact elements.
    *
    * @return a Matcher[DyadicTriple, Expression.
    */
  def matchMultiLevels: Matcher[DyadicTriple, Expression] = {
    case f ~ x ~ y =>
      combineTerms(f, matchMultiLevelsInner(f, Nil)(f ~ x ~ y), Miss("matchMultiLevelsInner", x))
  }

  /**
    * Method to match an Expression which is a Function and replace it with a simplified expression.
    *
    * @return an Transformer.
    */
  def functionSimplifier: Transformer =
    (matchFunction & ((matchSimplifyMonadicTerm & evaluateMonadicDuple) | (matchTwoMonadicLevels & matchAndCancelTwoMonadicLevels))) :| "functionSimplifier"

  def evaluateMonadicDuple: Matcher[MonadicDuple, Expression] = Matcher("evaluateMonadicDuple") {
    case f ~ x => exactMaterialization(None)(Function(x, f)) map (Literal(_)) // CONSIDER use exactMaterializer
  }

  def matchTwoMonadicLevels: Matcher[MonadicDuple, ExpressionFunction ~ MonadicDuple] = Matcher("matchTwoMonadicLevels") {
    case f ~ Function(x, g) => Match(f ~ (g ~ x))
    case x => Miss("matchTwoMonadicLevels: not a Function", x)
  }

  def matchAndCancelTwoMonadicLevels: Matcher[ExpressionFunction ~ MonadicDuple, Expression] = Matcher("matchAndCancelTwoMonadicLevels") {
    case f ~ (g ~ x) if complementaryMonadic(f, g) => Match(x)
    case f ~ (g ~ x) => Miss("cannot combine two monadic levels", f ~ (g ~ x))
  }

  /**
    * Matcher which matches on a trivial dyadic function, such as x * 0 or x + 0.
    * In the case where the result is non-constant, it will be further simplified if possible.
    *
    * @return a Match if one of the trivial situations is matched, else a Miss.
    */
  def matchDyadicTrivial: Matcher[DyadicTriple, Expression] = Matcher("matchDyadicTrivial") {
    case Sum ~ Zero ~ x => matchAndSimplify(x)
    case Sum ~ x ~ Zero => matchAndSimplify(x)
    case Sum ~ x ~ y if x == y => matchAndSimplify(BiFunction(Two, x, Product))
    case Product ~ Zero ~ _ => Match(Zero)
    case Product ~ _ ~ Zero => Match(Zero)
    case Product ~ One ~ x => matchAndSimplify(x)
    case Product ~ x ~ One => matchAndSimplify(x)
    case Product ~ x ~ y if x == y => matchAndSimplify(BiFunction(x, Two, Power))
    case Power ~ _ ~ Zero => Match(One)
    case Power ~ One ~ _ => Match(One)
    case Power ~ x ~ One => matchAndSimplify(x)
    case x => Miss("not a trivial dyadic function", x)
  }

  /**
    * Matcher which takes a DyadicTriple (which is not exact -- otherwise this method would not be called).
   * CONSIDER is it exact or not exact?
    *
   * CONSIDER this is redundant.
    *
   * @return a `Matcher[DyadicTriple, Expression]`
    */
  def evaluateExactDyadicTriple: Matcher[DyadicTriple, Expression] = Matcher("evaluateExactDyadicTriple") {
    case f ~ x ~ y =>
      replaceExactBiFunction(None)(BiFunction(x, y, f)) flatMap simplifier
    case z =>
      Miss("evaluateExactDyadicTriple: not an dyadic expression", z)
  }

  def matchTwoDyadicLevels: Matcher[DyadicTriple, ExpressionBiFunction ~ DyadicTriple ~ DyadicTriple] = Matcher("matchTwoDyadicLevels") {
    case f ~ BiFunction(w, x, g) ~ BiFunction(y, z, h) =>
      Match(f ~ (g ~ w ~ x) ~ (h ~ y ~ z))
    case z =>
      Miss("matchTwoDyadicLevels: not two BiFunctions", z)
  }

  def matchTwoDyadicLevelsL: Matcher[DyadicTriple, ExpressionBiFunction ~ DyadicTriple ~ Expression] = Matcher("matchTwoDyadicLevelsL") {
    case f ~ BiFunction(x, y, g) ~ z =>
      Match(f ~ (g ~ x ~ y) ~ z)
    case z =>
      Miss("matchTwoDyadicLevelsL: not a DyadicTriple of a (left) BiFunction", z)
  }

  def matchTwoDyadicLevelsR: Matcher[DyadicTriple, ExpressionBiFunction ~ Expression ~ DyadicTriple] = Matcher("matchTwoDyadicLevelsR") {
    case f ~ z ~ BiFunction(x, y, g) =>
      Match(f ~ z ~ (g ~ x ~ y))
    case z =>
      Miss("matchTwoDyadicLevelsR: not a DyadicTriple of a (right) BiFunction", z)
  }

  /**
    * Matcher which matches on Expressions that directly represent Numbers.
    *
    * @return an ExpressionMatcher[Field].
    */
  def value: ExpressionMatcher[Field] = {
    case Literal(x) => Match(x)
    case x@Number(_, _) => Match(Real(x))
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

  /**
   * Matcher which matches a BiFunction and results in a DyadicTriple.
   *
   * @return ExpressionMatcher[DyadicTriple]
   */
  def matchTotal: ExpressionMatcher[Expression] = ExpressionMatcher {
    case t@Total(xs) => matchAll[Expression](exactMaterializer).apply(xs) match {
      case Match(_) => Match(t)
      case Miss(m, e) => Miss(m, e)
      case Error(x) => Error(x)
      case _ => Miss("matchTotal: not all elements are exact", t) // NOTE: we shouldn't need this
    }
    case e => Miss("matchTotal: not a Total expression", e)
  }.named("matchTotal")

  private def exactFieldMaterializer(context: Option[Factor]): ExpressionMatcher[Field] = Matcher[Expression, Field]("exactFieldMaterializer")(exactMaterialization(context))

  private val matcher3: (MatchResult[Expression], MatchResult[Expression], MatchResult[ExpressionBiFunction]) => MatchResult[BiFunction] = matchResult3(BiFunction)

  private def evaluateExpressionBiFunction(f: ExpressionBiFunction): (Expression, Expression) => Expression = (x, y) => Expression(BiFunction(x, y, f).evaluate)

  private def matchAndSimplify(x: Expression): MatchResult[Expression] = Match(x) flatMap simplifier

  private def matchMultiLevelsInner(f: ExpressionBiFunction, matches: Seq[MatchResult[Expression]])(input: ExpressionBiFunction ~ Expression ~ Expression): Seq[MatchResult[Expression]] = input match {
    // TODO we need to check context, e.g.  Some(Scalar) if f is Sum. For Product, it's not a problem
    case `f` ~ BiFunction(a, b, `f`) ~ BiFunction(c, d, `f`) if f != Power =>
      matchMultiLevelsInner(f, matchMultiLevelsInner(f, matches)(f ~ a ~ b) ++ matches)(f ~ c ~ d)
    case `f` ~ x ~ BiFunction(a, b, `f`) if f != Power =>
      matchMultiLevelsInner(f, exactMaterializer(x) +: matches)(f ~ a ~ b) // TESTME
    case `f` ~ BiFunction(a, b, `f`) ~ y if f != Power =>
      matchMultiLevelsInner(f, exactMaterializer(y) +: matches)(f ~ a ~ b) // TESTME
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
      Match(w) flatMap simplifier // CONSIDER this is a special case and I'm not sure it really belongs here
    case (Sum, Product, Product) if w == y =>
      replaceAndSimplify(x, z, w, Sum, Product)
    case (Sum, Product, Product) if w == z =>
      replaceAndSimplify(x, y, w, Sum, Product) // TESTME
    case (Sum, Product, Product) if x == y =>
      replaceAndSimplify(w, z, x, Sum, Product) // TESTME
    case (Sum, Product, Product) if x == z =>
      replaceAndSimplify(w, y, x, Sum, Product) // TESTME
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
      combineAndSimplify(x, w, Product) // TESTME
    case (Product, Sum) =>
      val terms = Seq(w ~ y, x ~ y) map matchExpressionPair(Product)
      combineTerms(Sum, terms, Miss("collectTermsDyadicTwoLevelsL: no *++ terms to collect", f ~ g ~ w ~ x ~ y)) // TESTME
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
      combineAndSimplify(w, y, Power) // CONSIDER refactor using method, etc...  // TESTME
    case (Sum, Product) if w == x =>
      combineAndSimplify(w, y, Product)
    case (Sum, Product) if w == y =>
      combineAndSimplify(w, x, Product) // TESTME
    case (Product, Sum) =>
      val terms = Seq(w ~ x, w ~ y) map matchExpressionPair(Product)
      combineTerms(Sum, terms, Miss("collectTermsDyadicTwoLevelsR: no *++ terms to collect", f ~ g ~ w ~ x ~ y)) // TESTME
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
      val goodAccumulation: Real = Real(good.foldLeft(start)((accum, term) => term match {
        case Match(t) =>
          function match {
            case Sum => accum doAdd t
            case Product => accum doMultiply t
            case _ => throw ExpressionException(s"combineTerms: function not supported: $function")
          }
        case _ => accum
      }))
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

  // CONSIDER redefine as a Matcher
  private def effectivelyExactMaterialization(function: BiFunction): MatchResult[Expression] = function match {
    case BiFunction(a, b, f) => matchDyadicTwoLevels(f ~ a ~ b)
  }

  // CONSIDER redefine as a Matcher
  // NOTE: doesn't the second part (effectively...) do the exact same thing as matchDyadicTwoLevels which is still to be invoked from above/
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
  // private but used by unit tests
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
    case (Log, Exp) => true  // TESTME
    case _ => false
  }

  // CONSIDER inlining this
  private def associativeDyadic(f: ExpressionBiFunction, g: ExpressionBiFunction): Option[ExpressionBiFunction] = (f, g) match {
    case (Power, Power) => Some(Product)
    case _ => None
  }

  val logger: MatchLogger = matchLogger
}
