/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.matchers.{MatchLogger, ~}
import com.phasmidsoftware.number.core.Constants.{half, piBy2}
import com.phasmidsoftware.number.core.Expression.isIdentityFunction
import com.phasmidsoftware.number.core.Rational.{infinity, negInfinity}
import com.phasmidsoftware.number.matchers._
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/**
 * Matchers whose input is generally an Expression.
 *
 * These Matchers are used to simplify (lazy) Expressions before those Expressions get evaluated,
 * thus sometimes avoiding loss of precision.
 *
 * Rules for matching:
 * (1) All substitution is based on a successful match -- no match, no substitution;
 * CONSIDER this second rules seems strange
 * (2) The first match to try for any expression is the exactMaterializer which returns Match(Literal(value));
 * (2) Some matches return non-exact match results -- these should be passed to flatMap simplifier;
 * NOTE: do not pass anything to flatMap simplifier if it could possibly be the same as the input (else stack overflow).
 */
class ExpressionMatchers(implicit val matchLogger: MatchLogger) extends MatchersExtras {

  import com.phasmidsoftware.matchers.Matchers._

  /**
   * Abstract class `ExpressionMatcher`, which extends `Matcher` where the input type is always `Expression`.
   *
   * @tparam R the underlying type of `MatchResult`s.
   */
  abstract class ExpressionMatcher[+R] extends Matcher[Expression, R]

  /**
   * Type alias for a pair of expressions (purpose of this is solely for brevity).
   */
  private[core] type Expressions = Expression ~ Expression

  /**
   * Type alias for a dyadic triple (purpose of this is solely for brevity).
   */
  private[core] type DyadicTriple = ExpressionBiFunction ~ Expression ~ Expression

  /**
   * Type alias for the kind of ExpressionMatcher which results in a possibly different Expression.
   */
  private[core] type ExpressionTransformer = Transformer[Expression]

  /**
   * Type alias for a monadic duple (purpose of this is solely for brevity).
   */
  private[core] type MonadicDuple = ExpressionFunction ~ Expression

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
   * Matcher which tries to evaluate the input exactly as a PureNumber and then wraps the result as an Expression.
   * NOTE: currently, the expression will be materialized without regard to the `PureNumber` context.
   * CONSIDER change None to Some(PureNumber) [but I don't think that would be correct]
   *
   * @return Matcher[Expression, Expression]
   */
  def exactMaterializer: ExpressionTransformer = exactFieldMaterializer(None) map (Expression(_))

  /**
   * Evaluates an expression within a given context, matching it to a field if the evaluation is exact.
   *
   * @param context the evaluation context used to process the expression
   * @return an ExpressionMatcher for the evaluated field, returning a Match if the field is exact,
   *         or a Miss otherwise.
   */
  def exactEvaluator(context: Context): ExpressionMatcher[Field] = {
    x: Expression =>
      context match {
        case c@Some(_) =>
          matchExactField(x.evaluate(c), x) // TESTME
        case None =>
          x.context match {
            case None =>
              Miss("exactEvaluator: no common context", x)
            case c@Some(_) =>
              matchExactField(x.evaluate(c), x)
          }
      }
  }

  /**
   * `ExpressionMatcher`, which matches expressions that are exact in the given context,
   * it returns a `Match` of the value as a `Field`.
   *
   * @param context the context in which we want to evaluate this `Expression`.
   *                if context is `None` then, the result will depend solely on whether this is exact.
   * @return a Matcher[Expression, Field]
   */
  def exactMaterialization(context: Context): ExpressionMatcher[Field] = {
    case x: AtomicExpression =>
      Match(x.evaluateAsIs)
    case x =>
      val ctx = context orElse Some(PureNumber)
      if (x.isExactInContext(ctx))
        Match(x.evaluate(ctx))
      else
        Miss("exactMaterialization: non-exact", x)
  }

  /**
   * This method is called by materializer and therefore will return a Field, regardless of whether any simplifications
   * were possible.
   * CONSIDER this should be an instance method of Expression.
   * CONSIDER both cases should be normalized.
   *
   * @param x the Expression to be evaluated as a Field.
   * @return the value of the original Expression or a simplified version of the Expression.
   */
  def simplifyAndEvaluate(x: Expression, context: Context): Field =
    simplifier(x) match {
      case Match(e) =>
        // NOTE that, formerly, we checked for exactness and if non-exact, we normalized the result.
        e.evaluate(context)
      case _ =>
        x.evaluate(context) // TESTME
    }

  /**
   * Transformer which always succeeds.
   * If appropriate, the result will be a simplification of the input.
   *
   * @return a Transformer that applies the composite simplification logic
   */
  def simplifier: ExpressionTransformer = alt[Expression](matchAndSimplifyComposite & compositeSimplifier | always)

  /**
   * `Transformer` which simplifies various types of expressions including atomic expressions,
   * composite expressions such as `BiFunction`, `Function`, `Aggregate`, and `ReducedQuadraticRoot`.
   * The method applies specific simplification logic for each expression type and
   * recursively applies further simplifications where applicable.
   * NOTE that this Matcher always succeeds so you must not follow it
   * with anything recursive that depends on the success of the match.
   *
   * @return A `Transformer` instance that applies the defined simplification rules
   *         to matching expressions.
   */
  def compositeSimplifier: Matcher[CompositeExpression, Expression] = ExpressionMatcher {
      case b@BiFunction(_, _, _) =>
        biFunctionSimplifier(b) // TODO why is not not OK to follow with  & alt(simplifier) (presumably because some misses are encoded as matches)
      case f@Function(_, _) =>
        functionSimplifier(f) & alt(simplifier)
      case a@Aggregate(_, _) =>
        aggregateSimplifier(a) & alt(simplifier)
      case r@ReducedQuadraticRoot(_, _, _) => // TESTME
        Miss("simplification of ReducedQuadraticRoot not yet implemented", r) // TODO implement simplifications if any
      case x =>
        Miss("compositeSimplifier", x) // TESTME
  } :| "compositeSimplifier"

  /**
   * Simplifies the terms of a composite expression by applying a simplification procedure
   * to each term. Constructs a new composite expression with the simplified terms if
   * at least one term is successfully simplified, otherwise returns the original expression.
   *
   * CONSIDER moving all the code into `CompositeExpression` (but careful because you need to manage an appropriate `ExpressionMatchers` instance.
   * CONSIDER taking a parameter for context to pass into exactEvaluator.
   *
   * @param c the composite expression to be simplified
   * @return the simplified composite expression, or the original expression if simplification is unsuccessful
   */
  def simplifyTerms(c: CompositeExpression): CompositeExpression = {
    // CONSIDER this appears to be wrong. See ISSUE #89
    val frs: Seq[MatchResult[Field]] = c.terms map (alt(simplifier) & exactEvaluator(None))
    if (frs.forall(fr => !fr.successful)) c
    else {
      val substitutions: Seq[Expression] = for (er <- frs) yield er match {
        case Match(e) => Literal(e)
        case Miss(_, v: Expression) => v
        case Error(x) => throw x // TESTME
      }
      c.substituteTerms(substitutions)
    }
  }

  /**
   * Method to simplify a BiFunction if possible.
   * If the input is a BiFunction, it will be transformed into a DyadicTriple and passed to biFunctionTransformer.
   *
   * @return a Transformer.
   */
  def biFunctionSimplifier: ExpressionTransformer =
    (matchBiFunction & biFunctionTransformer) :| "biFunctionSimplifier"

  /**
   * Method to match an Expression, which is a Function, and replace it with a simplified expression.
   *
   * @return an Transformer.
   */
  def functionSimplifier: ExpressionTransformer =
    (matchFunction & alt(functionElementSimplifier) & (matchMonadicTrivial | (matchSimplifyMonadicTerm & evaluateMonadicDuple) | (matchTwoMonadicLevels & matchAndCancelTwoMonadicLevels))) :| "functionSimplifier"

  /**
   * Matches and aggregates expressions involving binary functions (BiFunction)
   * with operations such as Sum and Product, creating an Aggregate expression.
   * Simplification is applied to the resulting aggregate expression.
   *
   * @return a `Matcher` that matches a `DyadicTriple` and results in a `MatchResult[Expression]` by aggregating
   *         related components based on the matching rules for binary functions.
   */
  def biFunctionAggregator: Matcher[DyadicTriple, Expression] = Matcher("biFunctionAggregator") {
    case Sum ~ BiFunction(x, y, Sum) ~ z => simplifier(Aggregate(Sum, Seq(x, y, z))) // TESTME
    case Product ~ BiFunction(x, y, Product) ~ z => simplifier(Aggregate(Product, Seq(x, y, z))) // TESTME
    case Sum ~ x ~ BiFunction(y, z, Sum) => simplifier(Aggregate(Sum, Seq(x, y, z))) // TESTME
    case Product ~ x ~ BiFunction(y, z, Product) => simplifier(Aggregate(Product, Seq(x, y, z))) // TESTME
    case z => Miss("biFunctionAggregator: nothing to aggregate", z)
  }


  /**
   * Method to simplify an Aggregate if possible.
   * If the input is an Aggregate, simplification is if all elements are exact and can be reduced to zero, one, or two elements.
   *
   * @return a Transformer.
   */
  def aggregateSimplifier: ExpressionTransformer =
    (matchAggregate & simplifyAggregateTerms) :| "aggregateSimplifier"

  /**
   * Method to match an Expression which is a BiFunction and replace it with a simplified expression.
   *
   * NOTE: there may be some redundancies here.
   *
   * @return a Matcher[DyadicTriple, Expression].
   */
  def biFunctionTransformer: Matcher[DyadicTriple, Expression] =
    (simplifyIdentityDyadic | matchComplementary | matchDyadicTrivial | matchCombineAndMaybeSimplify | biFunctionAggregator | collectTerms | matchDyadicTwoLevels) :| "biFunctionTransformer"

  /**
   * Simplifies dyadic expressions where one side is an identity element according to a given function.
   * Matches expressions in the form where either the left operand or the right operand
   * is an identity element for the operation and reduces them to the non-identity operand.
   *
   * @return A Matcher that reduces dyadic expressions with identity elements to their simplified form.
   */
  def simplifyIdentityDyadic: Matcher[DyadicTriple, Expression] = Matcher("simplifyIdentityDyadic") {
    case ExpressionBiFunction(_, _, fo, None) ~ x ~ y if isIdentity(x, fo) => Match(y) // TESTME
    case ExpressionBiFunction(_, _, fo, None) ~ x ~ y if isIdentity(y, fo) => Match(x) // TESTME
    case ExpressionBiFunction(_, _, None, fo) ~ x ~ y if isIdentity(y, fo) => Match(x) // TESTME
    case ExpressionBiFunction(_, _, fo, _) ~ x ~ y if isIdentity(x, fo) => Match(y) // TESTME
    case ExpressionBiFunction(_, _, _, fo) ~ x ~ y if isIdentity(y, fo) => Match(x) // TESTME
    case t@ExpressionBiFunction(_, _, _, _) ~ _ ~ _ => Miss("simplifyIdentityDyadic", t) // XXX don't really need this case
  }

  /**
   * Matches a `DyadicTriple` against a complementary expression pattern.
   * Uses the specified matcher to attempt a matching process between the
   * provided triple and specific complementary expressions.
   * If a match is found, it evaluates the resulting expression and wraps it in a `Literal`.
   * If no match can be made, it returns a `Miss` with a corresponding message and inputs.
   *
   * TODO handle other complementary cases
   * CONSIDER eliminating this method as it really does nothing.
   *
   * @return A `Matcher` that performs complementary expression matching on a `DyadicTriple`,
   *         resulting in either a match with an evaluated expression or a miss.
   */
  def matchComplementary: Matcher[DyadicTriple, Expression] = Matcher("matchComplementary") {
    case f ~ x ~ y =>
      matchComplementaryExpressions(f ~ x ~ y) match {
        case Match(z) => Match(Literal(z.evaluateAsIs))
        case _ => Miss("matchComplementary: no match", x ~ y)
      }
  }

  /**
   * Matcher which succeeds if either of the BiFunction components of the input can be simplified to be exact.
   * If either operand can be simplified, then it will be.
   *
   * CONSIDER these matches would have been picked up before so this method should be eliminated.
   *
   * @return a Matcher[DyadicTriple, Expression].
   */
  def matchSimplifyDyadicTermsTwoLevels: Matcher[DyadicTriple, Expression] = Matcher("matchSimplifyDyadicTermsTwoLevels") {
    case Sum ~ (q@Function(_, _)) ~ (z@Function(_, _)) =>
      matchComplementaryExpressions(Sum ~ q ~ z)
    case Sum ~ x ~ (q@Function(_, _)) =>
      matchComplementaryExpressions(Sum ~ x ~ q)
    case Sum ~ (q@Function(_, _)) ~ y =>
      matchComplementaryExpressions(Sum ~ q ~ y)
    case Product ~ x ~ (q@Function(_, _)) =>
      matchComplementaryExpressions(Product ~ x ~ q)
    case Product ~ (q@Function(_, _)) ~ y =>
      matchComplementaryExpressions(Product ~ q ~ y) // TESTME
    case f ~ BiFunction(g, w, x) ~ BiFunction(h, y, z) =>
      matcher3(exactMaterializer(BiFunction(g, w, x)), exactMaterializer(BiFunction(h, y, z)), Match(f)).map(x => Expression(x.evaluateAsIs))
    case f ~ BiFunction(g, w, x) ~ y =>
      matcher3(exactMaterializer(BiFunction(g, w, x)), Match(y), Match(f)).map(x => Expression(x.evaluateAsIs))
    case f ~ y ~ BiFunction(g, w, x) =>
      matcher3(Match(y), exactMaterializer(BiFunction(g, w, x)), Match(f)).map(x => Expression(x.evaluateAsIs))
    // CONSIDER removing these complementary expression cases from here -- they should already have been handled
    case f ~ y ~ z =>
      // CONSIDER allowing these single-level expressions through
      Miss("matchSimplifyDyadicTermsTwoLevels: no dyadic match", f ~ y ~ z)
    case q => // CONSIDER this will never be reached
      Miss("matchSimplifyDyadicTermsTwoLevels: no match", q) // TESTME

  }

  /**
   * Matches complementary expressions in a given `DyadicTriple` to simplify or reduce them.
   * CONSIDER redefining this as a Matcher[DyadicTriple,Expression]
   *
   * This method examines the provided `DyadicTriple` and attempts to match patterns that indicate
   * complementary expressions, such as summing an entity with its negation or multiplying
   * an entity with its reciprocal, returning an appropriate simplified result if a match is found.
   *
   * @param z the `DyadicTriple` to be analyzed for matching complementary expressions
   * @return a `MatchResult[Expression]` containing the simplified expression if a match is found,
   *         or a `Miss` result with an appropriate message if no match is found
   */
  def matchComplementaryExpressions(z: DyadicTriple): MatchResult[Expression] =
    z match {
      case Sum ~ x ~ Function(y, Negate) if x == y => Match(Zero) // TESTME
      case Sum ~ Function(x, Negate) ~ y if x == y => Match(Zero) // TESTME
      case Sum ~ BiFunction(w, x, Sum) ~ Function(y, Negate) if x == y => Match(w) // TESTME
      case Sum ~ Function(x, Negate) ~ BiFunction(y, z, Sum) if x == z => Match(y) // TESTME
      case Sum ~ BiFunction(w, x, Sum) ~ Function(y, Negate) if w == y => Match(x) // TESTME
      case Sum ~ Function(x, Negate) ~ BiFunction(y, z, Sum) if x == y => Match(z) // TESTME
      case Product ~ x ~ Function(y, Reciprocal) if x == y => Match(One) // TESTME
      case Product ~ Function(x, Reciprocal) ~ y if x == y => Match(One) // TESTME
      case Product ~ BiFunction(w, x, Product) ~ Function(y, Reciprocal) if x == y => Match(w) // TESTME
      case Product ~ Function(x, Reciprocal) ~ BiFunction(w, z, Product) if x == w => Match(z) // TESTME
      case Product ~ BiFunction(w, x, Product) ~ Function(y, Reciprocal) if w == y => Match(x) // TESTME
      case Product ~ Function(x, Reciprocal) ~ BiFunction(w, z, Product) if x == z => Match(w) // TESTME
    case f ~ x ~ y =>
      complementaryFields(f, x, y) match {
        case Some(z) => Match(z)
        case None => Miss("matchComplementaryExpressions: no match", z)
      }
  }

  /**
   * Determines if two expressions are complementary based on a given binary function.
   * TODO find other method that does something similar
   * TODO move this method into Expression
   *
   * @param f The binary function to evaluate the expressions.
   * @param x The first expression to evaluate.
   * @param y The second expression to evaluate.
   * @return True if the expressions are complementary, according to the binary function, false otherwise.
   */
  def complementaryFields(f: ExpressionBiFunction, x: Expression, y: Expression): Option[Expression] =
    if (x.maybeFactor == y.maybeFactor) { // TODO logic here is same as for value in BiFunction
      val field = f(x.evaluateAsIs, y.evaluateAsIs)
      if (f.maybeIdentityL.contains(field)) Some(Literal(field))
      else None
    }
    else None

  /**
   * This method defines a single Matcher which combines the various two-level matchers which can be applied to an Expression.
   *
   * @return a Matcher.
   */
  def matchDyadicTwoLevels: Matcher[DyadicTriple, Expression] = (
    (matchTwoDyadicLevels & matchSimplifyDyadicTermsTwoLevels) |
      (matchTwoDyadicTripleLevels & matchAndCollectTwoDyadicLevels) |
      (matchTwoDyadicLevelsL & (matchAndCancelTwoDyadicLevelsL | matchAndCollectTwoDyadicLevelsL)) |
      (matchTwoDyadicLevelsR & (matchAndCancelTwoDyadicLevelsR | matchAndCollectTwoDyadicLevelsR)) |
      fail("twoLevel")
    ) :| "matchDyadicTwoLevels"

  /**
   * Defines a matcher for monadic expressions and constants, performing trivial simplifications.
   * The matcher evaluates pairs of monadic operators and constants, returning their simplified forms
   * if a specific pattern is identified, or marking it as a miss otherwise.
   *
   * TESTME none of the non-default cases appear to be tested
   *
   * @return A `Matcher` that matches a `MonadicDuple` (monadic operator and constant pair) to an `Expression`,
   *         providing pre-defined trivial simplifications or signaling no match with a descriptive message.
   */
  def matchMonadicTrivial: Matcher[MonadicDuple, Expression] = Matcher("matchMonadicTrivial") {
    case Negate ~ Zero => Match(Zero)
    case Negate ~ One => Match(MinusOne)
    case Negate ~ MinusOne => Match(One)
    case Reciprocal ~ One => Match(One)
    case Reciprocal ~ Zero => Match(Literal(infinity))
    case Reciprocal ~ Two => Match(Literal(half))
    case Exp ~ Literal(x, _) if x == Real(negInfinity) => Match(Zero)
    case Exp ~ Zero => Match(One)
    case Exp ~ One => Match(ConstE)
    case Log ~ One => Match(Zero)
    case Log ~ Zero => Match(Literal(negInfinity))
    case Log ~ ConstE => Match(One)
    case Sine ~ Literal(x, _) if x == piBy2 => Match(One)
    case Sine ~ Literal(x, _) if (x + piBy2).isZero => Match(MinusOne)
    case Sine ~ Zero | Sine ~ ConstPi => Match(Zero)
    case Cosine ~ Zero => Match(One)
    case Cosine ~ ConstPi => Match(MinusOne)
    case Cosine ~ Literal(x, _) if x == piBy2 => Match(Zero)
    case Cosine ~ Literal(x, _) if (x + piBy2).isZero => Match(Zero)
    case x => Miss(s"matchMonadicTrivial: no trivial match", x)
  }

  /**
   * If the operand can be simplified, then it will be.
   *
   * @return a `Transformer[MonadicDuple]`.
   */
  def matchSimplifyMonadicTerm: Transformer[MonadicDuple] = Matcher("matchSimplifyMonadicTerm") {
    case Cosine ~ x => Match(Sine) ~ matchAndMaybeSimplify(x plus ConstPi / 2) // CHECK this is OK
    case z => Miss("matchSimplifyMonadicTerm: no simplification available", z) // Match(f) ~ exactMaterializer(x)
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
   * Method to create an ExpressionMatcher.
   *
   * @param f a function Expression => MatchResult[R]
   * @tparam R the MatchResult type.
   * @return a Matcher[Expression, R] which is also an ExpressionMatcher[R].
   */
  def ExpressionMatcher[R](f: Expression => MatchResult[R]): ExpressionMatcher[R] = (e: Expression) => f(e)

  /**
   * Matches and evaluates a monadic duple, where a function and its operand are provided.
   * The method attempts to materialize the function expression with the operand, producing a literal result.
   * An exact materialization strategy is utilized for this evaluation.
   *
   * @return A Matcher that applies the evaluation logic for monadic duples, producing an Expression.
   */
  def evaluateMonadicDuple: Matcher[MonadicDuple, Expression] = Matcher("evaluateMonadicDuple") {
    case f ~ x => exactMaterialization(None)(Function(x, f)) map (Literal(_)) // CONSIDER use exactMaterializer
  }

  /**
   * Attempts to match and transform a nested monadic structure in two levels.
   * This matcher pattern matches between a MonadicDuple and a combination of
   * an ExpressionFunction and a MonadicDuple, handling a specific transformation
   * logic when the second operand is a Function.
   *
   * @return A Matcher object capable of processing the provided monadic structure
   *         and performing the matching logic.
   *         If matched, it returns a Match with
   *         the transformed representation. If no match is found, it returns a Miss.
   */
  def matchTwoMonadicLevels: Matcher[MonadicDuple, ExpressionFunction ~ MonadicDuple] = Matcher("matchTwoMonadicLevels") {
    case f ~ Function(x, g) =>
      Match(f ~ (g ~ x))
    case f ~ x =>
      Miss(s"matchTwoMonadicLevels: argument $x is not a Function", f ~ x)
  }

  /**
   * Matches and cancels two monadic levels if they are complementary, and extracts the inner expression.
   *
   * @return A `Matcher` that tries to match and cancel complementary monadic levels in an expression
   *         and produces the inner expression if successful, or provides a failure message otherwise.
   */
  def matchAndCancelTwoMonadicLevels: Matcher[ExpressionFunction ~ MonadicDuple, Expression] = Matcher("matchAndCancelTwoMonadicLevels") {
    case f ~ (g ~ x) if complementaryMonadic(f, g) => matchAndMaybeSimplify(x)
    case f ~ (g ~ x) => Miss("cannot combine two monadic levels", f ~ (g ~ x)) // TESTME
  }

  /**
   * Matcher which matches on a trivial dyadic function, such as x * 0 or x + 0.
   * In the case where the result is non-constant, it will be further simplified if possible.
   *
   * TESTME none of the explicit cases are tested.
   *
   * @return a Match if one of the trivial situations is matched, else a Miss.
   */
  def matchDyadicTrivial: Matcher[DyadicTriple, Expression] = Matcher("matchDyadicTrivial") {
    case Sum ~ Zero ~ x => matchAndMaybeSimplify(x)
    case Sum ~ x ~ Zero => matchAndMaybeSimplify(x)
    case Sum ~ x ~ y if x == y => matchAndMaybeSimplify(BiFunction(Two, x, Product))
    case Product ~ Zero ~ _ => Match(Zero)
    case Product ~ _ ~ Zero => Match(Zero)
    case Product ~ One ~ x => matchAndMaybeSimplify(x)
    case Product ~ x ~ One => matchAndMaybeSimplify(x)
    case Product ~ MinusOne ~ x => matchAndMaybeSimplify(Function(x, Negate))
    case Product ~ x ~ MinusOne => matchAndMaybeSimplify(Function(x, Negate))
    case Product ~ x ~ y if x == y => matchAndMaybeSimplify(BiFunction(x, Two, Power))
    case Product ~ x ~ Function(One, Negate) => matchAndMaybeSimplify(Function(x, Negate))
    case Product ~ Function(One, Negate) ~ y => matchAndMaybeSimplify(Function(y, Negate))
    case Power ~ _ ~ Zero => Match(One)
    case Power ~ One ~ _ => Match(One)
    case Power ~ x ~ One => matchAndMaybeSimplify(x)
    case Power ~ x ~ Two => matchSimplifySquare(x)
    case x => Miss("not a trivial dyadic function", x)
  }

  /**
   * Matches and combines expressions based on the specified operation and its factors.
   * If the factors match the expected operation (Sum, Product, or Power), the method evaluates
   * and potentially simplifies the resulting expression.
   * If no match is found, it returns a miss.
   *
   * @return A `Matcher[DyadicTriple, Expression]` that processes and simplifies expressions or returns a miss if no match occurs.
   */
  def matchCombineAndMaybeSimplify: Matcher[DyadicTriple, Expression] = Matcher("matchCombineAndMaybeSimplify") {
    case Sum ~ x ~ y if factorsMatch(Sum, x, y) =>
      matchAndMaybeSimplify(Literal(x.evaluateAsIs + y.evaluateAsIs))
    case Product ~ x ~ y if factorsMatch(Product, x, y) =>
      matchAndMaybeSimplify(Literal(x.evaluateAsIs * y.evaluateAsIs))
    case Power ~ x ~ y if factorsMatch(Power, x, y) =>
      matchAndMaybeSimplify(Literal(x.evaluateAsIs power y.evaluateAsIs))
    case x =>
      Miss("matchCombineAndMaybeSimplify: no match", x)
  }

  /**
   * Determines whether the factors of the given expressions match
   * according to the specified binary function.
   * NOTE there's another factorsMatch method in Expression
   *
   * @param f The binary function that determines the operation type (e.g., Sum, Product, Power).
   * @param x The first expression to compare.
   * @param y The second expression to compare.
   * @return True if the factors match according to the binary function, false otherwise.
   */
  def factorsMatch(f: ExpressionBiFunction, x: Expression, y: Expression): Boolean =
    (for (fx <- x.maybeFactor; fy <- y.maybeFactor) yield f match {
      case Sum =>
        fx.canAdd(fy)
      case Product =>
        fx.canMultiply(fy)
      case Power =>
        fx.canRaise(fy, y.evaluateAsIs)
    }).contains(true)

  /**
   * Matcher which matches on a trivial dyadic function, such as x * 0 or x + 0.
   * In the case where the result is non-constant, it will be further simplified if possible.
   *
   * @return a Match if one of the trivial situations is matched, else a Miss.
   */
  def collectTerms: Matcher[DyadicTriple, Expression] = Matcher("collectTerms") {
    // NOTE there are additional ways of generating x^2 - y^2 (not always the Negate function)
    case Product ~ BiFunction(w, x, Sum) ~ BiFunction(y, Function(z, Negate), Sum) if w == y && x == z =>
      matchAndMaybeSimplify(BiFunction(w ^ 2, Function(x ^ 2, Negate), Sum))
    case Product ~ BiFunction(w, Function(x, Negate), Sum) ~ BiFunction(y, z, Sum) if w == y && x == z =>
      matchAndMaybeSimplify(BiFunction(w ^ 2, Function(x ^ 2, Negate), Sum)) // TESTME
    case Product ~ BiFunction(w, x, Sum) ~ BiFunction(Function(y, Negate), z, Sum) if w == y && x == z =>
      matchAndMaybeSimplify(BiFunction(w ^ 2, Function(x ^ 2, Negate), Sum)) // TESTME
    case Product ~ BiFunction(Function(w, Negate), x, Sum) ~ BiFunction(y, z, Sum) if w == y && x == z =>
      matchAndMaybeSimplify(BiFunction(w ^ 2, Function(x ^ 2, Negate), Sum)) // TESTME
    case Product ~ BiFunction(w, x, Sum) ~ BiFunction(y, z, Sum) => // TESTME
      matchAndMaybeSimplify(Aggregate(Sum, Seq(w * y, w * z, x * y, x * z)))
    case x => Miss("not a trivial dyadic function", x)
  }

  /**
   * Matches two `DyadicTriple` objects where at least one of the operands has a depth greater than 1.
   *
   * @return A Matcher that validates if at least one of the two DyadicTriple operands has a depth greater than 1.
   *         Returns a Match with the input if the condition is met, or a Miss with a message otherwise.
   */
  def matchTwoDyadicLevels: Transformer[DyadicTriple] = Matcher("matchTwoDyadicLevels") {
    case f ~ x ~ y if x.depth > 1 || y.depth > 1 =>
      Match(f ~ x ~ y)
    case z =>
      Miss("matchTwoDyadicLevels: neither operand has depth > 1", z)
  }

  /**
   * Matches two levels of dyadic triples represented as BiFunction structures
   * and transforms them into a combined match result.
   *
   * This matcher decomposes two BiFunction structures encapsulated in a DyadicTriple and reconstructs them
   * into a nested structure if a match is successful. If the structure does not match the expected pattern,
   * it returns a miss with an error message and the unmatched input.
   *
   * @return A `Matcher` that attempts to match a `DyadicTriple` to two nested dyadic `BiFunction`s with a transformation applied.
   */
  def matchTwoDyadicTripleLevels: Matcher[DyadicTriple, ExpressionBiFunction ~ DyadicTriple ~ DyadicTriple] = Matcher("matchTwoDyadicTripleLevels") {
    case f ~ BiFunction(w, x, g) ~ BiFunction(y, z, h) =>
      Match(f ~ (g ~ w ~ x) ~ (h ~ y ~ z))
    case z =>
      Miss("matchTwoDyadicTripleLevels: not two BiFunctions", z)
  }

  /**
   * Matches a structure of a dyadic triple where a BiFunction is present on the left.
   *
   * @return A Matcher that processes a DyadicTriple and a BiFunction, unpacking the components
   *         into an ExpressionBiFunction combined with the DyadicTriple and Expression if matched,
   *         or produces a Miss otherwise.
   */
  def matchTwoDyadicLevelsL: Matcher[DyadicTriple, ExpressionBiFunction ~ DyadicTriple ~ Expression] = Matcher("matchTwoDyadicLevelsL") {
    case f ~ BiFunction(x, y, g) ~ z =>
      Match(f ~ (g ~ x ~ y) ~ z)
    case z =>
      Miss("matchTwoDyadicLevelsL: not a DyadicTriple of a (left) BiFunction", z)
  }

  /**
   * Matches a `DyadicTriple` structure into an `ExpressionBiFunction` applied to two `Expression` values,
   * expanding the right-hand side if the `DyadicTriple` contains a right `BiFunction`.
   *
   * @return A `Matcher` that either matches the `DyadicTriple` structure and rewrites it if applicable,
   *         or provides a `Miss` with an explanatory message if the structure does not match.
   */
  def matchTwoDyadicLevelsR: Matcher[DyadicTriple, ExpressionBiFunction ~ Expression ~ DyadicTriple] = Matcher("matchTwoDyadicLevelsR") {
    case f ~ z ~ BiFunction(x, y, g) =>
      Match(f ~ z ~ (g ~ x ~ y))
    case z =>
      Miss("matchTwoDyadicLevelsR: not a DyadicTriple of a (right) BiFunction", z)
  }

  /**
   * Matcher which matches on Expressions that directly represents a specific given Field.
   *
   * @param x the Number to match.
   * @return a Matcher[Expression, Number].
   */
  def matchValue(x: Field): ExpressionMatcher[Field] = (value & matchNumber(x)) :| s"matchValue($x)"

  /**
   * Matcher which matches on Expressions that directly represent Numbers.
   *
   * @return an ExpressionMatcher[Field].
   */
  def value: ExpressionMatcher[Field] = {
    case Literal(x, _) => Match(x) // TESTME
    case x@Number(_, _) => Match(Real(x)) // TESTME
    case x: Constant => Match(x.materialize)
    case x => Miss("value", x)
  }

  /**
   * Method to define a `Matcher` to match a specific Number.
   * CONSIDER using `matches`.
   *
   * @param x the `Number` to match.
   * @return a `Transformer[Field]` which matches only on x.
   */
  def matchNumber(x: Field): Transformer[Field] =
    Matcher("matchNumber") {
      case `x` => Match(x) // TESTME
      case e => Miss("matchNumber", e)
    }

  /**
   * Matcher which matches a BiFunction and results in a DyadicTriple.
   *
   * @return ExpressionMatcher[DyadicTriple]
   */
  def matchBiFunction: ExpressionMatcher[DyadicTriple] = ExpressionMatcher {
    case BiFunction(a, b, f) => Match(f ~ a ~ b) // TESTME
    case e => Miss("matchBiFunction", e) // TESTME
  }.named("matchBiFunction")

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
    case f ~ (g ~ w ~ x) ~ (h ~ y ~ z) => collectTermsDyadicTwoLevels(f, g, w, x, h, y, z) // TESTME
  }

  /**
   * A `Transformer` which matches an `Aggregate` and results in an `Expression`.
   *
   * @return ExpressionMatcher[Expression]
   */
  def matchAggregate: ExpressionMatcher[Expression] = ExpressionMatcher {
    case a@Aggregate(_, _) => Match(a) // TESTME
    case e => Miss("matchAggregate: not a Aggregate expression", e) // TESTME
  }.named("matchAggregate")

  /**
   * Simplifies a `Aggregate` expression by combining its terms in a more compact form.
   * This method handles several cases:
   * - If the `Aggregate` is empty, it produces a zero constant.
   * - If the `Aggregate` contains a single element, it simplifies that element.
   * - If all elements in the `Aggregate` can be interpreted as numbers, it combines them iteratively
   * and simplifies the resulting expression.
   * - If the `Aggregate` cannot be simplified further, the original structure is retained.
   *
   * NOTE: Need to fix #87
   *
   * @return A `Transformer` that matches and simplifies `Aggregate` expressions efficiently.
   * @throws NoSuchElementException due to invocation of get on Option (very unlikely).
   */
  def simplifyAggregateTerms: ExpressionTransformer = Matcher[Expression, Expression]("simplifyAggregateTerms") {
    case Aggregate(Sum, Nil) =>
      Match(Zero) // TESTME
    case Aggregate(Product, Nil) =>
      Match(One) // TESTME
    case Aggregate(Power, Nil) =>
      Miss("simplifyAggregateTerms: cannot simplify Power(0, 0)", Aggregate(Power, Nil)) // TESTME
    case Aggregate(_, x :: Nil) =>
      matchAndMaybeSimplify(x)
    case Aggregate(f, x :: y :: Nil) =>
      Match(BiFunction(x, y, f))
    case a@Aggregate(_, _) =>
      complementaryTermsEliminator(a)
  }

  /**
   * Matches and simplifies an `Aggregate` expression by identifying and eliminating
   * complementary terms among its components. Specifically, it sorts the components
   * of the aggregate expression, determines complementary elements based on a provided
   * function, and reduces the expression if a match is found.
   *
   * The method assumes that the provided aggregate function `f` typically involves
   * operations where complementary terms can be identified (e.g., summation).
   *
   * @return A `Matcher[Aggregate, Expression]` that either matches an aggregate
   *         expression with reduced complementary terms or returns a miss description
   *         if no reduction takes place.
   */
  def complementaryTermsEliminator: Matcher[Aggregate, Expression] = {
    case a@Aggregate(f, xs) =>
      // NOTE we should handle the very rare cases where the final get fails
      // NOTE this ordering is really only appropriate when f is Sum.
      // TODO find a better way to find complementary elements.
      Try(xs.sortBy { x =>
        (for {n1 <- x.asNumber; n1 <- n1.toPureNumber; d1 <- n1.toDouble} yield Math.abs(d1)).get
      }) match {
        case Success(sorted) =>
          val list = Bumperator[Expression](sorted) { (x, y) => isComplementary(f, x, y) }.toList
          if (list.length < xs.length)
            Match(Aggregate(f, list))
          else
            Miss(s"complementaryTermsEliminator: $a", a)
        case Failure(x) => Error(x) // XXX the result of an extremely improbable NoSuchElementException // TESTME
      }
    case x =>
      Miss(s"simplifyAggregateTerms: not an Aggregate", x)
  }

  /**
   * Determines if the given bi-function is complementary for the specified expressions.
   *
   * @param f the bi-function to evaluate
   * @param x the first expression to be checked
   * @param y the second expression to be checked
   * @return true if the bi-function is complementary for the given expressions, false otherwise
   */
  def isComplementary(f: ExpressionBiFunction, x: Expression, y: Expression): Boolean = {
    val identityCheck: Expression => Boolean = isIdentityFunction(f)
    (matchComplementaryExpressions(f ~ x ~ y) & filter(identityCheck)).successful
  }

  /**
   * Matches a `CompositeExpression` and, if successful,
   * attempts to simplify it.
   *
   * @return An `ExpressionMatcher` that matches instances of `CompositeExpression`.
   */
  private def matchAndSimplifyComposite: Matcher[Expression, CompositeExpression] = ExpressionMatcher {
    case c: CompositeExpression => Match(simplifyTerms(c))
    case z => Miss("matchAndSimplifyComposite", z)
  } :| "matchAndSimplifyComposite"

  /**
   * Matches an exact field against the given expression.
   *
   * @param field the field to evaluate for an exact match
   * @param x     the expression to match against the field
   * @return a MatchResult containing the field if the match is exact; otherwise, a Miss with an explanation and the expression
   */
  private def matchExactField(field: Field, x: Expression): MatchResult[Field] = {
    // CONSIDER If the Field is the same as for x, it should not really be a Match.
    if (field.isExact) Match(field) else Miss("exactEvaluator: non-exact", x)
  }

  /**
   * Checks if the given expression corresponds to the identity element with respect to the field option provided.
   *
   * @param x  the expression to evaluate.
   * @param fo an optional identity value (a `Field`) to compare against the evaluated result of the expression.
   * @return true if the expression matches the identity element of the given field, false otherwise.
   */
  private def isIdentity(x: Expression, fo: Option[Field]) = x match {
    case c: Constant => fo.contains(c.evaluateAsIs)
    case Literal(z, _) => fo.contains(z)
    case _ => false
  }

  /**
   * Transforms and simplifies `Function` expressions by applying the `maybeSimplify` function
   * to each element in the expression.
   *
   * @return A Transformer that matches and simplifies `Function` expressions.
   */
  private def functionElementSimplifier: Transformer[MonadicDuple] = Matcher[MonadicDuple, MonadicDuple]("functionElementSimplifier") {
    case f ~ x => simplifier(x) map (y => f ~ y)
  }

  /**
   * Attempts to simplify the given expression that is being squared it is a square root,
   * or a quadratic-like structure. Matches specific patterns in the expression
   * to reduce or transform it into a simplified form.
   * TESTME (partial)
   *
   * @param x The input expression to be matched and simplified.
   * @return A MatchResult containing the simplified expression if a match is
   *         found, or a Miss indicating that no simplification could be applied.
   */
  private def matchSimplifySquare(x: Expression): MatchResult[Expression] = x match {
    case ReducedQuadraticRoot(-1, 0, _) => Match(x)
    case ReducedQuadraticRoot(p, 0, _) => Match(x * -p) // CONSIDER matchAndMaybeSimplify
    case ReducedQuadraticRoot(-1, q, _) => Match(x plus -q) // CONSIDER matchAndMaybeSimplify
    case ReducedQuadraticRoot(p, q, _) => Match((x * -p) plus -q) // CONSIDER matchAndMaybeSimplify
    case Literal(z, _) if z.isExactInContext(Some(PureNumber)) => Match(Literal(z * z))
    case Literal(Real(ExactNumber(z, Root2)), _) => Match(Literal(Real(ExactNumber(z, PureNumber))))
    // NOTE x is being squared so if it is itself a square root, then the powers cancel.
    case BiFunction(z, y, Power) if y.materialize == Constants.half => Match(z) // CONSIDER matchAndMaybeSimplify
    case _ => Miss("matchSimplifySquare: can't be simplified", x)
  }

  /**
   * Materializes an expression into a field object within the given context.
   *
   * CONSIDER inlining this method as it performs no real function.
   *
   * @param context the context in which the field materialization process operates
   * @return an `ExpressionMatcher` that maps an `Expression` to a `Field` for exact materialization
   */
  private def exactFieldMaterializer(context: Context): ExpressionMatcher[Field] = Matcher[Expression, Field]("exactFieldMaterializer")(exactMaterialization(context))

  /**
   * A private value representing a matcher function that takes three match results as input
   * and produces a match result for a BiFunction. The inputs are two match results for
   * `Expression` and one match result for `ExpressionBiFunction`. The resulting match result
   * is for a `BiFunction`.
   */
  private val matcher3: (MatchResult[Expression], MatchResult[Expression], MatchResult[ExpressionBiFunction]) => MatchResult[BiFunction] = matchResult3(BiFunction)

  /**
   * Matches the given expression against a set of patterns and simplifies it using a simplifier function.
   *
   * @param x the expression to be matched and simplified
   * @return a MatchResult containing the simplified expression
   */
  private def matchAndMaybeSimplify(x: Expression): MatchResult[Expression] = Match(x) & alt(simplifier)

  /**
   * Collect equal terms (fuzzy terms since this method won't be called if all the terms are exact) from an expression of the following two types:
   * TESTME (partial)
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
      matchAndMaybeSimplify(w) // CONSIDER this is a special case and I'm not sure it really belongs here  // TESTME
    case (Sum, Product, Product) if w == y =>
      replaceAndSimplify(x, z, w, Sum, Product)
    case (Sum, Product, Product) if w == z =>
      replaceAndSimplify(x, y, w, Sum, Product) // TESTME
    case (Sum, Product, Product) if x == y =>
      replaceAndSimplify(w, z, x, Sum, Product) // TESTME
    case (Sum, Product, Product) if x == z =>
      replaceAndSimplify(w, y, x, Sum, Product)
    case (Product, Sum, Sum) => // TESTME
      val terms = cartesianProduct(w ~ x, y ~ z) map matchExpressionPair(Product)
      combineTerms(Sum, terms, Miss("collectTermsDyadicTwoLevels: no *++ terms to collect", f ~ g ~ w ~ x ~ h ~ y ~ z))
    case _ =>
      Miss("collectTermsDyadicTwoLevels: functions don't match", f ~ g ~ w ~ x ~ h ~ y ~ z)
  }

  /**
   * Replaces and simplifies an expression using specified bi-functions and simplification logic.
   *
   * @param a the first expression to be used in the bi-function
   * @param b the second expression to be used in the bi-function
   * @param c the expression to be used for forming the second bi-function
   * @param f the first bi-function to be applied
   * @param g the second bi-function to be applied during simplification
   */
  private def replaceAndSimplify(a: Expression, b: Expression, c: Expression, f: ExpressionBiFunction, g: ExpressionBiFunction) =
    replaceExactBiFunction(Some(PureNumber))(BiFunction(a, b, f)) map formBiFunction(c, g) flatMap simplifier

  /**
   * Combines and simplifies the given expressions using the specified binary function.
   * Invokes `replaceAndSimplify`.
   * CONSIDER why not just inline replaceAndSimplify?
   *
   * @param w        the first expression to be combined
   * @param x        the second expression to be combined
   * @param function the binary function used to combine the expressions
   */
  private def combineAndSimplify(w: Expression, x: Expression, function: ExpressionBiFunction) =
    replaceAndSimplify(One, x, w, Sum, function)

  /**
   * Matches and replaces a pair of expressions using the provided function.
   * Invokes `matchAndReplacePair`.
   * CONSIDER why not just inline matchAndReplacePair?
   *
   * @param function A function that defines the logic to match and replace the expression pair.
   * @param e        The input expressions to be matched and replaced.
   * @return A MatchResult containing the resulting expression after applying the function.
   */
  private def matchExpressionPair(function: ExpressionBiFunction)(e: Expressions): MatchResult[Expression] =
    matchAndReplacePair(function, e.l, e.r)

  /**
   * Matches and replaces a pair of expressions in a BiFunction format, provided that the result is exact in PureNumber context.
   * The result depends on [[replaceExactBiFunction]].
   *
   * @param function The bi-function to apply to the expressions.
   * @param x        The first expression in the pair.
   * @param y        The second expression in the pair.
   * @return A MatchResult containing the replaced expression if matching succeeds.
   */
  private def matchAndReplacePair(function: ExpressionBiFunction, x: Expression, y: Expression): MatchResult[Expression] =
    replaceExactBiFunction(Some(PureNumber))(BiFunction(x, y, function))

  /**
   * Computes the Cartesian product of two pairs of Expressions.
   * CONSIDER returning a Aggregate.
   *
   * @param p1 the first Expression pair
   * @param p2 the second Expression pair
   * @return a sequence of Expressions representing all combinations of the Cartesian product in order:
   *         LL, RL, LR, RR
   */
  private def cartesianProduct(p1: Expression ~ Expression, p2: Expression ~ Expression): Seq[Expressions] =
    Seq(p1.l ~ p2.l, p1.r ~ p2.l, p1.l ~ p2.r, p1.r ~ p2.r) // TESTME

  /**
   * Collect equal terms (fuzzy terms since this method won't be called if all the terms are exact) from an expression of the following two types:
   * TESTME (partial)
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
    case (Sum, Product) if x == y => // TESTME
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
   * TESTME (partial)
   *
   * @param f the higher level function.
   * @param g the lower level function of the left branch.
   * @param w the left/left expression.
   * @param x the left/right expression.
   * @param y the right/right expression.
   * @return a Match of the simplified expression, or a Miss.
   */
  private def collectTermsDyadicTwoLevelsR(f: ExpressionBiFunction, g: ExpressionBiFunction, w: Expression, x: Expression, y: Expression): MatchResult[Expression] = (f, g) match {
    case (Product, Power) if w == x => // TESTME
      combineAndSimplify(w, y, Power) // CONSIDER refactor using method, etc...
    case (Sum, Product) if w == x =>
      combineAndSimplify(w, y, Product)
    case (Sum, Product) if w == y => // TESTME
      combineAndSimplify(w, x, Product)
    case (Product, Sum) => // TESTME
      val terms = Seq(w ~ x, w ~ y) map matchExpressionPair(Product)
      combineTerms(Sum, terms, Miss("collectTermsDyadicTwoLevelsR: no *++ terms to collect", f ~ g ~ w ~ x ~ y))
    case _ =>
      Miss("collectTermsDyadicTwoLevelsR: functions don't match", f ~ g ~ w ~ x ~ y)
  }

  /**
   * Combines a sequence of match results of expressions using a specified binary function.
   * This function evaluates successful matches and accumulates their results based on the function
   * provided. If less than two successful matches exist, the fallback miss result is returned.
   * CONSIDER merging this with logic for Aggregate
   * TESTME only the good.size < 2 branch appears to be used.
   *
   * @param function The binary function (e.g., Sum or Product) used to combine the terms.
   * @param terms    A sequence of match results containing expressions to be combined.
   * @param miss     A fallback match result returned in case there are insufficient successful terms.
   * @return A match result containing the combined expression if the combination was successful,
   *         or the fallback miss result otherwise.
   */
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
      matchAndMaybeSimplify(result)
    }
  }

  /**
   * Creates a function that takes an `Expression` as input and applies a provided
   * `ExpressionBiFunction` to combine it with a fixed `Expression`.
   *
   * @param x        the fixed `Expression` to be used in the bi-function
   * @param function the `ExpressionBiFunction` to be applied
   * @return a function that takes an `Expression` as input and returns an `Expression`
   */
  private def formBiFunction(x: Expression, function: ExpressionBiFunction): Expression => Expression =
    BiFunction(x, _, function)

  /**
   * Performs a match operation on a BiFunction, verifying and processing
   * its components through a two-level dyadic match.
   * CONSIDER redefine as a Matcher
   *
   * @param function the BiFunction to be analyzed and processed
   * @return a MatchResult containing the processed Expression based on
   *         the match operation
   */
  private def effectivelyExactMaterialization(function: BiFunction): MatchResult[Expression] = function match {
    case BiFunction(a, b, f) => matchDyadicTwoLevels(f ~ a ~ b)
  }

  /**
   * Attempts to replace a `BiFunction` with a `MatchResult` of `Expression`.
   * This involves evaluating [[exactMaterialization]] on `function` (in the given `context`),
   * or falling back to invoke [[effectivelyExactMaterialization]] on `function`.
   *
   * @param context    An optional `Factor` that is used by the exact materialization method.
   * @param biFunction A `BiFunction` that is evaluated and potentially replaced during the materialization process.
   * @return A `MatchResult[Expression]`
   */
  private def replaceExactBiFunction(context: Context)(biFunction: BiFunction): MatchResult[Expression] =
    (exactMaterialization(context)(biFunction) map (Expression(_))) || effectivelyExactMaterialization(biFunction) // CONSIDER use exactMaterializer

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
   * TESTME this is never used, it appears.
   *
   * @param f the common commutative, associative function.
   * @param x an expression.
   * @param y an expression.
   * @param z an expression.
   * @return a match of a new expression or a miss of the originals.
   */
  private def combineExact(f: ExpressionBiFunction, x: Expression, y: Expression, z: Expression): MatchResult[Expression] = f match {
    case Sum =>
      matchAndMaybeSimplify(Aggregate(Sum, Seq(x, y, z)))
    case Product =>
    val list = List(x, y, z) map (x => exactMaterializer(x))
    combineTerms(f, list, Miss("nothing to combine", f ~ x ~ y ~ z))
  }

  /**
   * Matches and processes two dyadic levels in an expression tree.
   * If the dyadic levels are compatible for combination, it performs
   * the necessary transformation on the expression.
   * Otherwise, it returns a failure with an explanatory message.
   *
   * @return A `Matcher` that evaluates a pattern of `ExpressionBiFunction` combined with
   *         a `DyadicTriple` and an `Expression`, returning a transformed `Expression`
   *         if the conditions are met, or a failure if not.
   */
  private def matchAndCancelTwoDyadicLevelsL: Matcher[ExpressionBiFunction ~ DyadicTriple ~ Expression, Expression] = {
    case f ~ (g ~ x ~ y) ~ z if f == g && f != Power => combineExact(f, x, y, z)
    case f ~ (g ~ x ~ y) ~ z => associativeDyadic(f, g) match {
      case Some(d) => replaceExactBiFunction(Some(PureNumber))(BiFunction(x, BiFunction(z, y, d), f))
      case None => Miss(s"cannot combine two dyadic levels ($f and $g do not associate)", x)
    }
  }

  /**
   * Matches and combines two dyadic levels represented in the structure, provided they satisfy
   * specific requirements such as having the same operator and not being of the Power type.
   *
   * The method takes a matcher pattern of `ExpressionBiFunction ~ Expression ~ DyadicTriple`
   * and attempts to combine the representations when the conditions are met.
   * If the conditions are not satisfied, it produces a `Miss` with a descriptive error message.
   *
   * @return A `Matcher` that evaluates whether two dyadic levels can be matched and combined,
   *         returning the combined `Expression` on success or a `Miss` on failure.
   */
  private def matchAndCancelTwoDyadicLevelsR: Matcher[ExpressionBiFunction ~ Expression ~ DyadicTriple, Expression] = { // NOTE this layout doesn't work for power but it would work for other operators.
    case f ~ z ~ (g ~ x ~ y) if f == g && f != Power => combineExact(f, x, y, z)
    case x => Miss("matchAndCancelTwoDyadicLevelsR: cannot combine two dyadic levels", x)
  }

  /**
   * Determines whether the provided pair of `ExpressionFunction` values are complementary
   * monadic functions such as exponential and logarithmic.
   *
   * @param f The first `ExpressionFunction` value to compare.
   * @param g The second `ExpressionFunction` value to compare.
   * @return True if the functions are complementary, otherwise false.
   */
  private def complementaryMonadic(f: ExpressionFunction, g: ExpressionFunction) = (f, g) match {
    case (Exp, Log) => true
    case (Log, Exp) => true  // TESTME
    case (Negate, Negate) => true
    case (Reciprocal, Reciprocal) => true
    case _ => false // TESTME
  }

  // CONSIDER inlining this
  private def associativeDyadic(f: ExpressionBiFunction, g: ExpressionBiFunction): Option[ExpressionBiFunction] = (f, g) match {
    case (Power, Power) => Some(Product)
    case _ => None
  }
}
