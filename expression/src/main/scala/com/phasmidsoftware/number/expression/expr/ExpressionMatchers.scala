/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.matchers.{MatchLogger, ~}
import com.phasmidsoftware.number.algebra.*
import com.phasmidsoftware.number.algebra.core.{AnyContext, RestrictedContext, Valuable}
import com.phasmidsoftware.number.algebra.eager.{Angle, Monotone, Number}
import com.phasmidsoftware.number.core.inner.PureNumber
import com.phasmidsoftware.number.core.numerical
import com.phasmidsoftware.number.core.numerical.Field
import com.phasmidsoftware.number.expression.expr.BiFunction.asAggregate
import com.phasmidsoftware.number.expression.expr.Expression.{isIdentityFunction, matchSimpler}
import com.phasmidsoftware.number.expression.expr.{Aggregate, BiFunction, UniFunction}
import com.phasmidsoftware.number.expression.matchers.MatchersExtras
import com.phasmidsoftware.number.{core, expression}

import scala.language.implicitConversions
import scala.util.{Failure, Success}

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
class ExpressionMatchers(using val matchLogger: MatchLogger) extends MatchersExtras {

  self =>

  import com.phasmidsoftware.matchers.Matchers.*

  /**
    * Abstract class `ExpressionMatcher`, which extends `Matcher` where the input type is always `Expression`.
    *
    * @tparam R the underlying type of `MatchResult`s.
    */
  abstract class ExpressionMatcher[+R] extends Matcher[Expression, R]

  def MatchCheck[R](r: R)(o: R): Match[R] =
    if r == o then {
      System.err.println(s"Match is unchanged: $r")
      Match(r)
    } else
      Match(r)

  /**
    * Type alias for a pair of expressions (purpose of this is solely for brevity).
    */
  //  private[expression] type Expressions = Expression ~ Expression

  /**
    * Type alias for a dyadic triple (purpose of this is solely for brevity).
    */
  private[number] type DyadicTriple = ExpressionBiFunction ~ Expression ~ Expression

  /**
    * Type alias for the kind of ExpressionMatcher which results in a possibly different Expression.
    */
  private[number] type ExpressionTransformer = AutoMatcher[Expression]

  /**
    * Type alias for a monadic duple (purpose of this is solely for brevity).
    */
  private[number] type MonadicDuple = ExpressionMonoFunction ~ Expression

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
    * Matches complementary mathematical expressions based on specified patterns.
    * The method leverages rules for operations such as summation and multiplication
    * and identifies complementary pairs (e.g., a number and its negation or reciprocal).
    * The resulting expression is derived from the matched rule or an indication of no match.
    *
    * @return A `Matcher` that attempts to match the provided `DyadicTriple` to a simplified `Expression`,
    *         or returns information about a failure to match.
    */
  def matchComplementaryExpressions: Matcher[DyadicTriple, Expression] = Matcher("matchComplementaryExpressions") {
    case Sum ~ x ~ UniFunction(y, Negate) if x == y =>
      Match(Zero) // TESTME
    case Sum ~ UniFunction(x, Negate) ~ y if x == y =>
      Match(Zero) // TESTME
    case Sum ~ BiFunction(w, x, Sum) ~ UniFunction(y, Negate) if x == y =>
      Match(w) // TESTME
    case Sum ~ UniFunction(x, Negate) ~ BiFunction(y, z, Sum) if x == z =>
      Match(y) // TESTME
    case Sum ~ BiFunction(w, x, Sum) ~ UniFunction(y, Negate) if w == y =>
      Match(x) // TESTME
    case Sum ~ UniFunction(x, Negate) ~ BiFunction(y, z, Sum) if x == y =>
      Match(z) // TESTME
    case Product ~ x ~ UniFunction(y, Reciprocal) if x == y =>
      Match(One)
    case Product ~ UniFunction(x, Reciprocal) ~ y if x == y =>
      Match(One) // TESTME
    case Product ~ BiFunction(w, x, Product) ~ UniFunction(y, Reciprocal) if x == y =>
      Match(w) // TESTME
    case Product ~ UniFunction(x, Reciprocal) ~ BiFunction(w, z, Product) if x == w =>
      Match(z) // TESTME
    case Product ~ BiFunction(w, x, Product) ~ UniFunction(y, Reciprocal) if w == y =>
      Match(x) // TESTME
    case Product ~ UniFunction(x, Reciprocal) ~ BiFunction(w, z, Product) if x == z =>
      Match(w) // TESTME
    case Power ~ BiFunction(w, x, Power) ~ z if x :* z == One =>
      Match(w)
    case f ~ x ~ y =>
      ExpressionMatchers.complementaryExpressions(f, x, y) match {
        case Some(z) =>
          Match(z)
        case None =>
          Miss("matchComplementaryExpressions: no match", f ~ x ~ y)
      }
  }

  /**
    * Method to create an ExpressionMatcher.
    *
    * @param f a function Expression => MatchResult[R]
    * @tparam R the MatchResult type.
    * @return a Matcher[Expression, R] which is also an ExpressionMatcher[R].
    */
  def ExpressionMatcher[R](f: Expression => MatchResult[R]): ExpressionMatcher[R] = (e: Expression) => f(e)

  /**
    * Determines whether the factors of the given expressions match
    * according to the specified binary function.
    * NOTE there's another factorsMatch method in Expression
    * NOTE this method appears to be used only by ExpressionMatchersSpec
    *
    * @param f The binary function that determines the operation type (e.g., Sum, Product, Power).
    * @param x The first expression to compare.
    * @param y The second expression to compare.
    * @return True if the factors match according to the binary function, false otherwise.
    */
  def factorsMatch(f: ExpressionBiFunction, x: Expression, y: Expression): Boolean =
    (for fx <- x.maybeFactor(AnyContext); fy <- y.maybeFactor(AnyContext) yield f match {
      case Sum =>
        fx.canAdd(fy)
      case Product =>
        fx.canMultiply(fy)
      case Power =>
        y.evaluateAsIs match {
          case Some(Valuable(z: Field)) =>
            fx.canRaise(fy, z)
          case _ =>
            false
        }
      case _ =>
        false
    }).contains(true)

  /**
    * Matcher which matches on Expressions that directly represents a specific given Field.
    *
    * @param x the Number to match.
    * @return a Matcher[Expression, Number].
    */
  def matchValue(x: Valuable): ExpressionMatcher[Valuable] = (value & matchNumber(x)) :| s"matchValue($x)"

  /**
    * Matcher which matches on Expressions that directly represent Numbers.
    *
    * @return an ExpressionMatcher[Field].
    */
  def value: ExpressionMatcher[Valuable] = {
    case Literal(v, _) =>
      Match(v) // TESTME
    case ValueExpression(v, _) =>
      Match(v)
    //    case x@core.Number(_, _) => Match(Real(x)) // TESTME
    case x: ValueExpression =>
      matchIfDefined(x.evaluateAsIs)(x)
    case x =>
      Miss("value", x)
  }

  /**
    * Method to define a `Matcher` to match a specific Number.
    * CONSIDER using `matches`.
    *
    * @param x the `Number` to match.
    * @return a `AutoMatcher[Field]` which matches only on x.
    */
  private def matchNumber(x: Valuable): AutoMatcher[Valuable] =
    Matcher("matchNumber") {
      case `x` => Match(x) // TESTME
      case e => Miss("matchNumber", e)
    }

  /**
    * Matches and transforms nested BiFunction structures into an Aggregate if they meet
    * specific conditions where the nested functions share the same operation.
    * Specifically:
    * - When nested `BiFunction` objects share the same binary function at all levels, they are aggregated into a single `Aggregate` expression with all the operands.
    * - If no valid aggregation pattern is matched, the original expression is returned unchanged.
    *
    * @return A `Matcher[BiFunction, Aggregate]` that attempts to match a BiFunction and
    *         transform it into an Aggregate if conditions are satisfied. If the input
    *         does not meet the required conditions, it returns a Miss.
    */
  def matchBiFunctionAsAggregate: Matcher[BiFunction, expression.expr.Aggregate] = Matcher[BiFunction, expression.expr.Aggregate]("matchBiFunctionAsAggregate")(
    biFunction =>
      matchOptionFunc2(asAggregate)(biFunction))

  /**
    * Simplifies a `Aggregate` expression by combining its terms in a more compact form.
    * This method handles several cases:
    * - If the `Aggregate` is empty, it produces the appropriate identity.
    * - If the `Aggregate` contains a single element, it simplifies that element.
    * - If all elements in the `Aggregate` can be interpreted as numbers, it combines them iteratively
    * and simplifies the resulting expression.
    * - If the `Aggregate` cannot be simplified further, the original structure is retained.
    *
    * NOTE: Need to fix #87
    * NOTE: not deprecated because it's used in simplifyStructural
    *
    * @return A `ExpressionTransformer` that matches and simplifies `Aggregate` expressions efficiently.
    * @note Throws java.util.NoSuchElementException due to invocation of get on Option (very unlikely).
    */
  def simplifyAggregate: Matcher[expression.expr.Aggregate, Expression] = Matcher[expression.expr.Aggregate, Expression]("simplifyAggregate") {
    case Aggregate(Sum, Nil) =>
      Match(Zero) // TESTME
    case Aggregate(Product, Nil) =>
      Match(One) // TESTME
    case Aggregate(Power, Nil) =>
      Miss("simplifyAggregate: cannot simplify Power(0, 0)", Aggregate(Power, Nil)) // TESTME
    case Aggregate(_, x :: Nil) =>
      Match(x) // XXX we should not need to simplify x since simplifyComponents should already have done that
    // NOTE it's important that you do not reintroduce a match into a BiFunction!
    case a@Aggregate(_, _) =>
      // TODO asInstanceOf
      ((angleEliminatorAggregate | complementaryTermsEliminatorAggregate) & alt(matchSimpler.asInstanceOf[Matcher[Expression, Expression]]))(a)
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
    * TODO refactor and move what we can into Aggregate.
    *
    * @return A `Matcher[Aggregate, Expression]` that either matches an aggregate
    *         expression with reduced complementary terms or returns a miss description
    *         if no reduction takes place.
    */
  def complementaryTermsEliminatorAggregate: Matcher[Aggregate, Expression] = Matcher[Aggregate, Expression]("complementaryTermsEliminatorAggregate") {
    a =>
      a.eliminateComplementaryTerms(isComplementary) match {
        case Success(list) =>
          matchOrMiss(a.function, list)(a)
        case Failure(x) =>
          Error(x) // XXX the result of an extremely improbable NoSuchElementException // TESTME
      }
  }

  /**
    * A Matcher that processes an `Aggregate` with the `Product` function, identifying specific
    * expressions containing angles and their reciprocals. If both angles and reciprocal angles
    * are found, these expressions are restructured and returned as a new `Aggregate` with
    * modified components. If no appropriate angles or reciprocals are found, or the input is
    * not a `Product` aggregate, the Matcher will fail.
    *
    * @return A `Matcher[Aggregate, Expression]` that matches aggregates with angle and reciprocal
    *         angle expressions, restructuring them accordingly. Returns a `Miss` otherwise.
    */
  private def angleEliminatorAggregate: Matcher[Aggregate, Expression] = Matcher[Aggregate, Expression]("angleEliminatorAggregate") {
    case a@Aggregate(Product, xs) =>
      if (Aggregate.hasAngles(xs) && Aggregate.hasReciprocalAngles(xs))
        Match(Aggregate(Product, Aggregate.getAnglesEtc(xs)))
      else
        Miss("angleEliminatorAggregate: no angles", a)
    case a =>
      Miss("angleEliminatorAggregate: not a Product Aggregate", a)
  }

  /**
    * Matches and combines atomic literal expressions within an `Aggregate` structure.
    * If atomic literals are present and can be evaluated, they are combined and the result
    * is returned. The remaining non-literal expressions, if any, are preserved.
    *
    * @return A matcher that processes an `Aggregate` instance. Returns `Match` if literals
    *         are successfully combined, or `Miss` if no literals are found or they cannot
    *         be combined.
    */
  def literalsCombiner: Matcher[Aggregate, Expression] = Matcher[expression.expr.Aggregate, Expression]("literalsCombiner") {
    case g@Aggregate(f, xs) =>
      // XXX first, we partition `xs` according to which terms can be exactly combined because they are all pure numbers (the "literals").
      // The other terms may also be literal constants but not evaluatable in the `PureNumber` context.
      // NOTE that we might be being a little over-restrictive here.
      // CONSIDER relaxing `isAtomic`
      xs.partition(exp => exp.isAtomic && exp.evaluate(RestrictedContext(PureNumber)).isDefined) match {
        case (Nil, _) =>
          Miss("literalsCombiner: no pure numbers", g)
        case (literals, others) =>
          // XXX next, we evaluate an Aggregate based solely on the pure number elements (literals) and combine the result with the other elements (others)
          // CONSIDER using CompositeExpression.apply
          (Aggregate(f, literals).evaluate(RestrictedContext(PureNumber)), others) match {
            case (Some(z), Nil) =>
              Match(z)
            case (Some(z), h :: Nil) =>
              Match(BiFunction(z, h, f))
            case (Some(z), _) =>
              Match(Aggregate(f, others :+ z))
            case _ =>
              Miss(s"literalsCombiner: cannot combine literals", Aggregate(f, others))
          }
      }
  }

  /**
    * A matcher function that processes instances of `BiFunction` and evaluates whether its terms
    * are complementary based on a provided condition. If the terms are complementary, it attempts
    * to eliminate them by returning their identity literal. Otherwise, it provides information
    * about why the matching failed.
    *
    * @return A `Matcher[BiFunction, Expression]` where:
    *         - If the terms of the `BiFunction` are complementary and have an identity, it
    *           returns a `Match` containing the identity as a `Literal`.
    *         - If the terms are complementary but lack an identity, it returns a `Miss`
    *           with a corresponding message and the original `BiFunction`.
    *         - If the terms are not complementary, it returns a `Miss` with a message
    *           indicating this and the input term.
    */
  def complementaryTermsEliminatorBiFunction(complementaryPredicate: (ExpressionBiFunction, Expression, Expression) => Boolean): Matcher[BiFunction, Expression] = Matcher[BiFunction, Expression]("complementaryTermsEliminatorBiFunction") {
    case BiFunction(a, b, f) if complementaryPredicate(f, a, b) && a.maybeFactor(AnyContext).contains(Angle) =>
      Match(Literal(Angle.zero))
    case BiFunction(a, b, f) if complementaryPredicate(f, a, b) =>
      f.maybeIdentityL match {
        case Some(field) =>
          Match(Literal(field))
        case None =>
          Miss(s"complementaryTermsEliminatorBiFunction: no identity for $f", BiFunction(a, b, f))
      }
    case x =>
      Miss(s"complementaryTermsEliminatorBiFunction: not complementary", x)
  }

  /**
    * Determines whether the provided pair of `ExpressionMonoFunction` values are complementary
    * monadic functions such as exponential and logarithmic.
    *
    * TODO move this into ExpressionMonoFunction.
    *
    * @param f The first `ExpressionMonoFunction` value to compare.
    * @param g The second `ExpressionMonoFunction` value to compare.
    * @return True if the functions are complementary, otherwise false.
    */
  def complementaryMonadic(f: ExpressionMonoFunction, g: ExpressionMonoFunction): Boolean = (f, g) match {
    case (Exp, Ln) => true
    case (Ln, Exp) => true // TESTME
    case (Negate, Negate) => true
    case (Reciprocal, Reciprocal) => true
    case _ => false // TESTME
  }

  /**
    * Return a `Match` if the size of the list was reduced, otherwise return a `Miss`.
    * If the list is empty, a match on an appropriate identity is returned.
    * If the list is shorter than the original, a `Match` on a new `Aggregate` is returned.
    * If the list is the same length as the original, a `Miss` is returned.
    *
    * TODO refactor and move what we can into ExpressionBiFunction
    *
    * @param f    A function combining two expressions into a composite expression.
    * @param list The list of the remaining (non-complementary) expressions to be processed.
    * @param a    THe original aggregate instance used for additional computation and context.
    * @return A `MatchResult` which can either be a match or a miss
    *         depending on the contents of the given `list`.
    */
  private def matchOrMiss(f: ExpressionBiFunction, list: List[Expression])(a: Aggregate): MatchResult[Expression] =
    if list.isEmpty
    then
      // NOTE if the first term of xs is an angle, then the result will be an angle.
      // CONSIDER using convert here.
      if (a.xs.headOption.getOrElse(Zero).materialize.isInstanceOf[Angle])
        Match(Literal(Angle.zero))
      else
        Match(Zero)
    else if list != a.xs
    // CONSIDER writing instead `MatchCheck(CompositeExpression(f, list))` But be careful!
    then
      MatchCheck(Aggregate(f, list))(a)
    else
      Miss(s"complementaryTermsEliminatorAggregate: $a", a)

  /**
    * Determines if the given bi-function is complementary for the specified expressions.
    *
    * TODO move this into ExpressionBiFunction.
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
}

/**
  * The `ExpressionMatchers` object provides utilities for evaluating relationships between
  * two `Expression` objects using a given `ExpressionBiFunction`. It enables analysis
  * of complementary expressions under specific mathematical or logical contexts.
  *
  * These methods are here so that they're not part of the `ExpressionMatchers` API.
  */
object ExpressionMatchers {
  /**
    * Evaluates whether two `Expression` instances, when combined using the provided
    * `ExpressionBiFunction`, yield the appropriate identity value (although, in practice, we shortcut that logic a little).
    *
    * This method is designed to identify cases where the resulting `Expression` meets
    * particular characteristics, such as being a zero-valued `Monotone` or a unit-valued `Unitary`.
    *
    * TODO Move this into ExpressionBiFunction.
    *
    * @param f The binary function (`ExpressionBiFunction`) applied to evaluate the relationship
    *          between the two `Expression` instances.
    * @param x The first `Expression` operand used in the evaluation.
    * @param y The second `Expression` operand used in the evaluation.
    * @return An `Option[Expression]` containing the complementary result if the specified
    *         conditions are met, or `None` if no such complement is identified.
    */
  def complementaryExpressions(f: ExpressionBiFunction, x: Expression, y: Expression): Option[Expression] =
    f.evaluate(x, y)(AnyContext) match {
      case Some(z: Monotone) if z.isZero && f == Sum =>
        Some(Literal(z))
      case Some(z: Number) if z.isUnity && f == Product =>
        Some(Literal(z))
      case Some(z) if f.maybeIdentityL.contains(z) => // CONSIDER this case doesn't add anything
        Some(Literal(z))
      case x =>
        None
    }

  def getApproximateDouble(x: Expression): Double =
    x.approximation(true).flatMap(_.maybeDouble) getOrElse Double.NaN

}