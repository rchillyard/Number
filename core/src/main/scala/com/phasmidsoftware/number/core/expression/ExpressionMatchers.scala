/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.expression

import com.phasmidsoftware.matchers.{MatchLogger, ~}
import com.phasmidsoftware.number.core.expression.Expression.{isIdentityFunction, matchSimpler}
import com.phasmidsoftware.number.core.expression.Literal.someLiteral
import com.phasmidsoftware.number.core.inner.*
import com.phasmidsoftware.number.core.matchers.MatchersExtras
import com.phasmidsoftware.number.core.misc.Bumperator
import com.phasmidsoftware.number.core.numerical.{Field, Number, Real}

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

  self =>

  import com.phasmidsoftware.matchers.Matchers.*

  /**
    * Abstract class `ExpressionMatcher`, which extends `Matcher` where the input type is always `Expression`.
    *
    * @tparam R the underlying type of `MatchResult`s.
    */
  abstract class ExpressionMatcher[+R] extends Matcher[Expression, R]

  /**
    * Type alias for a pair of expressions (purpose of this is solely for brevity).
    */
//  private[expression] type Expressions = Expression ~ Expression

  /**
    * Type alias for a dyadic triple (purpose of this is solely for brevity).
    */
  private[expression] type DyadicTriple = ExpressionBiFunction ~ Expression ~ Expression

  /**
    * Type alias for the kind of ExpressionMatcher which results in a possibly different Expression.
    */
  private[expression] type ExpressionTransformer = AutoMatcher[Expression]

  /**
    * Type alias for a monadic duple (purpose of this is solely for brevity).
    */
  private[expression] type MonadicDuple = ExpressionMonoFunction ~ Expression

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
    case Sum ~ x ~ UniFunction(y, Negate) if x == y => Match(Zero) // TESTME
    case Sum ~ UniFunction(x, Negate) ~ y if x == y => Match(Zero) // TESTME
    case Sum ~ BiFunction(w, x, Sum) ~ UniFunction(y, Negate) if x == y => Match(w) // TESTME
    case Sum ~ UniFunction(x, Negate) ~ BiFunction(y, z, Sum) if x == z => Match(y) // TESTME
    case Sum ~ BiFunction(w, x, Sum) ~ UniFunction(y, Negate) if w == y => Match(x) // TESTME
    case Sum ~ UniFunction(x, Negate) ~ BiFunction(y, z, Sum) if x == y => Match(z) // TESTME
    case Product ~ x ~ UniFunction(y, Reciprocal) if x == y => Match(One)
    case Product ~ UniFunction(x, Reciprocal) ~ y if x == y => Match(One) // TESTME
    case Product ~ BiFunction(w, x, Product) ~ UniFunction(y, Reciprocal) if x == y => Match(w) // TESTME
    case Product ~ UniFunction(x, Reciprocal) ~ BiFunction(w, z, Product) if x == w => Match(z) // TESTME
    case Product ~ BiFunction(w, x, Product) ~ UniFunction(y, Reciprocal) if w == y => Match(x) // TESTME
    case Product ~ UniFunction(x, Reciprocal) ~ BiFunction(w, z, Product) if x == z => Match(w) // TESTME
    case f ~ x ~ y =>
      complementaryFields(f, x, y) match {
        case Some(z) => Match(z)
        case None => Miss("matchComplementaryExpressions: no match", f ~ x ~ y)
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
  private def complementaryFields(f: ExpressionBiFunction, x: Expression, y: Expression): Option[Expression] =
    if (x.maybeFactor == y.maybeFactor) { // TODO logic here is same as for value in BiFunction
      val fo = f.evaluateAsIs(x, y)
      (fo, f.maybeIdentityL) match {
        case (Some(field1), Some(field2)) if field1 == field2 =>
          someLiteral(field1)
        case (Some(Real(Number.zeroR)), Some(field2)) if field2.isZero =>
          someLiteral(field2)
        case _ =>
          None
      }
    }
    else None

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
        y.evaluateAsIs match {
          case Some(y) => fx.canRaise(fy, y)
          case _ => false
        }
      case _ => 
        false // TESTME
    }).contains(true)

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
    case FieldExpression(x, _) => Match(x)
    case x@Number(_, _) => Match(Real(x)) // TESTME
    case x: FieldExpression => matchIfDefined(x.evaluateAsIs)(x)
    case x => Miss("value", x)
  }

  /**
    * Method to define a `Matcher` to match a specific Number.
    * CONSIDER using `matches`.
    *
    * @param x the `Number` to match.
    * @return a `AutoMatcher[Field]` which matches only on x.
    */
  private def matchNumber(x: Field): AutoMatcher[Field] =
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
  def matchBiFunctionAsAggregate: Matcher[BiFunction, Aggregate] = Matcher[BiFunction, Aggregate]("matchBiFunctionAsAggregate") {
    case BiFunction(BiFunction(w, x, Sum), BiFunction(y, z, Sum), Product) =>
      Match(Aggregate(Sum, Seq((w * y).simplify, (w * z).simplify, (x * y).simplify, (x * z).simplify)))
    case BiFunction(BiFunction(w, x, f), BiFunction(y, z, g), h) if f == g && g == h =>
      Match(Aggregate(f, Seq(w, x, y, z)))
    case BiFunction(BiFunction(w, x, f), y, h) if f == h =>
      Match(Aggregate(f, Seq(w, x, y)))
    case BiFunction(x, BiFunction(y, z, f), h) if f == h =>
      Match(Aggregate(f, Seq(x, y, z)))
    case x =>
      Miss("matchBiFunctionAsAggregate: is not converted to Aggregate:", x)
  }

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
    * NOTE: not deprecated because it's used in simplifyComposite
    *
    * @return A `ExpressionTransformer` that matches and simplifies `Aggregate` expressions efficiently.
    * @note Throws java.util.NoSuchElementException due to invocation of get on Option (very unlikely).
    */
  def simplifyAggregate: Matcher[Aggregate, Expression] = Matcher[Aggregate, Expression]("simplifyAggregate") {
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
      (complementaryTermsEliminatorAggregate & alt(matchSimpler.asInstanceOf[Matcher[Expression, Expression]]))(a)
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
  def complementaryTermsEliminatorAggregate: Matcher[Aggregate, Expression] = Matcher("complementaryTermsEliminatorAggregate") {
    case a@Aggregate(f, xs) =>
      val invertFunction: Double => Double = f match {
        case Sum =>
          x => Math.abs(x)
        case Product =>
          x => if (x < 1) 1 / x else x
        case _ =>
          throw new IllegalArgumentException("complementaryTermsEliminatorAggregate: $f function not supported")
      }
      val sortFunction: Expression => Double =
        x => invertFunction(x.approximation.flatMap(_.maybeDouble) getOrElse Double.NaN)

      // NOTE we should handle the very rare cases where the final get fails
      // NOTE this ordering is really only appropriate when f is Sum.
      // TODO find a better way to find complementary elements.
      Try(xs.sortBy(sortFunction)) match {
        case Success(sorted) =>
          val list = Bumperator[Expression](sorted) { (x, y) => isComplementary(f, x, y) }.toList
          if (list != xs)
            // CONSIDER write=ing instead `Match(CompositeExpression(f, list))` But be careful!
            Match(Aggregate(f, list))
          else
            Miss(s"complementaryTermsEliminatorAggregate: $a", a)
        case Failure(x) =>
          Error(x) // XXX the result of an extremely improbable NoSuchElementException // TESTME
      }
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
  def literalsCombiner: Matcher[Aggregate, Expression] = Matcher("literalsCombiner") {
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
  def complementaryTermsEliminatorBiFunction: Matcher[BiFunction, Expression] = Matcher("complementaryTermsEliminatorBiFunction") {
    case BiFunction(a, b, f) if isComplementary(f, a, b) =>
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
    * Determines if the given bi-function is complementary for the specified expressions.
    *
    * @param f the bi-function to evaluate
    * @param x the first expression to be checked
    * @param y the second expression to be checked
    * @return true if the bi-function is complementary for the given expressions, false otherwise
    */
  private def isComplementary(f: ExpressionBiFunction, x: Expression, y: Expression): Boolean = {
    val identityCheck: Expression => Boolean = isIdentityFunction(f)
    (matchComplementaryExpressions(f ~ x ~ y) & filter(identityCheck)).successful
  }

  /**
    * Determines whether the provided pair of `ExpressionMonoFunction` values are complementary
    * monadic functions such as exponential and logarithmic.
    *
    * @param f The first `ExpressionMonoFunction` value to compare.
    * @param g The second `ExpressionMonoFunction` value to compare.
    * @return True if the functions are complementary, otherwise false.
    */
  def complementaryMonadic(f: ExpressionMonoFunction, g: ExpressionMonoFunction): Boolean = (f, g) match {
    case (Exp, Ln) => true
    case (Ln, Exp) => true  // TESTME
    case (Negate, Negate) => true
    case (Reciprocal, Reciprocal) => true
    case _ => false // TESTME
  }
}
