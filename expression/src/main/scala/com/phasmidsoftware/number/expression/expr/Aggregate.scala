/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.eager
import com.phasmidsoftware.number.algebra.eager.*
import com.phasmidsoftware.number.algebra.util.FP
import com.phasmidsoftware.number.core.inner.{Factor, PureNumber, Radian, Rational}
import com.phasmidsoftware.number.core.misc.Bumperator
import com.phasmidsoftware.number.core.numerical
import com.phasmidsoftware.number.core.numerical.{ComplexPolar, Number}
import com.phasmidsoftware.number.expression.algebraic
import com.phasmidsoftware.number.expression.algebraic.QuadraticEquation
import com.phasmidsoftware.number.expression.expr.Expression.em.{DyadicTriple, MonadicDuple}
import com.phasmidsoftware.number.expression.expr.Expression.{ExpressionOps, em, given_LatexRenderer_Expression, matchSimpler}
import com.phasmidsoftware.number.expression.expr.ExpressionMatchers.componentsSimplifier
import com.phasmidsoftware.number.{algebra, core, expression}

import java.util.Objects
import scala.language.implicitConversions
import scala.util.*

/**
  * Represents a composite expression that computes the total of a sequence of expressions.
  * The sequence must be non-empty.
  *
  * @constructor Constructs an Aggregate instance with a sequence of expressions.
  * @param xs A non-empty sequence of expressions to be totaled.
  */
case class Aggregate(function: ExpressionBiFunction, xs: Seq[Expression]) extends Multiple {
  /**
    * Determines whether this `Valuable` is exact, i.e., has no approximation.
    *
    * CONSIDER it may be possible that there are non-approximatable entities that are not exact either.
    *
    * The method returns `true` if there is no approximate representation
    * available (i.e., `approximation` is `None`), indicating that the
    * entity is exact. Otherwise, it returns `false`.
    *
    * @return a `Boolean` indicating whether the entity is exact (`true`)
    *         or has an approximation (`false`).
    */
  lazy val isExact: Boolean = xs.forall(_.isExact)

  /**
    * Renders the `CompositeExpression` as a string representation of the expression itself,
    * typically in a structured or human-readable mathematical format.
    *
    * @return a string representation of the `CompositeExpression` as an expression.
    */
  def renderAsExpression: String =
    s"{$function ${xs.map(_.render).mkString(" ")}}"

  /**
    * Simplifies the components of this `CompositeExpression` using a matching mechanism to identify
    * and transform sub-expressions into simpler forms if possible.
    *
    * @return the result of the simplification attempt encapsulated in a `MatchResult`, which either contains
    *         a simplified `Expression` or indicates that no simplification was possible.
    */
  lazy val operandsMatcher: em.AutoMatcher[Expression] =
    em.Matcher("Aggregate: simplifyOperands") {
      case Aggregate(f, xs) =>
        componentsSimplifier(xs, ys => Aggregate(f, ys))
    }

  /**
    * Simplifies a given `Expression` by removing trivial components or replacing grouped terms
    * with their corresponding higher-level mathematical function (if applicable).
    *
    * This method uses pattern matching to identify and transform instances of `Aggregate`:
    * - Removes identity elements for the aggregate function.
    * - Groups identical terms and replaces them with a new aggregate expression
    *   using the next higher mathematical function.
    *
    * @return an `AutoMatcher` for `Expression` capable of identifying and performing
    *         simplifications of trivial aggregates.
    */
  lazy val identitiesMatcher: em.AutoMatcher[Expression] =
    em.Matcher[Expression, Expression]("Aggregate: identitiesMatcher") {
      // Remove identity values
      case a@Aggregate(f, xs) =>
        val nonIdentity = xs filterNot (x => f.maybeIdentityL.contains(x))
        // NOTE we ensure that it only returns a Match when the result is smaller than the original
        em.MatchResult(nonIdentity.size < xs.size, a, Aggregate(f, nonIdentity))
    }

  /**
    * Simplifies a composite `Expression` by leveraging the `simplifyAggregate` method.
    * This is typically used to reduce composite mathematical expressions into their simplest form when possible.
    *
    * @return an `AutoMatcher` for `Expression` that applies the simplification logic defined in `simplifyAggregate`.
    */
  lazy val structuralMatcher: em.AutoMatcher[Expression] = em.Matcher[Expression, Expression]("BiFunction: structuralMatcher") {
    case t: Aggregate =>
      em.simplifyAggregate(t)
    case x: Expression =>
      em.Miss[Expression, Expression]("Aggregate.structuralMatcher: not aggregate", x)
  }

  /**
    * Eliminates complementary terms from the current aggregate based on a specified predicate.
    *
    * The method identifies pairs of expressions within the aggregate that satisfy the provided
    * `complementaryPredicate` function and removes them, returning the remaining expressions.
    *
    * CONSIDER This is currently called only once so it could be converted to a lazy def by fixing the parameter.
    *
    * @param complementaryPredicate a function that determines whether two expressions within the
    *                               context of a provided aggregate function are complementary.
    *                               The predicate takes three arguments:
    *                               - The aggregate function (`ExpressionBiFunction`: e.g., Sum or Product).
    *                               - The first expression to evaluate (`Expression`).
    *                               - The second expression to evaluate (`Expression`).
    *                               It returns a `Boolean` indicating if the expressions are complementary.
    * @return a `Try[List[Expression]]` containing the list of expressions after removing complementary pairs
    *         identified by the predicate. If an error occurs (e.g., unsupported operation), a `Failure` is returned.
    */
  def eliminateComplementaryTerms(complementaryPredicate: (ExpressionBiFunction, Expression, Expression) => Boolean): Try[List[Expression]] = {
    val invertFunction: Double => Double = function match {
      case Sum =>
        x => Math.abs(x)
      case Product =>
        x => if x < 1 then 1 / x else x
      case _ =>
        throw new IllegalArgumentException("complementaryTermsEliminatorAggregate: Power function not supported")
    }

    // NOTE this ordering is really only appropriate when f is Sum.
    // TODO find a better way to find complementary elements.
    val sortFunction: Expression => Double =
      x => invertFunction(FP.recover(x.approximation.map(_.toDouble))(ExpressionException(s"Cannot approximate $x for complementary term elimination")))

    val expressions = xs.sortBy(sortFunction)
    Try(expressions) map (sorted => Bumperator[Expression](sorted) { (x, y) => complementaryPredicate(function, x, y) }.toList)
  }

  /**
    * Attempts to retrieve a factor based on the provided context.
    * This method evaluates whether there is an applicable factor within the given context.
    *
    * @param context the context in which the factor is evaluated.
    * @return an optional `Factor` if one qualifies under the provided context; otherwise, `None`.
    */
  def maybeFactor(context: Context): Option[Factor] =
    evaluate(context) flatMap (v => v.maybeFactor(context))

  /**
    * Evaluates the given context to produce an optional field, leveraging the aggregate function
    * and associated evaluation logic. The method employs an iterative process to evaluate a sequence
    * of terms within the provided context while considering identity elements and context transformation rules.
    *
    * @param context the context in which the evaluation should take place. This defines the constraints
    *                and qualifications for the terms involved in the evaluation process.
    * @return an `Option[Field]` representing the result of the evaluation. Returns `None`
    *         if the evaluation cannot produce a valid field or if an invalid context is encountered.
    */
  def evaluate(context: Context): Option[Eager] = {
    val vo = xs.foldLeft(function.maybeIdentityL) {
      (ao, x) =>
        for (a <- ao; b <- x.evaluateAsIs; c <- function.applyExact(a, b)) yield c
    }
    context.qualifyingEagerValue(vo)
  }

  /**
    * Adds a new expression to the current aggregate.
    *
    * @param x the expression to be added
    * @return a new Aggregate instance with the updated list of expressions
    */
  def add(x: Expression): Aggregate =
    copy(xs = xs :+ x)

  /**
    * Adds a sequence of expressions to the current aggregate.
    *
    * @param ys the sequence of expressions to be added
    * @return a new Aggregate instance with the updated list of expressions
    */
  def addAll(ys: Seq[Expression]): Aggregate =
    copy(xs = xs ++ ys)

  /**
    * Provides the terms that comprise this `CompositeExpression`.
    *
    * @return xs.
    */
  lazy val terms: Seq[Expression] = xs

  /**
    * Method to determine the depth of this Expression.
    *
    * @return the depth (an atomic expression has depth of 1).
    */
  lazy val depth: Int =
    xs.map(_.depth).max + 1

  /**
    * Provides an approximation of this number, if applicable.
    *
    * This method attempts to compute an approximate representation of the number
    * in the form of a `Real`, which encapsulates uncertainty or imprecision
    * in its value. If no meaningful approximation is possible for the number, it
    * returns `None`.
    *
    * @return an `Option[Real]` containing the approximate representation
    *         of this `Number`, or `None` if no approximation is available.
    */
  def approximation(force: Boolean): Option[eager.Real] = {
    val identity: Eager = function.maybeIdentityL.getOrElse(Eager.zero) // NOTE should never require the default
    val vos: Seq[Option[eager.Real]] = xs map (x => x.approximation(force))
    // TODO asInstanceOf
    FP.sequence(vos) map (xs => xs.foldLeft[Eager](identity)(function.apply).asInstanceOf[eager.Real])
  }

  /**
    * Method to render this Structure in a presentable manner.
    *
    * @return a String
    */
  override def toString: String =
    xs.map(_.show).mkString(s"Aggregate{${function.toString},", ",", "}")
}

/**
  * Companion object for the `Aggregate` class.
  */
object Aggregate {

  /**
    * Creates an empty `Aggregate` instance using the provided binary function.
    * The resulting `Aggregate` will have an empty sequence of expressions.
    *
    * @param function the binary function used to compose the `Aggregate`.
    * @return an empty `Aggregate` instance.
    */
  def empty(function: ExpressionBiFunction): Aggregate =
    new Aggregate(function, Seq.empty)

  /**
    * Creates an instance of `Aggregate` using the given binary function and sequence of expressions.
    * Throws an `IllegalArgumentException` if the sequence of expressions is empty.
    *
    * @param function the binary function used to compose the `Aggregate`.
    * @param xs       a sequence of `Expression` objects to be aggregated.
    * @return an `Aggregate` instance composed of the binary function and the sequence of expressions.
    * @note Throws java.lang.IllegalArgumentException if the sequence of expressions is empty.
    */
  def create(function: ExpressionBiFunction, xs: Seq[Expression]): Aggregate =
    if xs.nonEmpty then
      new Aggregate(function, xs)
    else
      throw new IllegalArgumentException("total requires at least one argument (use empty if necessary)")

  /**
    * Constructs an `Aggregate` expression that applies the `Sum` operation
    * to a variable number of input expressions.
    *
    * @param xs a sequence of `Expression` instances that will be aggregated by the `Sum` operation.
    * @return an `Expression` representing the sum of the input expressions.
    */
  def total(xs: Expression*): Aggregate =
    create(Sum, xs)

  /**
    * Constructs an `Aggregate` expression that applies the `Product` operation
    * to a variable number of input expressions.
    *
    * @param xs a sequence of `Expression` instances that will be aggregated by the `Product` operation.
    * @return an `Expression` representing the product of the input expressions.
    */
  def product(xs: Expression*): Aggregate =
    create(Product, xs)

  /**
    * Processes a sequence of expressions and separates them into angles, reciprocal angles,
    * and other non-angle expressions. The method applies specific transformations to recognize
    * angles and reciprocal angles, and combines the results into a single sequence.
    *
    * @param xs the sequence of expressions (`Seq[Expression]`) to be analyzed and processed.
    *           The expressions may include angles, reciprocal angles, or other types.
    *
    * @return a sequence of processed expressions, including angles, reciprocal angles,
    *         and other non-angle expressions.
    */
  def getAnglesEtc(xs: Seq[Expression]): Seq[Expression] = {
    val angles: Seq[Expression] = xs.collect { case Literal(Angle(n, _), _) => Seq(Literal(n), Pi) }.flatten
    val others = xs.filterNot {
      case Literal(_: Angle, _) => true
      case _ => false
    }
    val reciprocalAngles = others.collect { case UniFunction(Literal(Angle(n, _), _), Reciprocal) => Seq(Literal(n).reciprocal, Pi.reciprocal) }.flatten
    val nonAngles = others.filterNot {
      case UniFunction(Literal(_: Angle, _), Reciprocal) => true
      case _ => false
    }
    angles ++ reciprocalAngles ++ nonAngles
  }

  /**
    * Determines whether the given sequence of expressions contains at least one
    * expression that represents a reciprocal angle.
    *
    * @param xs the sequence of `Expression` objects to analyze.
    *           Each `Expression` may represent an angle, a reciprocal angle,
    *           or another type of mathematical expression.
    *
    * @return `true` if the sequence contains a reciprocal angle, otherwise `false`.
    */
  def hasReciprocalAngles(xs: Seq[Expression]): Boolean =
    xs.exists {
      case UniFunction(Literal(_: Angle, _), Reciprocal) => true
      case _ => false
    }

  /**
    * Determines whether the given sequence of expressions contains at least one
    * expression that represents an angle.
    *
    * @param xs the sequence of `Expression` objects to analyze. Each `Expression`
    *           may represent an angle or another type of mathematical expression.
    *
    * @return `true` if the sequence contains at least one expression that represents an angle,
    *         otherwise `false`.
    */
  def hasAngles(xs: Seq[Expression]): Boolean =
    xs.exists {
      case Literal(_: Angle, _) => true
      case _ => false
    }
}
