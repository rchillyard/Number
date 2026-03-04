/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.eager
import com.phasmidsoftware.number.algebra.eager.Eager
import com.phasmidsoftware.number.algebra.util.FP
import com.phasmidsoftware.number.core.inner.PureNumber
import com.phasmidsoftware.number.expression.expr.Expression.{em, matchSimpler}
import com.phasmidsoftware.number.{algebra, expression}

import scala.language.implicitConversions

/**
  * A trait representing a composite mathematical or logical expression. It is a higher-order
  * expression that combines multiple sub-expressions into a structured form. This trait provides
  * methods and utilities for simplifying, rendering, and interpreting such expressions.
  *
  * `CompositeExpression` inherits from the `Expression` trait and serves as a foundation for more
  * specific composite structures, such as aggregates or compound expressions.
  *
  * NOTE: this trait was formerly sealed. It has been unsealed to allow splitting across multiple files.
  */
trait CompositeExpression extends Expression {
  /**
    * Indicates whether this expression is atomic.
    *
    * @return false as the default value, indicating the expression is not atomic.
    */
  def isAtomic: Boolean = false

  /**
    * If this `Valuable` is exact, it returns the exact value as a `Double`.
    * Otherwise, it returns `None`.
    * NOTE: do NOT implement this method to return a Double for a fuzzy Real--only for exact numbers.
    *
    * @return Some(x) where x is a Double if this is exact, else None.
    */
  lazy val maybeDouble: Option[Double] =
    FP.whenever(isExact)(evaluate(RestrictedContext(PureNumber)).flatMap(_.maybeDouble))

  /**
    * Determines if the current number is equal to zero.
    *
    * @return true if the number is zero, false otherwise
    */
  lazy val isZero: Boolean = evaluateAsIs.exists(x => x.isZero)

  /**
    * Determines whether this object represents unity.
    *
    * @return true if the object represents unity, false otherwise
    */
  lazy val isUnity: Boolean = evaluateAsIs.exists(x => x.isUnity)

  /**
    * Determines the sign of the Structure value represented by this instance.
    * Returns an integer indicating whether the value is positive, negative, or zero.
    *
    * @return 1 if the value is positive, -1 if the value is negative, and 0 if the value is zero
    */
  lazy val signum: Int = evaluateAsIs.map(_.signum).getOrElse(0)

  /**
    * Provides the terms that comprise this `CompositeExpression`.
    *
    * @return a sequence of `Expression` objects representing the individual terms of this `CompositeExpression`.
    */
  def terms: Seq[Expression]

  /**
    * Simplifies the components of this `CompositeExpression` using a matching mechanism to identify
    * and transform sub-expressions into simpler forms if possible.
    *
    * @return the result of the simplification attempt encapsulated in a `MatchResult`, which either contains
    *         a simplified `Expression` or indicates that no simplification was possible.
    */
  def operandsMatcher: em.AutoMatcher[Expression]

  /**
    * Simplifies the exact form of a `CompositeExpression` by identifying and transforming
    * sub-expressions into their precise, simplified forms. This method focuses on exact
    * mathematical simplifications that can be performed without introducing approximations.
    *
    * TODO try to merge this method with Expression.simplifyByEvaluation.
    *
    * @return an `em.AutoMatcher[Expression]` that encapsulates the logic for simplifying
    *         exact expressions. The result contains the simplified `Expression` if successful,
    *         or indicates no simplification was possible.
    */
  lazy val simplifyLazy: em.AutoMatcher[Expression] =
    em.Matcher("CompositeExpression:simplifyLazy") {
      (expr: Expression) =>
        // Don't evaluate if this expression should stay symbolic.
        if (CompositeExpression.shouldStaySymbolic(expr))
          em.Miss[Expression, Expression]("all components named, staying symbolic", this)
        else
          expr.evaluateAsIs match {
            case Some(value) =>
              // NOTE double-check that the result is actually exact.
              // CONSIDER is this necessary now what we have redefined this method as simplifyLazy?
              em.Match(ValueExpression(value)).filter(_.isExact)
            case None =>
              em.Miss[Expression, Expression]("CompositeExpression: simplifyLazy: no simplifications", this)
          }
    }

  /**
    * Attempts to simplify the `CompositeExpression` by identifying and reducing trivial expressions
    * into their simpler or more elementary forms, if possible.
    * A trivial simplification is one that depends on fortuitous expressions, not necessarily constants,
    * that can therefore be combined in some way.
    *
    * @return an `em.AutoMatcher[Expression]` encapsulating the logic to simplify trivial forms
    *         within the `CompositeExpression`. The result contains either the simplified `Expression`
    *         or indicates that no trivial simplifications were possible.
    */
  def identitiesMatcher: em.AutoMatcher[Expression]

  /**
    * Attempts to simplify this `CompositeExpression` by applying pattern-based rewrites.
    * It delegates the simplification logic to the defined matchers that aim to reduce the expression
    * into its simpler or optimized form, if possible.
    *
    * @return an `em.AutoMatcher[Expression]` encapsulating the logic to simplify the `CompositeExpression`.
    *         The result contains either the simplified `Expression` or indicates no further simplifications were possible.
    */
  def structuralMatcher: em.AutoMatcher[Expression]

  /**
    * Method to render this Structure in a presentable manner.
    * CONSIDER why don't we just render `simplified`` as an expression?
    *
    * @return a String
    */
  def render: String = matchSimpler(this) match {
    case em.Match(e) =>
      e.render
    case _ =>
      renderAsExpression
  }

  /**
    * Renders this `CompositeExpression` as a string representation of the expression itself,
    * typically in a structured or human-readable mathematical format.
    *
    * @return a string representation of the `CompositeExpression` as an expression.
    */
  def renderAsExpression: String
}

/**
  * Companion object for the `CompositeExpression` case class.
  *
  * Provides a utility method to create an `Expression` from a function and a sequence of `Expression`s.
  */
object CompositeExpression {

  /**
    * Applies a given `ExpressionBiFunction` to a sequence of `Expression` instances and
    * returns a corresponding `Expression` based on the sequence's contents.
    *
    * @param f  The `ExpressionBiFunction` used to combine or process the given expressions.
    * @param xs The sequence of `Expression` instances to which the function is applied.
    *           The sequence must not be empty.
    *
    * @return An `Expression` resulting from the specified processing:
    *         - If the sequence is empty, an `IllegalArgumentException` is thrown.
    *         - If the sequence contains a single element, that element is returned.
    *         - If the sequence contains two elements, a `BiFunction` expression is returned.
    *         - Otherwise, an `Aggregate` expression is created from the sequence.
    */
  def apply(f: ExpressionBiFunction, xs: Seq[Expression]): Expression =
    xs.toList match {
      case Nil =>
        throw new IllegalArgumentException("Empty Sequence")
      case h :: Nil =>
        h
      case h :: j :: Nil =>
        BiFunction(h, j, Sum)
      case _ =>
        Aggregate(Sum, xs)
    }

  /**
    * Creates a composite expression by applying a given `ExpressionBiFunction`
    * to a sequence of eager values.
    *
    * This method maps each `Eager` value to a literal expression and then applies
    * the provided function to generate the resulting expression.
    *
    * @param f  The `ExpressionBiFunction` used to combine or process the expressions
    *           derived from the given eager values.
    *
    * @param xs A variable-length argument list of `Eager` values which will be
    *           converted into literal expressions before applying the function.
    *
    * @return An `Expression` resulting from applying the function to the sequence
    *         of literal expressions.
    */
  def create(f: ExpressionBiFunction, xs: Eager*): Expression =
    apply(f, xs map (x => Literal(x, Some(x.render))))

  /**
    * Determines if all components within a given `Expression` are named.
    *
    * This method recursively checks whether each component of the provided `Expression`
    * either has a name (in the case of a `Nameable` expression) or, if it is a composite
    * expression (`CompositeExpression`), ensures that all of its terms also have names.
    *
    * @param expr The `Expression` to be evaluated. It can be a top-level expression
    *             or a composite expression containing multiple terms.
    * @return `true` if all components of the expression are named, otherwise `false`.
    */
  def shouldStaySymbolic(expr: Expression): Boolean = expr match {
    case _: Euler =>
      true
    case n: Nameable =>
      val symbolic1 = n.keepSymbolic
      symbolic1
    case BiFunction(x: Nameable, y: Nameable, _) =>
      val ySymbolic = y.keepSymbolic
      val symbolic2 = (x.keepSymbolic || ySymbolic) && x.maybeName.isDefined && y.maybeName.isDefined
      symbolic2
    case Euler(x: Nameable, y: Nameable) =>
      val symbolic3 = (x.keepSymbolic || y.keepSymbolic) && x.maybeName.isDefined && y.maybeName.isDefined
      symbolic3
    case c: CompositeExpression =>
      val symbolic4 = c.terms.exists(shouldStaySymbolic)
      symbolic4
    case _ =>
      false // Anything unnamed breaks the chain
  }
}

/**
  * Represents a composite expression consisting of multiple terms.
  * `Multiple` is a specialized trait that inherits from `CompositeExpression`
  * and provides functionality to manage a sequence of sub-expressions.
  *
  * Would like this to be package-sealed if they ever add that feature.
  */
trait Multiple extends CompositeExpression {
  /**
    * Provides the terms that comprise this `Multiple`.
    *
    * @return xs.
    */
  def terms: Seq[Expression]
}

/**
  * An extractor for the `Multiple` trait, allowing deconstruction into its sequence of terms.
  *
  * This object provides a pattern matching mechanism for `Multiple`,
  * enabling access to its underlying sequence of sub-expressions.
  */
object Multiple {
  /**
    * Deconstructs a `Multiple` instance into its components if it matches
    * a binary function or an aggregate structure.
    *
    * This extractor enables pattern matching on `Multiple` to access its underlying
    * binary function or aggregation operation along with the associated sequence
    * of sub-expressions.
    *
    * @param m the `Multiple` instance to be deconstructed.
    * @return an `Option` containing a tuple with the `ExpressionBiFunction` and
    *         a sequence of `Expression` terms if the input matches a `BiFunction`
    *         or `Aggregate` structure; otherwise, `None`.
    */
  def unapply(m: Multiple): Option[(ExpressionBiFunction, Seq[Expression])] = m match {
    case BiFunction(a, b, f) => Some((f, Seq(a, b)))
    case Aggregate(f, xs) => Some((f, xs))
  }
}
