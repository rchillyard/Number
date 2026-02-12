/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.eager.{Complex, Eager, Real}
import com.phasmidsoftware.number.core.inner.Factor
import com.phasmidsoftware.number.expression.expr.Expression.em

import scala.language.implicitConversions

/**
  * An Expression that is based on one simple constant value.
  * The four subtypes of AtomicExpression are:
  * - `ValueExpression`
  * - `Root`
  * - `Transcendental`
  * - `Noop`
  */

/** sealed */
trait AtomicExpression extends Expression with Nameable {

  /**
    * Indicates whether the expression is atomic.
    *
    * @return true, as this expression is atomic and cannot be further simplified.
    */
  def isAtomic: Boolean = true

  /**
    * @return 1.
    */
  def depth: Int = 1

  /**
    * Attempts to simplify an atomic expression, for example,
    * we replace `Literal(Valuable.pi)` with `Pi`.
    *
    * @return an `em.AutoMatcher[Expression]` representing
    *         the process of handling or matching the atomic expression.
    */
  def simplifyAtomic: em.AutoMatcher[Expression]
}

/**
  * Companion object for the `AtomicExpression` trait.
  * Provides utility methods for working with instances of `AtomicExpression`.
  *
  * The `unapply` method enables pattern matching on `AtomicExpression` instances, providing an
  * optional `Valuable` as a result based on the type of the expression. The behavior of this method
  * varies for different subtypes of `AtomicExpression`, such as `Complex`, `ValueExpression`,
  * `Literal`, `Valuable`, `Noop`, and `ReducedQuadraticRoot`.
  *
  * Notes:
  * - For `Complex`, the method directly returns the `Complex` instance as a `Valuable`.
  * - For `ValueExpression` and `Literal`, the value of the Valuable is returned, but the name
  *   might be discarded during extraction.
  * - For `Valuable`, the method directly returns the Valuable.
  * - For `ReducedQuadraticRoot`, the method attempts to evaluate it "as-is."
  * - For `Noop`, the method returns `None`.
  *
  * Considerations:
  * - The use of `Complex` and `Valuable` directly in the extraction may warrant re-evaluation.
  * - The potential loss of the name in `ValueExpression` and `Literal` is noted as a trade-off.
  */
object AtomicExpression {
  /**
    * Extracts an optional `Valuable` from an `AtomicExpression` instance based on its type.
    * This method provides a mechanism for pattern matching on subtypes of `AtomicExpression`,
    * returning a `Valuable` where applicable.
    *
    * @param arg the `AtomicExpression` instance from which the `Valuable` is to be extracted.
    *            This can be one of the subtypes such as `Complex`, `ValueExpression`, `Literal`,
    *            `Valuable`, `Noop`, or `ReducedQuadraticRoot`.
    * @return an `Option` containing the extracted `Valuable` if one can be determined based on the
    *         type of `arg`. Returns `None` if no `Valuable` can be extracted, e.g., in the case of `Noop`.
    */
  def unapply(arg: AtomicExpression): Option[Valuable] = arg match {
    case c: Complex =>
      Some(c) // CONSIDER eliminate this?
    case ValueExpression(x, _) =>
      Some(x) // NOTE we lose the name here.
    case Literal(x, _) =>
      Some(x) // NOTE we lose the name here.
    case r: Root =>
      r.evaluateAsIs
    case _ =>
      None
  }
}

/**
  * The `Noop` object is an atomic expression that represents a no-operation placeholder in an expression tree.
  * It cannot be evaluated, simplified, or associated with any specific factor. It is a concrete implementation
  * of the `AtomicExpression` trait.
  */
case class Noop(w: String) extends AtomicExpression {
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
  def isExact: Boolean = false

  /**
    * Determines if the current number is equal to zero.
    *
    * @return true if the number is zero, false otherwise
    */
  def isZero: Boolean = false

  /**
    * Determines whether this object represents unity.
    *
    * @return true if the object represents unity, false otherwise
    */
  def isUnity: Boolean = false

  /**
    * Determines the sign of the Monotone value represented by this instance.
    * Returns an integer indicating whether the value is positive, negative, or zero.
    *
    * @return 1 if the value is positive, -1 if the value is negative, and 0 if the value is zero
    */
  def signum: Int = 0

  /**
    * Method to determine what `Factor`, if there is such, this `Structure` object is based on.
    *
    * @return an optional `Factor`.
    */
  def maybeFactor(context: Context): Option[Factor] = None

  /**
    * Action to evaluate this `Expression` as a `Valuable`,
    * NOTE: no simplification occurs here.
    *
    * @return a `Valuable`.
    */
  def evaluate(context: Context): Option[Eager] =
    throw new UnsupportedOperationException(s"Can''t evaluate: $this")

  /**
    * Method to render this Structure in a presentable manner.
    *
    * @return a String
    */
  def render: String = toString

  /**
    * If this `Valuable` is exact, it returns the exact value as a `Double`.
    * Otherwise, it returns `None`.
    * NOTE: do NOT implement this method to return a Double for a fuzzy Real--only for exact numbers.
    *
    * @return Some(x) where x is a Double if this is exact, else None.
    */
  def maybeDouble: Option[Double] = None

  /**
    * Retrieves an optional name associated with the implementing class.
    *
    * The method returns an `Option` wrapping a `String`. If a name is available, it will be 
    * contained within the `Option` as `Some(name)`. If no name is present, the method will return `None`.
    *
    * @return an `Option[String]` representing the optional name
    */
  val maybeName: Option[String] = Some(w) // CONSIDER returning None here instead

  /**
    *
    */
  def simplifyAtomic: em.AutoMatcher[Expression] = em.Matcher[Expression, Expression]("simplifyAtomic")(
    _ =>
      em.Miss[Expression, Expression]("simplifyAtomic: ", this)
  )

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
  def approximation(force: Boolean): Option[Real] = None

  override def toString: String = s"Noop: not an Expression: $w"
}
