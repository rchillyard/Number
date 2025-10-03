/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.algebraic

import com.phasmidsoftware.number.core.Number.one
import com.phasmidsoftware.number.core.algebraic.Algebraic.{phi, psi}
import com.phasmidsoftware.number.core.inner._
import com.phasmidsoftware.number.core.{Field, Real}
import com.phasmidsoftware.number.expression.Expression.em
import com.phasmidsoftware.number.expression._

/**
  * Represents the root of an equation, associated with a specific branch.
  *
  * This class models the concept of a mathematical root, where an equation
  * is solved for a specific branch index, yielding a solution that adheres to
  * the constraints defined in the `Solution` trait. It extends `AtomicExpression`
  * to integrate with the broader mathematical expression framework.
  *
  * CONSIDER moving this class/object into AtomicExpression.scala.
  *
  * @param equation the mathematical equation whose solution is represented by this root
  * @param branch   the branch index used to solve the equation
  */
case class Root(equation: Equation, branch: Int) extends AtomicExpression {

  /**
    * Represents an algebraic instance derived from the associated equation and branch.
    * This value is lazily evaluated and used within the context of symbolic or algebraic computations.
    */
  lazy val algebraic: Algebraic = Algebraic(equation, branch)

  /**
    * Represents the solution of an equation for a specific branch in the context of a mathematical expression.
    * The solution is computed by invoking the `solve` method of the `equation` with the provided branch index.
    * The resulting solution is exact and adheres to the constraints described in the `Solution` trait.
    */
  lazy val solution: Solution = algebraic.solve

  /**
    * Lazily evaluates and determines a possible `Field` value based on the
    * provided `solution` and, if necessary, the `equation` and `branch`.
    *
    * The computation involves analyzing the type and properties of the
    * `solution` to generate the appropriate `Field`. For linear or
    * quadratic solutions, specific conditions are checked to determine
    * validity, and associated calculations are performed. For cases
    * outside these, additional context such as `equation` and `branch` is
    * utilized to derive potential results.
    *
    * The function handles the following cases:
    * - A `LinearSolution` generates a `Field` with the specified value.
    * - A `QuadraticSolution` derives a `Field` under specific constraints, such as when the offset is zero or the base is zero.
    * - For non-matching solutions, specific `equation` and `branch` combinations (e.g., golden ratio equations) are evaluated.
    * - If none of the conditions are satisfied, it results in `None`.
    *
    * @return an optional `Field` instance based on the evaluated criteria.
    */
  lazy val maybeValue: Option[Field] = solution match {
    case LinearSolution(x) =>
      Some(Field(x, PureNumber))
    case QuadraticSolution(base, offset, _, _) if Value.isZero(offset) =>
      Some(Field(base, PureNumber))
    case s@QuadraticSolution(base, _, factor, _) if Value.isZero(base) =>
      Some(Real(one.make(s.radicalTerm, factor)))
    case _ =>
      (equation, branch) match {
        case (Quadratic.goldenRatioEquation, 0) =>
          Some(phi)
        case (Quadratic.goldenRatioEquation, 1) =>
          Some(psi)
        case _ =>
          None
      }
  }

  /**
    * Method to determine what `Factor`, if there is such, this `NumberLike` object is based on.
    *
    * @return an optional `Factor`.
    */
  def maybeFactor: Option[Factor] = solution match {
    case LinearSolution(_) =>
      Some(PureNumber)
    case QuadraticSolution(base, _, factor, _) if Value.isZero(base) =>
      Some(factor)
    case QuadraticSolution(_, offset, _, _) if Value.isZero(offset) =>
      Some(PureNumber)
    case _ =>
      None
  }

  /**
    * Attempts to simplify an atomic expression, for example,
    * we replace `Literal(Constants.pi)` with `ConstPi`.
    *
    * @return an `em.AutoMatcher[Expression]` representing
    *         the process of handling or matching the atomic expression.
    */
  def simplifyAtomic: em.AutoMatcher[Expression] =
    em.Matcher[Expression, Expression]("Root.simplifyAtomic") {
      case r@Root(_, _) =>
        em.matchIfDefined(r.maybeValue)(r) flatMap matchAndSimplify
    }

  /**
    * Action to evaluate this `Expression` as a `Field`, if possible.
    * NOTE: no simplification or factor-based conversion occurs here.
    *
    * @return an optional `Field`.
    */
  def evaluate(context: Context): Option[Field] =
    maybeValue match {
      case x@Some(value) if context.fieldQualifies(value) =>
        x
      case _ =>
        Option.when(context == AnyContext)(algebraic)
    }

  /**
    * Computes and returns an approximate numerical value for this Approximatable.
    * All Fields, PowerSeries and Expressions that implement this method should work except for complex quantities.
    *
    * @return if possible, returns a `Real` representing the approximation of this expression.
    */
  def approximation: Option[Real] =
    maybeValue match {
      case Some(value) =>
        value.approximation
      case None =>
        solution.asNumber map (Real(_))
    }

  /**
    * Method to render this NumberLike in a presentable manner.
    *
    * @return a String
    */
  def render: String = algebraic.render

  /**
    * Computes the power of the current `Expression` raised to the specified `Rational` exponent.
    *
    * @param r the `Rational` exponent to which the current `Expression` is to be raised.
    *          If the exponent is negative, the method computes the reciprocal of the positive power.
    *          If the exponent is zero, the result is the identity `One`.
    *          Otherwise, the computation involves recursive calls to this method.
    * @return an `Expression` representing the result of raising the current `Expression` to the power of `r`.
    */
  def power(r: Rational): Expression = r match {
    case Rational.zero =>
      One
    case Rational.one =>
      this
    case x if x < 0 =>
      power(-x).reciprocal
    case Rational.half =>
      squareRoot(branch == 0)
    case x if x >= 2 =>
      import com.phasmidsoftware.number.expression.Expression.ExpressionOps
      squared * power(x - 2)
    case _ =>
      throw ExpressionException(s"power: unable to compute power of $this to $r")
  }

  /**
    * Computes the square of the current `Expression`.
    * If the current equation is quadratic, it computes the result of the operation: this * -p + q.
    * If the current equation is linear, it computes the result of the operation: this * -r.
    * Otherwise, it returns the square of this expression by performing this * this.
    *
    * @return the result of squaring the current `Expression`, evaluated according to the type of the equation.
    */
  def reciprocal: Expression = equation match {
    case Quadratic(p, q) =>
      import com.phasmidsoftware.number.expression.Expression.ExpressionOps
      this / Literal(-q) + Literal(-p / q)
    case _ =>
      One / this
  }

  /**
    * Computes the square root of this `Root`.
    * For a `Quadratic` equation, this method calculates one of its roots based on the specified parameter.
    * If the `Expression` is not quadratic, an `ExpressionException` is thrown.
    *
    * @param plus a boolean value that determines which square root (positive or negative root) to compute:
    *             if true, compute the positive root; if false, compute the negative root.
    * @return an `Expression` representing the computed square root of the current `Expression`.
    * @throws ExpressionException if the square root computation is not supported for the current `Expression`.
    */
  def squareRoot(plus: Boolean): Expression = equation match {
    case Quadratic(p, q) =>
      Root(Quadratic(-p.invert, -q / p), if (plus) 0 else 1)
    case _ =>
      throw ExpressionException(s"squareRoot: cannot compute square root of $this")
  }
  /**
    * Computes the square of the current `Expression`.
    * If the current equation is quadratic, it computes the result of the operation: this * -p + q.
    * If the current equation is linear, it computes the result of the operation: this * -r.
    * Otherwise, it returns the square of this expression by performing this * this.
    *
    * @return the result of squaring the current `Expression`, evaluated according to the type of the equation.
    */
  private def squared: Expression = equation match {
    case Quadratic(p, q) =>
      import com.phasmidsoftware.number.expression.Expression.ExpressionOps
      this * Literal(-p) + Literal(-q)
    case LinearEquation(r) =>
      this * Literal(-r)
    case _ =>
      this * this
  }

  /**
    * Matches the given `Field` instance and attempts to simplify its representation
    * into an `Expression`. This involves wrapping the `Field` in a `Literal` and
    * applying atomic simplification transformations.
    *
    * @param field the `Field` to match and simplify.
    * @return a `MatchResult` containing the resulting `Expression`.
    */
  private def matchAndSimplify(field: Field): em.MatchResult[Expression] =
    em.Match(Literal(field)) flatMap simplifyAtomic
}
