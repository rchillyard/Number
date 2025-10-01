/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.algebraic

import com.phasmidsoftware.number.core.Number.one
import com.phasmidsoftware.number.core.algebraic.Algebraic.{phi, psi}
import com.phasmidsoftware.number.core.inner._
import com.phasmidsoftware.number.core.{Field, Real}
import com.phasmidsoftware.number.expression.Expression.em
import com.phasmidsoftware.number.expression.{AtomicExpression, Expression, Literal}

/**
  * Represents the root of an equation, associated with a specific branch.
  *
  * This class models the concept of a mathematical root, where an equation
  * is solved for a specific branch index, yielding a solution that adheres to
  * the constraints defined in the `Solution` trait. It extends `AtomicExpression`
  * to integrate with the broader mathematical expression framework.
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
    * A lazily evaluated optional `Field` derived from the `solution`. The value depends on the specific
    * type and properties of the solution:
    *
    * - If the solution is of type `LinearSolution`, the `Field` is created using the solution's property `x`
    * and the factor `PureNumber`.
    * - If the solution is of type `QuadraticSolution` and the base value is zero, the `Field` is computed
    * by combining the offset, factor, and branch values through specific operations.
    * - If the solution is of type `QuadraticSolution` and the offset value is zero, the `Field` is created
    * based solely on the base value and the factor `PureNumber`.
    * - For all other cases, the value remains `None`.
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
  def simplifyAtomic: em.AutoMatcher[Expression] = em.Matcher[Expression, Expression]("Root.simplifyAtomic") {
    case r@Root(_, _) =>
      // CONSIDER refactoring this based on conditional match method.
      r.maybeValue match {
        case Some(value) =>
          matchAndSimplify(value)
        case None =>
          em.Miss("Root.simplifyAtomic: cannot simplify this Root: " + r, r)
      }
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
  def approximation: Option[Real] = maybeValue flatMap (_.approximation)

  /**
    * Method to render this NumberLike in a presentable manner.
    *
    * @return a String
    */
  def render: String = algebraic.render

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
