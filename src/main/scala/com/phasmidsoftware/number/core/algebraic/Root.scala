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
  * The `Root` trait represents a mathematical root derived from a specific equation.
  * It corresponds to a solution of a multivalued mathematical expression
  * that is typically associated with a monic polynomial equation.
  * Each root is uniquely identified by its underlying equation and a branch index
  * that represents a specific solution when multiple solutions are possible.
  */
trait Root extends AtomicExpression {
  /**
    * Retrieves the `Equation` associated with this `Root`.
    *
    * @return an `Equation`, which represents a mathematical relationship, typically
    *         defined by a monic polynomial and may have multiple branches of solutions.
    */
  def equation: Equation

  /**
    * Retrieves the branch index for this `Root`. A branch represents a specific solution
    * or interpretation of an associated multivalued mathematical expression, typically
    * derived from an underlying `Equation`.
    *
    * @return the index of the branch as an integer. This value is typically
    *         within the range `0` to `branches - 1`, where `branches` is the
    *         total number of solution branches for the corresponding `Equation`.
    */
  def branch: Int

  /**
    * Retrieves an optional value of type `Field` associated with this `Root`.
    * The result will be defined if either the base or the offset is zero (in the case of a quadratic root).
    *
    * @return an `Option[Field]` that may contain the value. If no value is associated, returns `None`.
    */
  def maybeValue: Option[Field]

  /**
    * Computes the result of raising the current `Root` to the power of the provided `Rational` value.
    *
    * @param r the `Rational` exponent to which the current `Root` is raised.
    *          It represents the power operation to apply to the `Root`.
    * @return an `Expression` representing the result of the operation,
    *         where the current `Root` is raised to the specified `Rational` power.
    */
  def power(r: Rational): Expression

  /**
    * Computes the square of the current `Expression`.
    * If the current equation is quadratic, it computes the result of the operation: this * -p + q.
    * If the current equation is linear, it computes the result of the operation: this * -r.
    * Otherwise, it returns the square of this expression by performing this * this.
    *
    * @return the result of squaring the current `Expression`, evaluated according to the type of the equation.
    */
  def reciprocal: Expression

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
  def squareRoot(plus: Boolean): Expression
}

/**
  * Represents a root of a quadratic equation with a specified branch.
  * This class is a case class extending the `AbstractRoot`, which models the root of an equation
  * and provides required functionality for specific implementations.
  *
  * The `QuadraticRoot` works specifically with quadratic equations, which have two branches corresponding
  * to their solutions. Each instance of `QuadraticRoot` is associated with one branch of the equation.
  *
  * @param equ    the equation whose root is being represented. It must be an instance of the `Equation` trait.
  * @param branch the branch index indicating the solution branch of the equation. This value should be within
  *               the valid range of branches supported by the equation, typically `0` or `1` for quadratic equations.
  */
case class QuadraticRoot(equ: Equation, branch: Int) extends AbstractRoot(equ, branch) {
  /**
    * Constructs a `Root` for a given quadratic equation and its specific solution branch.
    *
    * @param equ    the quadratic equation for which the root is to be constructed. It must be
    *               an instance of the `Equation` trait.
    * @param branch the branch index corresponding to the desired root of the equation.
    *               Typically, for quadratic equations, this value is 0 or 1.
    * @return a `Root` representing the solution branch of the specified quadratic equation.
    */
  def pure(equ: Equation, branch: Int): Root = QuadraticRoot(equ, branch)

  /**
    * Returns the associated `Quadratic` equation for this instance.
    *
    * @return the `Quadratic` equation represented by this root.
    */
  def equation: Quadratic = equ.asInstanceOf[Quadratic]

  override def toString: String = (equ, branch) match {
  case (Quadratic.goldenRatioEquation, 0) =>
    "\uD835\uDED7"
  case (Quadratic.goldenRatioEquation, 1) =>
    "\uD835\uDED9"
  case _ =>
    s"QuadraticRoot($equ, $branch)"
  }
}

/**
  * Represents the linear root of a given equation. A `LinearRoot` is an extension
  * of the `AbstractRoot` class, specialized for cases where the root corresponds
  * to a degree-1 (linear) polynomial equation.
  *
  * This implementation assumes the linear root is unique and thus corresponds
  * to the only solution available for a first-degree equation.
  *
  * @param equ the `Equation` instance associated with this root.
  *            Assumes the equation is linear (degree 1).
  */
case class LinearRoot(equ: Equation) extends AbstractRoot(equ, 0) {
  /**
    * @return 0.
    */
  def branch: Int = 0

  /**
    * Creates a new root instance corresponding to the specified equation and branch.
    * This method is used to produce a mathematical solution associated with the given input parameters.
    *
    * @param equ    the equation from which the root is derived. The equation represents a
    *               mathematical relationship, typically a monic polynomial.
    * @param branch the branch index associated with this root. It specifies a particular
    *               solution for the equation if multiple solutions exist.
    * @return a new `Root` instance corresponding to the provided equation and branch.
    */
  def pure(equ: Equation, branch: Int): Root = LinearRoot(equ)

  /**
    * Retrieves the `LinearEquation` associated with this `Root`.
    *
    * @return an `Equation`, which represents a mathematical relationship, typically
    *         defined by a monic polynomial and may have multiple branches of solutions.
    */
  def equation: LinearEquation = equ.asInstanceOf[LinearEquation]
}

/**
  * Represents the root of an equation, associated with a specific branch.
  *
  * This class models the concept of a mathematical root, where an equation
  * is solved for a specific branch index, yielding a solution that adheres to
  * the constraints defined in the `Solution` trait. It extends `AtomicExpression`
  * to integrate with the broader mathematical expression framework.
  *
  * @param equ the mathematical equation whose solution is represented by this root
  * @param branch   the branch index used to solve the equation
  */
abstract class AbstractRoot(equ: Equation, branch: Int) extends Root {

  /**
    * Computes and returns a `Root` associated with the given `Equation` and branch.
    *
    * @param equ    the `Equation` for which the `Root` is to be computed.
    * @param branch an integer value representing the specific branch of the `Equation`.
    * @return the `Root` corresponding to the provided `Equation` and branch.
    */
  def pure(equ: Equation, branch: Int): Root

  /**
    * Represents an algebraic instance derived from the associated equation and branch.
    * This value is lazily evaluated and used within the context of symbolic or algebraic computations.
    */
  lazy val algebraic: Algebraic = Algebraic(equ, branch)

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
      (equ, branch) match {
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
    case QuadraticSolution(Value.zero, offset, _, _) if Value.isZero(offset) =>
      Some(PureNumber)
    case QuadraticSolution(Value.zero, _, factor, _) =>
      Some(factor)
    case QuadraticSolution(_, Value.zero, _, _) =>
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
      case r: AbstractRoot =>
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
      pure(Quadratic(-p.invert, -q / p), if (plus) 0 else 1)
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

/**
  * The `Root` object provides predefined mathematical constants or solutions to
  * well-known equations modeled as roots. These constants are instances of `QuadraticRoot`
  * or `LinearRoot` corresponding to specific root solutions of their respective equations.
  */
object Root {
  /**
    * Applies the given `Equation` instance and a branch index to determine the corresponding `Root`.
    * Depending on the type of the equation, the method computes and returns an appropriate `Root` representation.
    *
    * @param equation the `Equation` instance to process, which represents a mathematical relationship.
    *                 It may include multiple solutions or branches (e.g., quadratic or linear equations).
    * @param branch   an integer representing the branch index to compute. The branch identifies
    *                 a specific solution for equations with multiple branches.
    * @return a `Root` instance that corresponds to the given `Equation` and branch index.
    *         The returned `Root` is specialized for `Quadratic` or `LinearEquation` types.
    */
  def apply(equation: Equation, branch: Int): Root = equation match {
    case q: Quadratic =>
      QuadraticRoot(q, branch)
    case l: LinearEquation =>
      LinearRoot(l)
  }

  /**
    * Represents the mathematical constant φ (phi), also known as the golden ratio.
    * The golden ratio is defined as the positive root of the quadratic equation `x² + x - 1 = 0`.
    * It is an irrational number approximately equal to 1.6180339887498948.
    *
    * This value is modeled using a `QuadraticRoot` instance, which takes the predefined golden ratio quadratic equation
    * (`Quadratic.goldenRatioEquation`) and specifies the branch `0`, representing the positive root.
    */
  val phi = QuadraticRoot(Quadratic.goldenRatioEquation, 0)
  /**
    * Represents the conjugate root of the golden ratio equation (`x² + x - 1 = 0`),
    * commonly referred to as ψ (psi). This value is the second root of the quadratic equation,
    * distinct from the golden ratio (φ, phi). It is calculated using the `QuadraticRoot` constructor,
    * with the golden ratio equation as its basis and branch index set to 1.
    */
  val psi = QuadraticRoot(Quadratic.goldenRatioEquation, 1)
  /**
    * Represents the constant root `1` of a quadratic equation.
    * This value is a particular solution of the quadratic equation `-2x + 1 = 0` on branch `0`.
    */
  val one = QuadraticRoot(Quadratic(-2, 1), 0)
  /**
    * Represents the quadratic root when both the quadratic coefficients and the branch index are zero.
    * This constant corresponds to the simplest quadratic equation with all coefficients as zero and the solution at branch zero.
    */
  val zero = QuadraticRoot(Quadratic(0, 0), 0)
  /**
    * Represents the root solution corresponding to the quadratic equation for ±√2.
    *
    * The `rootTwo` value is an instance of `QuadraticRoot` that is initialized with the predefined
    * quadratic equation `x² - 2 = 0` (`Quadratic.rootTwoEquation`) and specifies the primary root (`branch 0`).
    *
    * This root is a well-known mathematical constant (the square root of 2), widely used in
    * geometry, algebra, and various other mathematical applications.
    */
  val rootTwo = QuadraticRoot(Quadratic.rootTwoEquation, 0)
  /**
    * Represents the value one-half as a linear root, constructed from a linear equation
    * with a negated half rational coefficient.
    */
  val half = LinearRoot(LinearEquation(Rational.half.negate))
}