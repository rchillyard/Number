/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.*
import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.eager.*
import com.phasmidsoftware.number.algebra.util.FP
import com.phasmidsoftware.number.core.inner.{Factor, PureNumber, Rational}
import com.phasmidsoftware.number.expression.algebraic.QuadraticEquation.squareRootEquation
import com.phasmidsoftware.number.expression.algebraic.{Equation, LinearEquation, QuadraticEquation}
import com.phasmidsoftware.number.expression.expr.Expression.em

import java.util.Objects
import scala.language.implicitConversions

/**
  * The `Root` trait represents a mathematical root derived from a specific equation.
  * It corresponds to a solution of a multivalued mathematical expression
  * that is typically associated with a monic polynomial equation.
  * Each root is uniquely identified by its underlying equation and a branch index
  * that represents a specific solution when multiple solutions are possible.
  */
sealed trait Root extends AtomicExpression with Branched[Root] with Zeroable {
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
  def isExact: Boolean = true

  /**
    * Adds another `Root` to this `Root`, resulting in a new `Root` that
    * represents the sum of the two roots. If the addition is not valid or
    * cannot be performed, returns `None`.
    *
    * @param other the `Root` to be added to the current `Root`.
    *              This parameter represents another mathematical root to
    *              combine with the current `Root`.
    *
    * @return an `Option[Root]` containing the resulting `Root` if the addition
    *         is successful, or `None` if the addition is not valid.
    */
  def add(other: Root): Option[Root]

  /**
    * Computes the result of raising the current `Root` to the power of the provided `Rational` value.
    *
    * @param r the `Rational` exponent to which the current `Root` is raised.
    *          It represents the power operation to apply to the `Root`.
    *
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
    * Creates a new `Expression` that represents the negation of the current `Expression`.
    * This operation is equivalent to multiplying the current `Expression` by -1.
    *
    * @return an `Expression` representing the negation of the current `Expression`.
    */
  def negate: Expression

  /**
    * Computes the square root of this `Root`.
    * For a `Quadratic` equation, this method calculates one of its roots based on the specified parameter.
    * If the `Expression` is not quadratic, an `ExpressionException` is thrown.
    *
    * @param plus a boolean value that determines which square root (positive or negative root) to compute:
    *             if true, compute the positive root; if false, compute the negative root.
    *
    * @return an `Expression` representing the computed square root of the current `Expression`.
    * @note Throws com.phasmidsoftware.number.expression.expr.ExpressionException if the square root computation is not supported for the current `Expression`.
    */
  def squareRoot(plus: Boolean): Expression

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
  def approximation(force: Boolean): Option[Real]
}

/**
  * Represents the root of an equation, associated with a specific branch.
  *
  * This class models the concept of a mathematical root, where an equation
  * is solved for a specific branch index, yielding a solution that adheres to
  * the constraints defined in the `Algebraic` trait. It extends `AtomicExpression`
  * to integrate with the broader mathematical expression framework.
  *
  * @param equ    the mathematical equation whose solution is represented by this root
  * @param branch the branch index used to solve the equation
  */
sealed abstract class AbstractRoot(equ: Equation, branch: Int) extends Root {

  /**
    * Computes and returns a `Root` associated with the given `Equation` and branch.
    *
    * @param equ    the `Equation` for which the `Root` is to be computed.
    * @param branch an integer value representing the specific branch of the `Equation`.
    * @return the `Root` corresponding to the provided `Equation` and branch.
    */
  def pure(equ: Equation, branch: Int): Root

  /**
    * Represents the solution of an equation for a specific branch in the context of a mathematical expression.
    * The solution is computed by invoking the `solve` method of the `equation` with the provided branch index.
    * The resulting solution is exact and adheres to the constraints described in the `Algebraic` trait.
    */
  lazy val solution: Solution = equ match {
    case QuadraticEquation.goldenRatioEquation if branch == 0 => QuadraticSolution.phi
    case QuadraticEquation.goldenRatioEquation if branch == 1 => QuadraticSolution.psi
    case _ => equ.solve(branch)
  }

  /**
    * Represents an optional value for the current `AbstractRoot` instance, resulting from a numerical
    * transformation of its associated solution if the solution is exact.
    *
    * The value is computed by invoking the `whenever` method with a predicate that checks whether
    * the associated solution is exact (`solution.isExact`). If the predicate is true, the `asField` method
    * of the solution is accessed, and its value is converted to a real number and subsequently mapped to a
    * `Double`. If the predicate is false */
  lazy val maybeDouble: Option[Double] =
    FP.whenever(solution.isExact && solution.maybeFactor(RestrictedContext(PureNumber)).isDefined)(solution.maybeDouble)

  /**
    * Method to determine what `Factor`, if there is such, this `Structure` object is based on.
    *
    * @return an optional `Factor`.
    */
  def maybeFactor(context: Context): Option[Factor] = solution match {
    case QuadraticSolution(base, offset, branch, false) if offset.isZero =>
      base.maybeFactor(context)
    case QuadraticSolution(base, offset, branch, false) if base.isZero =>
      offset.maybeFactor(context)
    case QuadraticSolution(base, offset, branch, false) =>
      for {
        baseFactor <- base.maybeFactor(context)
        combinedFactor <- offset.maybeFactor(RestrictedContext(baseFactor))
      } yield combinedFactor
    case LinearSolution(value) =>
      value.maybeFactor(context)
    case _ =>
      None // TODO implement complex solutions
  }

  /**
    * Evaluates this `Expression` in the context of `AnyContext` without simplification or factor-based conversion.
    * This allows obtaining a direct evaluation of the `Expression` as a `Field`, if possible.
    * NOTE: no simplification or factor-based conversion occurs here.
    *
    * @return an `Option[Eager]` containing the evaluated value if evaluation is successful, or `None` otherwise.
    */
  override lazy val evaluateAsIs: Option[Eager] =
    evaluate(AnyContext)
//    maybeFactor(AnyContext) flatMap (f => evaluate(RestrictedContext(f)))

  /**
    * Attempts to simplify an atomic expression, for example,
    * we replace `Literal(Eager.pi)` with `Pi`.
    *
    * @return an `em.AutoMatcher[Expression]` representing
    *         the process of handling or matching the atomic expression.
    */
  def simplifyAtomic: em.AutoMatcher[Expression] =
    em.Matcher[Expression, Expression]("Root.simplifyAtomic") {
      case r: AbstractRoot =>
        em.Match(Literal(r.solution))
      case x =>
        em.Miss(s"Cannot simplify $x", x)
    }

  /**
    * Action to evaluate this `Expression` as a `Valuable`, if possible.
    * NOTE: no simplification or factor-based conversion occurs here.
    *
    * Evaluation only occurs if the solution is exact and it has a factor that qualifies in the given context.
    *
    * @return an optional `Valuable`.
    */
  def evaluate(context: Context): Option[Eager] =
    Option.when[Eager](solution.isExact && solution.maybeFactor(context).isDefined)(solution)

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
  def approximation(force: Boolean): Option[Real] =
    solution.approximation(force)

//    solution match {
//      case LinearSolution(_) if context.factorQualifies(PureNumber) =>
//        Some(solution.base)
//      case QuadraticSolution(Value.zero, offset, _, _) if Value.isZero(offset) && context.factorQualifies(PureNumber) =>
//        Some(PureNumber)
//      case QuadraticSolution(Value.zero, _, factor, _) if context.factorQualifies(factor) =>
//        Some(factor)
//      case QuadraticSolution(_, Value.zero, _, _) if context.factorQualifies(PureNumber) =>
//        Some(PureNumber)
//      case _ =>
//        None
//    }

  /**
    * Method to render this Structure in a presentable manner.
    *
    * @return a String
    */
  def render: String = solution.render

  /**
    * Computes the power of the current `Expression` raised to the specified `Rational` exponent.
    *
    * @param r the `Rational` exponent to which the current `Expression` is to be raised.
    *          If the exponent is negative, the method computes the reciprocal of the positive power.
    *          If the exponent is zero, the result is the identity `One`.
    *          Otherwise, the computation involves recursive calls to this method.
    *
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
      import com.phasmidsoftware.number.expression.expr.Expression.ExpressionOps
      squared :* power(x - 2)
    case _ =>
      throw ExpressionException(s"power: unable to compute power of $this to $r")
  }

  /**
    * Computes the reciprocal of the current `Expression`.
    *
    * For a quadratic equation in the form Quadratic(p, q), the reciprocal is computed as:
    * (this / Literal(-q)) + Literal(-p / q).
    * Otherwise, the reciprocal is calculated as `One / this`.
    *
    * @return an `Expression` representing the reciprocal of the current `Expression`.
    */
  def reciprocal: Expression = equation match {
    case QuadraticEquation(p, q) =>
      import com.phasmidsoftware.number.expression.expr.Expression.ExpressionOps
      this / Literal(-q) :+ Literal(-p / q)
    case _ =>
      One / this
  }

  /**
    * Creates a new `Expression` that represents the negation of the current `Expression`.
    * This operation is equivalent to multiplying the current `Expression` by -1.
    *
    * @return an `Expression` representing the negation of the current `Expression`.
    */
  def negate: Expression = // TODO create a new Root instance rather than a Literal.
    Literal(solution.negate)

  /**
    * Computes the square root of this `Root`.
    * For a `Quadratic` equation, this method calculates one of its roots based on the specified parameter.
    * If the `Expression` is not quadratic, an `ExpressionException` is thrown.
    *
    * @param plus a boolean value that determines which square root (positive or negative root) to compute:
    *             if true, compute the positive root; if false, compute the negative root.
    *
    * @return an `Expression` representing the computed square root of the current `Expression`.
    * @note Throws com.phasmidsoftware.number.expression.expr.ExpressionException if the square root computation is not supported for the current `Expression`.
    */
  def squareRoot(plus: Boolean): Expression = equation match {
    case QuadraticEquation(p, q) =>
      pure(QuadraticEquation(-p.invert, -q / p), if plus then 0 else 1)
    case _ =>
      throw ExpressionException(s"squareRoot: cannot compute square root of $this")
  }

  /**
    * Compares this `AbstractRoot` instance with another object for equality.
    * The method checks if the other object is of a compatible type and
    * whether all relevant Valuables of both objects are equal.
    *
    * @param other the object to compare for equality with this instance
    * @return true if the given object is an instance of `AbstractRoot`,
    *         has `canEqual` compatibility with this instance, and
    *         if all relevant Valuables are equal; otherwise, false
    */
  override def equals(other: Any): Boolean =
    other match {
      case that: AbstractRoot =>
        that.canEqual(this) &&
            equ == that.equation &&
            branch == that.branch
      case _ =>
        false
    }

  /**
    * Generates a hash code for the instance based on its `equ` and `branch` Valuables.
    *
    * @return an integer hash code value obtained by hashing the `equ` and `branch` Valuables.
    */
  override def hashCode(): Int =
    Objects.hash(equ, branch)

  /**
    * Determines if the given object is of a type that can be compared for equality with this instance.
    *
    * @param other the object to compare with this instance
    * @return true if the given object is an instance of AbstractRoot, false otherwise
    */
  private def canEqual(other: Any): Boolean =
    other.isInstanceOf[AbstractRoot]

  /**
    * Computes the square of the current `Expression`.
    * If the current equation is quadratic, it computes the result of the operation: this * -p + q.
    * If the current equation is linear, it computes the result of the operation: this * -r.
    * Otherwise, it returns the square of this expression by performing this * this.
    *
    * @return the result of squaring the current `Expression`, evaluated according to the type of the equation.
    */
  def squared: Expression = equation match {
    case QuadraticEquation(p, q) =>
      import com.phasmidsoftware.number.expression.expr.Expression.ExpressionOps
      this :* Literal(-p) :+ Literal(-q)
//    case LinearEquation(r) =>
//      this :* Literal(-r)
    case _ =>
      this :* this
  }
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
    * Returns the number of branches in the quadratic solution.
    *
    * For a quadratic equation, the number of solution branches is typically 2,
    * corresponding to the two possible roots.
    *
    * @return the number of branches available for a quadratic equation.
    */
  def branches: Int = 2

  /**
    * Constructs a `Root` for a given quadratic equation and retrieves the specific solution
    * branch corresponding to the provided index.
    *
    * @param index the branch index corresponding to the desired root of the equation.
    *              Typically, for quadratic equations, this value is 0 or 1.
    * @return a `Root` representing the solution branch of the specified quadratic equation.
    */
  def branched(index: Int): Root = QuadraticRoot(equ, index)

  /**
    * Constructs a `Root` for a given quadratic equation and its specific solution branch.
    *
    * @param equ    the quadratic equation for which the root is to be constructed. It must be
    *               an instance of the `Equation` trait.
    *
    * @param branch the branch index corresponding to the desired root of the equation.
    *               Typically, for quadratic equations, this value is 0 or 1.
    *
    * @return a `Root` representing the solution branch of the specified quadratic equation.
    */
  def pure(equ: Equation, branch: Int): Root = QuadraticRoot(equ, branch)

  /**
    * Adds another `Root` to this `Root`, yielding a new `Root` as a result.
    *
    * @param other the `Root` to be added to the current `Root`.
    *              This represents the operand added to this `Root`.
    *
    * @return a new `Root` which is the sum of this `Root` and the provided `other` `Root`.
    */
  infix def add(other: Root): Option[Root] = other match {
    case q: QuadraticRoot =>
      solution + q.solution match {
        case qq: QuadraticSolution =>
          Some(QuadraticRoot(qq))
        case _ =>
          None
      }
    case _ =>
      None
  }

  /**
    * Returns the associated `Quadratic` equation for this instance.
    *
    * @return the `Quadratic` equation represented by this root.
    */
  def equation: QuadraticEquation = equ.asInstanceOf[QuadraticEquation]

  /**
    * Produces a string representation of this `QuadraticRoot` instance.
    * Depending on the equation and branch, the result may represent specific
    * notable constants (e.g., the golden ratio and its conjugate) or a general
    * description of the quadratic root.
    *
    * @return a string representation of the `QuadraticRoot` based on its equation
    *         and branch index.
    */
  override def toString: String = (equ, branch) match {
    case (QuadraticEquation.goldenRatioEquation, 0) =>
      "\uD835\uDED7"
    case (QuadraticEquation.goldenRatioEquation, 1) =>
      "\uD835\uDED9"
    case _ =>
      s"QuadraticRoot($equ, $branch)"
  }

  /**
    * Determines if the current number is equal to zero.
    *
    * @return true if the number is zero, false otherwise
    */
  def isZero: Boolean = solution.isZero

  /**
    * Determines the sign of the Monotone value represented by this instance.
    * Returns an integer indicating whether the value is positive, negative, or zero.
    *
    * @return 1 if the value is positive, -1 if the value is negative, and 0 if the value is zero
    */
  def signum: Int = solution.signum
}

/**
  * The `QuadraticRoot` object provides functionality to construct and manipulate roots of quadratic equations.
  *
  * This object acts as a factory for creating instances of `QuadraticRoot` using algebraic data,
  * and offers methods for working with quadratic equations and their solutions.
  */
object QuadraticRoot {
  def apply(solution: QuadraticSolution): QuadraticRoot = {
    solution match {
      case QuadraticSolution(base: Q, offset: Q, branch, false) =>
        QuadraticRoot(QuadraticEquation(base.toRational * -2, (base.toRational ∧ 2) - offset.toRational), branch)
      case _ =>
        throw ExpressionException(s"apply($solution) is not supported")
    }
  }

  val phi = QuadraticRoot(QuadraticEquation.goldenRatioEquation, 0)
  val psi = QuadraticRoot(QuadraticEquation.goldenRatioEquation, 1)
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
  def branches: Int = 1

  def branched(index: Int): Root = this

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
    *
    * @param branch the branch index associated with this root. It specifies a particular
    *               solution for the equation if multiple solutions exist.
    *
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

  /**
    * Adds another `Root` to this `Root`, resulting in a new `Root` that
    * represents the sum of the two roots. If the addition is not valid or
    * cannot be performed, returns `None`.
    *
    * @param other the `Root` to be added to the current `Root`.
    *              This parameter represents another mathematical root to
    *              combine with the current `Root`.
    *
    * @return an `Option[Root]` containing the resulting `Root` if the addition
    *         is successful, or `None` if the addition is not valid.
    */
  def add(other: Root): Option[Root] = other match {
    case l: LinearRoot => ???
    case _ =>
      None
  }

  /**
    * Determines if the current number is equal to zero.
    *
    * @return true if the number is zero, false otherwise
    */
  def isZero: Boolean = solution.isZero

  /**
    * Determines the sign of the Monotone value represented by this instance.
    * Returns an integer indicating whether the value is positive, negative, or zero.
    *
    * @return 1 if the value is positive, -1 if the value is negative, and 0 if the value is zero
    */
  def signum: Int = solution.signum
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
    *
    * @param branch   an integer representing the branch index to compute. The branch identifies
    *                 a specific solution for equations with multiple branches.
    *
    * @return a `Root` instance that corresponds to the given `Equation` and branch index.
    *         The returned `Root` is specialized for `Quadratic` or `LinearEquation` types.
    */
  def apply(equation: Equation, branch: Int): Root = equation match {
    case q: QuadraticEquation =>
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
  val phi = QuadraticRoot(QuadraticEquation.goldenRatioEquation, 0)
  /**
    * Represents the conjugate root of the golden ratio equation (`x² + x - 1 = 0`),
    * commonly referred to as ψ (psi). This value is the second root of the quadratic equation,
    * distinct from the golden ratio (φ, phi). It is calculated using the `QuadraticRoot` constructor,
    * with the golden ratio equation as its basis and branch index set to 1.
    */
  val psi = QuadraticRoot(QuadraticEquation.goldenRatioEquation, 1)
  /**
    * Represents the constant root `1` of a quadratic equation.
    * This value is a particular solution of the quadratic equation `-2x + 1 = 0` on branch `0`.
    */
  val one = QuadraticRoot(QuadraticEquation(-2, 1), 0)
  /**
    * Represents the quadratic root when both the quadratic coefficients and the branch index are zero.
    * This constant corresponds to the simplest quadratic equation with all coefficients as zero and the solution at branch zero.
    */
  val zero = QuadraticRoot(QuadraticEquation(0, 0), 0)
  /**
    * Represents the root solution corresponding to the quadratic equation for ±√2.
    *
    * The `rootTwo` value is an instance of `QuadraticRoot` that is initialized with the predefined
    * quadratic equation `x² - 2 = 0` (`Quadratic.rootTwoEquation`) and specifies the primary root (`branch 0`).
    *
    * This root is a well-known mathematical constant (the square root of 2), widely used in
    * geometry, algebra, and various other mathematical applications.
    */
  val rootTwo = QuadraticRoot(QuadraticEquation.rootTwoEquation, 0)
  /**
    * Represents the negative root of the quadratic equation `x² - 2 = 0`.
    * This equation defines the square root of 2, with the two roots being `±√2`.
    * The `branch` parameter set to 1 in this instance corresponds to the negative root (i.e., `-√2`).
    */
  val negRootTwo = QuadraticRoot(QuadraticEquation.rootTwoEquation, 1)

  val rootThree = QuadraticRoot(QuadraticEquation.rootThreeEquation, 0)

  /**
    * Represents the value one-half as a linear root, constructed from a linear equation
    * with a negated half-rational coefficient.
    */
  val half = LinearRoot(LinearEquation(Rational.half.negate))

  /**
    * Computes the square root of a given rational number and selects a specific branch
    * of the solution.
    *
    * @param r      the `Rational` number for which the square root is to be calculated.
    *               The input represents a rational value that serves as the operand.
    *
    * @param branch an integer representing the branch index to compute. The branch identifies
    *               a specific solution when there are multiple possible square root results.
    *               0 gives the positive root, 1 gives the negative root.
    *
    * @return a `QuadraticRoot` instance representing the square root computation and branch selection.
    */
  def squareRoot(r: Rational, branch: Int) = QuadraticRoot(squareRootEquation(r), branch)
}