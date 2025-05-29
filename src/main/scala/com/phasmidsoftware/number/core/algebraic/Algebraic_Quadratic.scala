/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.algebraic

import com.phasmidsoftware.number.core.algebraic.Quadratic.goldenRatioEquation
import com.phasmidsoftware.number.core.inner.Operations.doComposeValueDyadic
import com.phasmidsoftware.number.core.inner.Rational.two
import com.phasmidsoftware.number.core.inner.Value.negateConditional
import com.phasmidsoftware.number.core.inner._
import com.phasmidsoftware.number.core.{Field, NumberException, algebraic}
import java.util.Objects

/**
  * Represents a specialized form of a solution derived from an equation, specifically
  * associated with a quadratic root representation (Quadratic). This case class extends the
  * `Algebraic` class and provides operations for algebraic and numerical transformations
  * of the solution, along with utilities to obtain properties like branch, sign, and
  * whether the solution is real or imaginary.
  *
  * @param equation An instance of the `Equation` class that encapsulates the quadratic equation.
  * @param pos      A boolean value indicating a position or branch in the solution space.
  */
case class Algebraic_Quadratic(equation: Quadratic, pos: Boolean) extends Algebraic {

  /**
    * Determines the branch index based on the state of the `pos` condition.
    * If `pos` is true, this method returns 0; otherwise, it returns 1.
    *
    * @return the branch index as an `Int`: 0 if `pos` is true, 1 otherwise.
    */
  def branch: Int = if (pos) 0 else 1

  /**
    * Determines the name associated with the given branch index, if applicable.
    * The method evaluates specific conditions and returns a symbolic name for
    * branches that match these conditions.
    *
    * @return an `Option[String]` containing the name of the branch as a string if
    *         a name can be determined, `None` otherwise.
    */
  def maybeName: Option[String] =
    if (equation.p == Rational.negOne && equation.q == Rational.negOne)
      branch match {
        case 0 => Some("\uD835\uDED7")
        case 1 => Some("\uD835\uDED9")
        case _ => None
      }
    else
      None

  /**
    * Method to render this NumberLike in a presentable manner.
    *
    * @return a String
    */
  def render: String = maybeName getOrElse toString

  /**
    * Scales the current `Algebraic_Quadratic` instance by a given `Rational` value.
    * TESTME
    *
    * @param x the scaling factor represented as a `Rational`.
    * @return a new `Algebraic_Quadratic` instance with scaled values of `p` and `q`.
    */
  def scale(x: Rational): Algebraic = this match {
    case phi if (phi eq Algebraic_Quadratic.phi) || (phi eq Algebraic_Quadratic.psi) =>
      if (x.signum < 0)
        throw NumberException("phi and psi cannot be scaled by negative number directly")
      copy(equation = equation.scale(x))
    case _ =>
      copy(equation = equation.scale(x))
  }



  /**
    * Adds a `Algebraic` instance to the current `Algebraic_Quadratic` instance based on specific conditions.
    *
    * If the input `Algebraic` matches the structure `Algebraic_Quadratic` with the same equation
    * and a position equal to the current position, the original instance is multiplied by 2.
    * Otherwise, the result is `Algebraic_Quadratic.zero`.
    *
    * @param s the `Algebraic` to be added to the current instance.
    * @return a `Algebraic` instance representing the result of the addition according to the specified conditions.
    */
  def add(s: Algebraic): Algebraic = s match {
    case Algebraic_Quadratic(_, `equation`, b) =>
      if (b == pos) this multiply Rational.two
      else Algebraic_Quadratic.zero
    case Algebraic_Quadratic(_, Quadratic(a, b), _) =>
      val horizontal: Rational = (equation.p - a) / two
      val vertical: Rational = b - equation.q + (a ∧ 2) / 4 + (equation.p ∧ 2)
      copy(equation = equation.shiftOrigin(horizontal)).add(vertical)
    case _ =>
      throw NumberException(s"add($s) is not supported for Algebraic_Quadratic")
  }

  /**
    * Multiply this Field by x and return the result.
    *
    * * @param x the multiplicand.
    * * @return the product.
    */
  def multiply(x: Field): Field = x match {
    case a: Algebraic =>
      multiply(a)
    case _ =>
      value multiply x
  }

  /**
    * Multiplies the provided Algebraic object with a predefined operation.
    *
    * @param that the Algebraic object to be multiplied
    * @return a new Algebraic resulting from the multiplication
    * @throws NumberException for all inputs.
    */
  def multiply(that: Algebraic): Field = {
    val thisSolution: Solution = this.solve
    val thatSolution = that.solve
    val plusFunctions = DyadicOperationPlus.functions
    val timesFunctions = DyadicOperationTimes.functions
    val iBase = thisSolution.base
    val oBase = thatSolution.base
    val maybeProductBases: Option[Value] = doComposeValueDyadic(iBase, oBase)(timesFunctions)
    val iOffset = thisSolution.offset
    val oOffset = thatSolution.offset
    val iN = thisSolution.negative
    val oN = thatSolution.negative
    val iF = thisSolution.factor
    val oF = thisSolution.factor
    val maybeProductOffsets: Option[ProtoNumber] = for {
      p <- iF.multiply(iOffset, oOffset, oF)
      (v, f, z) = p
      if f == PureNumber
      vDash = negateConditional(iN != oN)(v)
    } yield (vDash, f, z)

    val maybeCrossTerms =
      if (iOffset == oOffset && iBase == oBase && iF == oF)
        if (iN != oN)
          Some(Value.zero)
        else
          None
      else
        None

    val maybeAlgebraic: Option[Algebraic] = for {
      x <- maybeCrossTerms
      y <- maybeProductBases
      (v, PureNumber, None) <- maybeProductOffsets
      a <- doComposeValueDyadic(x, y)(plusFunctions)
      b <- doComposeValueDyadic(v, a)(plusFunctions)
      r <- Value.maybeRational(b)
    } yield Algebraic_Linear(LinearEquation(r))

    (maybeAlgebraic orElse {
      that.asReal map (r => this.multiply(r))
    }).get // XXX this can throw an Exception (though unlikely)
  }


  /**
    * Multiplies the current `Algebraic` instance by a specified `Rational` value, resulting in a new transformed `Algebraic`.
    * TESTME I have no idea if this is correct.
    * It was Claude who suggested the expression.
    *
    * @param x the multiplier, represented as a `Rational` value.
    * @return a new `Algebraic` instance with the equation modified by the multiplication.
    */
  def multiply(x: Rational): Algebraic =
    copy(equation = equation.transform((p, _) => p * x, (_, q) => q * (x ∧ 2))) // XXX where does this come from (it's from Claude)?

//  def addHorizontal(c: Rational): Algebraic =
//    copy(equation = quadratic.transform((p, _) => p - 2 * c, (p, q) => c ∧ 2 - p * c + q))
//
//  def addVertical(c: Rational): Algebraic =
//    copy(equation = quadratic.copy(q = quadratic.q + c))

  /**
    * Computes the result of raising this `Algebraic_Quadratic` instance to the power of the given exponent `k`.
    *
    * Depending on the value of `k`, different transformations and computations
    * are applied to generate the resulting `Algebraic`:
    * - If `k` is -1, the inverse of the solution is returned.
    * - If `k` is 0, the solution representing 1 is returned.
    * - If `k` is 1, the current instance is returned as-is.
    * - For higher values of `k`, specific equations and transformations are used to compute the result.
    *
    * @param k the exponent to which this `Algebraic_Quadratic` instance is raised.
    * @return a new `Algebraic` instance representing the result of the power operation.
    */
  def doPower(k: Int): Algebraic = k match {
    case -1 =>
      copy(equation = equation.invert)
    case 0 =>
      Algebraic_Quadratic.one
    case 1 =>
      this
    case 2 =>
      square
    case 3 =>
      val (p, q) = (equation.p, equation.q)
      val pq: Rational = p * q
      val factor = p ∧ 2 - q
      val addend = pq
      val solution = scale(factor)
      copy(equation = solution.equation.asInstanceOf[algebraic.Quadratic].shiftOrigin(addend))
    case 4 =>
      val (p, q) = (equation.p, equation.q)
      val pq: Rational = p * q
      val pSquaredMinusQ = p ∧ 2 - q
      val factor = -p * pSquaredMinusQ + pq
      val addend = -q * pSquaredMinusQ
      val solution = scale(factor)
      copy(equation = solution.equation.asInstanceOf[Quadratic].shiftOrigin(addend))
    case _ =>
      throw NumberException(s"power($k) is not supported for Algebraic_Quadratic")
  }

//  def invert: Algebraic =
//    copy(equation = quadratic.transform((p, q) => p / q, (_, q) => q.invert))

  /**
    * Computes the squared solution for the current instance by scaling the equation's `p` value and adjusting the result with the negated `q` value.
    *
    * This method applies transformations to the associated equation and retrieves a specific solution if possible.
    * If the solution cannot be computed, it throws a `NumberException`.
    *
    * @return the squared solution of the instance as a `Solution`
    * @throws NumberException if the operation is not supported for the current instance
    */
  def solutionSquared: Solution =
    solve scale (-equation.p) match {
      case Some(s) => s add -equation.q
      case None => throw NumberException(s"solutionSquared is not supported for $this")
    }

  /**
    * Computes the square of the current `Algebraic` by transforming its associated equation and toggling its position.
    *
    * @return a new `Algebraic` instance with the squared transformation applied.
    */
  def square: Algebraic =
    copy(equation = equation.transform((p, q) => 2 * q - p ∧ 2, (_, q) => q ∧ 2))

  /**
    * Determines if the given object can be considered equal to this instance.
    * This method is used to support the implementation of the equality operation.
    *
    * @param other the object to be compared against this instance
    * @return true if the given object is an instance of Algebraic_Quadratic; false otherwise
    */
  def canEqual(other: Any): Boolean =
    other.isInstanceOf[Algebraic_Quadratic]

  /**
    * Compares this `Algebraic_Quadratic` instance to another object for equality.
    * The comparison checks if the other object is of the same type and has equivalent values
    * for the properties of `p`, `q`, and `pos`.
    *
    * @param other the object to compare with this instance for equality.
    * @return `true` if the provided object is a `Algebraic_Quadratic` and has the same values
    *         for the relevant properties; otherwise, `false`.
    */
  override def equals(other: Any): Boolean = other match {
    case that: Algebraic_Quadratic =>
      that.canEqual(this) &&
          equation == that.equation &&
          pos == that.pos
    case _ =>
      false
  }

  /**
    * Computes the hash code for this instance based on its state.
    *
    * This method takes into account the values of the fields `p`, `q`, and `pos`,
    * ensuring that the hash code reflects the identity and equality of this object.
    *
    * @return an `Int` representing the hash code of this instance.
    */
  override def hashCode(): Int = {
    Objects.hash(equation, pos)
  }

  override def toString: String =
    s"Algebraic_Quadratic($solve, $equation, $pos)"

  /**
    * Adds the given Rational object to the current Algebraic.
    *
    * @param rational the Rational object to be added
    * @return a new Algebraic resulting from the addition
    */
  def add(rational: Rational): Algebraic = copy(equation = equation.copy(p = equation.p + rational))
}

/**
  * Represents a equation equation of the form `x² + p*x + q = 0`, where `p` and `q` are rational coefficients.
  *
  * @constructor Creates an instance of the equation equation with coefficients `p` and `q`.
  *              It enforces conditions to ensure the validity of the equation.
  * @param p the coefficient of the linear term (`x`).
  * @param q the constant term of the equation.
  */
case class Quadratic(p: Rational, q: Rational) extends Equation {

  /**
    * Attempts to find a solution for a mathematical equation corresponding to the given branch.
    * Solutions are represented as an instance of the `Algebraic` class.
    * If appropriate, a `Algebraic` can be converted into Complex form.
    *
    * @param branch the branch index for which the solution is being sought.
    *               The branch index identifies specific solutions for equations that may have multiple solutions.
    * @return an `Option[Algebraic]`, where `Some(solution)` contains the solution for the specified branch,
    *         or `None` if no solution exists for the given branch.
    */
  def solve(branch: Int): Solution =
    if (branch >= 0 && branch < 2)
      QuadraticSolution(Value.fromRational(-p / 2), Value.fromRational(discriminant / 4), SquareRoot, branch != 0)
    else
      throw NumberException(s"solve($branch) is not currently supported for complex roots of a Quadratic")

  /**
    * Shifts the origin of the equation equation by transforming its `p` and `q` components
    * based on the provided transformation functions dependent on the parameter `c`.
    *
    * @param c the `Rational` value used to determine the shift applied to the equation equation.
    * @return a new `Quadratic` instance with its origin shifted according to the transformations.
    */
  def shiftOrigin(c: Rational): Quadratic = transform((p, _) => p - 2 * c, (p, q) => c ∧ 2 - p * c + q)

  /**
    * Returns the number of branches for this instance.
    *
    * @return an `Int` representing the number of branches, which is always 2.
    */
  def branches: Int = 2

  /**
    * Yields the inverse of this Equation. CONSIDER Does this make sense??
    * CONSIDER should we make it an abstract method on Equation?
    */
  def invert: Quadratic =
    transform((p, q) => p / q, (_, q) => q.invert)

  /**
    * Transforms the current instance of `Quadratic` by applying provided functions
    * to the `p` and `q` components.
    *
    * @param fP a function that transforms the `p` component.
    * @param fQ a function that transforms the `q` component.
    * @return a new `Quadratic` instance with `p` transformed using `fP` and `q` transformed using `fQ`.
    */
  def transform(fP: (Rational, Rational) => Rational, fQ: (Rational, Rational) => Rational): Quadratic =
    copy(p = fP(p, q), q = fQ(p, q))

  /**
    * Computes the discriminant of a quadratic equation based on the formula `p^2 - 4 * q`.
    * The discriminant is a critical component in determining the nature of the roots of a
    * quadratic equation.
    * If the discriminant is positive then there are two distinct roots;
    * if negative, then there are no real roots;
    * if zero, then there's a double root.
    *
    * @return an `Int` representing the discriminant.
    */
  def discriminant: Rational =
    p * p - 4 * q

  override def toString: String = s"Quadratic Equation: x^2 ${Quadratic.termSign(p)}x ${Quadratic.termSign(q)} = 0"

  /**
    * Determines if the given object can be considered equal to this instance.
    * This method is used to support the implementation of the equality operation.
    *
    * @param other the object to be compared against this instance
    * @return true if the given object is an instance of Algebraic_Quadratic; false otherwise
    */
  def canEqual(other: Any): Boolean =
    other.isInstanceOf[Quadratic]

  /**
    * Scales the equation by a given rational factor.
    * We are essentially transforming the equation into a new equation with coefficients `x * p` and `x^2 * q`.
    *
    * The equation is represented by coefficients `p` and `q`, and scaling is applied as:
    * - `p` is multiplied by `x`
    * - `q` is multiplied by `x^2`
    *
    * @param x the scaling factor as a `Rational`.
    * @return a new `Equation` resulting from scaling the current equation by the factor `x`.
    */
  def scale(x: Rational): Quadratic =
    copy(p = x * p, q = x * x * q)
}

object Quadratic {
  val goldenRatioEquation: Quadratic = Quadratic(Rational.negOne, Rational.negOne)

  /**
    * Generates a string representation of the sign and the absolute value of the given rational number.
    * The sign is determined based on the value of the input: a negative sign ("-") for negative numbers
    * and a positive sign ("+") for non-negative numbers. The absolute value is appended to the sign.
    * TODO move this into Field
    *
    * @param x the rational number whose sign and absolute value are to be rendered as a string
    * @return a string in the format "+ n" or "- n", where "n" represents the absolute value of the input
    */
  def termSign(x: Field, add: Boolean = true): String = (if (add) if (x.signum < 0) "- " else "+ " else "") + x.abs.render
}

/**
  * Companion object for the `Algebraic_Quadratic` class.
  * Provides an unapply method for pattern matching.
  */
object Algebraic_Quadratic {
  def unapply(equation: Algebraic_Quadratic): Option[(Option[String], Equation, Boolean)] =
    Some(equation.maybeName, equation.equation, equation.pos)

  val phi: Algebraic_Quadratic = Algebraic_Quadratic(goldenRatioEquation, pos = true)
  val psi: Algebraic_Quadratic = Algebraic_Quadratic(goldenRatioEquation, pos = false)
  val one: Algebraic_Quadratic = Algebraic_Quadratic(Quadratic(-2, 1), pos = true)
  val zero: Algebraic_Quadratic = Algebraic_Quadratic(Quadratic(0, 0), pos = true)
}
