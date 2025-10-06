/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.algebraic

import com.phasmidsoftware.number.core
import com.phasmidsoftware.number.core.Constants.sPhi
import com.phasmidsoftware.number.core.inner.Operations.doComposeValueDyadic
import com.phasmidsoftware.number.core.inner.Value.{maybeRational, negateConditional}
import com.phasmidsoftware.number.core.inner._
import com.phasmidsoftware.number.core.{Field, NumberException, Real, algebraic}
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
  def render: String = maybeName getOrElse solve.render

  /**
    * Scales the current `Algebraic_Quadratic` instance by a given `Rational` value.
    * TESTME
    *
    * @param x the scaling factor represented as a `Rational`.
    * @return a new `Algebraic_Quadratic` instance with scaled values of `p` and `q`.
    */
  def scale(x: Rational): Algebraic =
    copy(equation = equation.scale(x), pos = if (x.signum > 0) pos else !pos)

  /**
    * Adds the given `Algebraic` instance to the current `Algebraic_Quadratic` instance,
    * performing computations based on the equation and the type of the provided input.
    *
    * @param algebraic the `Algebraic` instance to be added to this one.
    * @return a new `Algebraic` instance resulting from the addition.
    * @throws NumberException if the provided `Algebraic` type is not supported for addition.
    */
  def add(algebraic: Algebraic): Algebraic =
    this.solve add algebraic.solve match {
      case Some(s: QuadraticSolution) =>
        Algebraic_Quadratic(s)
      case Some(s: LinearSolution) =>
        Algebraic_Linear(s)
      case None =>
        algebraic match {
          case Algebraic_Quadratic(_, `equation`, b) =>
            if (b == pos) this multiply Rational.two
            else Algebraic.zero
          case Algebraic_Quadratic(_, Quadratic(a, b), _) =>
            val horizontal: Rational = (equation.p - a) / Rational.two
            val vertical: Rational = b - equation.q + (a ∧ 2) / 4 + (equation.p ∧ 2)
            copy(equation = equation.shiftOrigin(horizontal)).add(vertical)
          case _ =>
            throw NumberException(s"add($algebraic) is not supported for Algebraic_Quadratic")
        }
      case x =>
        throw NumberException(s"add($algebraic) is not supported for solutions which are not QuadraticSolutions: $x")
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
    val iN = thisSolution.branch
    val oN = thatSolution.branch
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
    copy(equation = equation.transform((p, _) => p * x, (_, q) => q * (x ∧ 2)))

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
      Algebraic.one
    case 1 =>
      this
    case 2 =>
      square
    case 3 =>
      // CONSIDER merging cases 3 and 4
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
    * @return `true` if the provided object is an `Algebraic_Quadratic` and has the same values
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
  def add(rational: Rational): Algebraic =
    copy(equation = equation shiftOrigin rational)
}

/**
  * Represents a (monic) equation of the form `x² + p*x + q = 0`, where `p` and `q` are rational coefficients.
  * A graph of this equation appears as follows: an upright parabola
  * (where p and q are both positive and (½p)⋏2 > q):
  * <pre>
  *          x                        |              x
  *           x                       |             x
  *             x                     |            x
  *                x              > ½p <         x
  *       -------------x----------+---|-------x------------------ v
  *                         x         |  x                        q
  *       - - - - - - - - - - - - x - + - - - - - - - - - - - - - ⋏
  *                                   |
  *</pre>
  * The roots of the equation (if such exist) are at `-w ± √(w^2-q)` where `w = ½p`
  * For more information regarding quadratic equations, see:
  * [[https://en.wikipedia.org/wiki/Quadratic_equation]]
  *
  * @constructor Creates an instance of the equation with coefficients `p` and `q`.
  * @param p the coefficient of the linear term (`x`).
  * @param q the constant term of the equation.
  */
case class Quadratic(p: Rational, q: Rational) extends Equation {

  /**
    * Attempts to find a solution for a mathematical equation corresponding to the given branch.
    * Solutions are represented as an instance of the `Algebraic` class.
    * If appropriate, an `Algebraic` can be converted into Complex form.
    *
    * @param branch the branch index for which the solution is being sought.
    *               The branch index identifies specific solutions for equations that may have multiple solutions.
    * @return an `Option[Algebraic]`, where `Some(solution)` contains the solution for the specified branch,
    *         or `None` if no solution exists for the given branch.
    */
  def solve(branch: Int): Solution =
    if (branch >= 0 && branch < 2)
      QuadraticSolution(Value.fromRational(-p / 2), Value.fromRational(discriminant / 4), SquareRoot, branch)
    else
      throw NumberException(s"solve($branch) is not currently supported for complex roots of a Quadratic")

  /**
    * Shifts the origin of the equation by transforming its `p` and `q` components
    * based on the provided transformation functions dependent on the parameter `c`.
    *
    * @param c the `Rational` value used to determine the shift applied to the equation.
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
    * Computes the sum of the conjugates for this instance of `Quadratic`.
    * The sum of conjugates is mathematically derived as the negation of the `p` coefficient
    * in a quadratic equation of the form `x^2 + p*x + q = 0`.
    *
    * @return a `Rational` value representing the sum of conjugates.
    */
  def conjugateSum: Rational =
    p.negate

  /**
    * Computes the product of the conjugate roots of the quadratic equation represented by this instance.
    * The product of the conjugates is determined directly from the coefficients of the quadratic equation
    * and is equivalent to the constant term divided by the leading coefficient.
    *
    * This is a special case of Vieta's formula for the product of the roots of a polynomial.
    *
    * @return a `Rational` value representing the product of the conjugate roots of the quadratic equation.
    */
  def conjugateProduct: Rational =
    q

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
    * If the discriminant is positive, then there are two distinct roots;
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

/**
  * The `Quadratic` object provides commonly used quadratic equations and utility methods related to them.
  */
object Quadratic {
  /**
    * Creates a quadratic equation whose solutions are plus or minus the square root of the given `Rational` number.
    * The quadratic equation is defined as `Quadratic(0, -r)`, where the first term is zero
    * and the second term is the negation of the input parameter.
    *
    * @param r the `Rational` number used as the coefficient for the square root equation.
    * @return a `Quadratic` instance that represents the square root equation.
    */
  def squareRootEquation(r: Rational): Quadratic = Quadratic(Rational.zero, r.negate)

  /**
    * An approximation of the golden ratio (φ), which is an irrational number
    * often encountered in mathematics, art, and nature. Its value is approximately
    * 1.6180339887498948.
    */
  val phiApprox = Real(sPhi)
  /**
    * A predefined quadratic equation relating to the golden ratio.
    * The equation is represented as `x² + x - 1 = 0`.
    * The roots of this equation are the golden ratio (phi) and its conjugate (psi).
    */
  val goldenRatioEquation: Quadratic = Quadratic(Rational.negOne, Rational.negOne)
  /**
    * Represents the equation `x² - 2 = 0`, which defines the relationship for the square root of 2.
    * This specific quadratic equation has the following form:
    *
    * `p = 0` (no linear term) and `q = -2` (constant term).
    *
    * Geometrically, this equation corresponds to a parabola, and its roots are at ±√2.
    * The equation is a notable instance of a quadratic, useful in various mathematical contexts.
    */
  val rootTwoEquation: Quadratic = squareRootEquation(Rational.two)

  /**
    * Formats the sign and absolute value of a given Field instance as a String.
    * The resulting string includes a sign ("+ " for positive or "- " for negative)
    * if the `add` parameter is true, followed by the absolute value of the Field
    * rendered as a String.
    *
    * @param x   the Field instance to be formatted.
    * @param add a Boolean flag indicating whether to prepend a sign ("+ " or "- ") to the result;
    *            defaults to true.
    * @return a formatted String representation of the Field's sign and absolute value.
    */
  private def termSign(x: Field, add: Boolean = true): String = (if (add) if (x.signum < 0) "- " else "+ " else "") + x.abs.render
}

/**
  * Companion object for the `Algebraic_Quadratic` class.
  * Provides an unapply method for pattern matching.
  */
object Algebraic_Quadratic {
  /**
    * Extractor method for the `Algebraic_Quadratic` class.
    *
    * @param equation An instance of `Algebraic_Quadratic` to be decomposed.
    * @return An `Option` containing a tuple with the optional name of the equation,
    *         its quadratic representation, and a boolean indicating a specific property.
    */
  def unapply(equation: Algebraic_Quadratic): Option[(Option[String], Quadratic, Boolean)] =
    Some(equation.maybeName, equation.equation, equation.pos)

  /**
    * Constructs an `Algebraic_Quadratic` from a given `QuadraticSolution`.
    *
    * Matches the provided quadratic solution components (`factor`, `base`, and `offset`)
    * to determine the appropriate `Algebraic_Quadratic` representation. If the solution
    * cannot be transformed, an exception is thrown.
    *
    * @param solution The quadratic solution to be transformed into an `Algebraic_Quadratic`.
    * @return An instance of `Algebraic_Quadratic` based on the given solution.
    * @throws NumberException if the provided solution cannot be converted to `Algebraic_Quadratic`.
    */
  def apply(solution: QuadraticSolution): Algebraic_Quadratic =
    (solution.factor, maybeRational(solution.base), maybeRational(solution.offset)) match {
      case (SquareRoot, Some(base), Some(offset)) =>
        Algebraic_Quadratic(Quadratic(base * -2, (base ∧ 2) - offset), solution.branch == 0)
      case _ =>
        throw NumberException(s"apply($solution) is not supported")
    }

  /**
    * Constructs an instance of `Algebraic_Quadratic` by processing the given base and offset numbers.
    * If the base number's factor is `PureNumber`, a `QuadraticSolution` is created using the base and offset,
    * and an `Algebraic_Quadratic` instance is generated from it.
    *
    * @param base   A `core.Number` representing the base value of the quadratic equation.
    * @param offset A `core.Number` representing the offset value of the quadratic equation.
    * @return An `Algebraic_Quadratic` instance derived from the given base and offset values.
    */
  def apply(base: core.Number, offset: core.Number, negative: Boolean): Algebraic_Quadratic = base.factor match {
    case PureNumber =>
      apply(QuadraticSolution(base.nominalValue, offset.nominalValue, offset.factor, branch = if (negative) 1 else 0))
    case _ =>
      throw NumberException(s"apply($base, $offset) is not supported if the base factor is not PureNumber")
  }

}
