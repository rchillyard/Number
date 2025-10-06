/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.algebraic

import com.phasmidsoftware.number.core.inner.Operations.doComposeValueDyadic
import com.phasmidsoftware.number.core.inner.Value.{fromRational, maybeRational}
import com.phasmidsoftware.number.core.inner._
import com.phasmidsoftware.number.core.{ComplexCartesian, ExactNumber, Field, Number, NumberLike, Real}
import com.phasmidsoftware.number.misc.FP
import scala.Option.when
import scala.util.Failure

/**
  * Trait to model the behavior of a solution to an equation.
  * All such solutions are exact.
  * Typically, however, such solutions cannot be represented exactly as pure numbers.
  * The four parameters of a Solution are: base, offset, branch, and factor.
  * Solutions are subject to various operations such as addition, scaling, and number conversions.
  * Extends the `NumberLike` trait, inheriting behavior common to number-like entities.
  *
  * CONSIDER it is inappropriate to include branch in `Solution`. A `Solution` should be a unique solution.
  */
trait Solution extends NumberLike {
  /**
    * Retrieves the base value of the solution.
    *
    * @return the base value of type Value
    */
  def base: Value

  /**
    * Retrieves the offset value of the solution.
    *
    * @return the offset value of type Value
    */
  def offset: Value

  /**
    * Retrieves the factor associated with the offset.
    *
    * @return the factor of type Factor
    */
  def factor: Factor

  /**
    * Branch number runs from `0` to `n-1` where `n` is the number of branches (or roots).
    * For a polynomial of degree `n`, there will, in general be `n` branches or roots.
    * Branch 0 is always the branch with the smallest positive offset.
    *
    * @return the branch number.
    */
  def branch: Int

  /**
    * Determines the factor associated with this solution based on certain conditions.
    * If the solution is a pure number, it returns `Some(PureNumber)`.
    * If the base value of the solution is zero, it returns `Some(factor)`.
    * Otherwise, it returns `None`.
    *
    * @return an `Option` of type `Factor`, where `Some` may contain the factor
    *         depending on the conditions, or `None` if no factor is applicable
    */
  def maybeFactor: Option[Factor] =
    if (isPureNumber)
      Some(PureNumber)
    else if (Value.isZero(base))
      Some(factor)
    else
      None

  /**
    * Method to determine if this NumberLike object is exact.
    * For instance, Number.pi is exact, although if you converted it into a PureNumber, it would no longer be exact.
    *
    * @return true if this NumberLike object is exact in the context of No factor, else false.
    */
  def isExact: Boolean = true

  /**
    * Determines whether the solution is a pure number.
    * A pure number is defined as one without any associated factors or offsets.
    *
    * @return true if the solution is a pure number, false otherwise
    */
  def isPureNumber: Boolean

  /**
    * Method to determine if this NumberLike is actually a real Number (i.e. not complex).
    * NOTE: to force this as a Number, use convertToNumber in the companion Object.
    *
    * CONSIDER redefining this as Option[Field] or Option[Real].
    *
    * @return a Some(x) if this is a Number; otherwise return None.
    */
  def asNumber: Option[Number] = asField.asNumber

  /**
    * Converts the solution to a representation in the Field domain.
    *
    * @return an instance of Field representing the solution.
    */
  def asField: Field

  /**
    * Determines whether the solution is zero.
    *
    * @return true if the solution is zero, false otherwise.
    */
  def isZero: Boolean

  /**
    * Determines whether the solution represents unity.
    *
    * @return true if the solution represents unity, false otherwise
    */
  def isUnity: Boolean

  /**
    * Determines the sign of the solution.
    *
    * @return an integer representing the sign of the solution: 0 if the solution is zero,
    *         a positive value if the solution is positive, or a negative value if the solution is negative.
    */
  def signum: Int

  /**
    * Adds a `Rational` value to the current solution and returns a new `Solution` as the result.
    *
    * @param addend the `Rational` value to be added to the current solution
    * @return a new `Solution` instance representing the sum of the current solution and the given `addend`
    */
  def add(addend: Rational): Solution

  /**
    * Adds the given solution to the current solution and returns an optional result.
    * The addition may fail under certain conditions, in which case `None` is returned.
    *
    * @param solution the `Solution` to be added to the current solution
    * @return an `Option` containing the resulting `Solution` if the addition is successful,
    *         or `None` if the addition cannot be performed
    */
  def add(solution: Solution): Option[Solution]

  /**
    * Scales the current solution by multiplying its base and offset values by the specified `Rational` factor.
    *
    * @param r the `Rational` value used as the scaling factor
    * @return an optional `Solution` instance, where `Some` contains the scaled solution if the operation is valid,
    *         or `None` if scaling cannot be performed
    */
  def scale(r: Rational): Option[Solution]
}

/**
  * Represents a mathematical solution, which can be either linear or quadratic in nature.
  * This object provides factory methods for creating instances of `Solution`.
  */
object Solution {
  /**
    * Creates a `Solution` from a given `Rational` value.
    *
    * @param r the rational value used to construct the solution
    * @return a `Solution` instance constructed from the given rational value
    */
  def apply(r: Rational): Solution =
    LinearSolution(Value.fromRational(r))

  /**
    * Creates an instance of `Solution` based on the given parameters. If the `offset` is zero, it returns
    * a `LinearSolution`, otherwise it returns a `QuadraticSolution`.
    *
    * @param base   the base `Value` for the solution
    * @param offset the offset `Value` for the solution
    * @param factor the `Factor` used in the solution computation
    * @param branch an integer that specifies the branch for the quadratic solution
    * @return a `Solution` instance representing either a `LinearSolution` or `QuadraticSolution`
    */
  def apply(base: Value, offset: Value, factor: Factor, branch: Int): Solution =
    if (Value.isZero(offset))
      LinearSolution(base)
    else
      QuadraticSolution(base, offset, factor, branch)
}

/**
  * Represents a solution to a quadratic equation in the context of a specific Factor.
  *
  * @constructor Creates a new instance of QuadraticSolution.
  * @param base   The base Value component of the solution.
  * @param offset The offset Value component of the solution, usually derived from the discriminant.
  * @param factor The Factor associated with the solution, influencing conversions and evaluations.
  *
  *               QuadraticSolution models the structure of solutions to equations of the form:
  *               x&#94;2 + px + q = 0.
  *               It encapsulates components necessary for representing and interpreting the result
  *               in terms of its `base`, the `offset` typically arising from the quadratic formula's discriminant,
  *               and any contextual modifications by a `factor`.
  *
  *               Instances of this class are designed to interact with the `Factor` type in performing various
  *               operations such as evaluating, converting, or rendering the solution accurately in diverse contexts.
  */
case class QuadraticSolution(base: Value, offset: Value, factor: Factor, branch: Int) extends Solution {

  /**
    * Represents the radical term of the quadratic solution (the square root of the discriminant with the correct sign applied).
    *
    * The `radicalTerm` is derived based on the `branch` value:
    * - If `branch` is `0`, the `offset` value is used directly.
    * - Otherwise, the negated value of `offset` is used.
    */
  lazy val radicalTerm: Value = if (branch == 0) offset else Value.negate(offset)

  /**
    * Method to render this NumberLike in a presentable manner.
    * CONSIDER improve this in the event that offset itself is negative (as opposed to imaginary).
    * However, that scenario is unlikely, I believe.
    *
    * @return a String
    */
  def render: String =
    if (this == QuadraticSolution.phi)
      Algebraic.phi.render
    else if (this == QuadraticSolution.psi)
      Algebraic.psi.render
    else if (Value.isZero(offset) || Value.isZero(base))
      asField.render
    else
      s"Solution: $base + ${if (branch == 1) "- " else "+ "} ${factor.render(offset)}"

  /**
    * Computes the conjugate of the current quadratic solution.
    *
    * The conjugate is calculated by inverting the branch of the solution,
    * effectively toggling between the two branches of the quadratic equation.
    *
    * @return a new QuadraticSolution instance representing the conjugate of the current solution
    */
  def conjugate: QuadraticSolution =
    copy(branch = 1 - branch)

  /**
    * Checks if the solution represented by this instance is equivalent to zero.
    *
    * A solution is considered zero if both its `base` and `offset` components are zero values.
    *
    * @return true if both the `base` and `offset` are zero, otherwise false.
    */
  def isZero: Boolean =
    Value.isZero(base) && isPureNumber

  /**
    * Determines if the current quadratic solution represents a pure number.
    *
    * A solution is considered a pure number if its offset component is zero.
    * This method relies on the `Value.isZero` utility to check whether the
    * offset is zero.
    *
    * @return true if the offset of the solution is zero, otherwise false.
    */
  def isPureNumber: Boolean =
    Value.isZero(offset)

  /**
    * Determines if the current QuadraticSolution instance represents unity (1).
    *
    * The solution is considered unity if its `base` component is equivalent to one
    * (as verified by the conversion to a Rational) and its `offset` is zero.
    *
    * @return true if the solution represents unity, otherwise false.
    */
  def isUnity: Boolean =
    isPureNumber && maybeRational(base).contains(Rational.one)

  /**
    * Determines the "sign" of the current quadratic solution.
    *
    * This method utilizes the `signum` method from the `asField` representation
    * of the solution to evaluate its sign. The sign can indicate whether the
    * solution is positive, negative, or zero based on its position relative to
    * the origin in the Field domain.
    *
    * @return +1 if the solution is positive, -1 if negative, or 0 if it is at the origin.
    */
  def signum: Int =
    asField.signum

  import scala.language.implicitConversions

  /**
    * Converts a given `Number` into a `Field`.
    *
    * TODO change this method to be non-implicit. Implicit conversions are evil.
    * Don't forget to remove the implicitConversions flag from other sources.
    *
    * @param x the `Number` to be converted into a `Field`.
    * @return a `Field` representation of the provided `Number`.
    */
  implicit def convertFromNumber(x: Number): Field =
    Real(x)

  /**
    * Converts the current QuadraticSolution instance into a Field representation.
    *
    * TODO refactor this code using radicalTerm
    *
    * The method computes a Field value based on the `base`, `offset`, and `factor`.
    * If the `offset` component is zero, the result is the `base` number as a Field.
    * Otherwise, it incorporates the `offset` and possibly an imaginary component
    * to produce either a ComplexCartesian number or a regular Field representation,
    * depending on the properties of the given solution.
    *
    * @return a Field representation of the current QuadraticSolution instance
    */
  def asField: Field =
    if (Value.isZero(offset))
      ExactNumber(base, PureNumber)
    else {
      val offsetNumber: ExactNumber = ExactNumber(offset, factor)

      def offsetToNumber = {
        val maybeValue = factor.convert(offset, PureNumber) map (v => ExactNumber(v, PureNumber))
        val maybeApproximation = offsetNumber.asNumber
        FP.toTry(maybeValue orElse maybeApproximation, Failure(new Exception("QuadraticSolution.asField: factor.convert failed")))
      }

      if (offsetNumber.isImaginary) { // XXX offset is negative and factor is SquareRoot
        val imaginaryPart = {
          if (branch == 0) // TESTME
            ExactNumber(Value.negate(offset), factor)
          else
            offsetToNumber.get // TODO convert to a proper exception
        } // CONSIDER handling this unlikely but possible exception properly
        ComplexCartesian(ExactNumber(base, PureNumber), imaginaryPart)
      }
      else {
        val variablePart = offsetToNumber.get // CONSIDER handling this unlikely but possible exception properly
        ExactNumber(base, PureNumber) + (if (branch == 1) variablePart.makeNegative else variablePart)
      }
    }

  /**
    * Adds a `Rational` value to the current solution and returns a new `Solution` as the result.
    * NOTE this only affects the `base` of this `Solution`.
    *
    * @param addend the `Rational` value to be added to the current solution
    * @return a new `Solution` instance representing the sum of the current solution and the given `addend`
    */
  def add(addend: Rational): Solution = {
    val functions = DyadicOperationPlus.functions
    val maybeX: Option[Value] = doComposeValueDyadic(base, fromRational(addend))(functions)
    copy(base = maybeX.get) // CONSIDER handling this possible exception properly
  }

  /**
    * Adds the specified solution to the current solution and returns an optional new solution
    * that represents the result of the addition. The addition occurs only when
    * the `offset`, `factor`, and `negative` properties of both solutions match the
    * specified conditions.
    *
    * @param solution the solution to be added to the current solution
    * @return an Option containing the resulting solution after the addition if the conditions are met, or None otherwise
    */
  def add(solution: Solution): Option[Solution] =
    solution match {
      case q: QuadraticSolution =>
        if (factor == q.factor) {
          // CONSIDER should we be using z here?
          val (a, b, z) = (branch, q.branch) match {
            case (0, _) => (this, q, branch)
            case (_, 0) => (q, this, q.branch)
            case _ => (this.conjugate, q.conjugate, 1 - branch)
          }
        for {
          x <- doComposeValueDyadic(a.base, b.base)(DyadicOperationPlus.functions)
          (v, f, z) <- factor.add(a.offset, b.offset, factor, b.branch == 1)
          if f == factor && z.isEmpty
        } yield Solution(x, v, factor, branch = a.branch)
      }
      else
        None  // TESTME
    }

  // TODO this needs more work and testing. For example, factors don't have to be the same.
  private def multiply(solution: Solution): Option[Solution] =
    solution match {
      case q: QuadraticSolution =>
        if (factor == q.factor) {
          // CONSIDER should we be using z here?
          val (a, b, z) = (branch, q.branch) match {
            case (0, _) => (this, q, branch)
            case (_, 0) => (q, this, q.branch)
            case _ => (this.conjugate, q.conjugate, 1 - branch)
          }
          for {
            x <- doComposeValueDyadic(a.base, b.base)(DyadicOperationTimes.functions)
            (v, f, z) <- factor.multiply(a.offset, b.offset, factor)
            if f == factor && z.isEmpty
          } yield Solution(x, v, factor, branch = a.branch)
        }
        else
          None  // TESTME
    }


  /**
    * Scales the quadratic solution using a given rational factor.
    *
    * This method computes a new quadratic solution by scaling the current
    * solution's components (`base` and `offset`) with a specified rational
    * multiplier. If any intermediate calculation fails, it returns `None`.
    *
    * @param r the scaling factor as a Rational value
    * @return an Option containing the scaled quadratic solution if calculations succeed, otherwise None
    */
  def scale(r: Rational): Option[Solution] =
    for {
      b <- maybeRational(base)
      x = fromRational(b * r)
      (v, g, None) <- factor.multiply(offset, fromRational(r), PureNumber)
    } yield QuadraticSolution(x, v, g, branch)

}

/**
  * Object containing predefined solutions to a quadratic equation and utilities.
  *
  * The object includes two specific solutions (`phi` and `psi`) that are parametrized
  * using the `QuadraticSolution` class, demonstrating the representation of roots
  * of a quadratic equation. These roots are computed using predefined rational
  * coefficients and specified root indices.
  */
object QuadraticSolution {
  /**
    * Represents the value `phi`, which is a solution of a quadratic equation
    * derived using `QuadraticSolution`. The solution is constructed from:
    * - The first coefficient provided by converting `1/2` into a `Value` using `Value.fromRational`.
    * - The second coefficient provided by converting `5/4` into a `Value` using `Value.fromRational`.
    * - The root type specified as `SquareRoot`.
    * - The root index specified as `0`, which identifies this as one of the possible roots.
    */
  val phi = QuadraticSolution(Value.fromRational(Rational.half), Value.fromRational(Rational(5, 4)), SquareRoot, 0)
  /**
    * Represents a specific quadratic solution characterized by its parameters.
    *
    * @see QuadraticSolution
    * @see Value.fromRational
    */
  val psi = QuadraticSolution(Value.fromRational(Rational.half), Value.fromRational(Rational(5, 4)), SquareRoot, 1)
}

/**
  * Represents a linear solution characterized by a single `value`.
  * Implements the `Solution` trait where the `base` is equal to the `value`,
  * the `offset` is `Value.zero`, and the `factor` is `PureNumber`.
  * CONSIDER we should not have to deal with `offset, factor, negative` attributes here.
  *
  * @param value the base value of the solution
  */
case class LinearSolution(value: Value) extends Solution {
  /**
    * Retrieves the base value of the solution.
    *
    * @return the base value of type Value
    */
  def base: Value = value

  /**
    * Retrieves the offset value of the solution.
    *
    * @return the offset value of type Value
    */
  def offset: Value = Value.zero

  /**
    * Retrieves the factor associated with the offset.
    *
    * @return the factor of type Factor
    */
  def factor: Factor = PureNumber

  /**
    * Determines whether the solution is a pure number.
    * A pure number is defined as one without any associated factors or offsets.
    *
    * @return true if the solution is a pure number, false otherwise
    */
  def isPureNumber: Boolean = true

  /**
    * Determines if the offset is negative.
    * A negative value may not be expressible with just `Value` and `Factor`, for example, if `factor` is `SquareRoot`.
    *
    * @return true if the offset is negative, false otherwise
    */
  def branch: Int = 0

  /**
    * Converts the solution to a representation in the Field domain.
    *
    * @return an instance of Field representing the solution.
    */
  def asField: Field = Real(ExactNumber(value, PureNumber))

  /**
    * Determines whether the solution is zero.
    *
    * @return true if the solution is zero, false otherwise.
    */
  def isZero: Boolean = Value.isZero(value)

  /**
    * Determines whether the solution represents unity.
    *
    * @return true if the solution represents unity, false otherwise
    */
  def isUnity: Boolean = Value.isEqual(value, Value.one)

  /**
    * Determines the sign of the solution.
    *
    * @return an integer representing the sign of the solution: 0 if the solution is zero,
    *         a positive value if the solution is positive, or a negative value if the solution is negative.
    */
  def signum: Int = Value.signum(value)

  /**
    * Adds the specified solution to the current solution and returns
    * a new solution that represents the result of the addition.
    *
    * TODO implement me
    *
    * @param solution the solution to be added to the current solution
    * @return an optional solution representing the sum of the current solution
    *         and the given solution, or None if the operation is not valid
    */
  def add(solution: Solution): Option[Solution] = solution match {
    case LinearSolution(v) =>
      when(Value.isZero(v))(this) orElse {
        val functions = DyadicOperationPlus.functions
        for {
          x <- doComposeValueDyadic(v, value)(functions)
        } yield LinearSolution(x)
      }
    case _ =>
      None
  }

  /**
    * Adds a `Rational` value to the current solution and returns a new `Solution` as the result.
    *
    * @param addend the `Rational` value to be added to the current solution
    * @return a new `Solution` instance representing the sum of the current solution and the given `addend`
    */
  def add(addend: Rational): Solution = {
    val po = PureNumber.add(value, fromRational(addend), PureNumber)
    po match {
      case Some((v, PureNumber, None)) =>
        LinearSolution(v)
      case _ =>
        throw new Exception("LinearSolution.add: PureNumber.add failed")
    }
  }

  /**
    * Scales the given rational value using the current value to produce an optional solution.
    *
    * @param r the rational value used as a multiplier during scaling
    * @return an optional `Solution` as the result of scaling, or `None` if the operation is not valid
    */
  def scale(r: Rational): Option[Solution] = for {
    x <- maybeRational(value)
  } yield LinearSolution(fromRational(x * r))

  /**
    * Renders the solution as a string representation.
    * It attempts to use a rational representation of the value if available;
    * otherwise, it falls back to the default string representation of the solution.
    *
    * @return a string representation of the solution
    */
  def render: String =
    maybeRational(value) map (_.toString) getOrElse toString
}