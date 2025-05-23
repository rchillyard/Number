/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.algebraic

import com.phasmidsoftware.number.core.Real.convertFromNumber
import com.phasmidsoftware.number.core.inner.Operations.doComposeValueDyadic
import com.phasmidsoftware.number.core.inner.Value.fromRational
import com.phasmidsoftware.number.core.inner._
import com.phasmidsoftware.number.core.{ComplexCartesian, ExactNumber, Field, Number, NumberLike, Real}
import com.phasmidsoftware.number.misc.FP
import scala.util.Failure

/**
  * Represents a solution characterized by a `base` value, an `offset` value,
  * and a `factor`.
  * The `base` value is always a `PureNumber`.
  * The `offset` value has the given `factor`.
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
    * Determines if the offset is negative.
    * A negative value may not be expressible with just `Value` and `Factor`, for example, if `factor` is `SquareRoot`.
    *
    * @return true if the offset is negative, false otherwise
    */
  def negative: Boolean

  /**
    * Method to determine what `Factor`, if there is such, this `NumberLike` object is based on.
    * Unlike context, a `None` result is not permissive.
    *
    * @return an optional `Factor`.
    */
  def maybeFactor: Option[Factor] = if (Value.isZero(base)) Some(factor) else if (Value.isZero(offset)) Some(PureNumber) else None

  /**
    * Method to determine if this NumberLike object is exact.
    * For instance, Number.pi is exact, although if you converted it into a PureNumber, it would no longer be exact.
    *
    * @return true if this NumberLike object is exact in the context of No factor, else false.
    */
  def isExact: Boolean = true

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
    * Method to render this NumberLike in a presentable manner.
    * TODO improve this in the event that offset itself is negative (as opposed to imaginary).
    *
    * @return a String
    */
  def render: String = s"Solution: $base + ${if (negative) "- " else "+ "} ${factor.render(offset)}"

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
case class QuadraticSolution(base: Value, offset: Value, factor: Factor, negative: Boolean) extends Solution {
  /**
    * Converts the solution to a representation in the Field domain.
    *
    * @return an instance of Field representing the solution.
    */
  def asField: Field = {
    val baseNumber = ExactNumber(base, PureNumber)
    val offsetNumber: ExactNumber = ExactNumber(offset, factor)

    def offsetToNumber = {
      val maybeValue = factor.convert(offset, PureNumber) map (v => ExactNumber(v, PureNumber))
      val maybeApproximation = offsetNumber.asNumber
      FP.toTry(maybeValue orElse maybeApproximation, Failure(new Exception("QuadraticSolution.asField: factor.convert failed")))
    }

    if (offsetNumber.isImaginary) { // XXX offset is negative and factor is SquareRoot
      val imaginaryPart =
        if (!negative)
          ExactNumber(Value.negate(offset), factor)
        else
          offsetToNumber.get // CONSIDER handling this unlikely but possible exception properly
      ComplexCartesian(baseNumber, imaginaryPart)
    }
    else {
      val variablePart = offsetToNumber.get // CONSIDER handling this unlikely but possible exception properly
      baseNumber + (if (negative) variablePart.makeNegative else variablePart)
    }
  }

  /**
    * Checks if the solution represented by this instance is equivalent to zero.
    *
    * A solution is considered zero if both its `base` and `offset` components are zero values.
    *
    * @return true if both the `base` and `offset` are zero, otherwise false.
    */
  def isZero: Boolean = Value.isZero(base) && Value.isZero(offset)

  /**
    * Determines if the current QuadraticSolution instance represents unity (1).
    *
    * The solution is considered unity if its `base` component is equivalent to one
    * (as verified by the conversion to a Rational) and its `offset` is zero.
    *
    * @return true if the solution represents unity, otherwise false.
    */
  def isUnity: Boolean = Value.maybeRational(base).contains(Rational.one) && Value.isZero(offset)

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
  def signum: Int = asField.signum

  /**
    * Adds a `Rational` value to the current solution and returns a new `Solution` as the result.
    *
    * @param addend the `Rational` value to be added to the current solution
    * @return a new `Solution` instance representing the sum of the current solution and the given `addend`
    */
  def add(addend: Rational): Solution = {
    val functions = DyadicOperationPlus.functions
    val maybeX: Option[Value] = doComposeValueDyadic(base, fromRational(addend))(functions)
    copy(base = maybeX.get) // CONSIDER handling this possible exception properly
  }
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
    * Determines if the offset is negative.
    * A negative value may not be expressible with just `Value` and `Factor`, for example, if `factor` is `SquareRoot`.
    *
    * @return true if the offset is negative, false otherwise
    */
  def negative: Boolean = false

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
    * Adds a `Rational` value to the current solution and returns a new `Solution` as the result.
    *
    * @param addend the `Rational` value to be added to the current solution
    * @return a new `Solution` instance representing the sum of the current solution and the given `addend`
    */
  def add(addend: Rational): Solution = {
    val po = PureNumber.add(value, Value.fromRational(addend), PureNumber)
    po match {
      case Some((v, PureNumber, None)) =>
        LinearSolution(v)
      case _ =>
        throw new Exception("LinearSolution.add: PureNumber.add failed")
    }
  }
}