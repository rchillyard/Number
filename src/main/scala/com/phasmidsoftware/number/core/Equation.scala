/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.inner.{Factor, Rational}

/**
  * The `Equation` trait represents a mathematical equation that may have multiple solutions or branches.
  * It provides methods for determining the number of branches and for solving the equation on a specified branch.
  */
trait Equation {

  /**
    * Method to compute the number of branches related to a specific computation or process.
    * For example, a solution to a quadratic equation has two branches, one for the real part and one for the imaginary part.
    *
    * @return the number of branches as an integer.
    */
  def branches: Int

  /**
    * Attempts to find a solution for a mathematical equation corresponding to the given branch.
    * Solutions are represented as an instance of the `Solution` class.
    * If appropriate, a `Solution` can be converted into Complex form.
    *
    * @param branch the branch index for which the solution is being sought.
    *               The branch index identifies specific solutions for equations that may have multiple solutions.
    * @return an `Option[Solution]`, where `Some(solution)` contains the solution for the specified branch,
    *         or `None` if no solution exists for the given branch.
    */
  def solve(branch: Int): Option[Solution]

  /**
    * Transforms the current equation by applying the provided functions to its components.
    *
    * @param fP a function that takes two `Rational` parameters and produces a `Rational` result.
    *           It is applied to the first component of the equation.
    * @param fQ a function that takes two `Rational` parameters and produces a `Rational` result.
    *           It is applied to the second component of the equation.
    * @return a new `Equation` instance that is the result of applying the specified transformations
    *         to the components of the current equation.
    */
  def transform(fP: (Rational, Rational) => Rational, fQ: (Rational, Rational) => Rational): Equation

  /**
    * Scales the current equation by multiplying it with the given rational factor.
    *
    * @param x the rational factor by which the equation is to be scaled.
    * @return a new `Equation` instance representing the scaled equation.
    */
  def scale(x: Rational): Equation

  /**
    * Produces an inverted version of the current equation.
    * The inversion process involves switching or rearranging key components
    * of the equation, depending on its mathematical structure.
    *
    * @return a new `Equation` instance representing the inverted form of the current equation.
    */
  def invert: Equation
}

/**
  * The `Solution` class is an abstract extension of the Field trait, representing a solution of a mathematical equation,
  * typically requiring two separate Number values, each with a different `Factor`..
  * In this sense it is very much parallel to ComplexPolar (thish has two number, each with a different `Factor`).
  * `Solution` supports a wide variety of operations including arithmetic, trigonometric, and analytical computations.
  * This class provides fundamental methods for handling fields, enabling intricate mathematical manipulations.
  *
  * Concrete implementations of this abstract class must define the specific behavior for the operations provided.
  */
abstract class Solution extends Field {

  /**
    * Retrieves the branch index associated with this solution.
    *
    * @return an `Int` representing the branch index.
    */
  def branch: Int

  /**
    * Represents the mathematical equation defined within this solution.
    *
    * @return an instance of `Equation` representing the specific equation associated with this solution.
    */
  def equation: Equation

  /**
    * An optional name for this solution.
    *
    * @return an `Option[String]` representing the name.
    *         Returns `None` if no name is available.
    */
  def maybeName(branch: Int): Option[String]

  /**
    * This method computes two values:  based on the provided branch index.
    *
    * @return a tuple where the first element is a `Field` representing the base value,
    *         the second element is a `Rational` relating to the branch number,
    *         and the third element is a `Field`, typically with a different `Factor` than the first element.
    */
  def value: (Field, Rational, Field)

  /**
    * Method to determine if this Field is real-valued (i.e. the point lies on the real axis).
    *
    * @return true if not imaginary.
    */
  def isReal: Boolean

  /**
    * Method to determine if this Field is imaginary-valued (i.e. the point lies on the imaginary axis).
    *
    * @return true if this is imaginary.
    */
  def isImaginary: Boolean

  /**
    * Add x to this Field and return the result.
    * See Number.plus for more detail.
    *
    * @param x the addend.
    * @return the sum.
    */
  def add(x: Field): Field

  /**
    * Multiply this Field by x and return the result.
    *
    * * @param x the multiplicand.
    * * @return the product.
    */
  def multiply(x: Field): Field

  /**
    * Multiplies this `Solution` by the given `Rational` number.
    *
    * @param x the multiplicand, represented as a `Rational`.
    * @return a new `Solution` instance resulting from the multiplication.
    */
  def multiply(x: Rational): Solution

  /**
    * Scales the solution by the given rational factor.
    *
    * CONSIDER shouldn't this be part of Equation?
    *
    * @param x the factor by which to scale the solution, represented as a `Rational`.
    * @return a new `Solution` instance that is scaled by the specified factor.
    */
  def scale(x: Rational): Solution

  /**
    * Adds the given rational number to this solution, producing a new solution.
    *
    * @param c the rational number to be added, represented as an instance of `Rational`.
    * @return a new `Solution` instance representing the result of the addition.
    */
  def add(c: Rational): Solution


  /**
    * Computes the square of the current `Solution` by transforming its associated equation and toggling its position.
    *
    * @return a new `Solution` instance with the squared transformation applied.
    */
  def square: Solution

  /**
    * Negates the current `Solution` by scaling it with a factor of -1.
    *
    * @return a new `Solution` instance representing the negation of the current instance.
    */
  def negate: Solution

  /**
    * Divide this Field by x and return the result.
    *
    * @param x the divisor.
    * @return the quotient.
    */
  def divide(x: Field): Field = ???

  /**
    * Raises this Field to the power of the specified number.
    *
    * @param p the exponent, provided as a Number.
    * @return the result of raising this Field to the power p.
    */
  def power(p: Number): Field

  /**
    * Raise this Field to the power p.
    *
    * @param p a Field.
    * @return this Field raised to power p.
    */
  def power(p: Field): Field = ???

  /**
    * Computes the sine of this Field.
    *
    * @return the sine of this Field, as an instance of Field.
    */
  def sin: Field = ???

  /**
    * Computes the trigonometric cosine of this Field.
    *
    * @return the cosine of this Field.
    */
  def cos: Field = ???

  /**
    * Computes the tangent of this Field.
    *
    * @return the tangent of this Field as a new Field.
    */
  def tan: Field = ???

  /**
    * Calculates the arctangent (inverse tangent) of the given Real number.
    *
    * @param y the Real number whose arctangent is to be calculated.
    * @return the arctangent of the specified Real number, represented as a Field.
    */
  def atan(y: Real): Field = ???

  /**
    * Computes the natural logarithm (log base e) of this Field.
    *
    * @return a new Field representing the result of the logarithmic computation.
    */
  def log: Field = ???

  /**
    * Computes the exponential of this Field.
    *
    * @return a Field representing the exponential of this instance.
    */
  def exp: Field = ???

  /**
    * Method to determine if this Numerical is equivalent to another Numerical object (x).
    *
    * @param x the other Numerical.
    * @return true if they are most probably the same, otherwise false.
    */
  def isSame(x: Numerical): Boolean

  /**
    * Method to determine if this Field has infinite magnitude.
    *
    * @return true if the magnitude of this Field is infinite.
    */
  def isInfinite: Boolean = false

  /**
    * Method to determine if this Field has zero magnitude.
    * Zero is the additive identity.
    *
    * @return true if the magnitude of this Field is zero.
    */
  def isZero: Boolean = ???

  /**
    * Method to determine if this Field has unity magnitude.
    * Unity is the multiplicative identity.
    *
    * @return true if the magnitude of this Field is one.
    */
  def isUnity: Boolean = ???

  /**
    * Determine the "sign" of this field.
    * For a real-valued quantity (Real or Number), we try to determine if it is to the right, left or at the origin.
    * For a complex number, we get the signum of the real part.
    *
    * @return +1 if to the right of the origin, -1 if to the left, 0 if at the origin.
    */
  def signum: Int

  /**
    * Change the sign of this Field.
    */
  def unary_- : Field = ???

  /**
    * Yields the inverse of this Field.
    * This Number is first normalized so that its factor is PureNumber, since we cannot directly invert Numbers with other
    * factors.
    */
  def invert: Field

  /**
    * Method to "normalize" a field.
    *
    * @return a Field which is in canonical form.
    */
  def normalize: Field

  /**
    * Method to return this Field as a Complex.
    * If this is a Real number x, return ComplexPolar(x) otherwise, return this.
    *
    * @return a Complex.
    */
  def asComplex: Complex = ???

  /**
    * Method to return this Field as a Real, if possible.
    * If this is a Real number x, return Some(x) otherwise, return None.
    *
    * @return an Option[Real].
    */
  def asReal: Option[Real] = ???

  /**
    * Method to determine what `Factor`, if there is such, this `NumberLike` object is based on.
    * Unlike context, a `None` result is not permissive.
    *
    * @return an optional `Factor`.
    */
  def maybeFactor: Option[Factor] = ???

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
  def asNumber: Option[Number]

  /**
    * Method to render this NumberLike in a presentable manner.
    *
    * @return a String
    */
  def render: String

  /**
    * Compares this `Field` with the specified `Field` for order.
    *
    * @param that the `Field` to be compared.
    * @return a negative integer, zero, or a positive integer as this `Field` is less than, equal to, or greater than the specified `Field`.
    */
  def compare(that: Field): Int = ???
}
