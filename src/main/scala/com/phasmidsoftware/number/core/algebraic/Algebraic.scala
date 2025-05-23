/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.algebraic

import com.phasmidsoftware.number.core.inner.{Factor, Rational, Value}
import com.phasmidsoftware.number.core.{Complex, Field, Number, Numerical, Real}

/**
  * The `Algebraic` class is an abstract extension of the Field trait, representing a solution of a mathematical equation,
  * typically requiring two separate Number values, each with a different `Factor`.
  * `Algebraic` supports a wide variety of operations including arithmetic, trigonometric, and analytical computations.
  * This class provides fundamental methods for handling fields, enabling intricate mathematical manipulations.
  *
  * Concrete implementations of this abstract class must define the specific behavior for the operations provided.
  */
trait Algebraic extends Field {

  /**
    * Retrieves the branch index associated with the current mathematical solution.
    * This index indicates a specific branch of the solution in cases where multiple branches exist.
    *
    * @return an integer representing the branch index.
    */
  def branch: Int

  /**
    * Represents the mathematical equation defined within this solution.
    *
    * @return an instance of `Equation` representing the specific equation associated with this solution.
    */
  def equation: Equation

  /**
    * Attempts to find a solution for a mathematical equation corresponding to the given branch.
    * Solutions are represented as an instance of the `Algebraic` or of `Complex` class.
    *
    * @return a `Field`, which is either a `Algebraic` (real-valued) or a `Complex`.
    */
  def solve: Solution = equation.solve(branch)

  /**
    * Returns the value of the solution represented as a `Field` for the specified branch index.
    *
    * @return a `Field` representing the value of the solution for the given branch.
    */
  def value: Field = solve.asField

  /**
    * An optional name for this solution.
    *
    * @return an `Option[String]` representing the name.
    *         Returns `None` if no name is available.
    */
  def maybeName: Option[String]

  /**
    * Method to determine if this Field is real-valued (i.e. the point lies on the real axis).
    *
    * @return true if not imaginary.
    */
  def isReal: Boolean = !isImaginary

  /**
    * Method to determine if this Field is imaginary-valued (i.e. the point lies on the imaginary axis).
    *
    * @return true if this is imaginary.
    */
  def isImaginary: Boolean = value.isImaginary

  /**
    * Add x to this Field and return the result.
    * See Number.plus for more detail.
    *
    * @param x the addend.
    * @return the sum.
    */
  def add(x: Field): Field = value add x

  /**
    * Multiply this Field by x and return the result.
    *
    * * @param x the multiplicand.
    * * @return the product.
    */
  def multiply(x: Field): Field

  /**
    * Computes the square of an algebraic expression.
    *
    * @return An instance of Algebraic representing the square of the input expression.
    */
  def square: Algebraic

  /**
    * Computes the absolute value of this Numerical instance.
    * The result is a Numerical object representing a non-negative magnitude of this instance.
    *
    * @return a Numerical object representing the absolute value of this instance.
    */
  def abs: Numerical = value.abs

  /**
    * Scales the solution by the given rational factor.
    *
    * CONSIDER shouldn't this be part of Equation?
    *
    * @param x the factor by which to scale the solution, represented as a `Rational`.
    * @return a new `Algebraic` instance that is scaled by the specified factor.
    */
  def scale(x: Rational): Algebraic

  /**
    * Adds the given Algebraic object to the current Algebraic.
    *
    * @param s the Algebraic object to be added
    * @return a new Algebraic resulting from the addition
    */
  def add(s: Algebraic): Algebraic

  /**
    * Adds the given Rational object to the current Algebraic.
    *
    * @param rational the Rational object to be added
    * @return a new Algebraic resulting from the addition
    */
  def add(rational: Rational): Algebraic

  /**
    * Negates the current `Algebraic` by scaling it with a factor of -1.
    *
    * @return a new `Algebraic` instance representing the negation of the current instance.
    */
  def negate: Algebraic =
    scale(Rational(-1))

  /**
    * Divide this Field by x and return the result.
    *
    * @param x the divisor.
    * @return the quotient.
    */
  def divide(x: Field): Field = value divide x

  /**
    * Raises this Field to the power of the specified number.
    *
    * @param p the exponent, provided as a Number.
    * @return the result of raising this Field to the power p.
    */
  def power(p: Number): Field = value power p

  /**
    * Raise this Field to the power p.
    *
    * @param p a Field.
    * @return this Field raised to power p.
    */
  def power(p: Field): Field = value power p

  /**
    * Computes the sine of this Field.
    *
    * @return the sine of this Field, as an instance of Field.
    */
  def sin: Field = value.sin

  /**
    * Computes the trigonometric cosine of this Field.
    *
    * @return the cosine of this Field.
    */
  def cos: Field = value.cos

  /**
    * Computes the tangent of this Field.
    *
    * @return the tangent of this Field as a new Field.
    */
  def tan: Field = value.tan

  /**
    * Calculates the arctangent (inverse tangent) of the given Real number.
    *
    * @param y the Real number whose arctangent is to be calculated.
    * @return the arctangent of the specified Real number, represented as a Field.
    */
  def atan(y: Real): Field = value.atan(y)

  /**
    * Computes the natural logarithm (log base e) of this Field.
    *
    * @return a new Field representing the result of the logarithmic computation.
    */
  def log: Field = value.log

  /**
    * Computes the exponential of this Field.
    *
    * @return a Field representing the exponential of this instance.
    */
  def exp: Field = value.exp

  /**
    * Method to determine if this Numerical is equivalent to another Numerical object (x).
    *
    * @param x the other Numerical.
    * @return true if they are most probably the same, otherwise false.
    */
  def isSame(x: Numerical): Boolean = value.isSame(x)

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
  def isZero: Boolean = solve.isZero

  /**
    * Method to determine if this Field has unity magnitude.
    * Unity is the multiplicative identity.
    *
    * @return true if the magnitude of this Field is one.
    */
  def isUnity: Boolean = solve.isUnity

  /**
    * Determine the "sign" of this field.
    * For a real-valued quantity (Real or Number), we try to determine if it is to the right, left or at the origin.
    * For a complex number, we get the signum of the real part.
    *
    * @return +1 if to the right of the origin, -1 if to the left, 0 if at the origin.
    */
  def signum: Int = solve.signum

  /**
    * Change the sign of this Field.
    */
  def unary_- : Field = -value

  /**
    * Yields the inverse of this Field.
    * This Number is first normalized so that its factor is PureNumber, since we cannot directly invert Numbers with other
    * factors.
    */
  def invert: Field = value.invert

  /**
    * Method to "normalize" a field.
    *
    * @return a Field which is in canonical form.
    */
  def normalize: Field = value.normalize

  /**
    * Method to return this Field as a Complex.
    * If this is a Real number x, return ComplexPolar(x) otherwise, return this.
    *
    * @return a Complex.
    */
  def asComplex: Complex = value.asComplex // CONSIDER getting this direct from the Solution.

  /**
    * Method to return this Field as a Real, if possible.
    * If this is a Real number x, return Some(x) otherwise, return None.
    *
    * @return an Option[Real].
    */
  def asReal: Option[Real] = value.asReal

  /**
    * Method to determine what `Factor`, if there is such, this `NumberLike` object is based on.
    * Unlike context, a `None` result is not permissive.
    *
    * @return an optional `Factor`.
    */
  def maybeFactor: Option[Factor] = value.maybeFactor

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
  def asNumber: Option[Number] = value.asNumber

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
  def compare(that: Field): Int = value.compare(that)
}

/**
  * Represents an algebraic transformation or operation based on a linear equation.
  * Extends the functionalities of the `Algebraic` class, enabling operations specific to linear equations.
  *
  * @param equation a `LinearEquation` object that defines the basis of this algebraic operation.
  */
case class Algebraic_Linear(equation: LinearEquation) extends Algebraic {
  /**
    * Retrieves the branch index associated with the current mathematical solution.
    * This index indicates a specific branch of the solution in cases where multiple branches exist.
    *
    * @return an integer representing the branch index.
    */
  def branch: Int = 0

  /**
    * An optional name for this solution.
    *
    * @return an `Option[String]` representing the name.
    *         Returns `None` if no name is available.
    */
  def maybeName: Option[String] = ???

  /**
    * Multiply this Field by x and return the result.
    *
    * * @param x the multiplicand.
    * * @return the product.
    */
  def multiply(x: Field): Field = ???

  /**
    * Computes the square of an algebraic expression.
    *
    * @return An instance of Algebraic representing the square of the input expression.
    */
  def square: Algebraic = ???

  /**
    * Scales the solution by the given rational factor.
    *
    * CONSIDER shouldn't this be part of Equation?
    *
    * @param x the factor by which to scale the solution, represented as a `Rational`.
    * @return a new `Algebraic` instance that is scaled by the specified factor.
    */
  def scale(x: Rational): Algebraic = ???

  /**
    * Adds the given Algebraic object to the current Algebraic.
    *
    * @param s the Algebraic object to be added
    * @return a new Algebraic resulting from the addition
    */
  def add(s: Algebraic): Algebraic = ???

  /**
    * Adds the given Rational object to the current Algebraic.
    *
    * @param rational the Rational object to be added
    * @return a new Algebraic resulting from the addition
    */
  def add(rational: Rational): Algebraic = ???

  /**
    * Method to render this NumberLike in a presentable manner.
    *
    * @return a String
    */
  def render: String = maybeName getOrElse toString
}

/**
  * Represents a linear equation of the form `P(x) = q` where `q` is a rational number.
  * This class provides various operations and transformations specific to linear equations.
  */
case class LinearEquation(q: Rational) extends Equation {
  /**
    * Method to compute the number of branches related to a specific computation or process.
    * For example, a solution to a quadratic equation has two branches, one for the real part and one for the imaginary part.
    *
    * @return the number of branches as an integer.
    */
  def branches: Int = 1

  /**
    * Attempts to find a solution for a mathematical equation corresponding to the given branch.
    * Solutions are represented as an instance of the `Algebraic` or of `Complex` class.
    *
    * @param branch the branch index for which the solution is being sought.
    *               The branch index identifies specific solutions for equations that may have multiple solutions.
    * @return a `Field`, which is either a `Algebraic` (real-valued) or a `Complex`.
    */
  def solve(branch: Int): Solution = LinearSolution(Value.fromRational(-q))

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
  def transform(fP: (Rational, Rational) => Rational, fQ: (Rational, Rational) => Rational): Equation = ???

  /**
    * Scales the current equation by multiplying it with the given rational factor.
    *
    * @param x the rational factor by which the equation is to be scaled.
    * @return a new `Equation` instance representing the scaled equation.
    */
  def scale(x: Rational): Equation = ???

  /**
    * Produces an inverted version of the current equation.
    * The inversion process involves switching or rearranging key components
    * of the equation, depending on its mathematical structure.
    *
    * @return a new `Equation` instance representing the inverted form of the current equation.
    */
  def invert: Equation = ???

  /**
    * Shifts the origin of the given equation by the specified rational value.
    * This method adjusts the equation's reference point, effectively translating
    * it to a new position in its domain.
    *
    * Given this equation `P(x) = 0`, such that `z` is a solution,
    * this method returns the equation `P(y) = 0` such that `z + c` is a solution.
    *
    * CONSIDER a better name for this.
    *
    * @param c the rational value by which to shift the origin of the equation.
    * @return a new `Equation` instance with its origin shifted by the specified value.
    */
  def shiftOrigin(c: Rational): LinearEquation = ???
}
