/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.inner.{Factor, PureNumber, Rational}
import com.phasmidsoftware.number.misc.{FP, Factorial}
import scala.util.Try

/**
  * A representation of a mathematical power series which is an infinite sum of terms expressed as powers of a variable.
  * Implements IndexedSeq to provide indexed access to its terms and supports numerical and approximatable operations.
  */
trait PowerSeries extends IndexedSeq[Number] with Numerical with Approximatable {
  /**
    * Retrieves the term of the power series at the specified index.
    *
    * @param n the index of the term to retrieve from the power series
    * @return the term of the power series at the given index as a Number
    */
  def getTerm(n: Int): Number

  /**
    * Evaluates the power series at the specified term index.
    *
    * @param n the index of the term to evaluate in the power series
    * @return a Try containing the result of the evaluation as a Number, or an exception if the evaluation fails
    */
  def evaluate(n: Int): Try[Number]

  /**
    * Evaluates the power series at the specified value of x.
    *
    * @param x the value at which the power series is to be evaluated
    * @return a Try containing the evaluated result of the power series as a Number, or an exception if the evaluation fails
    */
  def evaluate(x: Double): Try[Number]

  /**
    * Evaluates this (stable) power series using sufficient terms such that the error bound of the result is less than x.
    *
    * @return if possible, returns a `Real` representing the approximation of this expression.
    */
  def approximation(x: Double): Option[Real] = FP.toOption(evaluate(x)) map (xf => Real(xf))

  /**
    * Computes and returns an approximate numerical value for this power series to with 1E-6 (absolute value).
    *
    * @return if possible, returns a `Real` representing the approximation of this power series.
    */
  def approximation: Option[Real] = approximation(1E-6)

  /**
    * Method to determine if this Numerical is equivalent to another Numerical object (x).
    *
    * @param x the other Numerical.
    * @return true if they are most probably the same, otherwise false.
    */
  def isSame(x: Numerical): Boolean = approximation.exists(_.isSame(x))

  /**
    * Method to determine if this Field has infinite magnitude.
    *
    * @return true if the magnitude of this Field is infinite.
    */
  def isInfinite: Boolean = throw new UnsupportedOperationException("isInfinite")

  /**
    * Method to determine if this Field has zero magnitude.
    * Zero is the additive identity.
    *
    * @return true if the magnitude of this Field is zero.
    */
  def isZero: Boolean = isSame(Constants.zero)

  /**
    * Method to determine if this Field has unity magnitude.
    * Unity is the multiplicative identity.
    *
    * @return true if the magnitude of this Field is one.
    */
  def isUnity: Boolean = isSame(Constants.one)

  /**
    * Determine the "sign" of this field.
    * For a real-valued quantity (Real or Number), we try to determine if it is to the right, left or at the origin.
    * For a complex number, we get the signum of the real part.
    *
    * @return +1 if to the right of the origin, -1 if to the left, 0 if at the origin.
    */
  def signum: Int = approximation.map(_.signum).getOrElse(0)

  /**
    * Computes the absolute value of this Numerical instance.
    * The result is a Numerical object representing a non-negative magnitude of this instance.
    *
    * @return a Numerical object representing the absolute value of this instance.
    */
  def abs: Numerical = throw new UnsupportedOperationException("abs")

  /**
    * Change the sign of this PowerSeries.
    */
  def unary_- : Field = throw new UnsupportedOperationException("unary_-")

  /**
    * Yields the inverse of this Field.
    * This Number is first normalized so that its factor is PureNumber, since we cannot directly invert Numbers with other
    * factors.
    */
  def invert: Field = throw new UnsupportedOperationException("invert")

  /**
    * Method to "normalize" a field.
    *
    * @return a Field which is in canonical form.
    */
  def normalize: Field = throw new UnsupportedOperationException("invert")

  /**
    * Method to return this Field as a Complex.
    * If this is a Real number x, return ComplexPolar(x) otherwise, return this.
    *
    * @return a Complex.
    */
  def asComplex: Complex = throw new UnsupportedOperationException("invert")

  /**
    * Method to return this Field as a Real, if possible.
    * If this is a Real number x, return Some(x) otherwise, return None.
    *
    * @return an Option[Real].
    */
  def asReal: Option[Real] = approximation

  /**
    * Method to determine what `Factor`, if there is such, this `NumberLike` object is based on.
    * Unlike context, a `None` result is not permissive.
    *
    * @return an optional `Factor`.
    */
  def maybeFactor: Option[Factor] = Some(PureNumber)

  /**
    * @return false.
    */
  def isExact: Boolean = false

  /**
    * @return None.
    */
  def asNumber: Option[Number] = None

  /**
    * Method to render this NumberLike in a presentable manner.
    *
    * @return a String
    */
  def render: String = approximation.map(_.render).getOrElse(s"Series $this cannot be rendered")
}

/**
  * KnownPowerSeries is an abstract class representing a mathematical power series where the terms are known
  * and explicitly defined up to a certain point. This class provides methods to access the terms,
  * retrieve the number of terms, and evaluate the series either at a specific term count or with an error threshold.
  * It extends the PowerSeries trait, inheriting its numerical and approximatable capabilities.
  */
abstract class KnownPowerSeries() extends PowerSeries {
  /**
    * Retrieves the list of terms in the power series as numbers.
    *
    * @return a List of numbers representing the terms of the series
    */
  def terms: IndexedSeq[Number]

  /**
    * Retrieves the number of terms in the power series.
    *
    * @return the number of terms in the series as an integer
    */
  def n: Int

  /**
    * Evaluates this (stable) power series using sufficient terms such that the error bound of the result is less than x.
    *
    * @param x the threshold of the error bounds.
    * @return a `Try[Real]` containing the evaluated value of the power series if successful,
    *         or a `Failure` if the evaluation encounters an error, for instance, the power series is unstable.
    */
  def evaluate(x: Double): Try[Number] = ???

  /**
    * Evaluates the sum of the first n terms of the series as a real number.
    *
    * @param n the number of terms to take from the series for evaluation
    * @return a Try containing the sum of the first n terms as a Real, or a failure if the computation fails
    */
  def evaluate(n: Int): Try[Number] = FP.whenTry(n <= this.n)(terms.take(n).sum)
}

/**
  * Abstract class representing a lazy power series, where the terms of the series are
  * defined as a lazy list for potentially infinite series. This allows for on-demand
  * computation of terms and efficient handling of series with a large or infinite number
  * of terms.
  *
  * Inherits from the `PowerSeries` trait, enabling indexed access to its terms and operations
  * based on numerical and approximatable functions.
  */
abstract class LazyPowerSeries() extends PowerSeries {

  /**
    * Represents the series terms as a lazy list of numbers.
    * This lazy representation allows for efficient operations on series with potentially infinite terms.
    *
    * @return a lazy list of numbers representing the terms of the series
    */
  def terms: LazyList[Number]

  /**
    * Evaluates the sum of the first n terms of the series as a real number.
    *
    * @param n the number of terms to take from the series for evaluation
    * @return a Try containing the sum of the first n terms as a Real, or a failure if the computation fails
    */
  def evaluate(n: Int): Try[Number] = Try((terms.take(n).sum))
}

/**
  * Represents an infinite Taylor series implemented lazily, inheriting from `LazyPowerSeries`.
  *
  * The `InfiniteTaylorSeries` class provides a foundation for defining mathematical Taylor series
  * with an infinite number of terms. Each term of the series is calculated lazily, allowing for
  * efficient computation and evaluation on-demand. This abstract class serves as a base for any
  * specific Taylor series and leverages the functionality of `LazyPowerSeries`.
  *
  * By extending `LazyPowerSeries`, it inherits the capability to represent terms as a lazy list
  * and evaluate a specified number of terms.
  */
abstract class InfiniteTaylorSeries() extends LazyPowerSeries {

}

/**
  * FiniteTaylorSeries is an abstract class representing a Taylor series expansion with a finite number of terms.
  * It extends KnownPowerSeries, inheriting its capabilities for working with known power series terms.
  * This class provides access to the terms of the series, the ability to retrieve specific terms,
  * and the total number of terms in the series.
  */
abstract class FiniteTaylorSeries() extends KnownPowerSeries {
  /**
    * Retrieves the list of terms in the power series as numbers.
    *
    * @return a List of numbers representing the terms of the series
    */
  def terms: IndexedSeq[Number] = for (i <- Range(1, n)) yield getTerm(i)

  /**
    * Retrieves the term at the specified index from the finite Taylor series.
    *
    * @param i the index of the term to retrieve; should be within the valid range of the series
    * @return the term at the specified index as a Number
    */
  def apply(i: Int): Number = terms(i)

  /**
    * Retrieves the total number of terms in the finite Taylor series.
    *
    * @return the total number of terms in the series as an integer
    */
  def length: Int = n
}

/**
  * Represents a polynomial-based Taylor series centered at a given point `x`.
  * This class computes the terms of the Taylor series expansion for a given polynomial.
  *
  * @constructor Creates a new PolynomialTaylorSeries with a specific point `x` and an underlying polynomial.
  * @param x          The point at which the Taylor series is centered.
  * @param polynomial The polynomial used to compute the Taylor series.
  */
case class PolynomialTaylorSeries(x: Number, polynomial: Polynomial[Number]) extends FiniteTaylorSeries {

  /**
    * Computes the length of the power series, which corresponds to the degree of the underlying polynomial plus one.
    *
    * @return the length of the power series as an integer
    */
  def n: Int = polynomial.degree + 1

  /**
    * Computes the ith term of the Taylor series expansion for the polynomial at the specified center `x`.
    *
    * @param i The index of the term to compute. Must be non-negative.
    * @return The ith term of the Taylor series as a `Number`.
    * @throws NumberException if the computed term is not a real number.
    */
  def getTerm(i: Int): Number = {
    val z: Number = polynomial.nthDerivative(i, x)
    val q: Number = x power i
    val p: BigInt = Factorial(i)
    (q multiply Real(z)) multiply (Rational(p)) match {
      case Real(x) => x
      case _ => throw new NumberException(s"PolynomialTaylorSeries: getTerm: $z")
    }
  }
}

