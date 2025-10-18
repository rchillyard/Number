/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.misc.FP

/**
  * Represents a polynomial function with a numeric type parameter X.
  * Supports evaluation, differentiation, and construction of new polynomials.
  *
  * TODO create an abstract Polynomial which will have the implicit Numeric[X] value
  */
trait Polynomial[X] extends (X => X) {

  /**
    * Retrieves the Numeric instance associated with the polynomial.
    *
    * @return the Numeric instance of type X, which provides numeric operations for the coefficients
    */
  def numeric: Numeric[X]

  /**
    * Returns the degree of the polynomial.
    *
    * @return the degree of the polynomial as an integer
    */
  def degree: Int

  /**
    * Retrieves the coefficients of the polynomial in ascending order of powers.
    *
    * @return a sequence of coefficients of type X, where the ith element corresponds to the coefficient of the `x∧i` term
    */
  def coefficients: Seq[X]

  /**
    * Evaluates the polynomial for a given input value.
    *
    * The method calculates the polynomial value by summing the contributions of each term.
    * Each term is computed as the coefficient multiplied by the corresponding power of the input value.
    *
    * @param x the input value of type X at which the polynomial is to be evaluated
    * @return the value of the polynomial at the given input value
    */
  def apply(x: X): X =
    coefficients.zipWithIndex.foldLeft(numeric.zero) {
      case (a, (z, i)) =>
        implicit val xn: Numeric[X] = this.numeric
        val q = FP.power(x, i)
        val y: X = numeric.times(q, z)
        numeric.plus(a, y)
    }

  /**
    * Constructs a polynomial with the given degree and coefficients.
    *
    * @param degree       the degree of the polynomial, an integer representing the highest power of x
    * @param coefficients a sequence of coefficients of type X, where the ith element corresponds to the coefficient of the `x∧i` term
    * @param xn           an implicit Numeric instance that provides numeric operations for the type X
    * @return a new Polynomial instance of type X constructed with the specified degree and coefficients
    */
  def unit(degree: Int, coefficients: Seq[X])(implicit xn: Numeric[X]): Polynomial[X]

  /**
    * Computes the derivative of the polynomial.
    *
    * @return a new polynomial representing the derivative of the current polynomial
    */
  def derivative: Polynomial[X] = {
    val xs = coefficients.zipWithIndex map { case (x, i) => numeric.times(x, numeric.fromInt(i)) }
    unit(this.degree - 1, xs.drop(1))(numeric)
  }

  /**
    * Computes the nth derivative of the polynomial.
    *
    * TODO fix me
    *
    * @param n the number of times to compute the derivative, where n must be a non-negative integer
    * @return a new polynomial representing the nth derivative of the current polynomial
    */
  def derivativeN(n: Int): Polynomial[X] = (1 to n).foldLeft(this)((p, _) => p.derivative)

  /**
    * Computes the nth derivative of the polynomial and evaluates it at the given input value.
    *
    * @param n the order of the derivative to compute, where n is a non-negative integer
    * @param x the input value of type X at which the nth derivative will be evaluated
    * @return the result of evaluating the nth derivative of the polynomial at the given input value
    */
  def nthDerivative(n: Int, x: X): X = derivativeN(n)(x)

  /**
    * Generates a string representation of the polynomial in the format:
    * "Polynomial(degree, coefficient1, coefficient2, ..., coefficientN)".
    *
    * @return a string that represents the polynomial, including its degree and coefficients
    */
  def render: String = {
    val sb = new StringBuilder
    sb.append(s"Polynomial($degree: ${coefficients.mkString(", ")})")
    sb.toString()
  }
}

/**
  * A case class representing a polynomial of numbers, extending the generic Polynomial trait.
  * This polynomial includes a specific degree and a sequence of coefficients.
  *
  * @constructor Creates a NumberPolynomial instance with the specified degree, coefficients, and an implicit Numeric instance.
  * @param degree       the degree of the polynomial, which is the highest power of x.
  * @param coefficients a sequence of coefficients where the ith element corresponds to the coefficient of the `x∧i` term.
  * @param ev           an implicit Numeric instance providing numeric operations for the coefficient type.
  */
case class NumberPolynomial(degree: Int, coefficients: Seq[Number])(implicit ev: Numeric[Number]) extends Polynomial[Number] {
  /**
    * Constructs a polynomial with the given degree and coefficients.
    *
    * @param degree       the degree of the polynomial, an integer representing the highest power of x
    * @param coefficients a sequence of coefficients of type X, where the ith element corresponds to the coefficient of the `x∧i` term
    * @param xn           an implicit Numeric instance that provides numeric operations for the type X
    * @return a new Polynomial instance of type X constructed with the specified degree and coefficients
    */
  def unit(degree: Int, coefficients: Seq[Number])(implicit xn: Numeric[Number]): Polynomial[Number] =
    copy(degree = degree, coefficients = coefficients)(xn)

  /**
    * Retrieves the implicit Numeric instance associated with the polynomial.
    *
    * @return the Numeric instance of type X, which provides numeric operations for the coefficients
    */
  def numeric: Numeric[Number] = ev

  override def toString(): String = render
}

object NumberPolynomial {
  def apply(coefficients: Number*)(implicit ev: Numeric[Number]): NumberPolynomial =
    new NumberPolynomial(coefficients.size - 1, coefficients)(ev)
}

/**
  * Represents a rational polynomial, defined by a degree and a sequence of rational coefficients.
  *
  * @constructor Creates a new rational polynomial with a specified degree and coefficients.
  * @param degree       The degree of the polynomial, representing the highest power of x.
  * @param coefficients A sequence of coefficients of type Rational, where the ith element corresponds to the coefficient of the `x∧i` term.
  * @param ev           An implicit Numeric instance for Rational, providing numeric operations for the coefficients.
  */
case class RationalPolynomial(degree: Int, coefficients: Seq[Rational])(implicit ev: Numeric[Rational]) extends Polynomial[Rational] {
  /**
    * Constructs a polynomial with the given degree and coefficients.
    *
    * @param degree       the degree of the polynomial, an integer representing the highest power of x
    * @param coefficients a sequence of coefficients of type X, where the ith element corresponds to the coefficient of the `x∧i` term
    * @param xn           an implicit Numeric instance that provides numeric operations for the type X
    * @return a new Polynomial instance of type X constructed with the specified degree and coefficients
    */
  def unit(degree: Int, coefficients: Seq[Rational])(implicit xn: Numeric[Rational]): Polynomial[Rational] =
    copy(degree = degree, coefficients = coefficients)(xn)

  /**
    * Retrieves the implicit Numeric instance associated with the polynomial.
    *
    * @return the Numeric instance of type X, which provides numeric operations for the coefficients
    */
  def numeric: Numeric[Rational] = ev

  override def toString(): String = render
}

object RationalPolynomial {
  def apply(coefficients: Rational*)(implicit ev: Numeric[Rational]): RationalPolynomial =
    new RationalPolynomial(coefficients.size - 1, coefficients)(ev)
}