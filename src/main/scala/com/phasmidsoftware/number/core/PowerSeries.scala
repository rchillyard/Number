/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.inner.{FiniteSeries, InfiniteSeries, Rational, Series}
import com.phasmidsoftware.number.misc.Factorial


/**
  * A representation of a mathematical power series which is an infinite sum of terms expressed as powers of a variable.
  * Implements IndexedSeq to provide indexed access to its terms and supports numerical and approximatable operations.
  *
  * CONSIDER IndexedSeq is not necessary here (and is inappropriate for lazy lists)
  */
trait PowerSeries[X, Y] extends (X => Series[Y]) {

  def apply(x: X): Series[Y]

}

abstract class LazyPowerSeries[X: Numeric, Y: Numeric](coefficients: LazyList[Y])(f: X => Y) extends PowerSeries[X, Y] {

  val xn: Numeric[X] = implicitly[Numeric[X]]
  val yn: Numeric[Y] = implicitly[Numeric[Y]]

  def apply(x: X): Series[Y] = {
    val powers: LazyList[X] = LazyList.iterate(xn.one)(z => xn.times(z, x))
    val zipped: LazyList[(X, Y)] = powers zip coefficients
    val terms: LazyList[Y] = zipped map { case (p, c) => yn.times(c, f(p)) }
    InfiniteSeries(terms, 0.01)
  }
}

abstract class FinitePowerSeries[X: Numeric, Y: Numeric](coefficients: Seq[Y])(f: X => Y) extends PowerSeries[X, Y] {

  val xn: Numeric[X] = implicitly[Numeric[X]]
  val yn: Numeric[Y] = implicitly[Numeric[Y]]

  def apply(x: X): Series[Y] = {
    val powers: LazyList[X] = LazyList.iterate(xn.one)(z => xn.times(z, x))
    val zipped: Seq[(X, Y)] = (powers zip coefficients).toList
    val terms: Seq[Y] = zipped map { case (p, c) => yn.times(c, f(p)) }
    FiniteSeries(terms)
  }
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
case class TaylorSeries(point: Number, function: SeriesFunction, derivative: SeriesFunction => SeriesFunction) extends PowerSeries[Number, Number] {

  lazy val functions: LazyList[SeriesFunction] = LazyList.iterate(function)(f => derivative(f))
  lazy val coefficients: LazyList[Number] = functions.map(f => f(point))
  // CONSIDER we could use scanLeft and just multiply by the index (wouldn't need factorial, then)
  lazy val terms: LazyList[Number] = coefficients.zipWithIndex.map { case (c, i) => c doMultiple Rational(Factorial(i)).invert }

  def render: String = ""
//    s"TaylorSeries($point, function, derivative) with terms: ${terms.head}"

  def apply(x: Number): Series[Number] =
    InfiniteSeries(terms, 0.01)

  def length: Int = throw new UnsupportedOperationException("length")
}

object TaylorSeries {
  def createTaylorSeriesForSine(point: Number): TaylorSeries = {
    val sine: SeriesFunction = x => x.sin
    val cosine: SeriesFunction = x => x.cos
    val negSine: SeriesFunction = x => x.sin.makeNegative
    val negCosine: SeriesFunction = x => x.cos.makeNegative

    def getDerivative(function: SeriesFunction): SeriesFunction = function match {
      case sine => cosine
      case cosine => negSine
      case negSine => negCosine
      case negCosine => sine
    }

    TaylorSeries(point, sine, f => getDerivative(f))
  }
}

///**
//  * FiniteTaylorSeries is an abstract class representing a Taylor series expansion with a finite number of terms.
//  * It extends KnownPowerSeries, inheriting its capabilities for working with known power series terms.
//  * This class provides access to the terms of the series, the ability to retrieve specific terms,
//  * and the total number of terms in the series.
//  */
//abstract class FiniteTaylorSeries[X: Numeric]() extends FinitePowerSeries[X, X](coefficients)(identity) {
//  /**
//    * Retrieves the list of terms in the power series as numbers.
//    *
//    * @return a List of numbers representing the terms of the series
//    */
//  def terms: IndexedSeq[Number] = for (i <- Range(0, n)) yield getTerm(i)
//
//  /**
//    * Retrieves the term at the specified index from the finite Taylor series.
//    *
//    * @param i the index of the term to retrieve; should be within the valid range of the series
//    * @return the term at the specified index as a Number
//    */
//  def apply(i: Int): Number = terms(i)
//
//  /**
//    * Retrieves the total number of terms in the finite Taylor series.
//    *
//    * @return the total number of terms in the series as an integer
//    */
//  def length: Int = n
//}
//
///**
//  * Represents a polynomial-based Taylor series centered at a given point `x`.
//  * This class computes the terms of the Taylor series expansion for a given polynomial.
//  *
//  * NOTE: the Taylor series IS the polynomial so we can simplify getTerm.
//  *
//  * @constructor Creates a new PolynomialTaylorSeries with a specific point `x` and an underlying polynomial.
//  * @param x          The point at which the Taylor series is centered.
//  * @param polynomial The polynomial used to compute the Taylor series.
//  */
//case class PolynomialTaylorSeries(x: Number, polynomial: Polynomial[Number]) extends FiniteTaylorSeries {
//
//  /**
//    * Computes the length of the power series, which corresponds to the degree of the underlying polynomial plus one.
//    *
//    * @return the length of the power series as an integer
//    */
//  def n: Int = polynomial.degree + 1
//
//  /**
//    * Computes the ith term of the Taylor series expansion for the polynomial at the specified center `x`.
//    *
//    * @param i The index of the term to compute. Must be non-negative.
//    * @return The ith term of the Taylor series as a `Number`.
//    * @throws NumberException if the computed term is not a real number.
//    */
//  def getTerm(i: Int): Number = {
//    val z: Number = polynomial.nthDerivative(i, x)
//    val p: BigInt = Factorial(i)
//    z doMultiple Rational(1, p)
//  }
//
//  override def toString(): String = {
//    s"""x: $x; terms=${terms.mkString("(", ",", ")")}"""
//  }
//}

