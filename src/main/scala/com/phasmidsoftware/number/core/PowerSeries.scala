/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.inner.{FiniteSeries, InfiniteSeries, Rational, Series}
import com.phasmidsoftware.number.misc.Factorial

/**
  * A representation of a mathematical power series which is an infinite sum of terms expressed as powers of a variable.
  */
trait PowerSeries[X, Y] extends (X => Series[Y]) {

  /**
    * Applies the given input `x` to the power series, resulting in a mathematical series
    * of terms computed based on the input.
    *
    * @param x the input value of type `X` used to compute the corresponding series
    * @return a `Series[Y]` representing the resulting series after applying the input
    */
  def apply(x: X): Series[Y]
}

/**
  * Represents a lazy power series that is computed on demand, using a sequence of pre-specified
  * coefficients and a transformation function for powers of the input variable.
  *
  * This class allows defining power series where the terms are calculated as required, combining
  * both the provided coefficients and a transformation of the powers of the input.
  *
  * @tparam X the type of the input variable to the power series
  * @tparam Y the type of the coefficients and resulting series terms
  * @param coefficients a lazy, infinite list representing the coefficients of the power series
  * @param f            a function used to transform the powers of the input variable
  */
abstract class LazyPowerSeries[X: Numeric, Y: Numeric](coefficients: LazyList[Y])(f: X => Y) extends PowerSeries[X, Y] {

  /**
    * Computes the resulting power series by applying the transformation function to the powers of the input
    * and combining these with the series coefficients.
    *
    * The method generates an infinite series based on the input `x`, where each term of the series is
    * derived by multiplying the transformed power of `x` with the corresponding coefficient.
    *
    * @param x the input variable for which the power series is computed
    * @return an instance of `Series[Y]` representing the computed power series
    */
  def apply(x: X): Series[Y] = {
    val powers: LazyList[X] = LazyList.iterate(xn.one)(z => xn.times(z, x))
    val zipped: LazyList[(X, Y)] = powers zip coefficients
    val terms: LazyList[Y] = zipped map { case (p, c) => yn.times(c, f(p)) }
    InfiniteSeries(terms, 0.01)
  }

  val xn: Numeric[X] = implicitly[Numeric[X]]
  val yn: Numeric[Y] = implicitly[Numeric[Y]]
}

/**
  * Represents a finite power series, a mathematical series with a finite number of terms,
  * where each term is a power of a variable multiplied by a coefficient.
  *
  * @tparam X the type of the variable used in the series
  * @tparam Y the type of the coefficients and the resulting terms of the series
  * @param coefficients the coefficients of the power series, corresponding to each term
  * @param f            a function applied to the powers of the variable, which modifies each term in the series
  */
case class FinitePowerSeries[X: Numeric, Y: Numeric](coefficients: Seq[Y])(f: X => Y) extends PowerSeries[X, Y] {

  /**
    * Computes a finite power series by combining the coefficients with the corresponding
    * powers of the input value and applies a transformation function to each term.
    *
    * @param x the input value used to compute the powers in the series
    * @return a `Series[Y]` representing the resulting finite power series
    */
  def apply(x: X): Series[Y] = {
    val powers: LazyList[X] = LazyList.iterate(xn.one)(z => xn.times(z, x))
    val zipped: Seq[(X, Y)] = (powers zip coefficients).toList
    val terms: Seq[Y] = zipped map { case (p, c) => yn.times(c, f(p)) }
    FiniteSeries(terms)
  }

  val xn: Numeric[X] = implicitly[Numeric[X]]
  val yn: Numeric[Y] = implicitly[Numeric[Y]]
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
  * and evaluateToTolerance a specified number of terms.
  */
case class TaylorSeries(point: Number, function: SeriesFunction, derivative: SeriesFunction => SeriesFunction) extends PowerSeries[Number, Number] {

  lazy val functions: LazyList[SeriesFunction] = LazyList.iterate(function)(f => derivative(f))
  lazy val coefficients: LazyList[Number] = functions.map(f => f(point))
  // CONSIDER we could use scanLeft and just multiply by the index (wouldn't need factorial, then)
  lazy val terms: LazyList[Number] = coefficients.zipWithIndex.map { case (c, i) => c doMultiple Rational(Factorial(i)).invert }

  /**
    * Evaluates the infinite Taylor series for the given input `x` by generating a new series
    * representation with a specified tolerance for convergence.
    *
    * @param x the input value of type `Number` for which the series is evaluated
    * @return a new series of type `Series[Number]` representing the evaluated infinite Taylor series
    */
  def apply(x: Number): Series[Number] =
    InfiniteSeries(terms, 0.01)
}

/**
  * Companion object for the `TaylorSeries` class. Provides utility methods for creating instances
  * of `TaylorSeries` for commonly known functions.
  */
object TaylorSeries {
  /**
    * Creates a Taylor series representation for the sine function at a given point.
    *
    * @param point The point at which the Taylor series is centered.
    * @return A TaylorSeries instance representing the sine function.
    */
  def createSine(point: Number): TaylorSeries = {
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
