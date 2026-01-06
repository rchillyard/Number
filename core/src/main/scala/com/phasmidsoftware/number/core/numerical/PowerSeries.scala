/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.numerical

import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.misc.Factorial

/**
  * A representation of a mathematical power series which is the sum of a finite or infinite sequence terms expressed as powers of a variable.
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
  * TESTME (not currently used)
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

case class SeriesFunction(f: Number => Number, name: String) extends (Number => Number) {
  def apply(x: Number): Number = f(x)

  override def toString: String = name
}

/**
  * Represents a mathematical Taylor series, a specific type of power series that approximates
  * smooth functions near a specified point. The series is defined by its expansion point,
  * its starting function, derivative rules for generating further terms, and a convergence
  * tolerance.
  *
  * @constructor Creates a Taylor series with the given parameters.
  * @param point         The point at which the Taylor series is centered.
  * @param startFunction The initial function defining the Taylor series.
  * @param derivative    A function that computes the derivative of the series function.
  * @param convergence   A value specifying the convergence tolerance of the series.
  */
case class TaylorSeries(point: Number, startFunction: SeriesFunction, derivative: SeriesFunction => SeriesFunction, convergence: Double) extends PowerSeries[Number, Number] {

  lazy val functions: LazyList[SeriesFunction] =
    LazyList.iterate(startFunction) { f => derivative(f) }
  lazy val coefficients: LazyList[Number] = functions.map(f => f(point))
  // CONSIDER we could use scanLeft and just multiply by the index (wouldn't need factorial, then)
  lazy val terms: LazyList[Number] = coefficients.zipWithIndex.map {
    case (c, i) =>
      c `doMultiple` Rational(Factorial(i)).invert
  }

  /**
    * Evaluates the infinite Taylor series for the given input `x` by generating a new series
    * representation with a specified tolerance for convergence.
    *
    * @param x the input value of type `Number` for which the series is evaluated
    * @return a new series of type `Series[Number]` representing the evaluated infinite Taylor series
    */
  def apply(x: Number): Series[Number] = {
    val ys = terms zip LazyList.iterate(Number.one)(y => y.doMultiply(x))
    val xs = ys.map { case (a, b) => a.doMultiply(b) }
    InfiniteSeries(xs, convergence)
  }
}

/**
  * Companion object for the `TaylorSeries` class. Provides utility methods for creating instances
  * of `TaylorSeries` for commonly known functions.
  */
object TaylorSeries {
  /**
    * Creates a Taylor series representation of the sine function at a given point.
    *
    * @param point the x-coordinate at which the Taylor series is centered
    * @return an instance of `TaylorSeries` representing the sine function
    */
  def createSine(point: Number): TaylorSeries = {
    val sine: SeriesFunction =
      SeriesFunction(x => x.sin, "sine")
    val cosine: SeriesFunction =
      SeriesFunction(x => x.cos, "cosine")
    val negSine: SeriesFunction =
      SeriesFunction(x => x.sin.makeNegative, "negSine")
    val negCosine: SeriesFunction =
      SeriesFunction(x => x.cos.makeNegative, "negCosine")

    def getDerivative(function: SeriesFunction): SeriesFunction = function match {
      case `sine` => cosine
      case `cosine` => negSine
      case `negSine` => negCosine
      case `negCosine` => sine
      case _ => throw CoreException(s"getDerivative: unknown function $function") // Impossible (?)
    }

    // XXX this is an appropriate value of convergence fo the sine Taylor series.
    val convergence = 0.002
    TaylorSeries(point, sine, f => getDerivative(f), convergence)
  }
}
