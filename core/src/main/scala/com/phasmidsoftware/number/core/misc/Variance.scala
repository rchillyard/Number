/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.misc

import scala.language.postfixOps

/**
  * Object defining a set of mathematical operations related to variance and statistical computations.
  */
object Variance extends App {
  /**
    * Computes the sum of the squares of the elements in the given sequence.
    *
    * @param xs a sequence of Double values
    * @return the sum of the squares of the elements in the sequence
    */
  def sumOfSquares(xs: Seq[Double]): Double =
    xs map (x => x * x) sum

  /**
    * Computes the sum of the squares of two Double values.
    *
    * @param x the first Double value
    * @param y the second Double value
    * @return the sum of the squares of the two provided Double values
    */
  def sumOfSquares(x: Double, y: Double): Double =
    sumOfSquares(Seq(x, y))

  /**
    * Computes the square root of the sum of squares of the elements in the given sequence.
    *
    * @param xs a sequence of Double values
    * @return the square root of the sum of the squares of the elements in the sequence
    */
  def rootSumSquares(xs: Seq[Double]): Double =
    math.sqrt(sumOfSquares(xs))

  /**
    * Computes the square root of the sum of the squares of two Double values.
    *
    * @param x the first Double value
    * @param y the second Double value
    * @return the square root of the sum of the squares of the two input values
    */
  def rootSumSquares(x: Double, y: Double): Double =
    rootSumSquares(Seq(x, y))

  /**
    * Calculates the root-mean-square of a sequence of doubles.
    *
    * @param xs a sequence of Double values for which the root-mean-square is to be computed
    * @return the root-mean-square value of the given sequence
    */
  def rootMeanSquare(xs: Seq[Double]): Double =
    math.sqrt(sumOfSquares(xs) / xs.size)

  /**
    * Computes the root-mean-square of two given Double values.
    *
    * @param x the first value
    * @param y the second value
    * @return the root-mean-square of the two input values
    */
  def rootMeanSquare(x: Double, y: Double): Double =
    rootMeanSquare(Seq(x, y))

  /**
    * Computes a transformation on two Double values by calculating the square root of
    * the sum of the square of the first value and twice the product of the two input values.
    *
    * This is an approximation of the full formula which is:
    * `σ²(XY)/(XY)² = σ²(X)/X² + σ²(Y)/Y² + σ²(X)σ²(Y)/(X²Y²)`
    *
    * @param x the first Double value
    * @param y the second Double value
    * @return a Double representing the computed result of the transformation
    */
  def convolution(x: Double, y: Double): Double =
    math.sqrt(x * x + y * y + x * y)
}
