/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.algebraic

import com.phasmidsoftware.number.algebra.core.{Branched, Scalable}
import com.phasmidsoftware.number.algebra.eager.{Eager, Solution}
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.expression.expr.Expression

/**
  * The `Equation` trait represents a mathematical equation that may have multiple solutions or branches.
  * It provides methods for determining the number of branches and for solving the equation on a specified branch.
  * This type of equation is typically of the form `P(x) = 0` where `P(x)` is a monic polynomial in `x` and where the
  * coefficients `Pi` (i.e., the `i`th coefficient of `P`) are `Rational` numbers.
  */
trait Equation extends Branched[Expression] with Scalable[Equation] {

  /**
    * Attempts to find a solution for a mathematical equation corresponding to the given branch.
    * Solutions are represented as an instance of the `Algebraic` or of `Complex` class.
    *
    * @param branch the branch index for which the solution is being sought.
    *               The branch index identifies specific solutions for equations that may have multiple solutions.
    *
    * @return a `Field`, which is either a `Algebraic` (real-valued) or a `Complex`.
    */
  def solve(branch: Int): Solution

  /**
    * Evaluates this equation where the variable (`x`) is provided by `s`, the `Solution`.
    *
    * If `s` is a solution of this equation, then the expected value is zero.
    *
    * @param s the solution to be evaluated; encapsulates the details of the computation
    *          and the associated branch-specific information.
    * @return an eagerly computed result derived from the evaluation of the provided solution.
    */
  def evaluate(s: Solution): Eager

  /**
    * Transforms the current equation by applying the provided functions to its components.
    *
    * @param fP a function that takes two `Rational` parameters and produces a `Rational` result.
    *           It is applied to the first component of the equation.
    *
    * @param fQ a function that takes two `Rational` parameters and produces a `Rational` result.
    *           It is applied to the second component of the equation.
    *
    * @return a new `Equation` instance that is the result of applying the specified transformations
    *         to the components of the current equation.
    */
  def transform(fP: (Rational, Rational) => Rational, fQ: (Rational, Rational) => Rational): Equation

  /**
    * Produces an inverted version of the current equation.
    * The inversion process involves switching or rearranging key components
    * of the equation, depending on its mathematical structure.
    *
    * @return a new `Equation` instance representing the inverted form of the current equation.
    */
  def invert: Equation

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
  def shiftOrigin(c: Rational): Equation
}
