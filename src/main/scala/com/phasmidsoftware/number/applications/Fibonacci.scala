package com.phasmidsoftware.number.applications

import com.phasmidsoftware.number.core._
import com.phasmidsoftware.number.core.inner.PureNumber
import com.phasmidsoftware.number.core.inner.Value.maybeRational
import com.phasmidsoftware.number.expression.Expression.ExpressionOps
import com.phasmidsoftware.number.expression.{Expression, Root}

/**
  * Provides utilities and methods for working with the Fibonacci sequence, leveraging
  * algebraic expressions and the mathematical properties of the golden ratio (φ) and its conjugate (ψ).
  */
object Fibonacci {

  /**
    * Represents the golden ratio, denoted as φ (phi), as an instance of `Expression`.
    * The golden ratio is a mathematical constant that satisfies the quadratic equation
    * `x^2 = x + 1` and is associated with numerous applications in mathematics,
    * art, and nature.
    *
    * This value can be used in computations involving algebraic expressions,
    * such as in the calculation of Fibonacci numbers using Binet's formula.
    */
  val phi: Expression = Root.phi
  /**
    * Psi represents the conjugate of `Phi`, used in computations involving
    * properties of the Fibonacci sequence and Binet's formula.
    * It is a reduced quadratic root and part of the algebraic expressions
    * in the Fibonacci number calculations.
    */
  val psi: Expression = Root.psi

  /**
   * Computes the nth Fibonacci number using algebraic expressions for phi (the golden ratio)
   * and psi, derived from Binet's formula. The calculation involves powers, subtraction,
   * and division of these expressions and produces a BigInt representation of the result.
   * CONSIDER refactoring to avoid repetition of exceptions.
   *
   * @param n the index of the Fibonacci sequence (must be a non-negative integer).
   * @return the nth Fibonacci number as a BigInt.
   * @throws java.lang.IllegalArgumentException if the result cannot be computed as an exact BigInt.
   */
  def fib(n: Int): BigInt = {
    val expression = fibExpression(n)
    val errorMessage = s"fib($n) = ${((phi ^ n) - (psi ^ n)) / Constants.root5}"
    // CONSIDER should this be PureNumber or Scalar?
    expression.materialize match {
      case Real(x) => x match {
        case ExactNumber(y, PureNumber) => maybeRational(y) match {
          case Some(z) =>
            z.toBigInt
          case _ =>
            throw new IllegalArgumentException(errorMessage)
        }
        case _ =>
          throw new IllegalArgumentException(errorMessage)
      }
      case _ =>
        throw new IllegalArgumentException(errorMessage)
    }
  }

  private[applications] def fibExpression(n: Int): Expression =
    ((phi ^ n) - (psi ^ n)) / Constants.root5
}

