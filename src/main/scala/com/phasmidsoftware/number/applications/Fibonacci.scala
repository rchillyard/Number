package com.phasmidsoftware.number.applications

import com.phasmidsoftware.number.core._
import com.phasmidsoftware.number.core.inner.PureNumber
import com.phasmidsoftware.number.core.inner.Value.maybeRational
import com.phasmidsoftware.number.expression.Expression.ExpressionOps
import com.phasmidsoftware.number.expression.{Expression, Phi, Psi}

object Fibonacci {

  // CONSIDER reverting to the original definitions
  val phi: Expression = Phi // (Expression(Constants.one) plus Constants.root5) / Constants.two
  val psi: Expression = Psi // (Expression(Constants.one) - Constants.root5) / Constants.two


  /**
   * Computes the nth Fibonacci number using algebraic expressions for phi (the golden ratio)
   * and psi, derived from Binet's formula. The calculation involves powers, subtraction,
   * and division of these expressions and produces a BigInt representation of the result.
   * CONSIDER refactoring to avoid repetition of exceptions.
   *
   * @param n the index of the Fibonacci sequence (must be a non-negative integer).
   * @return the nth Fibonacci number as a BigInt.
   * @throws IllegalArgumentException if the result cannot be computed as an exact BigInt.
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

