package com.phasmidsoftware.number.applications

import com.phasmidsoftware.number.core.Expression.ExpressionOps
import com.phasmidsoftware.number.core.Value.maybeRational
import com.phasmidsoftware.number.core._

object Fibonacci {

  val phi: Expression = (Expression(Constants.one) plus Constants.root5) / Constants.two
  val psi: Expression = (Expression(Constants.one) - Constants.root5) / Constants.two

  def fib(n: Int): BigInt = {
    val expression = fibExpression(n)
    expression.materialize match {
      case Real(x) => x match {
        case ExactNumber(y, Scalar) => maybeRational(y) match {
          case Some(z) =>
            z.toBigInt
          case _ =>
            throw new IllegalArgumentException(s"fib($n) = ${((phi ^ n) - (psi ^ n)) / Constants.root5}")
        }
        case _ =>
          throw new IllegalArgumentException(s"fib($n) = ${((phi ^ n) - (psi ^ n)) / Constants.root5}")
      }
      case _ =>
        throw new IllegalArgumentException(s"fib($n) = ${((phi ^ n) - (psi ^ n)) / Constants.root5}")
    }
  }

  private[applications] def fibExpression(n: Int): Expression =
    ((phi ^ n) - (psi ^ n)) / Constants.root5
}

