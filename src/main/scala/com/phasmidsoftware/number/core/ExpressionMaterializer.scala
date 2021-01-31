package com.phasmidsoftware.number.core

trait ExpressionMaterializer extends (Expression => ValueResult)

trait ValueResult

case class Value(x: Number) extends ValueResult
case class NonMaterialized(e: Expression) extends ValueResult

class ExpressionMaterializers {

  def materialize: ExpressionMaterializer = e => e match {
    case x @ ExactNumber(_, _) => Value(x)
    case x @ FuzzyNumber(_, _, _) => Value(x)
    case x @ (Zero | MinusOne | One) => Value(x)
    case BiFunction(x, y, f) => **(f,y)(x)
    case Function(x, f) => !!(f)(x)
  }

  def !!(f: ExpressionFunction): ExpressionMaterializer = ???

  def **(f: ExpressionBiFunction, y: Expression): ExpressionMaterializer = ???
}
