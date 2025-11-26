/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.mill

import com.phasmidsoftware.number.core.{Field, Number, Real}

/**
  * Trait representing a mathematical expression, which can be combined or transformed
  * using various operations. This is intended as a fundamental building block for
  * creating complex expressions or performing symbolic computations.
  */
trait Expression {
  def value: Number

  /**
    * Combines the current expression with another expression using the addition operator.
    *
    * @param that another Expression to be added to the current Expression.
    * @return a new Expression representing the addition of the current Expression and the specified Expression.
    */
  def +(that: Expression): Expression = DyadicExpression(this, that, "+")

  /**
    * Multiplies this expression with another expression.
    *
    * @param that the expression to multiply with this expression
    * @return a new expression representing the multiplication of this expression and the given expression
    */
  def *(that: Expression): Expression = DyadicExpression(this, that, "*")

  /**
    * Combines this `Expression` with another `Expression` using the logical conjunction (∧) operator.
    *
    * @param that the `Expression` to combine with this `Expression`.
    * @return a new `Expression` representing the conjunction of this `Expression` and the given `Expression`.
    */
  def ∧(that: Expression): Expression = DyadicExpression(this, that, "∧")

  /**
    * Negates the current expression, effectively multiplying it by -1.
    *
    * @return a new expression representing the negation of the current expression.
    */
  def negate: Expression = MonadicExpression(this, "-")

  /**
    * Computes the reciprocal of the current expression, representing it as a monadic operation.
    *
    * @return a new Expression where the operation corresponds to the reciprocal (1 / this expression).
    */
  def reciprocal: Expression = MonadicExpression(this, "/")

  /**
    * Computes the square root of a numeric expression.
    *
    * @return An `Expression` representing the square root operation on the current expression.
    */
  def sqrt: Expression = MonadicExpression(this, "√")

  /**
    * Computes the natural logarithm (ln) of the current mathematical expression.
    *
    * @return a new Expression representing the natural logarithm of this Expression.
    */
  def ln: Expression = MonadicExpression(this, "ln")

  /**
    * Calculates the exponential of the current expression (e∧x), where x is the value
    * represented by the current Expression instance. This transformation is applied
    * monadically to the expression.
    *
    * @return a new Expression representing the exponential of the current expression.
    */
  def exp: Expression = MonadicExpression(this, "exp")

  /**
    * Computes the sine of the current mathematical expression.
    *
    * @return A new Expression representing the sine of the current expression.
    */
  def sin: Expression = MonadicExpression(this, "sin")

  /**
    * Computes the cosine of the current expression.
    *
    * This method applies the cosine function to the current mathematical expression,
    * returning a new expression that represents the result of the transformation.
    *
    * @return a new Expression representing the cosine of the current expression.
    */
  def cos: Expression = MonadicExpression(this, "cos")
}

/**
  * Represents a dyadic (binary) expression consisting of a left operand, a right operand,
  * and an operator connecting them in an infix notation.
  *
  * @param left     the left-hand side operand of the expression, represented as an `Expression`.
  * @param right    the right-hand side operand of the expression, represented as an `Expression`.
  * @param operator the operator as a `String` connecting the left and right operands.
  */
case class DyadicExpression(left: Expression, right: Expression, operator: String) extends Expression {
  override def toString: String = s"($left $operator $right)"

  def value: Number = operator match {
    case "+" => left.value.doAdd(right.value)
    case "*" => left.value.doMultiply(right.value)
    case "∧" => left.value.doPower(right.value)
    case _ => throw new IllegalArgumentException(s"unknown operator $operator")
  }

}

/**
  * A case class that models a monadic expression, which represents a single-operand operation
  * applied to another `Expression`.
  *
  * @param expression the underlying `Expression` on which the monadic operation is applied.
  * @param str        a string representation of the operator.
  */
case class MonadicExpression(expression: Expression, str: String) extends Expression {
  override def toString: String = s"$str($expression)"

  def value: Number = {
    val f: Number => Field = str match {
      case "-" => x => -x
      case "/" => x => x.invert
      case "√" => x => Real(x.sqrt)
      case "ln" => x => x.ln
      case "exp" => x => Real(x.exp)
      case "sin" => x => Real(x.sin)
      case "cos" => x => Real(x.cos)
      case _ => throw new IllegalArgumentException(s"unknown operator $str")
    }
    f(expression.value).asNumber.get // TODO this should not be a problem but we should make it more elegant
  }
}

/**
  * Represents a terminal expression in the expression hierarchy.
  *
  * A TerminalExpression is a leaf node in the expression structure,
  * encapsulating a numeric value. It directly stores a value of type
  * `com.phasmidsoftware.number.Number`.
  *
  * @constructor Creates a TerminalExpression with the specified numeric value.
  * @param value The numeric value encapsulated by this terminal expression.
  */
case class TerminalExpression(value: Number) extends Expression {
  override def toString: String = value.toString
}

/**
  * Represents an `Expression` in a symbolic mathematical computation system.
  *
  * This class provides operations for mathematical and logical manipulations,
  * including arithmetic, trigonometric, and monadic transformations.
  */
object Expression {
  /**
    * Parses a string in infix notation into an `Expression`, if valid.
    * NOTE that `Expression` refers to the mill package `Expression` (not the core `Expression`).
    *
    * @param x the input string representing a mathematical expression in infix notation.
    * @return an `Option` containing the resulting `Expression` if parsing and evaluation are successful, or `None` otherwise.
    */
  def parseToExpression(x: String): Option[Expression] =
    Mill.parseInfix(x).toOption.flatMap(_.evaluate)
}