/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.mill

import com.phasmidsoftware.number.core.numerical.*

/**
  * Trait to model the behavior of an item that goes into the Mill.
  */
trait CoreMillItem

/**
  * Sub-class of Item which represents a dyadic operator.
  *
  * @param precedence        the precedence level of the operator.
  * @param leftAssociativity the left-associativity of the operator.
  */
class Dyadic(val precedence: Int, val leftAssociativity: Boolean = true) extends CoreMillItem

/**
  * Companion object to Dyadic.
  */
object Dyadic {
  /**
    * Unapply method (required to be defined since Dyadic is not a leaf class and therefore not a case class).
    *
    * @param arg a Dyadic operator.
    * @return an Option of (Int, Boolean).
    */
  def unapply(arg: Dyadic): Option[(Int, Boolean)] = Some(arg.precedence -> arg.leftAssociativity)

  /**
    * Ordering for a Dyadic operator.
    *
    */
  implicit object DyadicOrdering extends Ordering[Dyadic] {
    def compare(x: Dyadic, y: Dyadic): Int = if ((x.leftAssociativity && x.precedence <= y.precedence) || (!x.leftAssociativity && x.precedence < y.precedence)) -1 else 0
  }
}

/**
  * Sub-trait of Item to define a Monadic operator.
  */
trait Monadic extends CoreMillItem

/**
  * Multiply operator (sub-type of Dyadic).
  */
case object Multiply extends Dyadic(3)

/**
  * Divide operator (sub-type of Dyadic).
  */
case object Divide extends Dyadic(3)

/**
  * Add operator (sub-type of Dyadic).
  */
case object Add extends Dyadic(2)

/**
  * Subtract operator (sub-type of Dyadic).
  */
case object Subtract extends Dyadic(2)

/**
  * Power operator (sub-type of Dyadic).
  */
case object Power extends Dyadic(4, false)

/**
  * CHS (change sign) operator (sub-type of Monadic).
  */
case object Chs extends Monadic

/**
  * Inv (invert) operator (sub-type of Monadic).
  */
case object Inv extends Monadic

/**
  * Sqrt (square root) operator (sub-type of Monadic).
  */
case object Sqrt extends Monadic

/**
  * Sin (sine) operator (sub-type of Monadic).
  */
case object Sin extends Monadic

/**
  * Cos (cosine) operator (sub-type of Monadic).
  */
case object Cos extends Monadic

/**
  * Ln (natural logarithm) operator (sub-type of Monadic).
  */
case object Ln extends Monadic

/**
  * Exponent (e&#94;x) operator (sub-type of Monadic).
  */
case object Exponent extends Monadic

/**
  * Swap operator (sub-type of Item).
  */
case object Swap extends CoreMillItem

/**
  * Clr (clear) operator (sub-type of Item).
  */
case object Clr extends CoreMillItem

/**
  * Noop (no-op) operator (sub-type of Item).
  */
case object Noop extends CoreMillItem

/**
  * Open (open parenthesis) operator (sub-type of Item).
  */
case object Open extends CoreMillItem


/**
  * Close (close parenthesis) operator (sub-type of Item).
  */
case object Close extends CoreMillItem


/**
  * Expr (expression) operator (sub-type of Item).
  * In old-fashioned terminology, x is the augend for the + operator,
  * and the multiplier for the * operator.
  */
case class Expr(x: CoreMillExpression) extends CoreMillItem {
  /**
    * Sum method.
    *
    * @param addend the expression to be added to x.
    * @return x + addend.
    */
  def +(addend: Expr): CoreMillExpression = x.+(addend.x)

  /**
    * Product method.
    *
    * @param multiplicand the expression to be multiplied by x.
    * @return x * multiplicand
    */
  def *(multiplicand: Expr): CoreMillExpression = x.*(multiplicand.x)

  override lazy val toString: String = x.toString
}

/**
  * Companion object to Expr.
  */
object Expr {
  /**
    * Apply method to take a Number and return an Expr based on the Number.
    *
    * @param x a Number.
    * @return Expr(Literal(x)).
    */
  def apply(x: Number): Expr = Expr(TerminalExpression(x))
}

/**
  * Companion object to Item.
  *
  */
object CoreMillItem {

  /**
    * Apply method to construct an Item according to the given String.
    * The input String s is case-independent.
    * <dl>
    * <dt>&#94;</dt><dd>Power</dd>
    * <dt>+</dt><dd>Add</dd>
    * <dt>-</dt><dd>Subtract</dd>
    * <dt>*</dt><dd>Multiply (also ×)</dd>
    * <dt>/</dt><dd>Divide (also ÷)</dd>
    * <dt>&lt;&gt;</dt><dd>Swap</dd>
    * <dt>v</dt><dd>Sqrt</dd>
    * <dt></dt><dd>Noop</dd>
    * <dt>(</dt><dd>Open</dd>
    * <dt>)</dt><dd>Close</dd>
    * </dl>
    *
    * @param s a String.
    * @return an Item.
    */
  def apply(s: String): CoreMillItem = s.toLowerCase match {
    // XXX Dyadic operators
    case "∧" => Power
    case "^" => Power
    case "+" => Add
    case "-" | "−" | "–" => Subtract
    case "*" | "×" => Multiply
    case "/" | "÷" => Divide
    // XXX Monadic operators
    case "chs" => Chs
    case "inv" => Inv
    case "v" | "sqrt" => Sqrt
    case "ln" => Ln
    case "exp" => Exponent
    case "sin" => Sin
    case "cos" => Cos
    // XXX Anadic operators
    case "<>" => Swap
    case "c" | "clr" => Clr
    case "" => Noop
    // XXX Parentheses
    case "(" => Open
    case ")" => Close
    // XXX Expressions
    case x => Expr(Number(x))
  }
}