package com.phasmidsoftware.number.mill

import com.phasmidsoftware.number.core.Expression._
import com.phasmidsoftware.number.core._

trait Item

class Dyadic(val precedence: Int, val leftAssociativity: Boolean = true) extends Item

object Dyadic {
  def unapply(arg: Dyadic): Option[(Int, Boolean)] = Some(arg.precedence -> arg.leftAssociativity)

  implicit object DyadicOrdering extends Ordering[Dyadic] {
    def compare(x: Dyadic, y: Dyadic): Int = if ((x.leftAssociativity && x.precedence <= y.precedence) || (!x.leftAssociativity && x.precedence < y.precedence)) -1 else 0
  }
}

trait Monadic extends Item

trait Anadic extends Item

case object Multiply extends Dyadic(3)

case object Divide extends Dyadic(3)

case object Add extends Dyadic(2)

case object Subtract extends Dyadic(2)

case object Power extends Dyadic(4, false)

case object Chs extends Monadic

case object Inv extends Monadic

case object Sqrt extends Monadic

case object Sin extends Monadic

case object Cos extends Monadic

case object Ln extends Monadic

case object Exponent extends Monadic

case object Swap extends Item

case object Clr extends Item

case object Noop extends Item

case object Open extends Item

case object Close extends Item

case class Expr(x: Expression) extends Item {
  def +(other: Expr): Expression = x.+(other.x)

  def *(other: Expr): Expression = x.*(other.x)

  override def toString: String = x.toString
}

object Item {
  def apply(s: String): Item = s.toLowerCase match {
    // XXX Dyadic operators
    case "^" => Power
    case "+" => Add
    case "-" | "−" | "–" => Subtract
    case "*" | "×" => Multiply
    case "/" | "÷" => Divide
    // XXX Monadic operators
    case "chs" => Chs
    case "inv" => Inv
    case "v" => Sqrt
    case "ln" => Ln
    case "exp" => Exponent
    case "sin" => Sin
    case "cos" => Cos
    // XXX Anadic operators
    case "<>" => Swap
    case "c" => Clr
    case "" => Noop
    // XXX Parentheses
    case "(" => Open
    case ")" => Close
    // XXX Expressions
    case x => Expr(GeneralNumber(x))
  }
}