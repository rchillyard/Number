package com.phasmidsoftware.number.mill

import com.phasmidsoftware.number.core.Expression._
import com.phasmidsoftware.number.core._

trait Item

trait Dyadic extends Item

trait Monadic extends Item

trait Anadic extends Item

case object Multiply extends Dyadic

case object Add extends Dyadic

case object Subtract extends Dyadic

case object Power extends Dyadic

case object Chs extends Monadic

case object Inv extends Monadic

case object Sqrt extends Monadic

case object Swap extends Item

case object Clr extends Item

case object Noop extends Item

case class Expr(x: Expression) extends Item {
  def +(other: Expr): Expression = x.+(other.x)

  def *(other: Expr): Expression = x.*(other.x)

  override def toString: String = x.toString
}

object Item {
  def apply(s: String): Item = s.toLowerCase match {
    // Dyadic operators
    case "^" => Power
    case "+" => Add
    case "-" => Subtract
    case "*" | "×" => Multiply
    // Monadic operators
    case "chs" => Chs
    case "inv" => Inv
    case "v" => Sqrt
    // Anadic operators
    case "<>" => Swap
    case "c" => Clr
    case "" => Noop
    case x => Expr(Number(x))
  }
}