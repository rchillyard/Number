package com.phasmidsoftware.number.mill

import com.phasmidsoftware.number.core.Expression._
import com.phasmidsoftware.number.core._

trait Item

trait Dyadic extends Item

trait Monadic extends Item

trait Anadic extends Item

case object Multiply extends Dyadic

case object Add extends Dyadic

case object Power extends Dyadic

case object Chs extends Monadic

case object Inv extends Monadic

case object Swap extends Item

case class Expr(x: Expression) extends Item {
  def +(other: Expr): Expression = x.+(other.x)

  def *(other: Expr): Expression = x.*(other.x)

  override def toString: String = x.toString
}

object Item {
  def apply(s: String): Item = s match {
    case "^" => Power
    case "+" => Add
    case "-" => Chs
    case "*" => Multiply
    case "inv" => Inv
    case x => Expr(Number(x))
  }
}