package com.phasmidsoftware.number.mill

import com.phasmidsoftware.number.core.Expression._
import com.phasmidsoftware.number.core._

trait Item

trait Dyadic extends Item

trait Monadic extends Item

trait Anadic extends Item

case object Multiply extends Dyadic

case object Add extends Dyadic

case object Chs extends Monadic

case object Swap extends Item

case class Constant(x: Expression) extends Item {
  def +(other: Constant): Expression = x.+(other.x)

  def *(other: Constant): Expression = x.*(other.x)
}

object Item {
  def apply(s: String): Item = s match {
    case "+" => Add
    case "-" => Chs
    case "*" => Multiply
    case x => Constant(Number(x))
  }
}