package com.phasmidsoftware.number.mill

import com.phasmidsoftware.number.core.Expression
import com.phasmidsoftware.number.core.Expression._

trait Mill {
  def push(x: Item): Mill

  def pop: (Option[Item], Mill)

  def isEmpty: Boolean

  def evaluate: (Option[Expression], Mill)
}

case class Stack(stack: List[Item]) extends Mill {
  def push(x: Item): Stack = Stack(x :: stack)

  def pop: (Option[Item], Stack) = stack match {
    case Nil => (None, this)
    case h :: t => (Some(h), Stack(t))
  }

  def isEmpty: Boolean = stack.isEmpty

  def evaluate: (Option[Expression], Stack) = pop match {
    case (Some(x), mill) => x match {
      case d: Dyadic =>
        val (Some(Constant(x1)), m1) = mill.pop
        val (Some(x2), m2) = m1.evaluate
        (None, m2.push(Constant(d match {
          case Multiply => x2.*(x1)
          case Add => x2.+(x1)
        })))
      case o: Monadic =>
        val (Some(x), m) = mill.evaluate
        (None, m.push(Constant(o match {
          case Chs => x * Expression(-1)
        })))
      case Constant(x) => (Some(x), mill)
      case x => throw MillException(s"$x not supported")
    }
    case (None, _) => throw MillException(s"logic error")
  }
}

case object Empty extends Mill {
  def push(x: Item): Mill = Stack(Nil).push(x)

  def pop: (Option[Item], Mill) = throw MillException(s"logic error: empty Mill")

  def isEmpty: Boolean = true

  def evaluate: (Option[Expression], Mill) = throw MillException(s"logic error: empty Mill")
}

object Mill {
  def empty: Mill = Empty
}

case class MillException(s: String) extends Exception(s)