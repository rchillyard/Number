package com.phasmidsoftware.number.mill

import com.phasmidsoftware.number.core.Expression
import com.phasmidsoftware.number.core.Expression._

trait Mill {
  /**
    * Method to create a new Mill with x on the "top".
    *
    * @param x an Item.
    * @return a Mill with the top = x and the rest the same as this.
    */
  def push(x: Item): Mill

  /**
    * Method to remove an element from the top of this Mill.
    *
    * @return a tuple consisting of the top element wrapped in Some, and the new Mill without that element.
    */
  def pop: (Option[Item], Mill)

  /**
    * @return true if this Mill is empty.
    */
  def isEmpty: Boolean

  /**
    * Method to evaluate this Mill.
    *
    * @return a tuple consisting of the an Expression wrapped in Some, and the new Mill that's left behind.
    */
  def evaluate: (Option[Expression], Mill)
}

case class Stack(stack: List[Item]) extends Mill {
  def push(x: Item): Mill = Stack(x :: stack)

  def pop: (Option[Item], Mill) = stack match {
    case h :: Nil => (Some(h), Empty)
    case h :: t => (Some(h), Stack(t))
  }

  def isEmpty: Boolean = false

  def evaluate: (Option[Expression], Mill) = pop match {
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
  def push(x: Item): Mill = Stack(List(x))

  def pop: (Option[Item], Mill) = throw MillException(s"logic error: empty Mill")

  def isEmpty: Boolean = true

  def evaluate: (Option[Expression], Mill) = throw MillException(s"logic error: empty Mill")
}

object Mill {
  def empty: Mill = Empty
}

case class MillException(s: String) extends Exception(s)