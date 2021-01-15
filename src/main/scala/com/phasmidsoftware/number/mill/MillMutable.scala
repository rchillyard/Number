package com.phasmidsoftware.number.mill

import com.phasmidsoftware.number.core._

import scala.collection.mutable

case class MillMutable(stack: mutable.Stack[Number]) {

  def push(n: Number): Unit = {
    stack.push(n)
  }

  def pop(): Option[Number] = if (stack.isEmpty) None else Some(stack.pop())

  def isEmpty: Boolean = stack.isEmpty
}

object MillMutable {
  def apply(): MillMutable = MillMutable(mutable.Stack())
}
