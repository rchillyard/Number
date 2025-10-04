/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.mill

import com.phasmidsoftware.number.expression.Expression
import com.phasmidsoftware.number.expression.Expression.ExpressionOps
import com.phasmidsoftware.number.parse.{MillParser, ShuntingYardParser}
import scala.language.postfixOps
import scala.util.Try

/**
  * Trait to define the behavior of a "mill."
  *
  * CONSIDER rename Stack as ListMill and extract push, pop and isEmpty as Stack[Item].
  *
  */
trait Mill extends Iterable[Item] {
  self =>

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
    * Method to evaluate this Mill.
    *
    * @return a tuple consisting of an Expression wrapped in Some, and the new Mill that's left behind.
    */
  def evaluate: Option[Expression]

  /**
    * Method required by Iterable[Item].
    *
    * @return an Iterator[Item].
    */
  def iterator: Iterator[Item] = {
    // XXX using a var here, but the entire concept of an iterator pretty much requires mutability.
    var mill = this

    new Iterator[Item] {
      /**
        * @return the value of mill.nonEmpty
        */
      def hasNext: Boolean = mill.nonEmpty

      /**
        * @return the next item and update the mill variable.
        */
      def next(): Item = {
        val (xo, m) = mill.pop
        mill = m
        xo.get // XXX this call to get is protected by the hasNext method. That's the way iterators are.
      }
    }
  }
}

/**
  * A case class to represent a stack of Items, providing behavior of Mill.
  *
  * @param stack a List[Item] which will represent the stack.
  */
case class Stack(stack: List[Item]) extends Mill {
  /**
    * Method to push an Item on to this Stack.
    *
    * @param x an Item.
    * @return a Mill with the top = x and the rest the same as this.
    */
  def push(x: Item): Mill = Stack(x :: stack)

  /**
    * Method to pop an Item from this Stack.
    *
    * @return a tuple consisting of the top element wrapped in Some, and the new Mill without that element.
    */
  def pop: (Option[Item], Mill) = stack match {
    case Nil => (None, Empty) // NOTE: in practice, this will never occur
    case h :: Nil => (Some(h), Empty)
    case h :: t => (Some(h), Stack(t))
  }

  /**
    * @return false.
    */
  override def isEmpty: Boolean = false

  /**
    * Method to evaluate this Mill.
    *
    * @return an Option[Expression]: Some(x) assuming this Mill is not empty and that there are no irregularities.
    * @throws MillException logic error when the Mill is not fully consumed or the optional expression is None.
    */
  def evaluate: Option[Expression] = evaluateInternal match {
    case (xo, Empty) => xo
    case (_, m) => throw MillException(s"evaluate: logic error: remaining stack is not empty: $m")
  }

  /**
    * Evaluate this Stack recursively.
    * We pop the top element from this Mill.
    * If it yields an Expression x with an empty Mill,
    * then we return the expression, appropriately wrapped, together with an empty Mill;
    * if it yields Item x with a non-empty Mill, then we invoke evaluate1(x) on the mill.
    *
    * Typically, the mill returned will be empty when a fully-formed (legal) stack is fully evaluated.
    *
    * @return a tuple consisting of an Expression wrapped in Some, and the new Mill that's left behind.
    * @throws MillException this Mill is empty or some other logic error occurred.
    */
  def evaluateInternal: (Option[Expression], Mill) = pop match {
    case (Some(Expr(e)), Empty) => (Some(e), Empty)
    case (Some(x), m: Stack) => m.evaluate1(x)
    case (None, _) => throw MillException(s"evaluate: this stack is empty")
    case (_, _) => throw MillException(s"evaluate: logic error: $this") // NOTE: improbable
  }

  /**
    * Evaluate this Mill with one additional parameter (x).
    *
    * @param x any Item.
    * @return if x is Dyadic, then we pop the next expression from this Mill, and invoke evaluate2;
    *         if x is Monadic, then we evaluate this Mill and apply x to it;
    *         if x is an expression, then we return it, wrapped in Some, along with this;
    *         otherwise, we throw an exception.
    * @throws MillException x is not supported.
    */
  private def evaluate1(x: Item): (Option[Expression], Mill) =
    x match {
      case d: Dyadic => evaluateDyadic(d)
      case o: Monadic => evaluateMonadic(o)
      case Expr(e) => (Some(e), this)
      case Clr => (None, Empty)
      case Noop => evaluateInternal
      case Swap => evaluateSwap
      case x => throw MillException(s"evaluate1: $x not a supported Item")
    }

  /**
    * Method to evaluate this Mill by applying the monadic operator f to its value.
    *
    * @param f the monadic operator to apply to this Mill.
    * @return a tuple of optional expression and a Mill which is the same as this Mill but with the top
    *         item replaced by the f(top).
    * @throws MillException this Mill is empty.
    */
  private def evaluateMonadic(f: Monadic): (Option[Expression], Mill) = evaluateInternal match {
    case (Some(e), m) =>
      val expression = calculateMonadic(f, e)
      m.push(Expr(expression)).asInstanceOf[Stack].evaluateInternal
    case (None, _) => throw MillException(s"evaluateMonadic: $this is empty")
  }

  /**
    * Method to evaluate this Mill by applying the monadic operator f to its value.
    * Pops the next expression e (may not be an operator) from this Mill and invokes
    * evaluate2(f, e) on the remaining Mill.
    *
    * @param f the dyadic operator to apply to this Mill.
    * @return a tuple of optional expression and a Mill which is the same as this Mill but with the top two items
    *         replaced by the f(top, next).
    * @throws MillException malformed stack.
    */
  private def evaluateDyadic(f: Dyadic): (Option[Expression], Mill) = {
    def inner(e: Expression, m: Mill) = m match {
      case Empty => throw MillException(s"evaluateDyadic: malformed stack (expression should be followed by non-empty stack): $this")
      case m: Stack => m.evaluate2(f, e)
    }

    pop match {
      case (Some(Expr(e)), m) => inner(e, m)
      case (Some(i), m) => m.push(i) match {
        case n: Stack => n.evaluateInternal match {
          case (Some(e), m) => inner(e, m)
          case _ => throw MillException(s"evaluateDyadic: logic error): $f, $n")
        }
        case Empty => throw MillException(s"evaluateDyadic: malformed stack (expression should be followed by non-empty stack): $this")
      }
      case _ => throw MillException(s"evaluateDyadic: malformed stack (expression should be followed by non-empty stack): $this")
    }
  }

  /**
    * Evaluate this Mill with two additional parameters: f and x.
    * The expression formed by applying f and x to this Mill is pushed onto this Mill and
    * the result is (recursively) evaluated.
    *
    * @param f a Dyadic operator.
    * @param x an Expression.
    * @return this Mill is evaluated and, assuming a valid result, calculateDyadic is invoked on f, x, and the resulting expression
    *         is pushed to this and the resulting Mill is (recursively) evaluated;
    *         if the result of evaluating this is not valid, we throw an exception.
    * @throws MillException this did not evaluate to an expression.
    */
  private def evaluate2(f: Dyadic, x: Expression): (Option[Expression], Mill) =
    evaluateInternal match {
      case (Some(e), m) =>
        val expression = calculateDyadic(f, x, e)
        m.push(Expr(expression)).asInstanceOf[Stack].evaluateInternal
      case (None, _) =>
        throw MillException(s"evaluate2: logic error: $this did not evaluate to an expression")
    }

  /**
    * Calculate the Expression based the Monadic operator applied to x.
    *
    * @param f the monadic operator.
    * @param x the expression to be operated on.
    * @return an Expression which is f(x).
    * @throws MillException operator f is not supported.
    */
  private def calculateMonadic(f: Monadic, x: Expression) = f match {
    case Chs => x * Expression(-1)
    case Inv => x reciprocal
    case Sqrt => x sqrt
    case Ln => x.ln
    case Exponent => x.exp
    case Sin => x.sin
    case Cos => x.cos
    case _ => throw MillException(s"calculateMonadic: $f not supported")
  }

  /**
    * Calculate the Expression based the Dyadic operator f applied to x1 and x2.
    *
    * @param f  the dyadic operator.
    * @param x1 the first expression to be operated on.
    * @param x2 the second expression to be operated on.
    * @return an Expression which is f(x2, x1)
    */
  private def calculateDyadic(f: Dyadic, x1: Expression, x2: Expression) = f match {
    case Multiply => x2 * x1
    case Add => x2 + x1
    case Subtract => x2 + -x1
    case Divide => x2 * x1.reciprocal
    case Power => x2 ^ x1
  }

  private def evaluateSwap = {
    val (zo, m) = pop
    val (yo, n) = m.pop
    val result: Option[(Option[Expression], Mill)] = (for (z <- zo; y <- yo; x = n.push(z).push(y)) yield x).map {
      case mill: Stack => mill.evaluateInternal
      case _ => throw MillException(s"evaluateSwap: logic error")
    }
    result match {
      case Some((eo, m)) => eo -> m
      case None => throw MillException(s"evaluateSwap: logic error")
    }
  }

  override def toString: String = s"""Stack(${stack.mkString(", ")})"""
}

/**
  * Empty Mill definition.
  */
case object Empty extends Mill {
  /**
    * Create a new Stack with x on it.
    *
    * @param x an Item.
    * @return a Stack with just one element (x).
    */
  def push(x: Item): Mill = Stack(List(x))

  /**
    * @return (None, Empty)
    */
  def pop: (Option[Item], Mill) = (None, Empty)

  /**
    * Overriding the isEmpty method of IterableOnceOps.
    * NOTE: don't worry about Codacy complaint: it's just that this object is called "Empty."
    *
    * @return true.
    */
  override def isEmpty: Boolean = true

  /**
    * @return None.
    */
  def evaluate: Option[Expression] = None
}

object Mill {
  /**
    * Empty Mill value.
    *
    * @return Empty
    */
  val empty: Mill = Empty

  /**
    * Method to take a sequence of Items and create an appropriate Mill.
    *
    * @param xs a comma-separated sequence of Item.
    * @return an appropriate Mill.
    */
  def apply(xs: Item*): Mill = if (xs.isEmpty) Empty else Stack(xs.reverse.to(List))

  /**
    * Alternative method of creating a Mill from a list of Items.
    *
    * @param items sequence of Item.
    * @return a new Mill.
    */
  def create(items: Seq[Item]): Mill = items.foldLeft(Mill.empty)((m, x) => m.push(x))

  /**
    * Method to parse a String in RPN (may extend over multiple lines) into a Mill.
    *
    * @param w the input String.
    * @return a Mill, wrapped in Try.
    */
  def parse(w: String): Try[Mill] = MillParser.parseMill(w)

  /**
    * Method to parse a String in infix notation (may extend over multiple lines) into a Mill.
    *
    * @param w the input String.
    * @return a Mill, wrapped in Try.
    */
  def parseInfix(w: String): Try[Mill] = ShuntingYardParser.parseInfix(w)
}

case class MillException(s: String) extends Exception(s)