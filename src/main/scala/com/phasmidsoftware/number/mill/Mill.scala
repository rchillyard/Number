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
    * @return a tuple consisting of an Expression wrapped in Some, and the new Mill that's left behind.
    */
  def evaluate: (Option[Expression], Mill)
}

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
  def isEmpty: Boolean = false

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
  def evaluate: (Option[Expression], Mill) = pop match {
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
  private def evaluate1(x: Item): (Option[Expression], Mill) = x match {
    case d: Dyadic => evaluateDyadic(d)
    case o: Monadic => evaluateMonadic(o)
    case Expr(e) => (Some(e), this)
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
  private def evaluateMonadic(f: Monadic): (Option[Expression], Mill) = evaluate match {
    case (Some(e), m) =>
      val expression = calculateMonadic(f, e)
      m.push(Expr(expression)).evaluate
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
  private def evaluateDyadic(f: Dyadic): (Option[Expression], Mill) = pop match {
    case (Some(Expr(e)), m) => m match {
      case Empty => throw MillException(s"evaluateDyadic: malformed stack (expression should be followed by non-empty stack): $this")
      case m: Stack => m.evaluate2(f, e)
    }
    case _ => throw MillException(s"evaluateDyadic: empty or malformed stack (dyadic operator must be followed by an expression): $this")
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
    evaluate match {
      case (Some(e), m) =>
        val expression = calculateDyadic(f, x, e)
        m.push(Expr(expression)).evaluate
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
    case Power => x2 ^ x1
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
    * @return true.
    */
  def isEmpty: Boolean = true

  /**
    * @throws MillException logic error: empty Mill.
    */
  def evaluate: (Option[Expression], Mill) = throw MillException(s"evaluate: logic error: empty Mill")
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
  def apply(xs: Item*): Mill = if (xs.isEmpty) Empty else Stack(xs.to(List))
}

case class MillException(s: String) extends Exception(s)