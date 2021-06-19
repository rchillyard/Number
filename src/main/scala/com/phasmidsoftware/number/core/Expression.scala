package com.phasmidsoftware.number.core

import com.phasmidsoftware.matchers.LogLevel
import com.phasmidsoftware.number.core.Number.negate
import com.phasmidsoftware.number.parse.ShuntingYardParser

/**
  * Trait Expression which defines the behavior of a lazily-evaluated tree of mathematical operations and operands.
  */
trait Expression {

  /**
    * Method to determine if this Expression can be evaluated exactly.
    *
    * @return true if materialize will result in an ExactNumber, else false.
    */
  def isExact: Boolean

  /**
    * If it is possible to simplify this Expression, then we do so.
    * Typically, we simplify non-exact expressions if possible.
    * There is no compelling need to simplify exact expressions.
    *
    * @return an Expression tree which is the simpler equivalent of this.
    */
  def simplify: Expression

  /**
    * Action to materialize this Expression as a Number,
    * that is to say we eagerly evaluate this Expression as a Number.
    *
    * @return the materialized Number.
    */
  def materialize: Number

  /**
    * Method to determine the depth of this Expression.
    *
    * @return the depth (an atomic expression has depth of 1).
    */
  def depth: Int

  /**
    * Action to materialize this Expression and render it as a String,
    * that is to say we eagerly evaluate this Expression as a String.
    *
    * @return a String representing the value of this expression.
    */
  def render: String

  def layout(x: Int): String

  //  override def toString: String = layout(0)

  /**
    * Eagerly compare this Expression with comparand.
    *
    * @param comparand the expression to be compared.
    * @return the result of comparing materialized this with materialized comparand.
    */
  def compare(comparand: Expression): Int = materialize.compare(comparand.materialize)
}

object Expression {

  implicit val ll: LogLevel = com.phasmidsoftware.matchers.LogDebug
  implicit val em: ExpressionMatchers = new ExpressionMatchers {}

  /**
    * The following method is helpful in getting an expression started.
    */
  def apply(x: Number): Expression = x match {
    case Number.zero => Zero
    case Number.one => One
    case _ => Literal(x)
  }

  /**
    * The following method is helpful in getting an expression started.
    */
  def apply(x: Int): Expression = x match {
    case 0 => Zero
    case 1 => One
    case _ => Expression(Number(x))
  }

  /**
    * Method to parse a String as an Expression.
    *
    * TODO this may not accurately parse all infix expressions.
    * The idea is for render and parse.get to be inverses.
    * NOTE that it might be a problem with render instead.
    */
  def parse(x: String): Option[Expression] = new ShuntingYardParser().parseInfix(x).toOption flatMap (_.evaluate)

  def indent(x: Int): String = "  " * x

  /**
    * The following constants are helpful in getting an expression started.
    */
  val zero: Expression = Zero
  val one: Expression = One
  val pi: Expression = Expression(Number.pi)
  val e: Expression = Expression(Number.e)

  /**
    * Implicit class to allow various operations to be performed on an Expression.
    *
    * @param x an Expression.
    */
  implicit class ExpressionOps(x: Expression) {

    /**
      * Method to lazily multiply the Number x by y.
      *
      * @param y another Expression.
      * @return an Expression which is the lazy product of x and y.
      */
    def plus(y: Expression): Expression = BiFunction(x, y, Sum)

    /**
      * Method to lazily multiply the Number x by y.
      *
      * @param y another Expression.
      * @return an Expression which is the lazy product of x and y.
      */
    def +(y: Expression): Expression = x plus y

    /**
      * Method to lazily multiply the Number x by y.
      *
      * @param y another Number.
      * @return an Expression which is the lazy product of x and y.
      */
    def +(y: Number): Expression = this.+(Expression(y))

    /**
      * Method to lazily multiply the Number x by y.
      *
      * @param y another Number.
      * @return an Expression which is the lazy product of x and y.
      */
    def +(y: Int): Expression = this.+(Number(y))

    /**
      * Method to lazily subtract the Number y from x.
      *
      * @param y another Number.
      * @return an Expression which is the lazy product of x and y.
      */
    def -(y: Number): Expression = BiFunction(x, -Expression(y), Sum)

    /**
      * Method to lazily change the sign of this expression.
      *
      * @return an Expression which is this negated.
      */
    def unary_- : Expression = BiFunction(x, MinusOne, Product)

    /**
      * Method to lazily subtract the Number y from x.
      *
      * @param y another Number.
      * @return an Expression which is the lazy product of x and y.
      */
    def -(y: Int): Expression = this.-(Number(y))

    /**
      * Method to lazily multiply the Number x by y.
      *
      * @param y another Expression.
      * @return an Expression which is the lazy product of x and y.
      */
    def *(y: Expression): Expression = BiFunction(x, y, Product)

    /**
      * Method to lazily multiply the Number x by y.
      *
      * @param y another Number.
      * @return an Expression which is the lazy product of x and y.
      */
    def *(y: Number): Expression = *(Expression(y))

    /**
      * Method to lazily multiply the Number x by y.
      *
      * @param y another Number.
      * @return an Expression which is the lazy product of x and y.
      */
    def *(y: Int): Expression = *(Number(y))

    /**
      * Method to lazily yield the reciprocal of this Expression.
      *
      * @return an Expression representing the reciprocal of this.
      */
    def reciprocal: Expression = BiFunction(x, MinusOne, Power)

    /**
      * Method to lazily divide the Number x by y.
      *
      * @param y another Number.
      * @return an Expression which is the lazy quotient of x and y.
      */
    def /(y: Number): Expression = *(Expression(y).reciprocal)

    /**
      * Method to lazily multiply the Number x by y.
      *
      * @param y another Number.
      * @return an Expression which is the lazy product of x and y.
      */
    def /(y: Int): Expression = /(Number(y))

    /**
      * Method to lazily raise x to the power of y.
      *
      * @param y the power to which x should be raised.
      * @return an Expression representing x to the power of y.
      */
    def ^(y: Expression): Expression = BiFunction(x, y, Power)

    /**
      * Method to lazily raise x to the power of y.
      *
      * @param y the power to which x should be raised.
      * @return an Expression representing x to the power of y.
      */
    def ^(y: Number): Expression = ^(Expression(y))

    /**
      * Method to lazily raise the Number x to the power of y.
      *
      * @param y the power.
      * @return an Expression which is the lazy power of x to the y.
      */
    def ^(y: Int): Expression = ^(Number(y))

    /**
      * Method to lazily get the square root of x.
      *
      * @return an Expression representing the square root of x.
      */
    def sqrt: Expression = this ^ Number(2).reciprocal

    /**
      * Method to lazily get the sine of x.
      *
      * @return an Expression representing the sin(x).
      */
    def sin: Expression = Function(x, Sine)

    /**
      * Method to lazily get the cosine of x.
      *
      * @return an Expression representing the cos(x).
      */
    def cos: Expression = Function(x, Cosine)

    /**
      * Method to lazily get the tangent of x.
      *
      * @return an Expression representing the tan(x).
      */
    def tan: Expression = sin * cos.reciprocal

    /**
      * Method to lazily get the natural log of x.
      *
      * @return an Expression representing the log of x.
      */
    def log: Expression = Function(x, Log)

    /**
      * Method to lazily get the value of e raised to the power of x.
      *
      * @return an Expression representing e raised to the power of x.
      */
    def exp: Expression = Function(x, Exp)

    // TODO add atan method.

    /**
      * Eagerly compare this expression with y.
      *
      * @param comparand the number to be compared.
      * @return the result of the comparison.
      */
    def compare(comparand: Number): Int = x compare comparand
  }
}

/**
  * An Expression which cannot be further simplified.
  */
trait AtomicExpression extends Expression {
  /**
    * @return this.
    */
  def simplify: Expression = this

  /**
    * @return 1.
    */
  def depth: Int = 1

  /**
    * CONSIDER a different mechanism here (or different implementation of ExpressionMatchers).
    *
    * @return an instance of ExpressionMatchers.
    */
  def matchers: ExpressionMatchers = new ExpressionMatchers()

  def layout(n: Int): String = s"$render"
}

/**
  * An abstract class which extends Expression while providing an instance of ExpressionMatchers for use
  * with simplification.
  *
  */
abstract class CompositeExpression extends Expression

/**
  * A literal number.
  *
  * @param x the Number.
  */
case class Literal(x: Number) extends AtomicExpression {

  /**
    * Method to determine if this Expression can be evaluated exactly.
    *
    * @return true if materialize will result in an ExactNumber, else false.
    */
  def isExact: Boolean = x.isExact

  /**
    * Action to materialize this Expression as a Number,
    * that is to say we eagerly evaluate this Expression as a Number.
    *
    * @return the materialized Number.
    */
  def materialize: Number = x

  /**
    * Action to materialize this Expression and render it as a String,
    * that is to say we eagerly evaluate this Expression as a String.
    *
    * @return a String representing the value of this expression.
    */
  def render: String = x.render

  /**
    * Generate a String for debugging purposes.
    *
    * @return a String representation of this Literal.
    */
  override def toString: String = s"$x"
}

object Literal {
  def apply(x: Int): Literal = Literal(Number(x))
}

/**
  * A known constant value, for example π (pi) or e.
  */
abstract class Constant extends AtomicExpression {

  /**
    * Method to determine if this Expression can be evaluated exactly.
    *
    * Important NOTE: Some constants will be fuzzy in which case this method must be overridden.
    *
    * @return true.
    */
  def isExact: Boolean = true

  /**
    * Action to materialize this Expression and render it as a String,
    * that is to say we eagerly evaluate this Expression as a String.
    *
    * @return String form of this Constant.
    */
  def render: String = materialize.render

  /**
    * Method to yield a String from this Constant.
    *
    * @return a String.
    */
  override def toString: String = render
}

case object Zero extends Constant {
  /**
    * Action to materialize this Expression as a Number,
    * that is to say we eagerly evaluate this Expression as a Number.
    *
    * @return Number.zero
    */
  def materialize: Number = Number.zero
}

case object One extends Constant {
  /**
    * Action to materialize this Expression as a Number,
    * that is to say we eagerly evaluate this Expression as a Number.
    *
    * @return the materialized Number.
    */
  def materialize: Number = Number.one
}

case object MinusOne extends Constant {
  /**
    * Action to materialize this Expression as a Number,
    * that is to say we eagerly evaluate this Expression as a Number.
    *
    * @return the materialized Number.
    */
  def materialize: Number = negate(Number.one)

  /**
    * Action to materialize this Expression and render it as a String,
    * that is to say we eagerly evaluate this Expression as a String.
    *
    * @return "1-".
    */
  override def render: String = "-1"
}

/**
  * The constant π (pi).
  * Yes, this is an exact number.
  */
case object ConstPi extends Constant {
  /**
    * Action to materialize this Expression as a Number,
    * that is to say we eagerly evaluate this Expression as a Number.
    *
    * @return the materialized Number.
    */
  def materialize: Number = Number.pi
}

/**
  * The constant e.
  * Yes, this is an exact number.
  */
case object ConstE extends Constant {
  /**
    * Action to materialize this Expression as a Number,
    * that is to say we eagerly evaluate this Expression as a Number.
    *
    * @return the materialized Number.
    */
  def materialize: Number = Number.e
}

/**
  * This class represents a monadic function of the given expression.
  *
  * @param x the expression being operated on.
  * @param f the function to be applied to x.
  */
case class Function(x: Expression, f: ExpressionFunction) extends CompositeExpression {

  /**
    * Method to determine if this Expression can be evaluated exactly.
    *
    * CONSIDER deal with each ExpressionFunction on an individual basis.
    *
    * @return false.
    */
  def isExact: Boolean = f(x.materialize).isExact

  /**
    * Method to determine the depth of this Expression.
    *
    * @return the 1 + depth of x.
    */
  def depth: Int = 1 + x.depth

  /**
    * Action to materialize this Expression as a Number.
    *
    * @return the materialized Number.
    */
  def materialize: Number = f(x.simplify.materialize)

  /**
    * If it is possible to simplify this Expression, then we do so.
    * Typically, we simplify non-exact expressions if possible.
    * There is no compelling need to simplify exact expressions.
    *
    * @return an Expression tree which is the simpler equivalent of this.
    */
  def simplify: Expression = {
    val z = x.simplify
    if (z != x) Function(z, f)
    else this
  }

  /**
    * Action to materialize this Expression and render it as a String.
    *
    * @return a String representing the value of this expression.
    */
  def render: String = materialize.toString

  def layout(n: Int): String = s"$f:\n${Expression.indent(n)}${x.layout(n + 1)}\n"

  //  override def toString: String = s"\n${layout(0)}"
  override def toString: String = s"$f($x)"
}

/**
  * This class represents a dyadic function of the two given expressions.
  *
  * @param a the first expression being operated on.
  * @param b the second expression being operated on.
  * @param f the function to be applied to a and b.
  */
case class BiFunction(a: Expression, b: Expression, f: ExpressionBiFunction) extends CompositeExpression {

  /**
    * Method to determine if this Expression can be evaluated exactly.
    *
    * CONSIDER deal with each ExpressionFunction on an individual basis.
    *
    * @return false.
    */
  def isExact: Boolean = value.isExact

  /**
    * Method to determine the depth of this Expression.
    *
    * @return the depth (an atomic expression has depth of 1).
    */
  def depth: Int = 1 + math.max(a.depth, b.depth)

  /**
    * Action to materialize this Expression as a Number.
    *
    * @return the materialized Number.
    */
  def materialize: Number =
    if (isExact) value else {
      val simplified = simplify
      if (simplified == this) value
      else simplified.materialize
    }

  /**
    * If it is possible to simplify this Expression, then we do so.
    * Typically, we simplify non-exact expressions if possible.
    * There is no compelling need to simplify exact expressions.
    *
    * @return an Expression tree which is the simpler equivalent of this.
    */
  def simplify: Expression =
    if (isExact)
      value
    else {
      import Expression._
      em.simplifier(this) match {
        case em.Match(e) => e.asInstanceOf[Expression]
        case _ => this
      }
    }

  /**
    * Action to materialize this Expression and render it as a String.
    *
    * @return a String representing the value of this expression.
    */
  def render: String = materialize.render

  def layout(n: Int): String = s"$f:\n${Expression.indent(n)}${a.layout(n + 1)}\n${Expression.indent(n)}${b.layout(n + 1)}\n"

  /**
    * Render this BiFunction for debugging purposes.
    *
    * @return a String showing a, f, and b in parentheses.
    */
  // TODO tidy up
  //  override def toString: String = layout(0)
  override def toString: String = s"($b $f $a)"

  private def inverter(name: String): Option[(ExpressionBiFunction, Expression, Expression)] = name match {
    case "+" => Some((Product, MinusOne, Zero))
    case "*" => Some((Power, MinusOne, One))
    case _ => None
  }

  private def cancel(l: Expression, r: Expression, name: String): Option[Expression] = inverter(name) match {
    case Some((op, exp, result)) => r match {
      case BiFunction(a, b, f) if same(l, a) && b == exp && f == op => Some(result)
      case BiFunction(a, b, f) if a == exp && same(l, b) && f == op => Some(result)
      case _ => None
    }
    case None => None
  }

  /**
    * NOTE: we do a materialize here, which isn't right.
    * TODO fix it.
    */
  private def same(l: Expression, a: Expression) = a == l || a == l.materialize

  private def grouper(name: String): Option[(ExpressionBiFunction, ExpressionBiFunction)] = name match {
    case "^" => Some((Power, Product))
    case _ => None
  }

  private def gatherer(l: Expression, r: Expression, name: String): Option[Expression] = grouper(name) match {
    case Some((op, f)) => l match {
      case BiFunction(a, b, `op`) =>
        val function1 = BiFunction(b, r, f)
        val simplify1 = function1.simplify
        val function2 = BiFunction(a, simplify1, op)
        val simplify2 = function2.simplify
        Some(simplify2)
      case _ => None
    }
    case None => None
  }

  private def cancelOperands(l: Expression, r: Expression, name: String): Option[Expression] = (l, r, name) match {
    case (MinusOne, One, "+") | (One, MinusOne, "+") => Some(Zero)
    case _ => None
  }

  /**
    * For now, leaving this old simplify method which has been replaced by the matcher-based simplify method above.
    *
    * @return
    */
//noinspection ScalaUnusedSymbol
private def oldSimplify: Expression = {
    val left = a.simplify
    val right = b.simplify
    val result = f.name match {
      case "+" if left == Zero => right
      case "*" if left == One => right
      case "+" if right == Zero => left
      case "*" if right == One => left
      case "^" if right == One => left
      case "^" if right == Zero => One
      case _ =>
        // NOTE: we check only for gatherer left, right
        cancelOperands(left, right, f.name) orElse
                cancel(left, right, f.name) orElse
                cancel(right, left, f.name) orElse
                gatherer(left, right, f.name) getOrElse
                BiFunction(left, right, f)
    }
    // TODO remove this debugging aid
    if (result != this) println(s"simplified $this to $result")
    result
  }

  private lazy val value: Number = f(a.materialize, b.materialize)
}

case object Sine extends ExpressionFunction(x => x.sin, "sin")

case object Cosine extends ExpressionFunction(x => x.cos, "cos")

case object Log extends ExpressionFunction(x => x.log, "log")

case object Exp extends ExpressionFunction(x => x.exp, "exp")

case object Sum extends ExpressionBiFunction((x, y) => x add y, "+")

case object Product extends ExpressionBiFunction((x, y) => x multiply y, "*")

case object Power extends ExpressionBiFunction((x, y) => x.power(y), "^", false)

/**
  * A lazy monadic expression function.
  * TODO need to mark whether this function is exact or not.
  *
  * @param f    the function Number => Number
  * @param name the name of this function.
  */
class ExpressionFunction(f: Number => Number, name: String) extends (Number => Number) {
  /**
    * Evaluate this function on x.
    *
    * @param x the parameter to the function.
    * @return the result of f(x).
    */
  override def apply(x: Number): Number = f(x)

  /**
    * Generate helpful debugging information about this ExpressionFunction.
    *
    * @return a String.
    */
  override def toString: String = s"$name"
}

/**
  * A lazy dyadic expression function.
  * TODO need to mark whether this function is exact or not.
  *
  * @param f        the function (Number, Number) => Number
  * @param name     the name of this function.
  * @param commutes true only if the parameters to f are commutative.
  */
class ExpressionBiFunction(val f: (Number, Number) => Number, val name: String, val commutes: Boolean = true) extends ((Number, Number) => Number) {
  /**
    * Evaluate this function on x.
    *
    * @param a the first parameter to the function.
    * @param b the second parameter to the function.
    * @return the result of f(x).
    */
  override def apply(a: Number, b: Number): Number = f(a, b)

  /**
    * Generate helpful debugging information about this ExpressionFunction.
    *
    * @return a String.
    */
  override def toString: String = s"$name"
}

object ExpressionBiFunction {
  def unapply(f: ((Number, Number)) => Number): Option[((Number, Number) => Number, String)] = f match {
    case e: ExpressionBiFunction => Some(e.f, e.name)
    case _ => None
  }
}