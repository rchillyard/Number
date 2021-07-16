package com.phasmidsoftware.number.core

import com.phasmidsoftware.matchers.{LogOff, MatchLogger}
import com.phasmidsoftware.number.core.Field.{convertToNumber, recover}
import com.phasmidsoftware.number.core.Number.negate
import com.phasmidsoftware.number.parse.ShuntingYardParser

/**
  * Trait Expression which defines the behavior of a lazily-evaluated tree of mathematical operations and operands.
  */
trait Expression {
  /**
    * Method to determine if this Expression cannot be simplified on account of it being atomic.
    *
    * @return true if this extends AtomicExpression
    */
  def isAtomic: Boolean

  /**
    * Method to determine if this Expression can be evaluated exactly.
    *
    * NOTE: the implementations of this don't always make perfect sense regarding maybeFactor.
    *
    * @return true if materialize will result in an ExactNumber, else false.
    */
  def isExact: Boolean

  /**
    * Method to determine if this Expression is based solely on a particular Factor and, if so, which.
    *
    * @return Some(factor) if expression only involves that factor; otherwise None.
    */
  def maybeFactor: Option[Factor]

  /**
    * If it is possible to simplify this Expression, then we do so.
    * Typically, we simplify non-exact expressions if possible.
    * There is no compelling need to simplify exact expressions.
    *
    * @return an Expression tree which is the simpler equivalent of this.
    */
  def simplify: Expression

  /**
    * Action to materialize this Expression as a Field,
    * that is to say we eagerly evaluate this Expression as a Field.
    *
    * @return the materialized Field.
    */
  def materialize: Field

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

  /**
    * Eagerly compare this Expression with comparand.
    *
    * TODO this will work only for Numbers. We need to be able to determine if two Complex numbers are essentially the same.
    *
    * @param comparand the expression to be compared.
    * @return the result of comparing materialized this with materialized comparand.
    */
  def compare(comparand: Expression): Int = recover(for (x <- materialize.asNumber; y <- comparand.materialize.asNumber) yield x.compare(y), NumberException("compare: logic error"))
}

object Expression {

  // NOTE this is where we turn logging on (by using LogDebug or LogInfo).
  implicit val logger: MatchLogger = MatchLogger(LogOff, classOf[Expression])
  implicit val em: ExpressionMatchers = new ExpressionMatchers {}

  /**
    * The following method is helpful in getting an expression from a Field.
    */
  def apply(x: Field): Expression = x match {
    case n@Number(_, _) => n
    case c@Complex(_, _) => c
    case _ => throw NumberException(s"logic error: $x is not a Number or a Complex")
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
      * Method to lazily multiply x by y.
      *
      * @param y another Expression.
      * @return an Expression which is the lazy product of x and y.
      */
    def plus(y: Expression): Expression = BiFunction(x, y, Sum)

    /**
      * Method to lazily multiply x by y.
      *
      * @param y another Expression.
      * @return an Expression which is the lazy product of x and y.
      */
    def +(y: Expression): Expression = x plus y

    /**
      * Method to lazily multiply x by y.
      *
      * @param y a Number.
      * @return an Expression which is the lazy product of x and y.
      */
    def +(y: Number): Expression = this.+(Expression(y))

    /**
      * Method to lazily multiply x by y.
      *
      * @param y a Field.
      * @return an Expression which is the lazy product of x and y.
      */
    def +(y: Field): Expression = this.+(Expression(y))

    /**
      * Method to lazily add x to y.
      *
      * @param y an Int.
      * @return an Expression which is the lazy product of x and y.
      */
    def +(y: Int): Expression = this.+(Number(y))

    /**
      * Method to lazily subtract the Number y from x.
      *
      * @param y a Number.
      * @return an Expression which is the lazy product of x and y.
      */
    def -(y: Number): Expression = BiFunction(x, -Expression(y), Sum)

    /**
      * Method to lazily subtract the Field y from x.
      *
      * @param y a Field.
      * @return an Expression which is the lazy product of x and y.
      */
    def -(y: Field): Expression = BiFunction(x, -Expression(y), Sum)

    /**
      * Method to lazily change the sign of this expression.
      *
      * @return an Expression which is this negated.
      */
    def unary_- : Expression = BiFunction(x, MinusOne, Product)

    /**
      * Method to lazily subtract y from x.
      *
      * @param y an Int.
      * @return an Expression which is the lazy product of x and y.
      */
    def -(y: Int): Expression = this.-(Number(y))

    /**
      * Method to lazily multiply x by y.
      *
      * @param y another Expression.
      * @return an Expression which is the lazy product of x and y.
      */
    def *(y: Expression): Expression = BiFunction(x, y, Product)

    /**
      * Method to lazily multiply x by y.
      *
      * @param y a Number.
      * @return an Expression which is the lazy product of x and y.
      */
    def *(y: Number): Expression = *(Expression(y))

    /**
      * Method to lazily multiply x by y.
      *
      * @param y a Field.
      * @return an Expression which is the lazy product of x and y.
      */
    def *(y: Field): Expression = *(Expression(y))

    /**
      * Method to lazily multiply x by y.
      *
      * @param y an Int.
      * @return an Expression which is the lazy product of x and y.
      */
    def *(y: Int): Expression = *(Number(y))

    /**
      * Method to lazily yield the reciprocal of x.
      *
      * @return an Expression representing the reciprocal of x.
      */
    def reciprocal: Expression = BiFunction(x, MinusOne, Power)

    /**
      * Method to lazily divide x by y.
      *
      * @param y a Number.
      * @return an Expression which is the lazy quotient of x and y.
      */
    def /(y: Number): Expression = *(Expression(y).reciprocal)

    /**
      * Method to lazily divide x by y.
      *
      * @param y a Field.
      * @return an Expression which is the lazy quotient of x and y.
      */
    def /(y: Field): Expression = *(Expression(y).reciprocal)

    /**
      * Method to lazily multiply x by y.
      *
      * @param y another Field.
      * @return an Expression which is the lazy product of x and y.
      */
    def /(y: Int): Expression = /(Number(y))

    /**
      * Method to lazily raise x to the power of y.
      *
      * @param y the power to which x should be raised (an Expression).
      * @return an Expression representing x to the power of y.
      */
    def ^(y: Expression): Expression = BiFunction(x, y, Power)

    /**
      * Method to lazily raise x to the power of y.
      *
      * @param y the power to which x should be raised (a Number).
      * @return an Expression representing x to the power of y.
      */
    def ^(y: Number): Expression = ^(Expression(y))

    /**
      * Method to lazily raise the Field x to the power of y.
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
    def atan(y: Expression): Expression = BiFunction(x, y, Atan)

    /**
      * Eagerly compare this expression with y.
      *
      * @param comparand the number to be compared.
      * @return the result of the comparison.
      */
    def compare(comparand: Expression): Int = x compare comparand
  }
}

/**
  * An Expression which cannot be further simplified.
  */
trait AtomicExpression extends Expression {

  def isAtomic: Boolean = true

  /**
    * @return this.
    */
  def simplify: Expression = this

  /**
    * @return 1.
    */
  def depth: Int = 1
}

/**
  * An abstract class which extends Expression while providing an instance of ExpressionMatchers for use
  * with simplification.
  *
  */
abstract class CompositeExpression extends Expression {
  def isAtomic: Boolean = false
}

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
    * @return Some(factor).
    */
  def maybeFactor: Option[Factor] = Some(x.factor)

  /**
    * Action to materialize this Expression as a Field,
    * that is to say we eagerly evaluate this Expression as a Field.
    *
    * @return x.
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
  def isExact: Boolean = maybeFactor.contains(Scalar)

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
    * Action to materialize this Expression as a Field,
    * that is to say we eagerly evaluate this Expression as a Field.
    *
    * @return Number.zero
    */
  def materialize: Number = Number.zero

  def maybeFactor: Option[Factor] = Some(Scalar)
}

case object One extends Constant {
  /**
    * Action to materialize this Expression as a Field,
    * that is to say we eagerly evaluate this Expression as a Field.
    *
    * @return 1.
    */
  def materialize: Number = Number.one

  def maybeFactor: Option[Factor] = Some(Scalar)
}

case object MinusOne extends Constant {
  /**
    * Action to materialize this Expression as a Field,
    * that is to say we eagerly evaluate this Expression as a Field.
    *
    * @return -1.
    */
  def materialize: Number = negate(Number.one)

  def maybeFactor: Option[Factor] = Some(Scalar)

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
    * Action to materialize this Expression as a Field,
    * that is to say we eagerly evaluate this Expression as a Field.
    *
    * @return pi.
    */
  def materialize: Number = Number.pi

  def maybeFactor: Option[Factor] = Some(Pi)
}

/**
  * The constant e.
  * Yes, this is an exact number.
  */
case object ConstE extends Constant {
  /**
    * Action to materialize this Expression as a Field,
    * that is to say we eagerly evaluate this Expression as a Field.
    *
    * @return e.
    */
  def materialize: Number = Number.e

  def maybeFactor: Option[Factor] = Some(E)
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
    * TODO implement properly according to the actual function involved.
    *
    * @return Some(factor) if expression only involves that factor; otherwise None.
    */
  def maybeFactor: Option[Factor] = None

  /**
    * Method to determine the depth of this Expression.
    *
    * @return the 1 + depth of x.
    */
  def depth: Int = 1 + x.depth

  /**
    * Action to materialize this Expression as a Field.
    *
    * @return the materialized Field.
    */
  def materialize: Field = f(x.simplify.materialize)

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
    * @return the value of exact which is based on a, b, and f.
    */
  def isExact: Boolean = exact && maybeFactor.isDefined && value.isExact

  /**
    * Method to determine if this Expression is based solely on a particular Factor and, if so, which.
    *
    * @return the value of factorsMatch for the function f and the results of invoking maybeFactor on each operand..
    */
  def maybeFactor: Option[Factor] = for (f1 <- a.maybeFactor; f2 <- b.maybeFactor; r <- factorsMatch(f, f1, f2)) yield r

  /**
    * Method to determine the depth of this Expression.
    *
    * @return the depth (an atomic expression has depth of 1).
    */
  def depth: Int = 1 + math.max(a.depth, b.depth)

  /**
    * Action to materialize this Expression as a Field.
    *
    * @return the materialized Field.
    */
  def materialize: Field =
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
    * TODO replace logic with something from Matchers (maybe?)
    *
    * @return an Expression tree which is the simpler equivalent of this.
    */
  def simplify: Expression =
    if (isExact) value
    else {
      import Expression._
      val z: em.MatchResult[Expression] = em.simplifier(this)
      z match {
        case em.Match(e) => e
        case _ => this
      }
    }

  /**
    * Action to materialize this Expression and render it as a String.
    *
    * @return a String representing the value of this expression.
    */
  def render: String = materialize.render

  /**
    * Render this BiFunction for debugging purposes.
    *
    * @return a String showing a, f, and b in parentheses (or in braces if not exact).
    */
  override def toString: String = if (exact) s"($a $f $b)" else s"{$a $f $b}"

  // TODO note that E numbers don't behave like other numbers so really should be excluded from all cases
  private def factorsMatch(f: ExpressionBiFunction, f1: Factor, f2: Factor): Option[Factor] = f match {
    case Sum if f1 == f2 && f1 != E =>
      Some(f1)
    case Product if f1 == f2 || f1 == Scalar || f2 == Scalar =>
      if (f1 == f2) Some(f1) else if (f2 == Scalar) Some(f1) else Some(f2)
    case Power if f2 == Scalar =>
      Some(f1)
    case _ =>
      None
  }

  private lazy val value: Field = f(a.materialize, b.materialize)

  /**
    * Determine if this dyadic expression has an exact result, according to f, the function.
    * NOTE that Product is always true, but it is possible that Sum or Power could be false.
    *
    * CONSIDER not checking a and b as exact because we wouldn't be here if they weren't.
    *
    * TODO refactor this method to remove all parameters since they are simply copies of the values in scope.
    *
    * @param f the function.
    * @param a first operand.
    * @param b second operand.
    * @return true if the result of the f(a,b) is exact.
    */
  def conditionallyExact(f: ExpressionBiFunction, a: Expression, b: Expression): Boolean = f match {
    case Power => b.materialize.asNumber.flatMap(x => x.toInt).isDefined
    case Sum => maybeFactor.isDefined
    case _ => false
  }

  private lazy val exact: Boolean = a.isExact && b.isExact && (f.isExact || conditionallyExact(f, a, b))
}

case object Sine extends ExpressionFunction(x => x.sin, "sin")

case object Cosine extends ExpressionFunction(x => x.cos, "cos")

case object Atan extends ExpressionBiFunction((x, y) => (for (a <- x.asNumber; b <- y.asNumber) yield a atan b).getOrElse(Number.NaN), "atan", false, false)

case object Log extends ExpressionFunction(x => x.log, "log")

case object Exp extends ExpressionFunction(x => x.exp, "exp")

case object Sum extends ExpressionBiFunction((x, y) => x add y, "+", isExact = false)

case object Product extends ExpressionBiFunction((x, y) => x multiply y, "*", isExact = true)

case object Power extends ExpressionBiFunction((x, y) => x.power(convertToNumber(y)), "^", isExact = false, commutes = false)

/**
  * A lazy monadic expression function.
  *
  * TODO need to mark whether this function is exact or not (but I can't think of many which are exact).
  *
  * TODO implement also for other fields than Numbers.
  *
  * @param f    the function Number => Number.
  * @param name the name of this function.
  */
class ExpressionFunction(val f: Number => Number, val name: String) extends (Field => Field) {
  /**
    * Evaluate this function on Field x.
    *
    * @param x the parameter to the function.
    * @return the result of f(x).
    */
  override def apply(x: Field): Field =
    recover(x.asNumber map f, ExpressionException(s"logic error: ExpressionFunction.apply($x)"))

  /**
    * Generate helpful debugging information about this ExpressionFunction.
    *
    * @return a String.
    */
  override def toString: String = s"$name"
}

object ExpressionFunction {
  def unapply(arg: ExpressionFunction): Option[(Number => Number, String)] = Some(arg.f, arg.name)
}

/**
  * A lazy dyadic expression function.
  *
  * @param f        the function (Field, Field) => Field
  * @param name     the name of this function.
  * @param isExact  true if this function is always exact, given exact inputs.
  * @param commutes true only if the parameters to f are commutative.
  */
class ExpressionBiFunction(val f: (Field, Field) => Field, val name: String, val isExact: Boolean, val commutes: Boolean = true) extends ((Field, Field) => Field) {
  /**
    * Evaluate this function on x.
    *
    * @param a the first parameter to the function.
    * @param b the second parameter to the function.
    * @return the result of f(x).
    */
  override def apply(a: Field, b: Field): Field = f(a, b)

  /**
    * Generate helpful debugging information about this ExpressionFunction.
    *
    * @return a String.
    */
  override def toString: String = s"$name"
}

object ExpressionBiFunction {
  def unapply(f: ((Field, Field)) => Field): Option[((Field, Field) => Field, String)] = f match {
    case e: ExpressionBiFunction => Some(e.f, e.name)
    case _ => None
  }
}

case class ExpressionException(str: String) extends Exception(str)