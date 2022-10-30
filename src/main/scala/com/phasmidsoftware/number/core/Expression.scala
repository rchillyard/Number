package com.phasmidsoftware.number.core

import com.phasmidsoftware.matchers.MatchLogger
import com.phasmidsoftware.number.core.Field.recover
import com.phasmidsoftware.number.parse.ShuntingYardParser

import scala.language.implicitConversions

/**
  * Trait Expression which defines the behavior of a lazily-evaluated tree of mathematical operations and operands.
  */
trait Expression extends NumberLike {
  /**
    * Method to determine if this Expression cannot be simplified on account of it being atomic.
    *
    * @return true if this extends AtomicExpression
    */
  def isAtomic: Boolean

  /**
    * Method to determine if this Expression is based solely on a particular Factor and, if so, which.
    *
    * @return Some(factor) if expression only involves that factor; otherwise None.
    */
  def maybeFactor: Option[Factor]

  /**
    * Action to evaluate this Expression as a Field,
    * NOTE no simplification occurs here.
    * Therefore, if an expression cannot be evaluated exactly,
    * then it will result in a fuzzy number.
    *
    * @return the value.
    */
  def evaluate: Field

  /**
    * Action to materialize this Expression as a Field.
    * If possible, this Expression will be simplified first.
    *
    * @return the materialized Field.
    */
  def materialize: Field = Expression.em.simplifyAndEvaluate(this)

  /**
    * Method to determine if this Expression corresponds to a real Number.
    *
    * @return a Some(x) if this is a Number; otherwise return None.
    */
  def asNumber: Option[Number] = materialize.asNumber

  /**
    * Method to determine the depth of this Expression.
    *
    * @return the depth (an atomic expression has depth of 1).
    */
  def depth: Int

  /**
    * Method to determine if this Expression evaluates to a Complex number.
    *
    * @return true if this Expression evaluates to a Complex number.
    */
  def isComplex: Boolean = evaluate.isComplex

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
  implicit val logger: MatchLogger = MatchLogger(com.phasmidsoftware.matchers.LogOff, classOf[Expression])
  implicit val em: ExpressionMatchers = new ExpressionMatchers {}

  //  trait LoggableExpression extends Loggable[Expression] {
  //    def toLog(t: Expression): String = t.render
  //  }
  //  implicit object LoggableExpression extends LoggableExpression
  //
  //  val flog = Flog[ExpressionMatchers]

  /**
    * The following method is helpful in getting an expression from a Field.
    *
    * CONSIDER improving the logic.
    */
  def apply(x: Field): Expression = x match {
    case Number.zero => Zero
    case Number.one => One
    case Number.negOne => MinusOne
    case Number.two => Two
    case Number.pi => ConstPi
    case Number.e => ConstE
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
  def parse(x: String): Option[Expression] = ShuntingYardParser.parseInfix(x).toOption flatMap (_.evaluate)

  /**
    * The following constants are helpful in getting an expression started.
    */
  val zero: Expression = Zero
  val one: Expression = One
  val pi: Expression = Expression(Number.pi)
  val e: Expression = Expression(Number.e)

  /**
    * Other useful expressions.
    */
  val phi: Expression = (one + Constants.root5) / Number.two

  implicit def convertFieldToExpression(f: Field): Expression = Expression(f)

  implicit def convertIntToExpression(x: Int): Expression = Number(x)

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
      * Method to lazily subtract the Field y from x.
      *
      * @param y a Field.
      * @return an Expression which is the lazy product of x and y.
      */
    def -(y: Expression): Expression = BiFunction(x, -y, Sum)

    /**
      * Method to lazily change the sign of this expression.
      *
      * @return an Expression which is this negated.
      */
    def unary_- : Expression = BiFunction(x, MinusOne, Product)

    /**
      * Method to lazily multiply x by y.
      *
      * @param y a Number.
      * @return an Expression which is the lazy product of x and y.
      */
    def *(y: Expression): Expression = BiFunction(x, y, Product)

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
      * @return an Expression which is the lazy quotient of x / y.
      */
    def /(y: Expression): Expression = *(y.reciprocal)

    /**
      * Method to lazily raise x to the power of y.
      *
      * @param y the power to which x should be raised (an Expression).
      * @return an Expression representing x to the power of y.
      */
    def ^(y: Expression): Expression = BiFunction(x, y, Power)

    /**
      * Method to lazily get the square root of x.
      *
      * @return an Expression representing the square root of x.
      */
    def sqrt: Expression = this ^ Literal(2).reciprocal

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

    /**
      * Method to lazily get the value of atan2(x, y), i.e. if the result is z, then tan(z) = y/x.
      *
      * @return an Expression representing atan2(x, y).
      */
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

  override def hashCode(): Int = materialize.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case x: AtomicExpression => materialize == x.materialize
    case _ => false
  }
}

object AtomicExpression {
  def unapply(arg: AtomicExpression): Option[Field] = arg match {
    case c: Complex => Some(c)
    case Literal(x) => Some(x)
    case c: Constant => Some(c.evaluate)
    case f: Field => Some(f)
//    case g: GeneralNumber => Some(g)
    case _ => None
  }
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
  * An AtomicExpression which represents a Number.
  *
  * @param x the Number.
  */
case class Literal(x: Field) extends AtomicExpression {

  /**
    * Method to determine if this Expression can be evaluated exactly.
    *
    * @return true if materialize will result in an ExactNumber, else false.
    */
  def isExact(maybeFactor: Option[Factor]): Boolean = x.isExact(maybeFactor)

  /**
    * @return Some(factor).
    */
  def maybeFactor: Option[Factor] = x match {
    case n: Number => Some(n.factor)
    case c: BaseComplex => if (c.real.factor == c.imag.factor) Some(c.real.factor) else None
  }

  /**
    * Action to evaluate this Expression as a Field,
    *
    * @return x.
    */
  def evaluate: Field = x

  /**
    * Action to materialize this Expression and render it as a String,
    * that is to say we eagerly evaluate this Expression as a String.
    *
    * @return a String representing the value of this expression.
    */
  def render: String = x.toString

  /**
    * Generate a String for debugging purposes.
    *
    * @return a String representation of this Literal.
    */
  override def toString: String = s"$x"
}

object Literal {
  def unapply(arg: Literal): Option[Field] = Some(arg.x)

  def apply(x: Int): Literal = Literal(Number(x))

  def apply(x: Rational): Literal = Literal(Number(x))

  def apply(x: Double): Literal = Literal(Number(x))
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
  def isExact(maybeFactor: Option[Factor]): Boolean = evaluate.isExact(maybeFactor)

  /**
    * Action to materialize this Expression and render it as a String,
    * that is to say we eagerly evaluate this Expression as a String.
    *
    * @return String form of this Constant.
    */
  def render: String = evaluate.render

  /**
    * Method to yield a String from this Constant.
    *
    * @return a String.
    */
  override def toString: String = render
}

case object Zero extends Constant {
  /**
    * @return Number.zero
    */
  def evaluate: Number = Number.zero

  def maybeFactor: Option[Factor] = Some(Scalar)
}

case object One extends Constant {
  /**
    * @return 1.
    */
  def evaluate: Number = Number.one

  def maybeFactor: Option[Factor] = Some(Scalar)
}

case object MinusOne extends Constant {
  /**
    * @return -1.
    */
  def evaluate: Number = Number.negate(Number.one)

  def maybeFactor: Option[Factor] = Some(Scalar)

  /**
    * Action to materialize this Expression and render it as a String,
    * that is to say we eagerly evaluate this Expression as a String.
    *
    * @return "1-".
    */
  override def render: String = "-1"
}

case object Two extends Constant {
  /**
    * @return 2.
    */
  def evaluate: Number = Number.two

  def maybeFactor: Option[Factor] = Some(Scalar)
}

/**
  * The constant π (pi).
  * Yes, this is an exact number.
  */
case object ConstPi extends Constant {
  /**
    * @return pi.
    */
  def evaluate: Number = Number.pi

  def maybeFactor: Option[Factor] = Some(Radian)
}

/**
  * The constant e.
  * Yes, this is an exact number.
  */
case object ConstE extends Constant {
  /**
    * @return e.
    */
  def evaluate: Number = Number.e

  /**
    * TEST me
    *
    * @return Some(factor) if expression only involves that factor; otherwise None.
    */
  def maybeFactor: Option[Factor] = Some(NatLog)
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
    * @param maybeFactor the context in which we want to evaluate this Expression.
    * @return false.
    */
  def isExact(maybeFactor: Option[Factor]): Boolean = f(x.materialize).isExact(maybeFactor)

  /**
    * TODO implement properly according to the actual function involved.
    *
    * TEST me
    *
    * @return Some(factor) if expression only involves that factor; otherwise None.
    */
  def maybeFactor: Option[Factor] = None

  /**
    * Method to determine the depth of this Expression.
    *
    * TEST me
    *
    * @return the 1 + depth of x.
    */
  def depth: Int = 1 + x.depth

  /**
    * Action to materialize this Expression as a Field.
    *
    * @return the materialized Field.
    */
  def evaluate: Field = f(x.materialize)

  /**
    * Action to materialize this Expression and render it as a String.
    *
    * TEST me.
    *
    * @return a String representing the value of this expression.
    */
  def render: String = materialize.toString

  /**
    * TEST me
    *
    * @return
    */
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
  def isExact(maybeFactor: Option[Factor]): Boolean = exact && value.isExact(maybeFactor)

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
  def evaluate: Field = value

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

  // TODO note that NatLog numbers don't behave like other numbers so really should be excluded from all cases
  private def factorsMatch(f: ExpressionBiFunction, f1: Factor, f2: Factor): Option[Factor] = f match {
    case Sum if f1 == f2 && f1 != NatLog =>
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
    * @return true if the result of the f(a,b) is exact.
    *         NOTE: this appears to be an inaccurate description of the result.
    */
  private lazy val conditionallyExact: Boolean = f match {
    case Power => b.materialize.asNumber.flatMap(x => x.toInt).isDefined
    case Sum => maybeFactor.isDefined
    case Product => true
    case _ => false
  }

  /**
    * Regular hashCode method.
    *
    * @return an Int depending on f, a, and b.
    */
  override def hashCode(): Int = java.util.Objects.hash(f, a, b)

  /**
    * An equals method which considers two BiFunctions, which are non-identical but symmetric, to be equal.
    *
    * @param obj the other object.
    * @return true if the values of these two expressions would be the same (without any evaluation).
    */
  override def equals(obj: Any): Boolean = obj match {
    case BiFunction(c, d, g) => f == g && (a == c && b == d | f != Power && a == d && b == c)
    case _ => false
  }

  private lazy val exact: Boolean = a.isExact(None) && b.isExact(None) && (f.isExact || conditionallyExact)
}

case object Sine extends ExpressionFunction(x => x.sin, "sin")

case object Cosine extends ExpressionFunction(x => x.cos, "cos")

case object Atan extends ExpressionBiFunction((x, y) => (for (a <- x.asNumber; b <- y.asNumber) yield a atan b).getOrElse(Number.NaN), "atan", false, false)

case object Log extends ExpressionFunction(x => x.log, "log")

case object Exp extends ExpressionFunction(x => x.exp, "exp")

case object Sum extends ExpressionBiFunction((x, y) => x add y, "+", isExact = false)

case object Product extends ExpressionBiFunction((x, y) => x multiply y, "*", isExact = true)

case object Power extends ExpressionBiFunction((x, y) => x.power(y), "^", isExact = false, commutes = false)

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