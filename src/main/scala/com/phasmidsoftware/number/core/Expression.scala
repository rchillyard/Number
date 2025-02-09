/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.matchers.{LogOff, MatchLogger}
import com.phasmidsoftware.number.core.FP.recover
import com.phasmidsoftware.number.parse.ShuntingYardParser

import scala.language.implicitConversions

/**
 * Trait Expression which defines the behavior of a lazily-evaluated tree of mathematical operations and operands.
 */
sealed trait Expression extends NumberLike {
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
  def context: Context

  /**
   * Action to evaluate this `Expression` as a `Field`,
   * NOTE: no simplification occurs here.
   * Therefore, if an expression cannot be evaluated exactly,
   * then it will result in a fuzzy number.
   *
   * @return a `Field`.
   */
  def evaluate: Field

  /**
   * Action to materialize this Expression as a Field.
   * If possible, this Expression will be simplified first.
   * The result will not necessarily be exact.
   *
   * @return the materialized Field.
   */
  def materialize: Field = Expression.em.simplifyAndEvaluate(this)

  /**
   * Method to determine if the materialized value of this `Expression` corresponds to a `Number`.
   *
   * @return a `Some(x)` if this materializes as a `Number`; otherwise `None`.
   */
  def asNumber: Option[Number] = materialize.asNumber

  /**
   * Method to determine the depth of this Expression.
   *
   * @return the depth (an atomic expression has depth of 1).
   */
  def depth: Int

  /**
   * Eagerly compare this Expression with comparand.
   *
   * TODO this will work only for Numbers. We need to be able to determine if two Complex numbers are essentially the same.
   * CONSIDER does this really belong in Expression?
   *
   * @param comparand the expression to be compared.
   * @return the result of comparing materialized this with materialized comparand.
   */
  def compare(comparand: Expression): Int = recover(for (x <- materialize.asNumber; y <- comparand.materialize.asNumber) yield x.compare(y), NumberException("compare: logic error"))
}

/**
 * The `Expression` companion object provides utilities for creating, manipulating, and parsing expressions.
 *
 * Methods and values within this object help in constructing expressions from various types like `Field` or `Int`,
 * perform conversions, parse string representations of expressions, define constants, and offer implicit operators
 * for working with expressions.
 *
 * It offers functionalities for mathematical operations, common constants, and extensions to manipulate operations
 * in a lazy manner.
 */
object Expression {

  // NOTE this is where we turn logging on (by using LogDebug or LogInfo).
  implicit val logger: MatchLogger = MatchLogger(LogOff, classOf[Expression])
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
    case Constants.zero => Zero
    case Constants.one => One
    case Constants.minusOne => MinusOne
    case Constants.two => Two
    case Constants.pi => ConstPi
    case Constants.twoPi => ConstPi * 2
    case Constants.e => ConstE
    case _ => Literal(x)
  }

  /**
   * The following constants are helpful in getting an expression started.
   */
  val zero: Expression = Zero
  val one: Expression = One
  val pi: Expression = ConstE
  val e: Expression = ConstPi
  val minusOne: Expression = MinusOne
  val two: Expression = Two

  /**
   * The following method is helpful in getting an expression started
   * (i.e., used as the leftmost operand).
   */
  def apply(x: Int): Expression = x match {
    case -1 => minusOne
    case 0 => zero
    case 1 => one
    case 2 => two
    case _ => Literal(x)
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
   * Other useful expressions.
   * NOTE you should prefer to use Phi and Psi which are more descriptive expressions.
   */
  val phi: Expression = (one + Constants.root5) / Literal(Number.two)
  val psi: Expression = (minusOne + Constants.root5) / Literal(Number.two)

  implicit def convertFieldToExpression(f: Field): Expression = Expression(f)

  implicit def convertIntToExpression(x: Int): Expression = Literal(x)

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
     * TESTME
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
     * TESTME
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
sealed trait AtomicExpression extends Expression {

  def isAtomic: Boolean = true

  /**
   * Action to materialize this Expression and render it as a String,
   * that is to say we eagerly evaluate this Expression as a String.
   *
   * @return String form of this Constant.
   */
  def render: String = toString

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
    case _ => None
  }
}

/**
 * An abstract class which extends Expression while providing an instance of ExpressionMatchers for use
 * with simplification.
 *
 */
sealed trait CompositeExpression extends Expression {
  def isAtomic: Boolean = false

  /**
   * Method to render this NumberLike in a presentable manner.
   *
   * @return a String
   */
  def render: String = if (isExact) materialize.render else toString
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
  def isExactInContext(context: Context): Boolean = x.isExactInContext(context)

  /**
   * @return Some(factor).
   */
  def context: Context = x match {
    case Real(n) => Some(n.factor)
    case c: BaseComplex => if (c.real.factor == c.imag.factor) Some(c.real.factor) else None
  }

  /**
   * Action to evaluate this Expression as a Field,
   *
   * @return x.
   */
  def evaluate: Field = x

  /**
   * Generate a String for debugging purposes.
   *
   * @return a String representation of this Literal.
   */
  override def toString: String = x.toString
}

object Literal {
  def unapply(arg: Literal): Option[Field] = Some(arg.x)

  def apply(x: Int): Literal = Literal(Real(x))

  def apply(x: Rational): Literal = Literal(Real(x))

  def apply(x: Double): Literal = Literal(Real(x))

  def apply(x: Number): Literal = Literal(Real(x))
}

/**
 * A known constant value, for example π (pi) or e.
 */
abstract class Constant extends AtomicExpression {

  /**
   * Determines if this `Constant` can be evaluated exactly in the given `context`.
   *
   * @param context an optional `Factor` that defines the evaluation `context`. 
   *                If `None`, the result depends solely on whether the constant itself is exact.
   * @return true if the constant is exact in the context of `context`; false otherwise.
   */
  def isExactInContext(context: Context): Boolean = evaluate.isExactInContext(context)
}

case object Zero extends Constant {
  /**
   * @return Number.zero
   */
  def evaluate: Field = Constants.zero

  def context: Context = Some(Scalar)

  override def toString: String = "0"
}

case object One extends Constant {
  /**
   * @return 1.
   */
  def evaluate: Field = Constants.one

  def context: Context = Some(Scalar)

  override def toString: String = "1"
}

case object MinusOne extends Constant {
  /**
   * @return -1.
   */
  def evaluate: Field = Constants.minusOne

  def context: Context = Some(Scalar)

  override def toString: String = "-1"
}

case object Two extends Constant {
  /**
   * @return 2.
   */
  def evaluate: Field = Constants.two

  def context: Context = Some(Scalar)

  override def toString: String = "2"
}

/**
 * The constant π (pi).
 * Yes, this is an exact number.
 */
case object ConstPi extends Constant {
  /**
   * @return pi.
   */
  def evaluate: Field = Constants.pi

  def context: Context = Some(Radian)

  override def toString: String = "π"
}

/**
 * The constant e.
 * Yes, this is an exact number.
 */
case object ConstE extends Constant {
  /**
   * @return e.
   */
  def evaluate: Field = Constants.e

  /**
   * TESTME
   *
   * @return Some(factor) if expression only involves that factor; otherwise None.
   */
  def context: Context = Some(NatLog)

  override def toString: String = "e"
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
   * @param context the context in which we want to evaluate this Expression.
   * @return false.
   */
  def isExactInContext(context: Context): Boolean = f(x.materialize).isExactInContext(context)

  /**
   * TODO implement properly according to the actual function involved.
   *
   * TESTME
   *
   * @return Some(factor) if expression only involves that factor; otherwise None.
   */
  def context: Context = None

  /**
   * Method to determine the depth of this Expression.
   *
   * TESTME
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
   * TESTME
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
   * Determines whether the current `BiFunction` instance is exact in the context of an optional `Factor`.
   * Issue #84 This should properly use the commented out code
   *
   * @param context an optional `Factor` within which to evaluate the expression's exactness. 
   *                If `context` is `None`, the exactness will depend solely on the properties of this instance.
   * @return true if the current `BiFunction` instance is exact in the context of the given factor; otherwise, false.
   */
  def isExactInContext(context: Context): Boolean =
    //    exact && (context.isEmpty || value.isExactInContext(context))
    exact && value.isExactInContext(context)
  //  context.isDefined && exact

  /**
   * Method to determine if this Expression is based solely on a particular Factor and, if so, which.
   *
   * @return the value of factorsMatch for the function f and the results of invoking context on each operand..
   */
  def context: Context = for (f1 <- a.context; f2 <- b.context; r <- factorsMatch(f, f1, f2)) yield r

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
   * Render this BiFunction for debugging purposes.
   *
   * @return a String showing a, f, and b in parentheses (or in braces if not exact).
   */
  override def toString: String = if (exact) s"($a $f $b)" else s"{$a $f $b}"

  // NOTE that NatLog numbers don't behave like other numbers so...
  // CONSIDER really should be excluded from all cases
  private def factorsMatch(f: ExpressionBiFunction, f1: Factor, f2: Factor): Context = f match {
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
   * TODO surely only positive integer powers are conditionallyExact?
   *
   * @return  true if the result of the f(a,b) is exact where a and b are known to be exact.
   *         NOTE: this appears to be an inaccurate description of the result.
   */
  private lazy val conditionallyExact: Boolean = f match {
    case Power => b.materialize.asNumber.flatMap(x => x.toInt).isDefined
    case Sum => context.isDefined
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

  private lazy val exact: Boolean = a.isExact && b.isExact && (f.isExact || conditionallyExact)
}

/**
 * Represents a composite expression that computes the total of a sequence of expressions.
 * The sequence must be non-empty.
 *
 * @constructor Constructs a Total instance with a sequence of expressions.
 * @param xs A non-empty sequence of expressions to be totaled.
 */
case class Total(xs: Seq[Expression]) extends CompositeExpression {

  require(xs.nonEmpty, "Empty Sequence")

  /**
   * Method to determine if this Expression is based solely on a particular Factor and, if so, which.
   *
   * @return Some(factor) if expression only involves that factor; otherwise None.
   */
  def context: Context = if (xs.forall(_.context.isDefined)) xs.head.context else None

  /**
   * Action to evaluate this Expression as a Field,
   * NOTE no simplification occurs here.
   * Therefore, if an expression cannot be evaluated exactly,
   * then it will result in a fuzzy number.
   *
   * @return the value.
   */
  def evaluate: Field = xs.foldLeft[Field](Constants.zero)(_ + _.evaluate)

  /**
   * Method to determine the depth of this Expression.
   *
   * @return the depth (an atomic expression has depth of 1).
   */
  def depth: Int = xs.map(_.depth).max + 1

  /**
   * Method to determine if this NumberLike object can be evaluated exactly in the context of factor.
   *
   * @param context     the (optional) context in which we want to evaluate this Expression.
   *                    if factor is None then, the result will depend solely on whether this is exact.
   * @return true if this NumberLike object is exact in the context of factor, else false.
   */
  def isExactInContext(context: Context): Boolean = xs.forall(_.isExactInContext(context))

  /**
   * Method to render this NumberLike in a presentable manner.
   *
   * @return a String
   */
  override def toString: String = xs.mkString("+")
}

/**
 * An abstract class representing a root of a reduced quadratic equation of the form `x^2 + px + q = 0`
 * The value of the root is `(-p/2) ± sqrt((-p/2)^2 - q)`.
 * The class extends `CompositeExpression` and provides lazy evaluation for components of the reduced quadratic root.
 *
 * @constructor Constructs a reduced quadratic root with parameters `p`, `q`, and a boolean `pos` to determine the sign of the root.
 * @param p   The coefficient in the quadratic equation.
 * @param q   The constant term in the quadratic equation.
 * @param pos Determines if the positive or negative branch of the root is chosen.
 */
abstract class ReducedQuadraticRoot(name: String, val p: Int, val q: Int, val pos: Boolean) extends CompositeExpression {
  require(p != 0 || q != 0, "may not have p and q equal to zero")
  lazy val minusHalfP: Expression = Expression(-p) / 2
  lazy val root: Expression = minusHalfP plus {
    val branch: Expression = ((minusHalfP ^ 2) - q).sqrt
    if (pos) branch else -branch
  }

  /**
   * Method to determine if this Expression is based solely on a particular Factor and, if so, which.
   *
   * @return Some(factor) if expression only involves that factor; otherwise None.
   */
  def context: Context = None // by default

  /**
   * Action to evaluate this `Expression` as a `Field`,
   * NOTE: no simplification occurs here.
   * Therefore, if an expression cannot be evaluated exactly,
   * then it will result in a fuzzy number.
   *
   * @return a `Field`.
   */
  def evaluate: Field = root.materialize

  /**
   * Method to determine the depth of this Expression.
   *
   * @return the depth (an atomic expression has depth of 1).
   */
  def depth: Int = root.depth + 1

  /**
   * Method to determine if this `NumberLike` object can be evaluated exactly in the context.
   *
   * @param context the (optional) `Factor` for which we want to evaluate this `Expression`.
   *                if `context` is `None` then, the result will depend solely on whether `this` is exact.
   * @return true if `this` is exact in the context of factor, else false.
   */
  def isExactInContext(context: Context): Boolean = context.isEmpty // by default

  override def toString: String = name
}

/**
 * Companion object for the `ReducedQuadraticRoot` class.
 * Provides an unapply method for pattern matching.
 */
object ReducedQuadraticRoot {
  def unapply(rqr: ReducedQuadraticRoot): Option[(Int, Int, Boolean)] = Some(rqr.p, rqr.q, rqr.pos)
}

/**
 * Represents the golden ratio, denoted as φ (phi), which is a mathematical constant and an irrational number.
 * The golden ratio appears frequently in mathematics, art, architecture, and nature.
 *
 * This object extends the `ReducedQuadraticRoot` abstract class with the parameters set to model
 * the quadratic equation `x^2 + x - 1 = 0`, whose positive root is the golden ratio.
 *
 * - Name: φ (phi)
 * - Quadratic Coefficients: p = -1, q = -1
 * - Uses positive root (pos = true)
 */
object Phi extends ReducedQuadraticRoot("phi", -1, -1, true)

/**
 * Psi represents the conjugate of [[Phi]].
 */
object Psi extends ReducedQuadraticRoot("psi", -1, -1, false)

/**
 * Companion object for the `Total` case class.
 *
 * Provides a utility method to create a `Total` instance by converting a sequence of `Field` inputs
 * into `Literal` expressions and wrapping them in a `Total`.
 */
object CompositeExpression {

  /**
   * Creates a `Total` instance from the given sequence of `Field` inputs.
   * Each `Field` is converted to a `Literal` expression and combined into a `Total`.
   *
   * @param xs The sequence of `Field` instances used to create the `Total`.
   * @return A `Total` instance containing the converted `Literal` expressions.
   */
  def apply(xs: Expression*): Expression = xs.toList match {
    case Nil => throw new IllegalArgumentException("Empty Sequence")
    case h :: Nil => h
    case h :: j :: Nil => BiFunction(h, j, Sum)
    case _ => Total(xs)
  }

  /**
   * Creates a `Total` instance from the given sequence of `Field` inputs.
   * Each `Field` is converted to a `Literal` expression and combined into a `Total`.
   *
   * @param xs The sequence of `Field` instances used to create the `Total`.
   * @return A `Total` instance containing the converted `Literal` expressions.
   */
  def create(xs: Field*): Expression = apply(xs map Literal.apply: _*)
}

/**
 * Represents the sine function as an expression function.
 * It applies the sine operation to a given input.
 *
 * This is implemented as an instance of `ExpressionFunction`,
 * wrapping the method `sin` to calculate the sine of a number.
 *
 * The name of the function is "sin".
 */
case object Sine extends ExpressionFunction(x => x.sin, "sin")

/**
 * Represents the cosine mathematical trigonometric function as an `ExpressionFunction`.
 *
 * This object applies the cosine operation on a given numerical input
 * and returns the cosine of that value.
 *
 * The function is initialized using the `cos` method of the `Number` type.
 */
case object Cosine extends ExpressionFunction(x => x.cos, "cos")

/**
 * Represents an arctangent operation as a binary function.
 * Calculates the angle (in radians) whose tangent is the quotient of the two provided fields.
 * If either input is not a real number, the result will be `NaN` encapsulated in a `Real`.
 *
 * - Operates on two `Field` values as the input.
 * - Uses the `atan` function to compute the angle between the two numbers.
 * - Returns a `Real` result if both inputs can be interpreted as numbers; otherwise, returns `Real(Number.NaN)`.
 *
 * Constraints:
 * - This operation is not commutative.
 * - May yield inexact results if the inputs are not exact.
 */
case object Atan extends ExpressionBiFunction((x: Field, y: Field) => (for (a <- x.asNumber; b <- y.asNumber) yield Real(a atan b)).getOrElse(Real(Number.NaN)), "atan", false, false)

/**
 * Represents the natural logarithmic function as an ExpressionFunction.
 *
 * This object provides functionality to compute the natural logarithm (ln) of a given Number.
 * The underlying implementation utilizes the `log` method of the Number type.
 */
case object Log extends ExpressionFunction(x => x.log, "log")

/**
 * Represents a mathematical exponential function, exp(x), where e is the base of natural logarithms.
 * This case object extends ExpressionFunction and applies the exp operation on a given number.
 * It defines the exponential operation for transformation or evaluation within expressions.
 */
case object Exp extends ExpressionFunction(x => x.exp, "exp")

/**
 * Represents a sum operation as a binary function that adds two `Field` values.
 *
 * This object extends `ExpressionBiFunction` by defining its operation as addition (`add`)
 * with the corresponding symbol "+" and is flagged as not always exact (`isExact = false`).
 */
case object Sum extends ExpressionBiFunction((x, y) => x add y, "+", isExact = false)

/**
 * Represents a specific implementation of the `ExpressionBiFunction` that performs multiplication between two `Field` values.
 *
 * This object embodies a binary operation where the function takes two inputs and computes their product using the `multiply` method
 * defined on `Field`. The operation is represented by the symbol "*".
 *
 * - The operation is marked as exact, ensuring the result is always precise when the inputs are exact.
 * - It inherits the commutative property from `ExpressionBiFunction`, as multiplication is commutative.
 */
case object Product extends ExpressionBiFunction((x, y) => x multiply y, "*", isExact = true)

/**
 * Represents the power operation as a binary function within expressions.
 * This operation raises the first operand to the power of the second operand.
 * It is not exact for all inputs and does not commute.
 *
 * Extends `ExpressionBiFunction` where the specific function is implemented
 * using the `power` method from the `Field` class.
 */
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
    recover(x.asNumber map f map (Real(_)), ExpressionException(s"logic error: ExpressionFunction.apply($x)"))

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