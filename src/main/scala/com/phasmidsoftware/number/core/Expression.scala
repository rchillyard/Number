/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.matchers.{LogOff, MatchLogger}
import com.phasmidsoftware.number.core.FP.recover
import com.phasmidsoftware.number.core.Number.negate
import com.phasmidsoftware.number.parse.ShuntingYardParser

import scala.language.implicitConversions

/**
 * Trait Expression which defines the behavior of a lazily evaluated tree of mathematical operations and operands.
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
   * CONSIDER eliminating this method.
   *
   * @return Some(factor) if expression only involves that factor; otherwise None.
   */
  def context: Context

  /**
   * Action to evaluate this `Expression` as a `Field`,
   * NOTE: no simplification occurs here.
   *
   * @return a `Field`.
   */
  def evaluate(context: Context): Field

  /**
   * Evaluates this `Expression` as a `Field` without requiring an explicit context.
   * This is a convenience method that assumes a default context of `None`.
   *
   * @return a `Field` representing the evaluated `Expression`.
   */
  def evaluateAsIs: Field = evaluate(None)

  /**
   * Materializes the Expression and evaluates it to a Field.
   * If the Expression is atomic, it is directly evaluated using the provided context.
   * Otherwise, the Expression is simplified and then evaluated.
   *
   * @param context the execution context for evaluating the Expression. Defaults to None.
   * @return a Field representing the evaluated Expression.
   */
  def materialize(context: Context): Field = this match {
    case x: AtomicExpression => x.evaluate(context)
    case e => Expression.em.simplifyAndEvaluate(e, context)
  }

  /**
   * Materializes the Expression and evaluates it to a Field.
   * This is a convenience method that leverages a default context of `None`.
   *
   * @return a Field representing the evaluated Expression.
   */
  def materialize: Field = materialize(None)

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
  def compare(comparand: Expression): Int =
    recover(for (x <- materialize.asNumber; y <- comparand.materialize.asNumber) yield x.compare(y), NumberException("compare: logic error"))
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
    case Constants.zero => Zero // TESTME (applies to all except default case)
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
    case 0 => zero // TESTME
    case 1 => one
    case 2 => two // TESTME
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

  // TODO have a cache of existing values
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
     * @return an Expression which is this negated.
     */
    def unary_- : Expression = Function(x, Negate)

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
     * NOTE we shouldn't really be trying to simplify here. That should be taken care of downstream.
     *
     * @return an Expression representing the reciprocal of x.
     */
    def reciprocal: Expression = Function(x, Reciprocal)

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
    def sqrt: Expression = x match {
      case z: AtomicExpression =>
        z.evaluateAsIs.asNumber match {
          case Some(q) => Literal(q.sqrt)
          case _ => x ^ Constants.half // TESTME
        }
      case _ => x ^ Constants.half
    }

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
     * Method to lazily get the value of atan2(x, y), i.e., if the result is z, then tan(z) = y/x.
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

  /**
   * Method to yield a function which can determine if a given expression is an identity for the provided binary function.
   *
   * @param f the binary function for which the identity property is to be checked.
   * @return a function that takes an `Expression` and returns true if the expression
   *         is an identity element for the binary function `f`, otherwise false.
   */
  def isIdentityFunction(f: ExpressionBiFunction): Expression => Boolean = Expression.isIdentity(f)

  /**
   * Determines if a given `Expression` is an identity element for a specified binary function.
   *
   * @param f the binary function for which the identity property is being checked. This includes metadata like
   *          optional identity elements, which are leveraged for comparison.
   * @param z the `Expression` to be tested as a potential identity element for the binary function `f`.
   * @return true if the evaluated result of `z` matches the optional left identity element of the binary function `f`,
   *         otherwise false.
   */
  def isIdentity(f: ExpressionBiFunction)(z: Expression): Boolean =
    f.maybeIdentityL contains z.evaluateAsIs
}

/**
 * An Expression which cannot be further simplified.
 */
sealed trait AtomicExpression extends Expression {

  /**
   * Indicates whether the expression is atomic.
   *
   * @return true, as this expression is atomic and cannot be further simplified.
   */
  def isAtomic: Boolean = true

  /**
   * Method to determine what `Factor`, if there is such, this `NumberLike` object is based on.
   *
   * @return an optional `Factor`.
   */
  def maybeFactor: Option[Factor] = evaluateAsIs.maybeFactor

  /**
   * Action to materialize this Expression and render it as a `String`,
   * that is to say, we eagerly evaluate this Expression as a `String`.
   *
   * @return `String` form of this `Constant`.
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
    case Literal(x, _) => Some(x) // NOTE we lose the name here.
    case c: Constant => Some(c.evaluateAsIs)
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
  /**
   * Indicates whether this expression is atomic.
   *
   * @return false as the default value, indicating the expression is not atomic.
   */
  def isAtomic: Boolean = false

  /**
   * Method to determine what `Factor`, if there is such, this `NumberLike` object is based on.
   *
   * @return an optional `Factor`.
   */
  def maybeFactor: Option[Factor] = context

  /**
   * Provides the terms that comprise this `CompositeExpression`.
   *
   * @return a sequence of `Expression` objects representing the individual terms of this `CompositeExpression`.
   */
  def terms: Seq[Expression]

  /**
   * Substitutes the terms of this `CompositeExpression` with the provided sequence of expressions.
   *
   * @param terms the sequence of `Expression` objects to replace the terms of this `CompositeExpression`.
   * @return a new `Expression` where the terms are substituted with the given sequence.
   */
  def substituteTerms(terms: Seq[Expression]): CompositeExpression

  /**
   * Method to render this NumberLike in a presentable manner.
   *
   * @return a String
   */
  def render: String = {
    val field = materialize
    if (field.isExactInContext(None)) field.render else toString
  }
}

/**
 * An AtomicExpression which represents a Number.
 *
 * @param x the Number.
 */
case class Literal(x: Field, maybeName: Option[String] = None) extends AtomicExpression {

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
  def evaluate(context: Context): Field = x

  /**
   * Generate a String for debugging purposes.
   *
   * @return a String representation of this Literal.
   */
  override def toString: String = maybeName getOrElse x.toString
}

/**
 * Companion object for the `Literal` class, providing factory methods and pattern matching support.
 */
object Literal {
  /**
   * Extracts a Field value from a Literal instance.
   *
   * @param arg the Literal instance to extract the Field from.
   * @return an Option containing the extracted Field, or None if extraction is not possible.
   */
  def unapply(arg: Literal): Option[(Field, Option[String])] = Some(arg.x, arg.maybeName)

  /**
   * Creates a Literal instance using an integer value.
   *
   * @param x the integer value to be used for constructing the Literal.
   * @return a Literal instance wrapping the provided integer value as a Real number.
   */
  def apply(x: Int): Literal = Literal(Real(x))

  /**
   * Creates a Literal instance from a Rational value.
   *
   * @param x the Rational value to be wrapped in a Literal
   * @return a Literal instance containing the given Rational value encapsulated in a Real
   */
  def apply(x: Rational): Literal = Literal(Real(x))

  /**
   * Creates a new Literal instance wrapping the given Double value.
   *
   * @param x the Double value to be wrapped in the Literal.
   * @return a Literal instance containing the provided Double value as a Real.
   */
  def apply(x: Double): Literal = Literal(Real(x)) // TESTME

  /**
   * Creates a Literal instance from a given number.
   *
   * @param x the number to convert into a Literal
   * @return a Literal instance representing the given number
   */
  def apply(x: Number): Literal = Literal(Real(x))
}

/**
 * A known constant value, for example, π (pi) or e.
 */
abstract class Constant extends AtomicExpression {

  /**
   * Determines if this `Constant` can be evaluated exactly in the given `context`.
   *
   * @param context an optional `Factor` that defines the evaluation `context`. 
   *                If `None`, the result depends solely on whether the constant itself is exact.
   * @return true if the constant is exact in the context of `context`; false otherwise.
   */
  def isExactInContext(context: Context): Boolean =
    evaluate(context).isExact
}

/**
 * Represents the mathematical constant zero.
 * This object extends the `Constant` class, providing implementation specific to zero.
 */
case object Zero extends Constant {
  /**
   * @return Number.zero
   */
  def evaluate(context: Context): Field = Constants.zero

  def context: Context = Some(PureNumber)

  override def toString: String = "0"
}

/**
 * Represents the mathematical constant 1/2 (half).
 *
 * This object is a specific implementation of the `Constant` trait
 * and is used to represent the value of half within mathematical expressions.
 *
 * It provides:
 * - An evaluation method that returns the predefined constant value of half.
 * - A context definition for the constant.
 * - A string representation of the constant.
 */
case object Half extends Constant {
  def evaluate(context: Context): Field = Constants.half

  def context: Context = Some(PureNumber)

  override def toString: String = "\u00BD"
}

/**
 * Represents the constant numeric value `1`.
 *
 * `One` is a case object that extends `Constant`, indicating that it is a well-defined,
 * immutable, and atomic mathematical value.
 */
case object One extends Constant {
  def evaluate(context: Context): Field = Constants.one

  def context: Context = Some(PureNumber)

  override def toString: String = "1"
}

/**
 * Represents the constant value -1.
 *
 * MinusOne is a specific instance of the `Constant` class, which evaluates to
 * the numerical value -1 regardless of the context.
 *
 * This constant can be used in mathematical expressions involving fields and
 * supports operations defined in the `Field` trait.
 */
case object MinusOne extends Constant {
  def evaluate(context: Context): Field = Constants.minusOne

  def context: Context = Some(PureNumber)

  override def toString: String = "-1"
}

/**
 * Represents the constant number 2 as a case object, extending the `Constant` class.
 *
 * This object evaluates to the numeric constant `2` within the given context.
 * It can be used in mathematical expressions and operations involving constants.
 */
case object Two extends Constant {
  def evaluate(context: Context): Field = Constants.two

  def context: Context = Some(PureNumber)

  override def toString: String = "2"
}

/**
 * ConstPi represents the mathematical constant π (pi) exactly.
 */
case object ConstPi extends Constant {
  def evaluate(context: Context): Field = Constants.pi

  /**
   * Provides the default context for the `ConstPi` class.
   *
   * @return the context associated with the mathematical constant π, specifically `Radian`
   */
  lazy val context: Context = Some(Radian)

  override def toString: String = "π"
}

/**
 * The constant e.
 * Yes, this is an exact number.
 */
case object ConstE extends Constant {
  def evaluate(context: Context): Field = Constants.e

  /**
   * Provides access to the default logarithmic context, which is `NatLog`.
   * This context is used for evaluations involving natural logarithmic
   * or exponential arithmetic, relating to the mathematical constant e.
   */
  lazy val context: Context = Some(NatLog)

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
  def isExactInContext(context: Context): Boolean =
    x.isExactInContext(context) && f.isExactInContext(context)(x)

  /**
   * Provides the terms that comprise this `CompositeExpression`.
   *
   * @return a sequence of `Expression` objects representing the individual terms of this `CompositeExpression`.
   */
  def terms: Seq[Expression] = Seq(x)

  /**
   * Substitutes the terms of this `CompositeExpression` with the provided sequence of expressions.
   *
   * @param terms the sequence of `Expression` objects to replace the terms of this `CompositeExpression`.
   * @return a new `Expression` where the terms are substituted with the given sequence.
   */
  def substituteTerms(terms: Seq[Expression]): CompositeExpression = copy(x = terms.head)

  /**
   * Retrieves the context associated with the expression `x` using the function `f`.
   * The context typically provides additional information or constraints necessary
   * for evaluating or manipulating the expression.
   */
  lazy val context: Context = f.context(x)

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
  def evaluate(context: Context): Field = f(x.materialize)
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
    exact && (context.isEmpty || context == this.context) // CONSIDER refactoring this expression

  /**
   * Method to determine if this Expression is based solely on a particular Factor and, if so, which.
   *
   * @return the value of factorsMatch for the function f and the results of invoking context on each operand.
   */
  lazy val context: Context = for (f1 <- a.context; f2 <- b.context; r <- factorsMatch(f, f1, f2)) yield r

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
  def evaluate(context: Context): Field = value

  /**
   * Provides the terms that comprise this `CompositeExpression`.
   *
   * @return a sequence of `Expression` objects representing the individual terms of this `CompositeExpression`.
   */
  def terms: Seq[Expression] = Seq(a, b)

  /**
   * Substitutes the terms of this `CompositeExpression` with the provided sequence of expressions.
   *
   * @param terms the sequence of `Expression` objects to replace the terms of this `CompositeExpression`.
   * @return a new `Expression` where the terms are substituted with the given sequence.
   */
  def substituteTerms(terms: Seq[Expression]): CompositeExpression = {
    val Seq(a, b) = terms
    BiFunction(a, b, f)
  }

  /**
   * Render this BiFunction for debugging purposes.
   *
   * @return a String showing a, f, and b in parentheses (or in braces if not exact).
   */
  override def toString: String = s"{$a $f $b}"

  /**
   * Determines the result of combining two factors (`f1` and `f2`) using a specific binary function (`f`) in a given context.
   * NOTE that NatLog numbers don't behave like other numbers so...
   * CONSIDER really should be excluded from all cases
   * NOTE also that there are now methods for NumberLike objects viz. canAdd, canMultiply, canPower.
   *
   * @param f  the binary function of type `ExpressionBiFunction` used to evaluate the combination of the factors.
   * @param f1 the first factor to be combined.
   * @param f2 the second factor to be combined.
   * @return a `Context` object containing the result of the combination, or `None` if the factors cannot be combined based on the function.
   */
  private def factorsMatch(f: ExpressionBiFunction, f1: Factor, f2: Factor): Context = f match {
    case Sum if f1 == f2 && f1 != NatLog =>
      Some(f1)
    case Product if f1.canMultiply(f2) => Some(f1)
    case Power if f2 == PureNumber =>
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
    case Product => true // TESTME
    case _ => false // TESTME
  }

  /**
   * Regular hashCode method.
   * TESTME
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
 * @constructor Constructs an Aggregate instance with a sequence of expressions.
 * @param xs A non-empty sequence of expressions to be totaled.
 */
case class Aggregate(function: ExpressionBiFunction, xs: Seq[Expression]) extends CompositeExpression {

  require(xs.nonEmpty, "Empty Sequence")

  /**
   * Determines the applicable context for the current aggregate expression based on the function type
   * and the contexts of its constituent expressions.
   *
   * For the `Sum` function, this method evaluates whether all sub-expressions are aligned to the same additive
   * context. If so, it returns that context; otherwise, it returns `None`.
   *
   * @return Some(context) if all sub-expressions align to the same additive context for `Sum`,
   *         or None if no uniform context exists or the function is not `Sum`.
   */
  lazy val context: Context = function match {
    case Sum =>
      val additives = for {x <- xs; c <- x.context} yield c.isAdditive
      val fo = if (additives.length == xs.length && (additives forall (_ == true))) xs.head.context else None
      if (xs.forall(_.context == fo)) fo else None
    case _ => None
  }

  /**
   * Action to evaluate this Expression as a Field,
   * NOTE no simplification occurs here.
   * Therefore, if an expression cannot be evaluated exactly,
   * then it will result in a fuzzy number.
   *
   * @return the value.
   */
  def evaluate(context: Context): Field = {
    val identity: Field = function.maybeIdentityL.getOrElse(Constants.zero) // NOTE should never require the default
    xs.map(e => e.evaluate(context)).foldLeft[Field](identity)(function)
  }

  /**
   * Provides the terms that comprise this `CompositeExpression`.
   *
   * @return xs.
   */
  def terms: Seq[Expression] = xs

  /**
   * Substitutes the terms of this `CompositeExpression` with the provided sequence of expressions.
   *
   * @param terms the sequence of `Expression` objects to replace the terms of this `CompositeExpression`.
   * @return a new `Expression` where the terms are substituted with the given sequence.
   */
  def substituteTerms(terms: Seq[Expression]): CompositeExpression = copy(xs = terms)

  /**
   * Method to determine the depth of this Expression.
   *
   * @return the depth (an atomic expression has depth of 1).
   */
  def depth: Int = xs.map(_.depth).max + 1

  /**
   * Method to determine if this `NumberLike` object can be evaluated exactly in the context of a `Factor`.
   *
   * @param context     the (optional) context in which we want to evaluate this `Expression`.
   *                    if `None`, the result will depend solely on whether this is exact.
   * @return true if this `NumberLike` object is exact in the context of factor, else false.
   */
  def isExactInContext(context: Context): Boolean = context == this.context && xs.forall(_.isExactInContext(context))

  /**
   * Method to render this NumberLike in a presentable manner.
   *
   * @return a String
   */
  override def toString: String = xs.mkString(function.toString)
}

/**
 * Companion object for the `Aggregate` class.
 */
object Aggregate {

  /**
   * Constructs an `Aggregate` expression that applies the `Sum` operation
   * to a variable number of input expressions.
   *
   * @param xs a sequence of `Expression` instances that will be aggregated by the `Sum` operation.
   * @return an `Expression` representing the sum of the input expressions.
   */
  def total(xs: Expression*): CompositeExpression = Aggregate(Sum, xs)

  /**
   * Constructs an `Aggregate` expression that applies the `Product` operation
   * to a variable number of input expressions.
   *
   * @param xs a sequence of `Expression` instances that will be aggregated by the `Product` operation.
   * @return an `Expression` representing the product of the input expressions.
   */
  def product(xs: Expression*): CompositeExpression = Aggregate(Product, xs)

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
  private lazy val minusHalfP: Expression = Expression(-p) / 2
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
  def evaluate(context: Context): Field = root.materialize

  /**
   * Provides the terms that comprise this `CompositeExpression`.
   *
   * @return a sequence of `Expression` objects representing the individual terms of this `CompositeExpression`.
   */
  def terms: Seq[Expression] = Nil

  /**
   * Substitutes the terms of this `CompositeExpression` with the provided sequence of expressions.
   * In fact, this method simply returns `this` and ignores `terms`.
   * TESTME
   *
   * @param terms the sequence of `Expression` objects to replace the terms of this `CompositeExpression`.
   * @return `this`.
   */
  def substituteTerms(terms: Seq[Expression]): CompositeExpression = this

  /**
   * Method to determine the depth of this Expression.
   * TESTME
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
 * Companion object for the `Aggregate` case class.
 *
 * Provides a utility method to create an `Aggregate` instance by converting a sequence of `Field` inputs
 * into `Literal` expressions and wrapping them in an `Aggregate`.
 */
object CompositeExpression {

  /**
   * Creates an `Aggregate` instance from the given sequence of `Field` inputs.
   * Each `Field` is converted to a `Literal` expression and combined into an `Aggregate`.
   *
   * @param xs The sequence of `Field` instances used to create the `Aggregate`.
   * @return An `Aggregate` instance containing the converted `Literal` expressions.
   */
  def apply(xs: Expression*): Expression = xs.toList match {
    case Nil => throw new IllegalArgumentException("Empty Sequence")
    case h :: Nil => h
    case h :: j :: Nil => BiFunction(h, j, Sum)
    case _ => Aggregate(Sum, xs)
  }

  /**
   * Creates a `Aggregate` instance from the given sequence of `Field` inputs.
   * Each `Field` is converted to a `Literal` expression and combined into a `Aggregate`.
   * TESTME
   *
   * @param xs The sequence of `Field` instances used to create the `Aggregate`.
   * @return An `Aggregate` instance containing the converted `Literal` expressions.
   */
  def create(xs: Field*): Expression = apply(xs map (x => Literal(x, None)): _*)
}

/**
 * Represents a trigonometric function, either sine or cosine, that is derived from the abstract `ExpressionFunction`.
 *
 * The `SineCos` class applies a specified trigonometric function (`sin` or `cos`) to an `Expression`.
 * The function to be applied is determined by the given `sine` boolean parameter.
 *
 * If `sine` is true, the sine (`sin`) function is applied, otherwise, the cosine (`cos`) function is applied.
 *
 * @param sine a boolean indicating whether the sine function should be used (`true` for sine, `false` for cosine).
 */
abstract class SineCos(sine: Boolean) extends ExpressionFunction(x => if (sine) x.sin else x.cos, if (sine) "sin" else "cos") {
  /**
   * Determines if the given expression is considered exact in the context provided.
   * TESTME
   *
   * @param context the context in which the evaluation is made.
   * @param x       the expression to be checked for exactness within the given context.
   * @return true if the expression is exact in the specified context; false otherwise.
   */
  def isExactInContext(context: Context)(x: Expression): Boolean = x match {
    case a: AtomicExpression =>
      a.evaluateAsIs match {
        // TODO add other cases
        case Constants.zero | Constants.pi | Constants.piBy2 => true
        case _ => false
      }
  }

  /**
   * Determines the context of the given expression based on its associated factor.
   *
   * If the factor of the expression is `Radian`, the context is set to `PureNumber`.
   * Otherwise, no specific context is assigned.
   *
   * @param x an instance of `Expression` whose context is to be determined.
   * @return the corresponding context as an instance of `Context`, or `None` if no specific context applies.
   */
  def context(x: Expression): Context = x.maybeFactor match {
    case Some(Radian) => Some(PureNumber)
    case Some(PureNumber) if x.evaluateAsIs.isZero => Some(PureNumber)
    case _ => None
  }
}

/**
 * Represents the sine trigonometric function.
 *
 * `Sine` is a case object of the `SineCos` abstract class with the `sine` parameter set to `true`,
 * which designates that the sine (`sin`) function is to be applied to expressions.
 *
 * It applies the sine function to a given mathematical expression and inherits all behavior
 * defined in the `SineCos` abstract class.
 */
case object Sine extends SineCos(true)

/**
 * Represents the cosine trigonometric function.
 *
 * `Cosine` is a specific instance of the `SineCos` class with `sine` set to `false`,
 * meaning it applies the cosine (`cos`) function to an `Expression`.
 */
case object Cosine extends SineCos(false)

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
case object Atan extends ExpressionBiFunction(
  Real.atan,
  "atan",
  false,
  None,
  None
)

/**
 * Represents the natural logarithmic function as an ExpressionFunction.
 *
 * This object provides functionality to compute the natural logarithm (ln) of a given Number.
 * The underlying implementation utilizes the `log` method of the Number type.
 */
case object Log extends ExpressionFunction(x => x.log, "log") {
  /**
   * Determines the context of a given `Expression` by evaluating if it is based on a pure scalar factor (e.g., `PureNumber`).
   *
   * @param x the `Expression` to analyze, which may or may not be associated with a specific factor.
   * @return a `Context` containing the factor if the `Expression` is based solely on `PureNumber`; otherwise, `None`.
   */
  def context(x: Expression): Context = x.maybeFactor match {
    case c@Some(PureNumber) => c
    case _ => None
  }

  /**
   * Determines if a given `Expression` is exact within a specified `Context`.
   *
   * @param context the `Context` in which the exactness of the `Expression` is evaluated.
   * @param x       the `Expression` to be evaluated for exactness within the given `context`.
   * @return `true` if the `Expression` is exact in the given `Context`; otherwise, `false`.
   */
  def isExactInContext(context: Context)(x: Expression): Boolean = x match {
    case a: AtomicExpression =>
      a.evaluateAsIs match {
        case Constants.e | Constants.zero | Constants.one => true
        case _ => false
      }
  }
}

/**
 * Represents a mathematical exponential function, exp(x), where e is the base of natural logarithms.
 * This case object extends ExpressionFunction and applies the exp operation on a given number.
 * It defines the exponential operation for transformation or evaluation within expressions.
 */
case object Exp extends ExpressionFunction(x => x.exp, "exp") {
  /**
   * Determines the context of the given expression, based on its factor.
   * If the expression has a factor that is a `PureNumber`, it returns that factor wrapped in `Some`.
   * Otherwise, it returns `None`.
   * TESTME
   *
   * @param x the expression whose context is to be determined
   * @return an optional context (`Some(PureNumber)` if the factor is a `PureNumber`; otherwise `None`)
   */
  def context(x: Expression): Context = x.maybeFactor match {
    case c@Some(PureNumber) => c
    case _ => None
  }
  /**
   * Determines whether the provided expression is exact within the given context.
   * CONSIDER do we really need context?
   *
   * @param context the context in which the expression is being evaluated for exactness
   * @param x       the expression to be checked for exactness
   * @return true if the expression is exact within the given context; otherwise false
   */
  def isExactInContext(context: Context)(x: Expression): Boolean = x match {
    case a: AtomicExpression => a.evaluateAsIs.isZero || a.evaluateAsIs.isUnity
    case _ => false // CONSIDER testing for the x to evaluate to exactly 0 or 1 (but such cases should already have been simplified)
  }
}

/**
 * Negate is a specific implementation of the ExpressionFunction that changes the sign of a numeric value.
 *
 * This object represents the mathematical negation operation ("-") applied to a numeric input.
 * It uses the `negate` method, which evaluates the negation of a given Number while handling specific cases
 * like imaginary numbers and converting non-pure factors to PureNumber form as necessary.
 *
 * The function is exact and operates lazily.
 */
case object Negate extends ExpressionFunction(x => negate(x), "-") {
  /**
   * Determines the context of the given expression based on its factor.
   * If the expression's factor is a scalar, it returns that factor as the context.
   * Otherwise, returns None as the context.
   *
   * @param x the expression whose context is to be determined
   * @return the context of the given expression as an Option[Scalar], or None if no scalar factor is found
   */
  def context(x: Expression): Context = x.maybeFactor match {
    case c: Option[Scalar] => c
    case _ => None // TESTME
  }
  /**
   * Determines whether the provided expression is exact within the given context.
   * TODO implement this correctly for `Root` and `Log` type numbers.
   *
   * @param context the context in which the expression is being evaluated for exactness
   * @param x       the expression to be checked for exactness
   * @return true if the expression is exact within the given context; otherwise false
   */
  def isExactInContext(context: Context)(x: Expression): Boolean = true
}

/**
 * `Reciprocal` is an `ExpressionFunction` representing the mathematical reciprocal operation.
 * The function takes a numeric input `x` and computes `1 / x`.
 * It is identified by the name "rec".
 *
 * The operation is performed lazily and adheres to the behavior defined in its parent class.
 */
case object Reciprocal extends ExpressionFunction(x => 1 / x, "rec") {
  /**
   * Determines the context of the provided expression based on its factor.
   *
   * @param x the expression whose context is to be determined.
   * @return the context of the expression, `Some(PureNumber)` if the factor is `PureNumber`; otherwise `None`.
   */
  def context(x: Expression): Context = x.maybeFactor match {
    case c@Some(PureNumber) => c
    case _ => None
  }
  /**
   * Determines whether the provided expression is exact within the given context.
   * TODO implement this correctly for `Root` and `Log` type numbers.
   * CONSIDER is this really true for all values? I don't think so!
   *
   * @param context the context in which the expression is being evaluated for exactness
   * @param x       the expression to be checked for exactness
   * @return true if the expression is exact within the given context; otherwise false
   */
  def isExactInContext(context: Context)(x: Expression): Boolean = true
}

/**
 * Represents a sum operation as a binary function that adds two `Field` values.
 *
 * This object extends `ExpressionBiFunction` by defining its operation as addition (`add`)
 * with the corresponding symbol "+" and is flagged as not always exact (`isExact = false`).
 */
case object Sum extends ExpressionBiFunction((x, y) => x add y, "+", isExact = false, Some(Constants.zero), maybeIdentityR = None)

/**
 * Represents a specific implementation of the `ExpressionBiFunction` that performs multiplication between two `Field` values.
 *
 * This object embodies a binary operation where the function takes two inputs and computes their product using the `multiply` method
 * defined on `Field`. The operation is represented by the symbol "*".
 *
 * - The operation is marked as exact, ensuring the result is always precise when the inputs are exact.
 * - It inherits the commutative property from `ExpressionBiFunction`, as multiplication is commutative.
 */
case object Product extends ExpressionBiFunction((x, y) => x multiply y, "*", isExact = true, Some(Constants.one), maybeIdentityR = None) // NOTE can create a fuzzy number

/**
 * Represents the power operation as a binary function within expressions.
 * This operation raises the first operand to the power of the second operand.
 * It is not exact for all inputs and does not commute.
 *
 * TODO check on Constants.zero as identityL.
 *
 * Extends `ExpressionBiFunction` where the specific function is implemented
 * using the `power` method from the `Field` class.
 */
case object Power extends ExpressionBiFunction((x, y) => x.power(y), "^", isExact = false, None, Some(Constants.one))

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
abstract class ExpressionFunction(val f: Number => Number, val name: String) extends (Field => Field) {

  /**
   * Determines the context of the given Expression instance. The context is typically used
   * to provide additional information or constraints for evaluating or manipulating the Expression.
   * This method is abstract and should be implemented in extending classes to define the
   * specific way the context is derived.
   *
   * @param x the Expression for which the context is to be determined
   * @return the Context associated with the given Expression
   */
  def context(x: Expression): Context

  /**
   * Determines whether the provided expression is exact within the given context.
   * CONSIDER do we really need context?
   *
   * @param context the context in which the expression is being evaluated for exactness
   * @param x       the expression to be checked for exactness
   * @return true if the expression is exact within the given context; otherwise false
   */
  def isExactInContext(context: Context)(x: Expression): Boolean

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
  override def toString: String = name
}

/**
 * The companion object for the ExpressionFunction class.
 * Provides utility methods for working with ExpressionFunction instances.
 */
object ExpressionFunction {
  /**
   * Extractor method for `ExpressionFunction`, enabling pattern matching.
   * TESTME ?
   *
   * @param arg the `ExpressionFunction` instance from which components are extracted.
   * @return an `Option` containing a tuple of the function `Number => Number` and the name `String` of the `ExpressionFunction`, or `None` if the input is null.
   */
  def unapply(arg: ExpressionFunction): Option[(Number => Number, String)] = Some(arg.f, arg.name)
}

/**
 * Represents a bi-functional operation on two `Field` arguments that returns a `Field` result.
 * Provides additional metadata such as the function's name, whether the function is exact,
 * and optional identity elements.
 *
 * CONSIDER changing `isExact` to a predicate based on two `Field` objects.
 *
 * @param f              the binary evaluation function to be applied to two `Field` arguments.
 * @param name           the name of the function, used for debugging and descriptive purposes.
 * @param isExact        a boolean indicating if the function is exact in its computations.
 * @param maybeIdentityL the optional left identity element for the function.
 *                       That's to say, `identityL f y` can be replaced by `y`.
 *                       If `None`, then the function has no identity value.
 * @param maybeIdentityR an optional right identity element for the function, if applicable.
 *                       That's to say, `x f identityR` can be replaced by `x`.
 *                       If `None`, then the function is commutative and the only identity
 *                       required is given by `identityL`.
 */
class ExpressionBiFunction(val f: (Field, Field) => Field, val name: String, val isExact: Boolean, val maybeIdentityL: Option[Field], val maybeIdentityR: Option[Field]) extends ((Field, Field) => Field) {
  /**
   * Evaluate this function on x.
   *
   * @param a the first parameter to the function.
   * @param b the second parameter to the function.
   * @return the result of f(x).
   */
  def apply(a: Field, b: Field): Field = f(a, b)

  /**
   * Generate helpful debugging information about this ExpressionFunction.
   *
   * @return a String.
   */
  override def toString: String = name
}

/**
 * Companion object for the `ExpressionBiFunction` class.
 *
 * Provides an extractor method to deconstruct `ExpressionBiFunction` instances
 * into their associated function and name.
 */
object ExpressionBiFunction {
  /**
   * Extracts the components of an `ExpressionBiFunction` instance.
   * TESTME ?? Not currently used.
   * CONSIDER returning other fields.
   *
   * @param f the binary function of type `((Field, Field)) => Field` to be matched and deconstructed.
   * @return an `Option` containing a tuple of the function `(Field, Field) => Field` and its associated name `String`
   *         if the input matches an `ExpressionBiFunction`, or `None` otherwise.
   */
  def unapply(f: ExpressionBiFunction): Option[((Field, Field) => Field, String, Option[Field], Option[Field])] = f match {
    case e: ExpressionBiFunction => Some(e.f, e.name, e.maybeIdentityL, e.maybeIdentityR)
    case _ => None
  }
}

/**
 * A custom exception that represents errors related to expressions.
 * TESTME (unused)
 *
 * @param str The error message providing details about the expression error.
 */
case class ExpressionException(str: String) extends Exception(str)