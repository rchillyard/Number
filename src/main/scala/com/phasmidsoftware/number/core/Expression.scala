/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.matchers.{LogOff, MatchLogger}
import com.phasmidsoftware.number.core.Expression.em.ExpressionTransformer
import com.phasmidsoftware.number.core.Expression.{em, matchSimpler}
import com.phasmidsoftware.number.core.FP.recover
import com.phasmidsoftware.number.core.Number.{convertInt, negate}
import com.phasmidsoftware.number.parse.ShuntingYardParser
import scala.language.implicitConversions

/**
  * Trait Expression which defines the behavior of a lazily evaluated tree of mathematical operations and operands.
  */
sealed trait Expression extends NumberLike with Approximatable {
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
    * Action to evaluate this `Expression` as a `Field`, if possible.
    * NOTE: no simplification or factor-based conversion occurs here.
    *
    * @return an optional `Field`.
    */
  def evaluate(context: Context): Option[Field]

  /**
    * Evaluates this `Expression` as a `Field` without requiring an explicit context.
    * This is a convenience method that assumes a default context of `None`.
    *
    * @return an optional `Field` representing the evaluated `Expression`.
    */
  def evaluateAsIs: Option[Field] =
    evaluate(None)

  /**
    * Simplifies this Expression by attempting to match it with a simpler form using the provided matcher.
    * If no simplification is possible, this Expression is returned as is.
    * NOTE: do NOT invoke this method recursively.
    *
    * @return the simplified Expression if a simplification is available; otherwise, returns this Expression.
    */
  def simplify: Expression =
    matchSimpler(this) getOrElse this

  /**
    * Materializes this `Expression` and evaluates it as a Field.
    * `Materialize` differs from `evaluate` as follows:
    * (1) first, the expression is simplified if possible;
    * (2) then, the expression is evaluated first with context `None`, then with `Some(PureNumber)`.
    *
    * @return a Field representing the evaluated Expression.
    */
  def materialize: Field =
    simplify.evaluate(None) orElse simplify.evaluate(Some(PureNumber)) getOrElse Real.NaN

  /**
    * Method to determine if the materialized value of this `Expression` is defined and corresponds to a `Number`.
    * CONSIDER replacing materialize with evaluateAsIs
    *
    * @return a `Some(x)` if this materializes as a `Number`; otherwise `None`.
    */
  def asNumber: Option[Number] =
    materialize.asNumber

  /**
    * Computes and returns an approximate numerical value for this expression.
    * The returned value is based on `asNumber`.
    *
    * @return if possible, returns a `Real` representing the approximation of this expression.
    */
  def approximation: Option[Real] =
    asNumber map (Real(_))

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
    recover(for (x <- asNumber; y <- comparand.asNumber) yield x.compare(y), NumberException("compare: logic error"))
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
    * Converts a given number into an Expression by wrapping it as a Real.
    *
    * @param x the number to be converted into an Expression
    * @return an Expression representing the input number
    */
  implicit def convert(x: Number): Expression =
    apply(Real(x))

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
  def parse(x: String): Option[Expression] =
    ShuntingYardParser.parseInfix(x).toOption flatMap (_.evaluate)

  /**
   * Other useful expressions.
   * NOTE you should prefer to use Phi and Psi which are more descriptive expressions.
   */
  val phi: Expression =
    (one + Constants.root5) / Literal(Number.two)
  val psi: Expression =
    (minusOne + Constants.root5) / Literal(Number.two)

  implicit def convertFieldToExpression(f: Field): Expression =
    Expression(f)

  // TODO have a cache of existing values
  implicit def convertIntToExpression(x: Int): Expression =
    Literal(x)

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
    def plus(y: Expression): Expression =
      BiFunction(x, y, Sum)

    /**
     * Method to lazily multiply x by y.
     *
     * @param y another Expression.
     * @return an Expression which is the lazy product of x and y.
     */
    def +(y: Expression): Expression =
      x plus y

    /**
     * Method to lazily subtract the Field y from x.
     *
     * @param y a Field.
     * @return an Expression which is the lazy product of x and y.
     */
    def -(y: Expression): Expression =
      BiFunction(x, -y, Sum)

    /**
     * Method to lazily change the sign of this expression.
     *
     * @return an Expression which is this negated.
     */
    def unary_- : Expression =
      Function(x, Negate)

    /**
     * Method to lazily multiply x by y.
     *
     * @param y a Number.
     * @return an Expression which is the lazy product of x and y.
     */
    def *(y: Expression): Expression =
      BiFunction(x, y, Product)

    /**
     * Method to lazily yield the reciprocal of x.
     *
     * NOTE we shouldn't really be trying to simplify here. That should be taken care of downstream.
     *
     * @return an Expression representing the reciprocal of x.
     */
    def reciprocal: Expression =
      Function(x, Reciprocal)

    /**
     * Method to lazily divide x by y.
     *
     * @param y a Number.
     * @return an Expression which is the lazy quotient of x / y.
     */
    def /(y: Expression): Expression =
      *(y.reciprocal)

    /**
     * Method to lazily raise x to the power of y.
     *
     * @param y the power to which x should be raised (an Expression).
     * @return an Expression representing x to the power of y.
     */
    def ^(y: Expression): Expression =
      BiFunction(x, y, Power)

    /**
     * Method to lazily get the square root of x.
     *
     * @return an Expression representing the square root of x.
     */
    def sqrt: Expression = x match {
      case z: AtomicExpression =>
        z.evaluateAsIs flatMap (_.asNumber) match {
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
    def sin: Expression =
      Function(x, Sine)

    /**
     * Method to lazily get the cosine of x.
     *
     * @return an Expression representing the cos(x).
     */
    def cos: Expression =
      Function(x, Cosine)

    /**
     * Method to lazily get the tangent of x.
     *
     * TESTME
     *
     * @return an Expression representing the tan(x).
     */
    def tan: Expression =
      sin * cos.reciprocal

    /**
     * Method to lazily get the natural log of x.
     *
     * @return an Expression representing the log of x.
     */
    def log: Expression =
      Function(x, Log)

    /**
     * Method to lazily get the value of e raised to the power of x.
     *
     * @return an Expression representing e raised to the power of x.
     */
    def exp: Expression =
      Function(x, Exp)

    /**
     * Method to lazily get the value of atan2(x, y), i.e., if the result is z, then tan(z) = y/x.
     *
     * @return an Expression representing atan2(x, y).
     */
    def atan(y: Expression): Expression =
      BiFunction(x, y, Atan)

    /**
     * Eagerly compare this expression with y.
     *
     * @param comparand the number to be compared.
     * @return the result of the comparison.
     */
    def compare(comparand: Expression): Int =
      x compare comparand
  }

  /**
   * Method to yield a function which can determine if a given expression is an identity for the provided binary function.
   *
   * @param f the binary function for which the identity property is to be checked.
   * @return a function that takes an `Expression` and returns true if the expression
   *         is an identity element for the binary function `f`, otherwise false.
   */
  def isIdentityFunction(f: ExpressionBiFunction): Expression => Boolean =
    Expression.isIdentity(f)

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
    f.maybeIdentityL == z.evaluateAsIs // CONSIDER comparing with maybeIdentityR if this is not true

  /**
    * Defines a `Matcher` for simplifying expressions by applying a set of rules
    * based on the type of the given expression.
    *
    * - If the expression is an `AtomicExpression`, it returns a miss, indicating that
    * atomic expressions cannot be further simplified.
    * - If the expression is a `CompositeExpression`, it applies component simplification
    * and attempts to simplify constant or composite parts of the expression.
    * - For unsupported expression types, it returns an error indicating that the type
    * of the expression cannot be handled.
    *
    * @return an `ExpressionTransformer` that defines how different types of expressions
    *         should be processed for simplification.
    */
  def matchSimpler: ExpressionTransformer = {
    case x: AtomicExpression =>
      em.Miss("matchSimpler: cannot be simplified", x)
    case x: CompositeExpression =>
      em.eitherOr(x.simplifyComponents, x.simplifyConstant | x.simplifyComposite)(x)
    case x =>
      em.Error(ExpressionException(s"matchSimpler unsupported expression type: $x"))
  }

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
  def maybeFactor: Option[Factor] =
    evaluateAsIs flatMap (_.maybeFactor)

  /**
    * Action to simplifyAndEvaluate this Expression and render it as a `String`,
   * that is to say, we eagerly evaluate this Expression as a `String`.
   *
   * @return `String` form of this `Constant`.
   */
  def render: String = toString

  /**
   * @return 1.
   */
  def depth: Int = 1
//
//  override def hashCode(): Int = materialize.hashCode()
//
//  override def equals(obj: Any): Boolean = obj match {
//    case x: AtomicExpression => materialize == x.materialize
//    case _ => false
//  }
}

/**
  * Companion object for the `AtomicExpression` trait.
  * Provides utility methods for working with instances of `AtomicExpression`.
  *
  * The `unapply` method enables pattern matching on `AtomicExpression` instances, providing an
  * optional `Field` as a result based on the type of the expression. The behavior of this method
  * varies for different subtypes of `AtomicExpression`, such as `Complex`, `FieldExpression`,
  * `Literal`, `Field`, `Noop`, and `ReducedQuadraticRoot`.
  *
  * Notes:
  * - For `Complex`, the method directly returns the `Complex` instance as a `Field`.
  * - For `FieldExpression` and `Literal`, the value of the field is returned, but the name
  * might be discarded during extraction.
  * - For `Field`, the method directly returns the field.
  * - For `ReducedQuadraticRoot`, the method attempts to evaluate it "as-is."
  * - For `Noop`, the method returns `None`.
  *
  * Considerations:
  * - The use of `Complex` and `Field` directly in the extraction may warrant re-evaluation.
  * - The potential loss of the name in `FieldExpression` and `Literal` is noted as a trade-off.
  */
object AtomicExpression {
  /**
    * Extracts an optional `Field` from an `AtomicExpression` instance based on its type.
    * This method provides a mechanism for pattern matching on subtypes of `AtomicExpression`,
    * returning a `Field` where applicable.
    *
    * @param arg the `AtomicExpression` instance from which the `Field` is to be extracted.
    *            This can be one of the subtypes such as `Complex`, `FieldExpression`, `Literal`,
    *            `Field`, `Noop`, or `ReducedQuadraticRoot`.
    * @return an `Option` containing the extracted `Field` if one can be determined based on the
    *         type of `arg`. Returns `None` if no `Field` can be extracted, e.g., in the case of `Noop`.
    */
  def unapply(arg: AtomicExpression): Option[Field] = arg match {
    case c: Complex => Some(c) // CONSIDER eliminate this?
    case FieldExpression(x, _) => Some(x) // NOTE we lose the name here.
    case Literal(x, _) => Some(x) // NOTE we lose the name here.
    case f: Field => Some(f) // CONSIDER eliminate this?
    case Noop => None
    case r@ReducedQuadraticRoot(_, _, _, _) => r.evaluateAsIs
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
    * Simplifies the components of this `CompositeExpression` using a matching mechanism to identify
    * and transform sub-expressions into simpler forms if possible.
    *
    * @return the result of the simplification attempt encapsulated in a `MatchResult`, which either contains
    *         a simplified `Expression` or indicates that no simplification was possible.
    */
  def simplifyComponents: em.AutoMatcher[Expression]

  /**
    * Simplifies constant sub-expressions within this `CompositeExpression`.
    * The method identifies and reduces sub-expressions that involve constant values
    * into a simpler or more compact form, if possible.
    *
    * @return an `em.AutoMatcher[Expression]` that encapsulates the simplification logic
    *         and either provides a simplified constant expression or indicates no simplification was possible.
    */
  def simplifyConstant: em.AutoMatcher[Expression] = em.Matcher[Expression, Expression]("simplifyConstant") {
    expression =>
      expression.evaluateAsIs match {
        case Some(f) => em.Match(Literal(f))
        case _ => em.Miss("matchSimpler: cannot be simplified", expression)
      }
  }

  /**
    * Attempts to simplify this `CompositeExpression` by applying matching mechanisms on its components.
    * It delegates the simplification logic to the defined matchers that aim to reduce the expression
    * into its simpler or optimized form, if possible.
    *
    * @return an `em.AutoMatcher[Expression]` encapsulating the logic to simplify the `CompositeExpression`.
    *         The result contains either the simplified `Expression` or indicates no further simplifications were possible.
    */
  def simplifyComposite: em.AutoMatcher[Expression]

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
  def render: String =
    (context, evaluate(context)) match {
      case (Some(f), Some(Real(x))) if f.canShow(x.nominalValue) => x.render
      case _ => toString
    }
}

/**
  * The `Noop` object is an atomic expression that represents a no-operation placeholder in an expression tree.
  * It cannot be evaluated, simplified, or associated with any specific factor. It is a concrete implementation
  * of the `AtomicExpression` trait.
  */
case object Noop extends AtomicExpression {

  val value: Field = throw new UnsupportedOperationException("Noop.value")

  /**
    * Method to determine if this Expression is based solely on a particular Factor and, if so, which.
    * CONSIDER eliminating this method.
    *
    * @return Some(factor) if expression only involves that factor; otherwise None.
    */
  def context: Context = None

  /**
    * Action to evaluate this `Expression` as a `Field`,
    * NOTE: no simplification occurs here.
    *
    * @return a `Field`.
    */
  def evaluate(context: Context): Option[Field] = throw new UnsupportedOperationException("Noop.evaluate")

  /**
    * Method to determine if this `NumberLike` object can be evaluated exactly in the given context.
    *
    * @param context the (optional) `Factor` for which we want to evaluate this `Expression`.
    *                if `context` is `None` then, the result will depend solely on whether `this` is exact.
    * @return true if `this` is exact in the context of factor, else false.
    */
  def isExactInContext(context: Context): Boolean = false

  /**
    * Computes and returns an approximate numerical value for this expression.
    *
    * @return a `Double` representing the approximation of this expression.
    */
  override def approximation: Option[Real] = None
}

/**
  * An AtomicExpression which represents a Number.
  *
  * @param x the Number.
  */
abstract class FieldExpression(val value: Field, val maybeName: Option[String] = None) extends AtomicExpression {

  /**
    * Method to determine if this Expression can be evaluated exactly.
    *
    * @return true if simplifyAndEvaluate will result in an ExactNumber, else false.
    */
  def isExactInContext(context: Context): Boolean =
    value.isExactInContext(context)

  /**
    * @return Some(factor).
    */
  def context: Context = value match {
    case Real(n) => Some(n.factor)
    case c: BaseComplex => if (c.real.factor == c.imag.factor) Some(c.real.factor) else None
  }

  /**
    * Action to evaluate this Expression as a Field,
    *
    * @return x.
    */
  def evaluate(context: Context): Option[Field] =
    Option.when(context.isEmpty || context == this.context)(value)

  /**
    * Attempts to approximate the current field expression as a Real number.
    *
    * @return Some(Real) if the field can be approximated as a Real number, otherwise None.
    */
  override def approximation: Option[Real] = value match {
    case r: Real => Some(r)
    case _ => None
  }

  /**
    * Generate a String for debugging purposes.
    *
    * @return a String representation of this Literal.
    */
  override def toString: String =
    maybeName getOrElse value.toString

  private def canEqual(other: Any): Boolean =
    other.isInstanceOf[FieldExpression]

  private def namesMatch(other: FieldExpression): Boolean =
    (maybeName, other.maybeName) match {
    case (Some(x), Some(y)) => x == y
    case _ => true
  }

  override def equals(other: Any): Boolean = other match {
    case that: FieldExpression =>
      that.canEqual(this) &&
          value == that.value &&
          namesMatch(that)
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(value, maybeName)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object FieldExpression {
  def unapply(f: FieldExpression): Option[(Field, Option[String])] = f match {
    case Literal(x, _) => Some((x, None))
    case z => Some((z.value, z.maybeName))
  }
}

/**
  * An AtomicExpression which represents a literal `Field`.
  *
  * @param value     the `Field`.
  * @param maybeName an optional name.
  */
case class Literal(override val value: Field, override val maybeName: Option[String] = None) extends FieldExpression(value, maybeName)

/**
 * Companion object for the `Literal` class, providing factory methods and pattern matching support.
 */
object Literal {
  /**
   * Extracts a Field value from a Literal instance.
    * CONSIDER this may never be invoked.
   *
   * @param arg the Literal instance to extract the Field from.
   * @return an Option containing the extracted Field, or None if extraction is not possible.
   */
  def unapply(arg: Literal): Option[(Field, Option[String])] =
    Some(arg.value, arg.maybeName)

  /**
   * Creates a Literal instance using an integer value.
   *
   * @param x the integer value to be used for constructing the Literal.
   * @return a Literal instance wrapping the provided integer value as a Real number.
   */
  def apply(x: Int): Literal =
    Literal(Real(x))

  /**
   * Creates a Literal instance from a Rational value.
   *
   * @param x the Rational value to be wrapped in a Literal
   * @return a Literal instance containing the given Rational value encapsulated in a Real
   */
  def apply(x: Rational): Literal =
    Literal(Real(x))

  /**
   * Creates a new Literal instance wrapping the given Double value.
   *
   * @param x the Double value to be wrapped in the Literal.
   * @return a Literal instance containing the provided Double value as a Real.
   */
  def apply(x: Double): Literal =
    Literal(Real(x)) // TESTME

  /**
   * Creates a Literal instance from a given number.
   *
   * @param x the number to convert into a Literal
   * @return a Literal instance representing the given number
   */
  def apply(x: Number): Literal =
    Literal(Real(x))
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
    evaluate(context).isDefined
}

/**
 * Represents the mathematical constant zero.
 * This object extends the `Constant` class, providing implementation specific to zero.
 */
case object Zero extends FieldExpression(Constants.zero)

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
case object Half extends FieldExpression(Constants.half, Some("\u00BD"))

/**
 * Represents the constant numeric value `1`.
 *
 * `One` is a case object that extends `Constant`, indicating that it is a well-defined,
 * immutable, and atomic mathematical value.
 */
case object One extends FieldExpression(Constants.one)

/**
 * Represents the constant value -1.
 *
 * MinusOne is a specific instance of the `Constant` class, which evaluates to
 * the numerical value -1 regardless of the context.
 *
 * This constant can be used in mathematical expressions involving fields and
 * supports operations defined in the `Field` trait.
 */
case object MinusOne extends FieldExpression(Constants.minusOne, Some("-1"))

/**
 * Represents the constant number 2 as a case object, extending the `Constant` class.
 *
 * This object evaluates to the numeric constant `2` within the given context.
 * It can be used in mathematical expressions and operations involving constants.
 */
case object Two extends FieldExpression(Constants.two)

/**
 * ConstPi represents the mathematical constant π (pi) exactly.
 */
case object ConstPi extends FieldExpression(Constants.pi, Some("π"))

/**
 * The constant e.
 * Yes, this is an exact number.
 */
case object ConstE extends FieldExpression(Constants.e)

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
  def substituteTerms(terms: Seq[Expression]): CompositeExpression =
    copy(x = terms.head)

  /**
   * Retrieves the context associated with the expression `x` using the function `f`.
   * The context typically provides additional information or constraints necessary
   * for evaluating or manipulating the expression.
   */
  lazy val context: Context =
    f.context(x)

  /**
   * Method to determine the depth of this Expression.
   *
   * @return the 1 + depth of x.
   */
  def depth: Int =
    1 + x.depth
  /**
    * Action to simplifyAndEvaluate this Expression as a Field.
   *
   * @return the materialized Field.
   */
  def evaluate(context: Context): Option[Field] = x match {
    case AtomicExpression(field) => f.applyExact(field)(context)
    case _ => x.evaluate(context) map f
  }
//  {
//    (f, x.evaluateAsIs) match {
//      case (Log, Some(Constants.e)) => Some(Constants.one)
//      case (Log, Some(Constants.one)) => Some(Constants.zero)
//      case (Log, Some(Constants.zero)) => Some(Constants.negInfinity)
//      case (Exp, Some(Constants.one)) => Some(Constants.e)
//      case (Exp, Some(Constants.zero)) => Some(Constants.one)
//      case (Exp, Some(Constants.negInfinity)) => Some(Constants.zero)
//      case (Negate, Some(Constants.one)) => Some(Constants.minusOne)
//      case (Negate, Some(Constants.zero)) => Some(Constants.zero)
//      case (Reciprocal, Some(Constants.two)) => Some(Constants.half)
//      case (Reciprocal, Some(Constants.half)) => Some(Constants.two)
//      case (Reciprocal, Some(Constants.one)) => Some(Constants.one)
//      case (Reciprocal, Some(Constants.zero)) => Some(Constants.infinity)
//      case (Reciprocal, Some(Constants.two)) => Some(Constants.half)
//      case (Reciprocal, Some(Constants.half)) => Some(Constants.two)
//      case _ => x.evaluate(context) map f
//    }
//  }

  /**
    * Provides an approximation of the result of applying the function `f` to the
    * `approximation` of the expression `x`, if one exists.
    *
    * @return an `Option` containing the approximated result as a `Real` if the
    *         approximation is successfully computed; otherwise, `None`.
    */
  override def approximation: Option[Real] =
    x.approximation map f match {
      case Some(r: Real) => Some(r)
      case _ => None
    }

  /**
    * Simplifies the components of this `Expression` by transforming it using the `matchSimpler`
    * expression transformer. The resulting transformed expression is used to create a copy
    * of the current instance with the updated components.
    *
    * @return an `em.AutoMatcher[Expression]` representing the transformation of the components
    *         of this `Expression` using the `matchSimpler` logic.
    */
  def simplifyComponents: em.AutoMatcher[Expression] =
    em.Matcher("Function: simplifyComponents") {
      case Function(x, _) =>
        val matcher = matchSimpler map (z => copy(x = z))
        matcher(x)
    }

  /**
    * Simplifies a composite `Expression` by attempting to match it with a simpler form.
    * This method applies specific rules to detect and handle cases where the composite expression
    * consists of functions that are complementary (e.g., exponential and logarithmic, negation and negation).
    * If no such simplification is possible, the method returns a miss case without modification to the input.
    *
    * @return An `em.AutoMatcher[Expression]` that will either match the simplified expression or
    *         indicate a miss if no simplification can be applied.
    */
  def simplifyComposite: em.AutoMatcher[Expression] =
    em.Matcher("simplifyComposite") {
      case Function(Function(x, f), g) if em.complementaryMonadic(f, g) => em.Match(x)
      case x: Expression => em.Miss[Expression, Expression]("Function.simplifyComposite: not complementary", x)
    }
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
    * Simplifies the components of this `CompositeExpression` using a matching mechanism to identify
    * and transform sub-expressions into simpler forms if possible.
    * NOTE: we always convert a BiFunction into an Aggregate before simplifying its components.
    * That's because we only want that simplification logic in one place and that place has to be Aggregate.
    *
    * @return the result of the simplification attempt encapsulated in a `MatchResult`, which either contains
    *         a simplified `Expression` or indicates that no simplification was possible.
    */
  def simplifyComponents: em.AutoMatcher[Expression] =
    em.Matcher("BiFunction: simplifyComponents") {
      case BiFunction(x, y, f) =>
        val matcher: em.Matcher[Seq[Expression], BiFunction] =
          em.sequence(matchSimpler) & em.lift { xs => val Seq(newX, newY) = xs; BiFunction(newX, newY, f) }
        matcher.apply(List[Expression](x, y))
    }

  def simplifyComposite: em.AutoMatcher[Expression] =
    asAggregate.simplifyComposite


  /**
    * Transforms this `BiFunction` expression into an `Aggregate` expression if certain patterns and conditions are met.
    *
    * Specifically:
    * - When nested `BiFunction` objects share the same binary function at all levels, they are aggregated into a single `Aggregate` expression with all the operands.
    * - If no valid aggregation pattern is matched, the original expression is returned unchanged.
    *
    * @return an `Expression` in the form of an `Aggregate` if the transformation is successful or the original expression otherwise.
    */
  def asAggregate: CompositeExpression = this match {
    case BiFunction(BiFunction(w, x, f), BiFunction(y, z, g), h) if f == g && g == h =>
      Aggregate(f, Seq(w, x, y, z))
    case BiFunction(BiFunction(w, x, f), y, h) if f == h =>
      Aggregate(f, Seq(w, x, y))
    case BiFunction(x, BiFunction(y, z, f), h) if f == h =>
      Aggregate(f, Seq(x, y, z))
    case _ =>
      Aggregate(f, Seq(a, b))
  }

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
    * Action to simplifyAndEvaluate this Expression as a Field.
   *
   * @return the materialized Field.
   */
  def evaluate(context: Context): Option[Field] =
    f.evaluate(a, b)(context)

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

  /**
    * Computes and returns an approximate numerical value for this expression.
    *
    * @return a `Double` representing the approximation of this expression.
    */
  override def approximation: Option[Real] =
    (for (x <- a.approximation; y <- b.approximation) yield f(x, y)) match {
      case Some(r: Real) => Some(r)
      case _ => None
    }

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
    case Power => b.asNumber.flatMap(x => x.toInt).isDefined
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

  /**
    * Simplifies the components of this `CompositeExpression` using a matching mechanism to identify
    * and transform sub-expressions into simpler forms if possible.
    *
    * @return the result of the simplification attempt encapsulated in a `MatchResult`, which either contains
    *         a simplified `Expression` or indicates that no simplification was possible.
    */

  def simplifyComponents: em.AutoMatcher[Expression] =
    em.Matcher("Aggregate: simplifyComponents") {
      case Aggregate(f, xs) =>
        val matcher: em.Matcher[Seq[Expression], Expression] =
          em.sequence(matchSimpler) & em.lift { ys => Aggregate(f, ys) }
        matcher(xs)
  }

  /**
    * Simplifies a composite `Expression` by leveraging the `simplifyAggregate` method.
    * This is typically used to reduce composite mathematical expressions into their simplest form when possible.
    *
    * @return an `AutoMatcher` for `Expression` that applies the simplification logic defined in `simplifyAggregate`.
    */
  def simplifyComposite: em.AutoMatcher[Expression] =
    em.simplifyAggregate

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
  def evaluate(context: Context): Option[Field] = {
    val identity: Field = function.maybeIdentityL.getOrElse(Constants.zero) // NOTE should never require the default
    val maybeFields: Seq[Option[Field]] = xs.map(e => e.evaluate(context))
    FP.sequence(maybeFields) map (xs => xs.foldLeft[Field](identity)(function))
  }

  /**
    * Adds a new expression to the current aggregate.
    *
    * @param x the expression to be added
    * @return a new Aggregate instance with the updated list of expressions
    */
  def add(x: Expression): Aggregate =
    copy(xs = xs :+ x)

  /**
    * Adds a sequence of expressions to the current aggregate.
    *
    * @param ys the sequence of expressions to be added
    * @return a new Aggregate instance with the updated list of expressions
    */
  def addAll(ys: Seq[Expression]): Aggregate =
    copy(xs = xs ++ ys)

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
  def substituteTerms(terms: Seq[Expression]): CompositeExpression =
    copy(xs = terms)

  /**
   * Method to determine the depth of this Expression.
   *
   * @return the depth (an atomic expression has depth of 1).
   */
  def depth: Int =
    xs.map(_.depth).max + 1

  /**
   * Method to determine if this `NumberLike` object can be evaluated exactly in the context of a `Factor`.
   *
   * @param context     the (optional) context in which we want to evaluate this `Expression`.
   *                    if `None`, the result will depend solely on whether this is exact.
   * @return true if this `NumberLike` object is exact in the context of factor, else false.
   */
  def isExactInContext(context: Context): Boolean =
    context == this.context && xs.forall(_.isExactInContext(context))

  /**
    * Computes and returns an approximate numerical value for this expression.
    *
    * @return a `Double` representing the approximation of this expression.
    */
  override def approximation: Option[Real] = {
    val identity: Field = function.maybeIdentityL.getOrElse(Constants.zero) // NOTE should never require the default
    val maybeFields: Seq[Option[Field]] = xs.map(e => e.approximation)
    FP.sequence(maybeFields) map (xs => xs.foldLeft[Field](identity)(function)) match {
      case Some(r: Real) => Some(r)
      case _ => None
    }
  }

  /**
   * Method to render this NumberLike in a presentable manner.
   *
   * @return a String
   */
  override def toString: String =
    xs.mkString(function.toString)
}

/**
 * Companion object for the `Aggregate` class.
 */
object Aggregate {

  /**
    * Creates an empty `Aggregate` instance using the provided binary function.
    * The resulting `Aggregate` will have an empty sequence of expressions.
    *
    * @param function the binary function used to compose the `Aggregate`.
    * @return an empty `Aggregate` instance.
    */
  def empty(function: ExpressionBiFunction): Aggregate =
    new Aggregate(function, Seq.empty)

  def create(function: ExpressionBiFunction, xs: Seq[Expression]): Aggregate =
    if (xs.nonEmpty)
      new Aggregate(function, xs)
    else
      throw new IllegalArgumentException("total requires at least one argument (use empty if necessary)")

  /**
   * Constructs an `Aggregate` expression that applies the `Sum` operation
   * to a variable number of input expressions.
   *
   * @param xs a sequence of `Expression` instances that will be aggregated by the `Sum` operation.
   * @return an `Expression` representing the sum of the input expressions.
   */
  def total(xs: Expression*): CompositeExpression =
    create(Sum, xs)

  /**
   * Constructs an `Aggregate` expression that applies the `Product` operation
   * to a variable number of input expressions.
   *
   * @param xs a sequence of `Expression` instances that will be aggregated by the `Product` operation.
   * @return an `Expression` representing the product of the input expressions.
   */
  def product(xs: Expression*): CompositeExpression =
    create(Product, xs)
}

/**
  * An abstract class representing a root of a reduced quadratic equation of the form `x^2 + px + q = 0`
  * The value of the root is `(-p/2) ± sqrt((-p/2)^2 - q)`.
  * The class extends `CompositeExpression` and provides lazy evaluation for components of the reduced quadratic root.
  *
  * NOTE that, for now at least, the types of p and q are Int.
  *
  * CONSIDER should we extend CompositeExpression instead? Then, all AtomicExpressions could have a value.
  *
  * @constructor Constructs a reduced quadratic root with parameters `p`, `q`, and a boolean `pos` to determine the sign of the root.
  * @param p   The coefficient in the quadratic equation.
  * @param q   The constant term in the quadratic equation.
  * @param pos Determines if the positive or negative branch of the root is chosen.
  */
class ReducedQuadraticRoot(val name: String, val p: Int, val q: Int, val pos: Boolean) extends AtomicExpression {
  require(p != 0 || q != 0, "may not have p and q equal to zero")
  require(discriminant >= 0, "discriminant must not be negative")

  lazy val root: Expression = {
    (Literal(-p) plus {
      // NOTE discriminant is required to be non-negative
      val branch: Expression = Literal(discriminant.sqrt)
      if (pos) branch else -branch
    }) / 2
  }

  /**
    * Converts this quadratic root expression into an optional `Number` if possible.
    * The result depends on the discriminant:
    * - If the discriminant is negative, no numeric representation exists, and `None` is returned.
    * - If the discriminant is zero, it represents a double root, and a single exact `Number` instance is returned.
    * - If the discriminant is positive, a (fuzzy) `Number` is calculated and returned.
    *
    * @return an `Option[Number]` representing the numeric equivalent of this quadratic root if calculable; otherwise, `None`.
    */
  override def asNumber: Option[Number] = {
    if (discriminant < 0) None
    else if (discriminant == 0) Some(Number(Rational(p, 2)))
    else Some(Number(discriminant, Root2).negateConditional(!pos) doSubtract Number(p) doDivide Number.two)
  }

  /**
    * Computes the discriminant of a quadratic equation based on the formula `p^2 - 4 * q`.
    * The discriminant is a critical component in determining the nature of the roots of a
    * quadratic equation.
    * If the discriminant is positive then there are two distinct roots;
    * if negative, then there are no real roots;
    * if zero, then there's a double root.
    *
    * @return an `Int` representing the discriminant.
    */
  def discriminant: Int =
    p * p - 4 * q

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
  def evaluate(context: Context): Option[Field] =
    root.evaluate(context)

  /**
   * Method to determine if this `NumberLike` object can be evaluated exactly in the context.
   *
   * @param context the (optional) `Factor` for which we want to evaluate this `Expression`.
   *                if `context` is `None` then, the result will depend solely on whether `this` is exact.
   * @return true if `this` is exact in the context of factor, else false.
   */
  def isExactInContext(context: Context): Boolean =
    context.isEmpty // by default

  override def toString: String = name

  private def canEqual(other: Any): Boolean =
    other.isInstanceOf[ReducedQuadraticRoot]

  override def equals(other: Any): Boolean = other match {
    case that: ReducedQuadraticRoot =>
      that.canEqual(this) &&
          p == that.p &&
          q == that.q &&
          pos == that.pos
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(p, q, pos)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

/**
 * Companion object for the `ReducedQuadraticRoot` class.
 * Provides an unapply method for pattern matching.
 */
object ReducedQuadraticRoot {
  def unapply(rqr: ReducedQuadraticRoot): Option[(String, Int, Int, Boolean)] =
    Some(rqr.name, rqr.p, rqr.q, rqr.pos)
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
  def create(xs: Field*): Expression =
    apply(xs map (x => Literal(x, None)): _*)
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
    * Applies the exact trigonometric transformation based on predefined constants.
    *
    * The result depends on the input `x` and whether the operation is for sine or cosine.
    * For specific values of `x`, the method returns exact results based on the constants defined.
    * For other values of `x`, it returns `None`, indicating no exact transformation is available.
    *
    * @param x       the input value as an instance of `Field`. Expected values are mathematical constants such as zero, π/2, π, etc.
    * @param context the contextual information used to evaluate or transform the input value.
    * @return an `Option[Field]`, where a `Some[Field]` contains the exact transformed value, or `None` if no exact value is applicable.
    */
  def applyExact(x: Field)(context: Context): Option[Field] = x match {
    case Constants.zero => Some(if (sine) Constants.zero else Constants.one)
    case Constants.piBy2 => Some(if (sine) Constants.one else Constants.zero)
    case Constants.pi => Some(if (sine) Constants.zero else -Constants.one)
    case Constants.piBy2Times3 => Some(if (sine) -Constants.one else Constants.zero)
    case _ => None
  }

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
    case Some(PureNumber) => x.evaluateAsIs match {
      case Some(Constants.zero) => Some(PureNumber)
      case _ => None
    }
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
) {
  /**
    * Applies a binary operation to the provided `Field` elements `a` and `b`, with stricter evaluation rules,
    * and returns an optional result.
    * The evaluation succeeds only if the operation satisfies specific conditions
    * (e.g., exact representations or mathematical constraints).
    *
    * TODO there are many cases which are unimplemented and will fail.
    * For example, other angles based pi/3.
    *
    * @param a the first operand, a `Field` instance.
    * @param b the second operand, a `Field` instance.
    * @return an `Option[Field]` containing the result of the operation if it can be computed exactly,
    *         or `None` if the operation fails to meet exactness requirements.
    */
  def applyExact(a: Field, b: Field): Option[Field] =
    (a, b) match {
      case (x, Constants.zero) if x.signum > 0 =>
        Some(Real(Number.zeroR))
      case (x, Constants.zero) if x.signum < 0 =>
        Some(Constants.pi)
      case (Constants.one, Constants.one) =>
        Some(Constants.piBy4)
      case (Constants.one, Constants.root3) =>
        Some(Constants.piBy3)
      case (Constants.zero, Constants.one) =>
        Some(Constants.piBy2)
      case (Real(ExactNumber(x, PureNumber)), Real(ExactNumber(y, PureNumber))) =>
        for {
          q <- Value.maybeRational(x)
          p <- Value.maybeRational(y)
          r = p / q
          // TODO test this--I have no idea if this is correct
          d = if (Value.signum(x) == Value.signum(y)) 1 else -1
          v <- Operations.doTransformValueMonadic(Value.fromRational(r))(MonadicOperationAtan(d).functions)
        } yield Real(ExactNumber(v, Radian))
      case _ =>
        None
    }
}

/**
 * Represents the natural logarithmic function as an ExpressionFunction.
 *
 * This object provides functionality to compute the natural logarithm (ln) of a given Number.
 * The underlying implementation utilizes the `log` method of the Number type.
 */
case object Log extends ExpressionFunction(x => x.log, "log") {
  /**
    * Applies this function to the given field `x` within the specified `context`,
    * and returns an `Option` containing the result if the function can be
    * evaluated exactly in the given context, or None if it cannot.
    *
    * NOTE This method assumes that isExactInContext has been tested.
    * But why?
    *
    * @param x       the field to which the function is applied.
    * @param context the context within which exact evaluation is performed.
    * @return an `Option` containing the result of the exact application if successful,
    *         or None if the application is not exact in the provided context.
    */
  def applyExact(x: Field)(context: Context): Option[Field] = x match {
    case Constants.e => Some(Constants.one)
    case Constants.one => Some(Constants.zero)
    case Constants.zero => Some(Constants.negInfinity)
    case _ => None
  }

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
    * Applies this function to the given field `x` within the specified `context`,
    * and returns an `Option` containing the result if the function can be
    * evaluated exactly in the given context, or None if it cannot.
    *
    * NOTE we ignore context in at least some of the implementations.
    *
    * NOTE This method assumes that isExactInContext has been tested.
    * But why?
    *
    * @param x       the field to which the function is applied.
    * @param context the context within which exact evaluation is performed.
    * @return an `Option` containing the result of the exact application if successful,
    *         or None if the application is not exact in the provided context.
    */
  def applyExact(x: Field)(context: Context): Option[Field] = x match {
    case Constants.negInfinity => Some(Constants.zero)
    case Constants.zero => Some(Constants.one)
    case Constants.one => Some(Constants.e)
    case _ => None
  }

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
    case a: AtomicExpression =>
      a.evaluateAsIs match {
        case Some(z) => z.isZero || z.isUnity
        case None => false
      }
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
    * Applies this function to the given field `x` within the specified `context`,
    * and returns an `Option` containing the result if the function can be
    * evaluated exactly in the given context, or None if it cannot.
    *
    * NOTE we ignore context in at least some of the implementations.
    *
    * NOTE This method assumes that isExactInContext has been tested.
    * But why?
    *
    * @param x       the field to which the function is applied.
    * @param context the context within which exact evaluation is performed.
    * @return an `Option` containing the result of the exact application if successful,
    *         or None if the application is not exact in the provided context.
    */
  def applyExact(x: Field)(context: Context): Option[Field] = x match {
    case Real(ExactNumber(v, f@PureNumber)) => Some(Real(ExactNumber(Value.negate(v), f)))
    case Real(ExactNumber(v, f@Radian)) => Some(Real(ExactNumber(Value.negate(v), f)))
    case _ => None
  }

  /**
   * Determines the context of the given expression based on its factor.
   * If the expression's factor is a scalar, it returns that factor as the context.
   * Otherwise, returns None as the context.
   *
   * @param x the expression whose context is to be determined
   * @return the context of the given expression as an Option[Scalar], or None if no scalar factor is found
   */
  def context(x: Expression): Context = x.maybeFactor match {
    case Some(c) => c match {
      case s: Scalar => Some(s)
      case _ => None
    }
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
    * Applies this function to the given field `x` within the specified `context`,
    * and returns an `Option` containing the result if the function can be
    * evaluated exactly in the given context, or None if it cannot.
    *
    * NOTE we ignore context in at least some of the implementations.
    *
    * NOTE This method assumes that isExactInContext has been tested.
    * But why?
    *
    * @param x       the field to which the function is applied.
    * @param context the context within which exact evaluation is performed.
    * @return an `Option` containing the result of the exact application if successful,
    *         or None if the application is not exact in the provided context.
    */
  def applyExact(x: Field)(context: Context): Option[Field] = x match {
    case Real(ExactNumber(v, f@PureNumber)) => Value.inverse(v) map (x => Real(ExactNumber(x, f)))
    case Real(ExactNumber(v, f@Logarithmic(_))) => Some(Real(ExactNumber(Value.negate(v), f)))
    case Real(ExactNumber(v, f@Root(_))) => Value.inverse(v) map (x => Real(ExactNumber(x, f)))
    case _ => None
  }

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
case object Sum extends ExpressionBiFunction((x, y) => x add y, "+", isExact = false, Some(Constants.zero), maybeIdentityR = None) {
  /**
    * Applies a binary operation to the provided `Field` elements `a` and `b`, with stricter evaluation rules,
    * and returns an optional result.
    * The evaluation succeeds only if the operation satisfies specific conditions
    * (e.g., exact representations or mathematical constraints).
    * NOTE that the factors of each parameter must be the same.
    * CONSIDER explicitly checking for that.
    *
    * @param a the first operand, an exact `Field` instance.
    * @param b the second operand, an exact `Field` instance.
    * @return an `Option[Field]` containing the result of the operation if it can be computed exactly,
    *         or `None` if the operation fails to meet exactness requirements.
    */
  def applyExact(a: Field, b: Field): Option[Field] =
    Some(a add b) // If both parameters are exact, then the result must also be exact, right?
}

/**
 * Represents a specific implementation of the `ExpressionBiFunction` that performs multiplication between two `Field` values.
 *
 * This object embodies a binary operation where the function takes two inputs and computes their product using the `multiply` method
 * defined on `Field`. The operation is represented by the symbol "*".
 *
 * - The operation is marked as exact, ensuring the result is always precise when the inputs are exact.
 * - It inherits the commutative property from `ExpressionBiFunction`, as multiplication is commutative.
 */
case object Product extends ExpressionBiFunction((x, y) => x multiply y, "*", isExact = true, Some(Constants.one), maybeIdentityR = None) {
  /**
    * Applies a binary operation to the provided `Field` elements `a` and `b`, with stricter evaluation rules,
    * and returns an optional result.
    * The evaluation succeeds only if the operation satisfies specific conditions
    * (e.g., exact representations or mathematical constraints).
    *
    * @param a the first operand, a `Field` instance.
    * @param b the second operand, a `Field` instance.
    * @return an `Option[Field]` containing the result of the operation if it can be computed exactly,
    *         or `None` if the operation fails to meet exactness requirements.
    */
  def applyExact(a: Field, b: Field): Option[Field] =
    Some(a multiply b)
} // NOTE can create a fuzzy number

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
case object Power extends ExpressionBiFunction((x, y) => x.power(y), "^", isExact = false, None, Some(Constants.one)) {
  /**
    * Applies a binary operation to the provided `Field` elements `a` and `b`, with stricter evaluation rules,
    * and returns an optional result.
    * The evaluation succeeds only if the operation satisfies specific conditions
    * (e.g., exact representations or mathematical constraints).
    *
    * @param a the first operand, a `Field` instance.
    * @param b the second operand, a `Field` instance.
    * @return an `Option[Field]` containing the result of the operation if it can be computed exactly,
    *         or `None` if the operation fails to meet exactness requirements.
    */
  def applyExact(a: Field, b: Field): Option[Field] = b match {
    case Real(ExactNumber(v, PureNumber)) =>
      Value.maybeRational(v) map (_ => a power (b))
    case _ => None
  }
}

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
    * Applies this function to the given field `x` within the specified `context`,
    * and returns an `Option` containing the result if the function can be
    * evaluated exactly in the given context, or None if it cannot.
    *
    * NOTE we ignore context in at least some of the implementations.
    *
    * NOTE This method assumes that isExactInContext has been tested.
    * But why?
    *
    * @param x       the field to which the function is applied.
    * @param context the context within which exact evaluation is performed.
    * @return an `Option` containing the result of the exact application if successful,
    *         or None if the application is not exact in the provided context.
    */
  def applyExact(x: Field)(context: Context): Option[Field]

  /**
   * Evaluate this function on Field x.
   *
   * @param x the parameter to the function.
   * @return the result of f(x).
   */
  def apply(x: Field): Field =
    this.context(x).flatMap(c => applyExact(x)(Some(c))).getOrElse(applyInexact(x))

  private def applyInexact(x: Field) =
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
  def unapply(arg: ExpressionFunction): Option[(Number => Number, String)] =
    Some(arg.f, arg.name)
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
  * @param isExact       a boolean indicating if the function is exact in all its computations.
  *                      Even if false, there may be special cases that are exact.
 * @param maybeIdentityL the optional left identity element for the function.
 *                       That's to say, `identityL f y` can be replaced by `y`.
 *                       If `None`, then the function has no identity value.
 * @param maybeIdentityR an optional right identity element for the function, if applicable.
 *                       That's to say, `x f identityR` can be replaced by `x`.
 *                       If `None`, then the function is commutative and the only identity
 *                       required is given by `identityL`.
 */
abstract class ExpressionBiFunction(val f: (Field, Field) => Field, val name: String, val isExact: Boolean, val maybeIdentityL: Option[Field], val maybeIdentityR: Option[Field]) extends ((Field, Field) => Field) {
  /**
   * Evaluate this function on x.
   *
   * @param a the first parameter to the function.
   * @param b the second parameter to the function.
   * @return the result of f(x).
   */
  def apply(a: Field, b: Field): Field = f(a, b)

  /**
    * Applies a binary operation to the provided `Field` elements `a` and `b`, with stricter evaluation rules,
    * and returns an optional result.
    * The evaluation succeeds only if the operation satisfies specific conditions
    * (e.g., exact representations or mathematical constraints).
    *
    * @param a the first operand, a `Field` instance.
    * @param b the second operand, a `Field` instance.
    * @return an `Option[Field]` containing the result of the operation if it can be computed exactly,
    *         or `None` if the operation fails to meet exactness requirements.
    */
  def applyExact(a: Field, b: Field): Option[Field]

  /**
    * Evaluates two expressions within the given context and attempts to combine their results
    * using an exact binary operation. The exact operation is applied only if the evaluations
    * of both expressions are successful.
    *
    * @param x       the first `Expression` to be evaluated.
    * @param y       the second `Expression` to be evaluated.
    * @param context the `Context` in which the expressions will be evaluated.
    * @return an `Option[Field]` containing the result of the exact operation if both evaluations succeed
    *         and the operation can be applied; otherwise, `None`.
    */
  def evaluate(x: Expression, y: Expression)(context: Context): Option[Field] =
    for {
      a <- x.evaluate(context)
      b <- y.evaluate(context)
      z <- applyExact(a, b)
    } yield z

  /**
    * Evaluates two expressions as-is (without any simplification or conversion) and applies the function `f`
    * to the results if both evaluations are successful.
    *
    * @param x the first expression to be evaluated.
    * @param y the second expression to be evaluated.
    * @return an `Option[Field]` containing the result of applying the binary function to the
    *         evaluated results of `x` and `y`, or `None` if either evaluation fails.
    */
  def evaluateAsIs(x: Expression, y: Expression): Option[Field] =
    for (a <- x.evaluateAsIs; b <- y.evaluateAsIs) yield f(a, b)

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