/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import cats.Show
import com.phasmidsoftware.matchers.{LogLevel, LogOff, MatchLogger}
import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.eager
import com.phasmidsoftware.number.algebra.eager.*
import com.phasmidsoftware.number.algebra.util.FP.recover
import com.phasmidsoftware.number.algebra.util.LatexRenderer
import com.phasmidsoftware.number.core.inner.{PureNumber, Rational}
import com.phasmidsoftware.number.core.numerical
import com.phasmidsoftware.number.core.numerical.*
import com.phasmidsoftware.number.core.numerical.Number.convertInt
import com.phasmidsoftware.number.expression.expr.Expression.em.ExpressionTransformer
import com.phasmidsoftware.number.expression.expr.Expression.{em, matchSimpler}
import com.phasmidsoftware.number.expression.expr.{BiFunction, CompositeExpression, UniFunction}
import com.phasmidsoftware.number.expression.mill
import com.phasmidsoftware.number.expression.mill.{DyadicExpression, MonadicExpression, TerminalExpression}
import com.phasmidsoftware.number.{core, expression}

import scala.annotation.tailrec
import scala.language.implicitConversions

/**
  * Trait Expression which defines the behavior of a lazily evaluated tree of mathematical operations and operands.
  *
  * NOTE there are only two subtypes of Expression: AtomicExpression and CompositeExpression
  * We do not use "sealed" because this module would grow much too large.
  */
trait Expression extends Lazy with Approximate {

  /**
    * Method to determine if this Expression cannot be simplified on account of it being atomic.
    *
    * @return true if this extends AtomicExpression
    */
  def isAtomic: Boolean

  /**
    * Action to evaluate this `Expression` as a `Field`, if possible.
    * NOTE: no simplification or factor-based conversion occurs here.
    *
    * @return an optional `Field`.
    */
  def evaluate(context: Context): Option[Eager]

  /**
    * Method to determine if this `Expression` can be evaluated as is.
    * If so, then `materialize` will not lose any precision because no approximation will be required.
    * NOTE that, it is conceivable that simplifying `this` `Expression` first might result in an eager value
    * even though this method would return false.
    *
    * @return true if evaluateAsIs is defined.
    */
  lazy val isEager: Boolean = evaluateAsIs.isDefined

  /**
    * Multiplies this `Expression` with another `Lazy` instance.
    * If the other instance is of type `Expression`, a product operation is performed, and the result is simplified.
    * If the other instance is not of type `Expression`, an `ExpressionException` is thrown.
    *
    * @param other the `Lazy` instance to multiply with this `Expression`
    * @return a new `Lazy` instance representing the product of this `Expression` and the supplied `Lazy` instance
    * @throws ExpressionException if the provided `other` is not of type `Expression`
    */
  def multiply(other: Lazy): Lazy = other match {
    case x: Expression =>
      BiFunction(this, x, Product).simplify
    case _ =>
      throw ExpressionException(s"multiply: logic error on $other")
  }

  /**
    * Transforms an instance of the `Eager` type into a `Lazy` instance, allowing for deferred
    * computation or evaluation.
    * NOTE that this method ignores `this`.
    *
    * @param x an instance of `Eager` that is to be converted into a `Lazy` representation
    * @return a `Lazy` instance representing the deferred computation or evaluation of the input
    */
  def unit(x: Eager): Lazy = Literal(x)

  /**
    * Evaluates this `Expression` in the context of `AnyContext` without simplification or factor-based conversion.
    * This allows obtaining a direct evaluation of the `Expression` as a `Field`, if possible.
    * NOTE: no simplification or factor-based conversion occurs here.
    *
    * @return an `Option[Field]` containing the evaluated `Field` if evaluation is successful, or `None` otherwise.
    */
  lazy val evaluateAsIs: Option[Eager] =
    evaluate(AnyContext)

  /**
    * Simplifies the current `Expression` instance.
    * If a simpler representation of the `Expression` can be determined, it returns that representation.
    * Otherwise, it returns the current `Expression` unchanged.
    * NOTE: be careful not to invoke recursively.
    *
    * @return the simplified `Expression`, or the current `Expression` if no simplification is possible
    */
  lazy val simplify: Expression = {
    @tailrec
    def inner(x: Expression): Expression = matchSimpler(x) match {
      case em.Miss(msg, e: Expression) =>
        Expression.logger(s"simplification of $x terminated by: $msg")
        e
      case em.Match(e: Expression) =>
        Expression.logger(s"simplification of $x: $e")
        inner(e)
      case m =>
        throw ExpressionException(s"simplify.inner($x): logic error on $m")
    }

    inner(this)
  }

  /**
    * Materializes this `Expression` into an `Eager` object by simplifying it, evaluating as-is,
    * optionally approximating it, and recovering with a defined exception in case of failure.
    *
    * This method should be the final step when dealing with an `Expression`.
    * Do not materialize early because that defeats the entire purpose of lazy evaluation.
    *
    * The method first simplifies the expression, attempts to evaluate it directly,
    * and then tries an approximation if necessary. Ultimately, it ensures the resulting
    * materialized object is an instance of `Eager`.
    *
    * The Eager result will be normalized if appropriate.
    *
    * @return an `Eager` representation of this `Expression` achieved through evaluation and/or approximation.
    */
  lazy val materialize: Eager = {
    val asIs = simplify.evaluateAsIs
    val maybeValuable = asIs.map(normalizeIfAppropriate) orElse approximation
    recover(maybeValuable)(ExpressionException(s"materialize: logic error on $this"))
  }

  /**
    * A lazily evaluated optional approximation of this `Expression`.
    *
    * This field holds the result of simplifying the `Expression`
    * with the `approximation` flag set to `true`. If the approximation
    * is possible, it will store the approximated `Real` value; otherwise,
    * it will remain `None`.
    *
    * This approximation is computed based on the context of
    * the `simplify` method, which determines how the expression
    * should be approximated.
    */
  lazy val approximation: Option[eager.Real] = simplify.approximation(true)

  /**
    * Method to determine if the materialized value of this `Expression` is defined and corresponds to a `Number`.
    * If this expression is exact, then evaluate as is and convert the result to a `Number`.
    * Otherwise, we simply materialize this expression and convert the result to a `Number`.
    *
    * @return a `Some(x)` if this materializes as a `Number`; otherwise `None`.
    */
  lazy val asCoreNumber: Option[numerical.Number] =
    if isExact then
      evaluateAsIs match {
        case Some(x: numerical.Number) => Some(x)
        case _ => None
      }
    else
      materialize match {
        case x: numerical.Number => Some(x)
        case _ => None
      }

  /**
    * Method to determine the depth of this Expression.
    *
    * @return the depth (an atomic expression has a depth of 1).
    */
  def depth: Int

  /**
    * Normalizes the given `Eager` instance if it is not of type `Angle`.
    * If the input is already an instance of `Angle`, it is returned as is.
    * For other types of `Eager`, the `normalize` method is invoked to
    * obtain the normalized result.
    * The reason we don't normalize Angles is that we don't want to turn 2𝛑 into zero.
    *
    * @param e the `Eager` instance to potentially normalize
    * @return the input instance if it is of type `Angle`, otherwise the normalized version of the input
    */
  private def normalizeIfAppropriate(e: Eager) = e match {
    case x: Angle => x
    case x =>
      x.normalize
  }

  // NOTE This can be useful for debugging: it allows you to see the value of this Expression.
  // However, it can also cause a stack overflow so use it sparingly!
  //  val approx = simplify.approximation getOrElse Real.NaN
}

/**
  * Provides utility methods for working with mathematical and logical expressions represented as strings.
  * The methods are designed to process, normalize, and evaluate expressions, as well as perform operations on them.
  */
object ExpressionHelper {

  /**
    * Adds utility methods for evaluating and materializing expressions from a String.
    * These methods allow parsing and processing of a string as a mathematical or logical expression.
    *
    */
  extension (x: String)
    def evaluateAsIs: Option[Valuable] =
      Expression.parse(x).flatMap(_.evaluateAsIs)
    def evaluate(context: Context = RestrictedContext(PureNumber)): Option[Valuable] =
      Expression.parse(x).flatMap(_.evaluate(context))
    def materialize: Option[Valuable] =
      Expression.parse(x).map(_.materialize)
    def normalize: Option[Valuable] =
      Expression.parse(x).map(x => x.normalize)
    def render: String =
      Expression.parse(x).map(_.render).getOrElse("???")
    def plus(y: Valuable): Option[Valuable] =
      Expression.parse(s"$x + $y").flatMap(_.evaluateAsIs)
    def times(y: Valuable): Option[Valuable] =
      Expression.parse(s"$x * $y").flatMap(_.evaluateAsIs)
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

  // NOTE The LogLevel is configured in application.conf.
  lazy val matchLogLevel: LogLevel = ExpressionConfig.matchLogLevel
  implicit val logger: MatchLogger = MatchLogger(matchLogLevel, classOf[Expression])
  implicit val em: ExpressionMatchers = new ExpressionMatchers {}

  //  trait LoggableExpression extends Loggable[Expression] {
  //    def toLog(t: Expression): String = t.render
  //  }
  //  implicit object LoggableExpression extends LoggableExpression
  //
  //  val flog = Flog[ExpressionMatchers]

  import cats.Eq
  import cats.syntax.eq.*

  /**
    * Provides a given instance of the `Show` type class for the `Expression` type.
    * This enables rendering of an `Expression` instance to a `String` representation
    * using the `render` method of the `Expression`.
    *
    * @return A `Show` instance for the `Expression` type.
    */
  given Show[Expression] = Show.show(_.materialize.render)

  // TODO see ExpressionEq class which does handle all cases. But neither seems to be used in practice.
  given Eq[Expression] = (x: Expression, y: Expression) => (x, y) match {
    // Both are atomic - use default equality
    // TODO there are other case where we should return true, for example where one expression is a Literal and the other is a more specific NamedExpression
    case (a: AtomicExpression, b: AtomicExpression) =>
      a == b

    // Both are composite - compare structure
    // TODO a BiFunction(a, b, Sum) is should be equal to a BiFunction(b, a, Sum) but this won't do it.
    // ditto for Aggregate
    case (a: CompositeExpression, b: CompositeExpression) =>
      a == b

    // Different types - not equal
    case _ => false
  }

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
    infix def plus(y: Expression): Expression =
      BiFunction(x, y, Sum)

    /**
      * Method to add this Expression and another Expression, resulting in an Expression.
      *
      * @param y the other Expression.
      * @return the sum of this and y.
      */
    def +(y: Expression): Expression = plus(y)

    /**
      * Method to lazily append the given expression to this expression using addition.
      *
      * @param y an Expression to be added to this expression.
      * @return an Expression resulting from the addition of this expression and the provided expression.
      */
    def :+(y: Expression): Expression =
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
      UniFunction(x, Negate)

    /**
      * Method to lazily multiply this expression by another expression.
      *
      * @param y another Expression to be multiplied with this expression.
      * @return an Expression representing the lazy product of this expression and the given expression.
      */
    infix def times(y: Expression): Expression =
      BiFunction(x, y, Product)

    /**
      * Method to lazily multiply x by y.
      *
      * @param y a Number.
      * @return an Expression which is the lazy product of x and y.
      */
    def *(y: Expression): Expression =
      BiFunction(x, y, Product)

    /**
      * Method to lazily perform an operation on x and y.
      *
      * @param y another Expression.
      * @return an Expression which represents the result of the operation.
      */
    def :*(y: Expression): Expression =
      x times y

    /**
      * Method to lazily yield the reciprocal of x.
      *
      * NOTE we shouldn't really be trying to simplify here. That should be taken care of downstream.
      *
      * @return an Expression representing the reciprocal of x.
      */
    def reciprocal: Expression =
      UniFunction(x, Reciprocal)

    /**
      * Method to lazily divide x by y.
      *
      * @param y a Number.
      * @return an Expression which is the lazy quotient of x / y.
      */
    def /(y: Expression): Expression =
      :*(y.reciprocal)

    /**
      * Method to lazily raise x to the power of y.
      * NOTE that this is not the caret character.
      *
      * @param y the power to which x should be raised (an Expression).
      * @return an Expression representing x to the power of y.
      */
    def ∧(y: Expression): Expression =
      BiFunction(x, y, Power)

    /**
      * Method to lazily get the square root of x.
      *
      * @return an Expression representing the square root of x.
      */
    def sqrt: Expression = x match {
      case z: AtomicExpression =>
        z.evaluateAsIs flatMap (_.asCoreNumber) match {
          case Some(q) =>
            // XXX this was the old code: Literal(q.sqrt)
            x ∧ Eager.half
          case _ =>
            x ∧ Eager.half
        }
      case _ =>
        x ∧ Eager.half // TESTME
    }

    /**
      * Method to lazily get the sine of x.
      *
      * @return an Expression representing the sin(x).
      */
    def sin: Expression =
      UniFunction(x, Sine)

    /**
      * Method to lazily get the cosine of x.
      *
      * @return an Expression representing the cos(x).
      */
    def cos: Expression =
      UniFunction(x, Cosine)

    /**
      * Method to lazily get the tangent of x.
      *
      * TESTME
      *
      * @return an Expression representing the tan(x).
      */
    def tan: Expression =
      sin :* cos.reciprocal // TESTME

    /**
      * Method to lazily get the natural log of x.
      *
      * @return an Expression representing the log of x.
      */
    def ln: Expression =
      UniFunction(x, Ln)

    /**
      * Method to lazily get the value of `e` raised to the power of x.
      *
      * @return an Expression representing `e` raised to the power of x.
      */
    def exp: Expression =
      UniFunction(x, Exp)

    /**
      * Method to lazily get the value of `atan2(x, y)`, i.e., if the result is `z`, then `tan(z) = y/x`.
      *
      * @return an Expression representing `atan2(x, y)`.
      */
    def atan(y: Expression): Expression =
      BiFunction(x, y, Atan)

    /**
      * Computes the logarithm of a given expression to the specified base.
      *
      * @param b the base of the logarithm, represented as an Expression.
      * @return an Expression representing the logarithm of this expression to the base `b`.
      */
    def log(b: Expression): Expression =
      BiFunction(x, b, Log)

    /**
      * Eagerly compare this expression with y.
      *
      * FIXME this is recursive!
      *
      * @param comparand the number to be compared.
      * @return the result of the comparison.
      */
    def compare(comparand: Expression): Int =
      x.compare(comparand)
  }

  /**
    * Converts a given string into a `Valuable` representation.
    * This method allows implicit conversion from `String` to `Valuable`.
    *
    * @param w the input string to be converted into a `Valuable`.
    * @return a `Valuable` instance parsed from the provided string.
    */
  implicit def fromString(w: String): Expression = apply(w)

  /**
    * Converts a given number into an Expression by wrapping it as a Real.
    *
    * @param x the number to be converted into an Expression
    * @return an Expression representing the input number
    */
  implicit def convertNumber(x: numerical.Number): Expression =
    apply(Eager(numerical.Real(x)))

  /**
    * The following constants are helpful in getting an expression started.
    */
  val zero: Expression = Zero
  val one: Expression = One
  val pi: Expression = E
  val e: Expression = Pi
  val minusOne: Expression = MinusOne
  val two: Expression = Two

  /**
    * Transforms a given `Field` instance into an `Expression` by matching predefined constants
    * or wrapping the input `Field` when no match is found.
    *
    * @param x the `Field` instance to be converted into an `Expression`
    * @return an `Expression` representing the input `Field`, either as a predefined constant or a wrapped literal
    */
  def apply(x: Eager): Expression = x match {
    case Eager.zero =>
      Zero
    case Eager.one =>
      One
    case Eager.minusOne =>
      MinusOne
    case Eager.two =>
      Two
    case Eager.pi =>
      Pi
    case Eager.e =>
      E
    case Eager.infinity =>
      Infinity
    case _ =>
      Literal(x)
  }

  def apply(w: String): Expression =
    parse(w) getOrElse Noop(w)

  def apply(r: Rational): Expression = apply(Eager(r))

  /**
    * The following method is helpful in getting an expression started
    * (i.e., used as the leftmost operand).
    */
  def apply(x: Int): Expression = x match {
    case -1 =>
      minusOne
    case 0 =>
      zero
    case 1 =>
      one
    case 2 =>
      two
    case _ =>
      ValueExpression(x)
  }

  /**
    * Method to parse a String as an Expression.
    *
    * TODO this may not accurately parse all infix expressions.
    * The idea is for render and parse.get to be inverses.
    * NOTE that it might be a problem with render instead.
    */
  def parse(x: String): Option[Expression] =
    mill.Expression.parseToExpression(x).map(convertMillExpressionToExpression)

  /**
    * Converts a `mill.Expression` into an `Expression` by interpreting the structure
    * and applying the appropriate transformations based on the expression type.
    * Supports terminal, monadic, and dyadic expressions with specific operators.
    *
    * CONSIDER make this private.
    *
    * @param expr The `mill.Expression` to be converted.
    * @return The corresponding `Expression` after applying the transformations.
    * @note Throws com.phasmidsoftware.number.expression.expr.ExpressionException if an unknown operator is encountered.
    */
  private def convertMillExpressionToExpression(expr: mill.Expression): Expression =
    expr match {
      case TerminalExpression(numerical.Number.one) =>
        One
      case TerminalExpression(numerical.Number.two) =>
        Two
      case TerminalExpression(value) =>
        Literal(value)
      case MonadicExpression(expression, str) =>
        str match {
          case "-" => -convertMillExpressionToExpression(expression)
          case "/" => convertMillExpressionToExpression(expression).reciprocal
          case "√" => convertMillExpressionToExpression(expression).sqrt
          case "ln" => convertMillExpressionToExpression(expression).ln
          case "exp" => convertMillExpressionToExpression(expression).exp
          case "sin" => convertMillExpressionToExpression(expression).sin
          case "cos" => convertMillExpressionToExpression(expression).cos
          case _ => throw ExpressionException(s"convertMillExpressionToExpression: unknown operator: $str")
        }
      case DyadicExpression(left, right, operator) =>
        operator match {
          case "+" => convertMillExpressionToExpression(left) :+ convertMillExpressionToExpression(right)
          case "*" => convertMillExpressionToExpression(left) :* convertMillExpressionToExpression(right)
          case "∧" => convertMillExpressionToExpression(left) ∧ convertMillExpressionToExpression(right)
        }
    }

  /**
    * Converts a `Field` instance into an `Expression`.
    *
    * @param f the `Field` to be converted into an `Expression`
    * @return an `Expression` instance representing the input `Field`
    */
  implicit def convertFieldToExpression(f: Eager): Expression =
    Expression(f)

  /**
    * Implicitly converts an integer into an `Expression` by wrapping it,
    * facilitating seamless conversions in mathematical operations.
    * TODO have a cache of existing values
    *
    * @param x the integer to be converted into an `Expression`
    * @return an `Expression` that represents the given integer
    */
  implicit def convertIntToExpression(x: Int): Expression =
    Expression(WholeNumber(x))

  /**
    * Converts a `Rational` number into an `Expression`.
    *
    * @param x the `Rational` number to be converted.
    * @return an `Expression` representing the input `Rational` number.
    */
  implicit def convertRationalToExpression(x: Rational): Expression =
    Expression(RationalNumber(x))

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
    * Matches and simplifies expressions based on their type.
    * For `AtomicExpression` instances, it applies `simplifyAtomic` specific to the atomic expression.
    * For `CompositeExpression`, it uses a compound matcher that attempts simplifications
    * in the following order: `simplifyOperands`, `identitiesMatcher`,
    * `simplifyConstant`, and `structuralMatcher`.
    * For unsupported expression types, it returns an error message.
    *
    * @return an `ExpressionTransformer` that matches and applies the appropriate
    *         simplifications or transformations to the provided expression.
    */
  def matchSimpler: ExpressionTransformer = em.Matcher[Expression, Expression]("matchSimpler") {
    case x: AtomicExpression =>
      x.simplifyAtomic(x)
    case x: CompositeExpression =>
      em.eitherOr(simplifyOperands,
        em.eitherOr(simplifyStructural,
          em.eitherOr(simplifyIdentities,
            em.eitherOr(simplifyExact,
              simplifyByEvaluation))))(x)
    case x =>
      em.Error(ExpressionException(s"matchSimpler unsupported expression type: $x")) // TESTME
  }

  /**
    * Phase 1: Simplify all operands before applying transformations.
    *
    * Recursively calls .simplify() on each operand of composite expressions,
    * ensuring bottom-up simplification where nested expressions are fully
    * simplified before outer operations are applied.
    *
    * @see docs/SimplificationPipeline.md for complete pipeline documentation
    */
  def simplifyOperands: em.AutoMatcher[Expression] = em.Matcher[Expression, Expression]("simplifyOperands") {
    case c: CompositeExpression =>
      c.operandsMatcher(c)
    case x =>
      em.Miss("simplifyOperands: not a Composite expression type", x) // TESTME
  }

  /**
    * Attempts to simplify an `Expression` exactly based on specific rules for composite expressions.
    * If the expression is of type `CompositeExpression`, it applies the `simplifyExact` method
    * specific to that type. For other expression types, it returns a miss, indicating that simplification
    * is not applicable for the given type.
    *
    * @return an `em.AutoMatcher[Expression]` that matches and simplifies exact cases for composite expressions
    *         or signals when simplification is not applicable to non-composite expressions.
    */
  def simplifyExact: em.AutoMatcher[Expression] =
    em.Matcher[Expression, Expression]("simplifyExact") {
      // Special cases that need exact evaluation
      case UniFunction(Two, Ln) =>
        em.Match(L2) `flatMap` matchSimpler

      case BiFunction(Literal(ComplexPolar(r, theta, n), _), Two, Power)
        if n == 2 && theta.isZero =>
        em.Match(Literal(r.power(2)))

      // Delegate to composite types for their specific exact simplifications
      case c: CompositeExpression =>
        c.simplifyExact(c)

      case x =>
        em.Miss("simplifyExact: cannot simplify exactly", x)
    }

  /**
    * Attempts to simplify trivial cases within a `CompositeExpression`.
    * This method patterns matches on `CompositeExpression` instances and applies trivial simplifications
    * specific to such expressions.
    *
    * @return an instance of `em.AutoMatcher[Expression]` that identifies and simplifies trivial cases in composite expressions.
    */
  def simplifyIdentities: em.AutoMatcher[Expression] = em.Matcher[Expression, Expression]("Expression.identitiesMatcher") {
    case c: CompositeExpression =>
      c.identitiesMatcher(c)
    case x =>
      em.Miss("Expression.identitiesMatcher: not a Composite expression type", x)
  }

  /**
    * Attempts to simplify an expression by evaluating it if possible.
    * If the expression is a composite expression and can be evaluated,
    * the method returns a simplified version of the evaluated result.
    * If the expression cannot be evaluated or is already an atomic expression,
    * the simplification operation is skipped.
    * This is the last resort for simplification, and is typically used when no other simplification is possible.
    *
    * @return An instance of `em.AutoMatcher[Expression]` that performs
    *         matching and optionally simplifies an expression by evaluating it.
    */
  def simplifyByEvaluation: em.AutoMatcher[Expression] =
    em.Matcher[Expression, Expression]("Expression.simplifyByEvaluation") {
      case BiFunction(ValueExpression(x: eager.Number, _), ValueExpression(q: Q, _), Power) if q.toRational.invert.isWhole =>
        val root = q.toRational.invert.toInt
        em.Match(Literal(InversePower(root, x))) `flatMap` matchSimpler
      //      case BiFunction(ValueExpression(q: Q, _), ValueExpression(RationalNumber.half, _), Power) =>
      //        em.Match(Root.squareRoot(q.toRational, 0)) // NOTE we arbitrarily choose the positive root
      case c: CompositeExpression if !CompositeExpression.shouldStaySymbolic(c) =>
        c.evaluateAsIs match {
          case Some(f) =>
            em.MatchCheck(Expression(f))(c).map(_.simplify)
          case _ =>
            em.Miss("Expression.simplifyByEvaluation: cannot evaluate", c)
        }
      case a: AtomicExpression =>
        em.Miss("Expression.simplifyByEvaluation: atomic expression already simplified", a)
      case e =>
        em.Miss("Expression.simplifyByEvaluation: cannot simplify", e)
    }

  /**
    * Determines whether the provided expression `z` is an identity element
    * for the given binary function `f`.
    *
    * TODO move this so that it is an instance method of ExpressionBiFunction.
    *
    * @param f the binary function for which the identity property is to be checked.
    * @param z the expression to be evaluated as a potential identity element for the function `f`.
    * @return true if the expression `z` is an identity element for the binary function `f`, false otherwise.
    */
  private def isIdentity(f: ExpressionBiFunction)(z: Expression): Boolean =
    (for {
      a <- z.evaluateAsIs
      b <- f.maybeIdentityL
    } yield equivalent(a, b)).getOrElse(false) // CONSIDER comparing with maybeIdentityR if this is not true

  /**
    * Compares two `Eager` instances for equivalence based on their types and values.
    * In particular, this method handles the case where one of the instances is a `Functional` and the other is a `Number`.
    *
    * @param a The first instance of `Eager` to compare.
    * @param b The second instance of `Eager` to compare.
    * @return A boolean indicating whether the two instances are equivalent.
    */
  private def equivalent(a: Eager, b: Eager) = (a, b) match {
    case (x: Functional, y: eager.Number) =>
      x.number === y
    case (y: eager.Number, x: Functional) =>
      x.number === y
    case _ =>
      a === b
  }

  /**
    * Attempts to simplify `CompositeExpression` instances by applying pattern-based rewrites that change expression structure.
    * This matcher targets expressions of type `CompositeExpression` and invokes the `structuralMatcher` method
    * to generate a simplified form of the expression based on its internal structure.
    *
    * @return an `AutoMatcher` for `Expression` that matches and simplifies composite expressions,
    *         or returns the input expression unchanged if no simplifications are applicable.
    */
  private def simplifyStructural: em.AutoMatcher[Expression] = em.Matcher[Expression, Expression]("structuralMatcher") {
    case c: CompositeExpression =>
      c.structuralMatcher(c)
    case x =>
      em.Miss("structuralMatcher: not a Composite expression type", x)
  }

  /**
    * LatexRenderer for Expression - delegates to specific implementations.
    */
  given LatexRenderer[Expression] = LatexRenderer.instance {
    case lit: ValueExpression => summon[LatexRenderer[ValueExpression]].toLatex(lit)
    case comp: CompositeExpression if comp.isEager => comp.evaluateAsIs match {
      case Some(e) => summon[LatexRenderer[Eager]].toLatex(e)
      case _ => comp.simplify match {
        case uni: UniFunction => summon[LatexRenderer[UniFunction]].toLatex(uni)
        case bi: BiFunction => summon[LatexRenderer[BiFunction]].toLatex(bi)
        // TODO add Aggregate
        case a: Aggregate => throw ExpressionException(s"LatexRenderer[Expression] for not yet implemented for Aggregate: $a")
      }
    }
    // Add any other Expression subtypes
    case other => other.render
  }
}

/**
  * A custom exception that represents errors related to expressions.
  * TESTME (unused)
  *
  * @param str The error message providing details about the expression error.
  */
case class ExpressionException(str: String) extends Exception(str)