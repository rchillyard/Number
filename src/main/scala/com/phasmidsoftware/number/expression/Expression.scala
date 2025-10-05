/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression

import com.phasmidsoftware.matchers.{LogOff, MatchLogger}
import com.phasmidsoftware.number.core.Number.convertInt
import com.phasmidsoftware.number.core.inner._
import com.phasmidsoftware.number.core.{Approximatable, ComplexPolar, Constants, Field, Number, NumberException, NumberLike, Real}
import com.phasmidsoftware.number.expression.Expression.em.ExpressionTransformer
import com.phasmidsoftware.number.expression.Expression.{em, matchSimpler}
import com.phasmidsoftware.number.misc.FP.recover
import com.phasmidsoftware.number.parse.ShuntingYardParser
import scala.annotation.tailrec
import scala.language.implicitConversions

/**
  * Trait Expression which defines the behavior of a lazily evaluated tree of mathematical operations and operands.
  *
  * NOTE there are only two subtypes of Expression: AtomicExpression and CompositeExpression
  * We do not use "sealed" because this module would grow much too large.
  */
trait Expression extends NumberLike with Approximatable {

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
  def evaluate(context: Context): Option[Field]

  /**
    * Evaluates this `Expression` in the context of `AnyContext` without simplification or factor-based conversion.
    * This allows obtaining a direct evaluation of the `Expression` as a `Field`, if possible.
    * NOTE: no simplification or factor-based conversion occurs here.
    *
    * @return an `Option[Field]` containing the evaluated `Field` if evaluation is successful, or `None` otherwise.
    */
  lazy val evaluateAsIs: Option[Field] =
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
      case em.Miss(_, e: Expression) =>
        e
      case em.Match(e: Expression) =>
        inner(e)
      case m =>
        throw ExpressionException(s"simplify.inner($x): logic error on $m")
    }

    inner(this)
  }

  /**
    * Materializes the current `Expression` by simplifying and evaluating it as a `Field`.
    * If the evaluation fails, an `ExpressionException` is thrown, indicating a logic error.
    *
    * @return the materialized `Field` representation of the `Expression`.
    */
  def materialize: Field = {
    val simplified = simplify
    recover(simplified.evaluateAsIs orElse simplified.approximation, ExpressionException(s"materialize: logic error on $this"))
  }

  /**
    * Method to determine if the materialized value of this `Expression` is defined and corresponds to a `Number`.
    * If this expression is exact, then evaluate as is and convert the result to a `Number`.
    * Otherwise, we simply materialize this expression and convert the result to a `Number`.
    *
    * @return a `Some(x)` if this materializes as a `Number`; otherwise `None`.
    */
  def asNumber: Option[Number] =
    if (isExact)
      evaluateAsIs flatMap (_.asNumber)
    else
      materialize.asNumber

  /**
    * Method to determine the depth of this Expression.
    *
    * @return the depth (an atomic expression has a depth of 1).
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

  // NOTE This can be useful for debugging: it allows you to see the value of this Expression.
  // However, it can also cause a stack overflow so use it sparingly!
  //  val approx = simplify.approximation getOrElse Real.NaN
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
      UniFunction(x, Negate)

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
      UniFunction(x, Reciprocal)

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
          case Some(q) =>
            Literal(q.sqrt)
          case _ =>
            x ^ Constants.half // TESTME
        }
      case _ =>
        x ^ Constants.half // TESTME
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
      sin * cos.reciprocal // TESTME

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
      * @param comparand the number to be compared.
      * @return the result of the comparison.
      */
    def compare(comparand: Expression): Int =
      x compare comparand
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
    * Transforms a given `Field` instance into an `Expression` by matching predefined constants
    * or wrapping the input `Field` when no match is found.
    *
    * @param x the `Field` instance to be converted into an `Expression`
    * @return an `Expression` representing the input `Field`, either as a predefined constant or a wrapped literal
    */
  def apply(x: Field): Expression = x match {
    case Constants.zero =>
      Zero // TESTME (applies to all except default case)
    case Constants.one =>
      One
    case Constants.minusOne =>
      MinusOne
    case Constants.two =>
      Two
    case Constants.pi =>
      ConstPi
    case Constants.e =>
      ConstE
    case _ =>
      Literal(x)
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
    * Converts a `Field` instance into an `Expression`.
    *
    * @param f the `Field` to be converted into an `Expression`
    * @return an `Expression` instance representing the input `Field`
    */
  implicit def convertFieldToExpression(f: Field): Expression =
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
    Expression(x)

  /**
    * Converts a `Rational` number into an `Expression`.
    *
    * @param x the `Rational` number to be converted.
    * @return an `Expression` representing the input `Rational` number.
    */
  implicit def convertRationalToExpression(x: Rational): Expression =
    Expression(x)

  /**
    * The following method is helpful in getting an expression started
    * (i.e., used as the leftmost operand).
    */
  def apply(x: Int): Expression = x match {
    case -1 =>
      minusOne
    case 0 =>
      zero // TESTME
    case 1 =>
      one
    case 2 =>
      two // TESTME
    case _ =>
      Literal(x)
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
    * Attempts to simplify an `Expression` by pattern matching on its type and applying
    * the appropriate simplification logic based on the expression's structure.
    *
    * For `AtomicExpression` instances, a message indicating the inability to simplify is returned.
    * For `CompositeExpression` instances, a series of simplification methods (`simplifyComponents`,
    * `simplifyTrivial`, `simplifyConstant`, and `simplifyComposite`) are applied, either recursively
    * or selectively, to achieve a simplified expression.
    * For unsupported expression types, an error is returned.
    *
    * @return an `ExpressionTransformer` that performs the transformation or simplification of
    *         the input `Expression`, depending on its type and structure.
    */
  def matchSimpler: ExpressionTransformer = {
    case x: AtomicExpression =>
      x.simplifyAtomic(x)
    case x: CompositeExpression =>
      em.eitherOr(simplifyComponents,
        em.eitherOr(simplifyTrivial,
          em.eitherOr(simplifyConstant,
            simplifyComposite)))(x)
    case x =>
      em.Error(ExpressionException(s"matchSimpler unsupported expression type: $x")) // TESTME
  }

  /**
    * Matches and simplifies the components of a `CompositeExpression`.
    * For input expressions of type `CompositeExpression`, it applies the `simplifyComponents` method recursively
    * to its underlying components.
    *
    * @return an `em.AutoMatcher[Expression]` that performs the matching and simplification of components
    *         in a `CompositeExpression`.
    */
  def simplifyComponents: em.AutoMatcher[Expression] = em.Matcher[Expression, Expression]("simplifyComponents") {
    case a: AtomicExpression =>
      a.simplifyAtomic(a)
    case c: CompositeExpression =>
      c.simplifyComponents(c)
    case x =>
      em.Miss("simplifyComponents: not a Composite expression type", x) // TESTME
  }

  /**
    * Attempts to simplify trivial cases within a `CompositeExpression`.
    * This method patterns matches on `CompositeExpression` instances and applies trivial simplifications
    * specific to such expressions.
    *
    * @return an instance of `em.AutoMatcher[Expression]` that identifies and simplifies trivial cases in composite expressions.
    */
  def simplifyTrivial: em.AutoMatcher[Expression] = em.Matcher[Expression, Expression]("simplifyTrivial") {
    case c: CompositeExpression =>
      c.simplifyTrivial(c)
    case x =>
      em.Miss("simplifyTrivial: not a Composite expression type", x) // TESTME
  }

  /**
    * Attempts to simplify an `Expression` by focusing on constant folding within composite expressions.
    * For composite expressions, this method applies constant-specific simplification logic.
    * It does not handle other types of simplifications or unsupported expression types.
    *
    * @return an `em.AutoMatcher[Expression]` that matches and simplifies composite expressions
    *         where constants can be evaluated or reduced, returning the simplified `Expression`.
    */
  def simplifyConstant: em.AutoMatcher[Expression] = em.Matcher[Expression, Expression]("simplifyConstant") {
    case BiFunction(Literal(ComplexPolar(r, theta, n), _), Two, Power)
      if n == 2 && theta.isZero =>
      em.Match(Literal(r.power(2)))
    case c: CompositeExpression =>
      c.simplifyConstant(c)
    case x =>
      em.Miss("simplifyConstant: not a Composite expression type", x)
  }

  /**
    * Attempts to simplify `CompositeExpression` instances by applying a defined transformation logic.
    * This matcher targets expressions of type `CompositeExpression` and invokes the `simplifyComposite` method
    * to generate a simplified form of the expression based on its internal structure.
    *
    * @return an `AutoMatcher` for `Expression` that matches and simplifies composite expressions,
    *         or returns the input expression unchanged if no simplifications are applicable.
    */
  private def simplifyComposite: em.AutoMatcher[Expression] = em.Matcher[Expression, Expression]("simplifyComposite") {
    case c: CompositeExpression =>
      c.simplifyComposite(c)
    case x =>
      em.Miss("simplifyComposite: not a Composite expression type", x)
  }
}

/**
  * A custom exception that represents errors related to expressions.
  * TESTME (unused)
  *
  * @param str The error message providing details about the expression error.
  */
case class ExpressionException(str: String) extends Exception(str)