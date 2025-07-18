/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression

import com.phasmidsoftware.matchers.{LogOff, MatchLogger}
import com.phasmidsoftware.number.core.Number.convertInt
import com.phasmidsoftware.number.core.algebraic.{Algebraic, Algebraic_Quadratic, Quadratic, Solution}
import com.phasmidsoftware.number.core.inner.Context.{AnyLog, AnyRoot, AnyScalar}
import com.phasmidsoftware.number.core.inner.Rational.toIntOption
import com.phasmidsoftware.number.core.inner._
import com.phasmidsoftware.number.core.{Approximatable, Complex, ComplexCartesian, ComplexPolar, Constants, ExactNumber, Field, Number, NumberException, NumberLike, Real}
import com.phasmidsoftware.number.expression.Expression.em.{DyadicTriple, ExpressionTransformer}
import com.phasmidsoftware.number.expression.Expression.{em, matchSimpler}
import com.phasmidsoftware.number.misc.FP
import com.phasmidsoftware.number.misc.FP.recover
import com.phasmidsoftware.number.parse.ShuntingYardParser
import scala.Option.when
import scala.annotation.tailrec
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
      sin * cos.reciprocal // TESTME

    /**
      * Method to lazily get the natural log of x.
      *
      * @return an Expression representing the log of x.
      */
    def log: Expression =
      Function(x, Log)

    /**
      * Method to lazily get the value of `e` raised to the power of x.
      *
      * @return an Expression representing `e` raised to the power of x.
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
    *
    * @param x the integer to be converted into an `Expression`
    * @return an `Expression` that represents the given integer
    */// TODO have a cache of existing values
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
    case x: ReducedQuadraticRoot =>
      em.Match(x.asAlgebraic)
    case x: AtomicExpression =>
      em.Miss("matchSimpler: cannot be simplified", x)
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
  * An Expression which cannot be further simplified.
  */
sealed trait AtomicExpression extends Expression {
  /**
    * Method to determine if this NumberLike object is exact.
    * For instance, Number.pi is exact, although if you converted it into a PureNumber, it would no longer be exact.
    *
    * @return true if this NumberLike object is exact in the context of No factor, else false.
    */
  def isExact: Boolean =
    evaluateAsIs.exists(_.isExact)

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
  def maybeFactor: Option[Factor]

  /**
    * @return 1.
    */
  def depth: Int = 1
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
    case c: Complex =>
      Some(c) // CONSIDER eliminate this?  // TESTME
    case FieldExpression(x, _) =>
      Some(x) // NOTE we lose the name here.
    case Literal(x, _) =>
      Some(x) // NOTE we lose the name here. // TESTME
    case f: Field =>
      Some(f) // CONSIDER eliminate this?
    case r@ReducedQuadraticRoot(_, _, _, _) => // TESTME
      r.evaluateAsIs
    case Noop => // TESTME
      None
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
    * Method to determine if this NumberLike object is exact.
    * For instance, Number.pi is exact, although if you converted it into a PureNumber, it would no longer be exact.
    *
    * @return true if this NumberLike object is exact in the context of No factor, else false.
    */
  def isExact: Boolean =
    evaluateAsIs.exists(_.isExact)

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
    * Attempts to simplify the `CompositeExpression` by identifying and reducing trivial expressions
    * into their simpler or more elementary forms, if possible.
    * A trivial simplification is one that depends on fortuitous expressions, not necessarily constants,
    * that can therefore be combined in some way.
    *
    * @return an `em.AutoMatcher[Expression]` encapsulating the logic to simplify trivial forms
    *         within the `CompositeExpression`. The result contains either the simplified `Expression`
    *         or indicates that no trivial simplifications were possible.
    */
  def simplifyTrivial: em.AutoMatcher[Expression]

  /**
    * Simplifies an `Expression` by checking if its value can be directly evaluated into a constant.
    * If the `Expression` can be evaluated as a `Field`, it is replaced with a `Literal` containing that value.
    * Otherwise, no simplification is performed, and the match operation indicates a miss.
    *
    * @return an `em.AutoMatcher[Expression]` that matches `Expression` instances which can be directly
    *         evaluated into constants, and simplifies them by replacing with a `Literal`. If simplification
    *         is not possible, it returns a miss state with the original `Expression`.
    */
  def simplifyConstant: em.AutoMatcher[Expression] = em.Matcher[Expression, Expression]("simplifyConstant") {
    expr =>
      expr.evaluateAsIs match {
        case Some(f) =>
          em.Match(Expression(f)) map (_.simplify)
        case _ =>
          em.Miss("matchSimpler: cannot be simplified", expr)
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
  @deprecated("Use substituteTerms instead", "0.1.0")
  def substituteTerms(terms: Seq[Expression]): CompositeExpression

  /**
    * Method to render this NumberLike in a presentable manner.
    *
    * @return a String
    */
  def render: String =
    materialize.render

  /**
    * Method to determine what `Factor`, if there is such, this `NumberLike` object is based on.
    *
    * @return an optional `Factor`.
    */
  def maybeFactor: Option[Factor] =
    evaluateAsIs flatMap (_.maybeFactor)
}

/**
  * The `Noop` object is an atomic expression that represents a no-operation placeholder in an expression tree.
  * It cannot be evaluated, simplified, or associated with any specific factor. It is a concrete implementation
  * of the `AtomicExpression` trait.
  */
case object Noop extends AtomicExpression {

  lazy val value: Field =
    throw new UnsupportedOperationException("Noop.value")

  /**
    * Action to evaluate this `Expression` as a `Field`,
    * NOTE: no simplification occurs here.
    *
    * @return a `Field`.
    */
  def evaluate(context: Context): Option[Field] =
    throw new UnsupportedOperationException("Noop.evaluate")

  /**
    * Provides an approximation of the expression represented by this object.
    * Since this implementation represents a no-operation (Noop), there is
    * no meaningful approximation available.
    *
    * @return None, indicating that no approximation can be provided.
    */
  def approximation: Option[Real] = None

  /**
    * Method to render this NumberLike in a presentable manner.
    *
    * @return a String
    */
  def render: String = "Noop"

  /**
    * Method to determine what `Factor`, if there is such, this `NumberLike` object is based on.
    *
    * @return an optional `Factor`.
    */
  def maybeFactor: Option[Factor] = None
}

/**
  * Represents an abstract expression for a field that can optionally be associated with a name.
  *
  * A `FieldExpression` provides methods for evaluating its qualifications within
  * a given context, rendering its representation, and determining equality or hashing.
  * It extends the `AtomicExpression` trait, allowing it to be used wherever atomic
  * expressions are valid.
  *
  * @param value     the `Field` associated with the expression
  * @param maybeName an optional name for the field expression
  */
abstract class FieldExpression(val value: Field, val maybeName: Option[String] = None) extends AtomicExpression {

  /**
    * Applies the given `ExpressionFunction` to the current context of the `FieldExpression`
    * and attempts to produce an atomic expression as its result.
    * NOTE that, if there is a result defined, it is exact and, preferably, a `FieldExpression`.
    *
    * @param f the `ExpressionFunction` to be applied. This function determines how the
    *          evaluation will transform the current `FieldExpression` into a potential result.
    * @return an `Option` containing a `FieldExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionFunction): Option[FieldExpression]

  /**
    * Evaluates the current field expression within the given context and determines
    * if the field qualifies based on the context's rules.
    *
    * NOTE: we now have Field values that may not have a unique factor (Algebraics).
    * That's why we need to check first if value has a unique factor.
    *
    * If the field meets the qualifications specified by the context, the method
    * returns an `Option` containing the field. Otherwise, it returns `None`.
    *
    * @param context the context in which the field is evaluated. It defines the
    *                qualification rules for determining whether the field is valid.
    * @return `Some(Field)` if the field qualifies within the given context, otherwise `None`.
    */
  def evaluate(context: Context): Option[Field] =
    Option.when(value.maybeFactor.isDefined && context.fieldQualifies(value))(value)

  /**
    * Method to determine what `Factor`, if there is such, this `NumberLike` object is based on.
    *
    * @return an optional `Factor`.
    */
  def maybeFactor: Option[Factor] =
    evaluateAsIs flatMap (_.maybeFactor)

  /**
    * Attempts to approximate the current field expression as a Real number.
    *
    * @return Some(Real) if the field can be approximated as a Real number, otherwise None.
    */
  def approximation: Option[Real] = value match {
    case r: Real =>
      Some(r)
    case algebraic: Algebraic =>
      algebraic.solve.asField match {
        case r: Real =>
          Some(r)
        case _ =>
          None // TESTME
      }
    case _ =>
      None // TESTME
  }

  /**
    * Method to render this NumberLike in a presentable manner.
    *
    * @return a String
    */
  def render: String = maybeName getOrElse value.render // TESTME

  /**
    * Generate a String for debugging purposes.
    *
    * @return a String representation of this Literal.
    */
  override def toString: String =
    maybeName getOrElse value.toString

  /**
    * Determines whether the provided object can be considered equal to an instance of `FieldExpression`.
    *
    * @param other the object to compare with this instance.
    * @return true if the provided object is an instance of `FieldExpression`, false otherwise.
    */
  private def canEqual(other: Any): Boolean =
    other.isInstanceOf[FieldExpression]

  override def equals(other: Any): Boolean = other match {
    case that: FieldExpression =>
      that.canEqual(this) &&
          value == that.value &&
          namesMatch(that)
    case _ =>
      false
  }

  /**
    * Compares the `maybeName` field of this `FieldExpression` with the `maybeName` field
    * of another `FieldExpression` to determine if they match.
    *
    * @param other the other `FieldExpression` instance to compare with.
    * @return true if both `maybeName` values are defined and equal, or if any of them is undefined; false otherwise.
    */
  private def namesMatch(other: FieldExpression): Boolean =
    (maybeName, other.maybeName) match {
      case (Some(x), Some(y)) =>
        x == y
      case _ =>
        true
    }

  override def hashCode(): Int = { // TESTME
    val state = Seq(value, maybeName)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

/**
  * The `FieldExpression` companion object provides a mechanism to extract elements from a `FieldExpression` instance.
  * It defines the `unapply` method to enable pattern matching and decomposition of `FieldExpression` objects.
  *
  * The extraction operation retrieves the underlying `Field` and an optional name, if available.
  * It handles two main cases:
  * 1. For instances of `Literal`, it extracts the `Field` value along with `None` for the optional name.
  * 2. For other cases, it extracts the `Field` value along with its associated optional name.
  *
  * @see FieldExpression
  * @see Literal
  */
object FieldExpression {
  /**
    * Extracts components from a `FieldExpression` instance, enabling pattern matching.
    *
    * The `unapply` method decomposes a `FieldExpression` into its constituent `Field` and an
    * optional name. For instances of `Literal`, the optional name is always `None`.
    *
    * @param f the `FieldExpression` to be decomposed.
    * @return an `Option` containing a tuple of the `Field` and an optional name derived
    *         from the given `FieldExpression`. If the input cannot be decomposed, returns `None`.
    */
  def unapply(f: FieldExpression): Option[(Field, Option[String])] = f match {
    case Literal(x, _) =>
      Some((x, None))
    case _ =>
      Some((f.value, f.maybeName))
  }
}

/**
  * A `FieldExpression` based on a literal `Field`.
  *
  * @param value     the `Field`.
  * @param maybeName an optional name (typically this will be None).
  */
case class Literal(override val value: Field, override val maybeName: Option[String] = None) extends FieldExpression(value, maybeName) {
  /**
    * Applies the given `ExpressionFunction` to the current context of the `FieldExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionFunction` to be applied. This function determines how the
    *          evaluation will transform the current `FieldExpression` into a potential result.
    * @return an `Option` containing an `FieldExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionFunction): Option[FieldExpression] = f match {
    case Negate =>
      value match {
        case Real(ExactNumber(_, _: Scalar)) =>
          Some(Literal(-value))
        case _ =>
          None
      }
    // TODO implement for other functions
    case _ =>
      None
  }
}

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
  def apply(x: Int): Expression = x match {
    case 0 => Zero
    case 1 => One
    case -1 => MinusOne
    case 2 => Two
    case _ => Literal(Rational(x))
  }

  /**
    * Creates a Literal instance from a Rational value.
    *
    * @param x the Rational value to be wrapped in a Literal
    * @return a Literal instance containing the given Rational value encapsulated in a Real
    */
  def apply(x: Rational): Expression = x match {
    case Rational(n, Rational.bigOne) =>
      Literal(Real(n))
    case Rational.half =>
      Half // TESTME
    case _ => Literal(Real(x))
  }

  /**
    * Creates a new Literal instance wrapping the given Double value.
    *
    * @param x the Double value to be wrapped in the Literal.
    * @return a Literal instance containing the provided Double value as a Real.
    */
  def apply(x: Double): Literal =
    Literal(Real(x))

  /**
    * Creates a Literal instance from a given number.
    *
    * @param x the number to convert into a Literal
    * @return a Literal instance representing the given number
    */
  def apply(x: Number): Expression = x match {
    case Number.one =>
      One
    case Number.zero =>
      Zero
    case Number.pi =>
      ConstPi
    case Number.two =>
      Two
    case Number.negOne =>
      MinusOne
    case Number.half =>
      Half
    case Number.e =>
      ConstE
    case _ =>
      Literal(Real(x))
  }
}

/**
  * Represents a specific constant whose value is a `Field`, with an associated name.
  *
  * This abstract class extends [[FieldExpression]], allowing for named representation
  * of constants in mathematical expressions, while being tied to a specific field type.
  *
  * @constructor Creates a named constant within the context of the provided field.
  * @param x    the mathematical field to which this constant belongs.
  * @param name the name associated with this constant.
  */
abstract class NamedConstant(x: Field, name: String) extends FieldExpression(x, Some(name))

/**
  * An abstract representation of a scalar constant in a specific mathematical field,
  * for example, π (pi), 1, 0, but not `e`.
  *
  * This class extends `NamedConstant`, tying the scalar constant to a particular
  * `Field` instance and associating it with a specific name. It is designed to
  * represent immutable scalar constants in mathematical expressions, leveraging
  * the properties and operations of the `Field`.
  *
  * @param x    the `Field` instance representing the value of the scalar constant.
  * @param name the name associated with the scalar constant.
  */
abstract class ScalarConstant(x: Field, name: String) extends NamedConstant(x, name)

/**
  * Represents the mathematical constant zero.
  * This object extends the `ScalarConstant` class, providing implementation specific to zero.
  */
case object Zero extends ScalarConstant(Constants.zero, "0") {
  /**
    * Applies the given `ExpressionFunction` to the current context of the `FieldExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionFunction` to be applied. This function determines how the
    *          evaluation will transform the current `FieldExpression` into a potential result.
    * @return an `Option` containing an `FieldExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionFunction): Option[FieldExpression] = f match {
    case Negate =>
      Some(this)
    case Exp =>
      Some(One)
    case Reciprocal =>
      Some(Infinity)
    case _ =>
      None
  }
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
case object Half extends ScalarConstant(Constants.half, "\u00BD") {
  /**
    * Applies the given `ExpressionFunction` to the current context of the `FieldExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionFunction` to be applied. This function determines how the
    *          evaluation will transform the current `FieldExpression` into a potential result.
    * @return an `Option` containing an `FieldExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionFunction): Option[FieldExpression] = f match {
    case Negate =>
      Some(Literal(-Constants.half)) // TESTME
    case Reciprocal =>
      Some(Two) // TESTME
    case _ =>
      None // TESTME
  }
}

/**
  * Represents the constant numeric value `1`.
  *
  * `One` is a case object that extends `ScalarConstant`, indicating that it is a well-defined,
  * immutable, and atomic mathematical value.
  */
case object One extends ScalarConstant(Constants.one, "1") {
  /**
    * Applies the given `ExpressionFunction` to the current context of the `FieldExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionFunction` to be applied. This function determines how the
    *          evaluation will transform the current `FieldExpression` into a potential result.
    * @return an `Option` containing an `FieldExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionFunction): Option[FieldExpression] = f match {
    case Negate =>
      Some(MinusOne)
    case Reciprocal =>
      Some(this)
    case Exp =>
      Some(ConstE)
    case Log =>
      Some(Zero)
    case _ =>
      None // TESTME
  }
}

/**
  * Represents the constant value -1.
  *
  * MinusOne is a specific instance of the `ScalarConstant` class, which evaluates to
  * the numerical value -1 regardless of the context.
  *
  * This constant can be used in mathematical expressions involving fields and
  * supports operations defined in the `Field` trait.
  */
case object MinusOne extends ScalarConstant(Constants.minusOne, "-1") {
  /**
    * Applies the given `ExpressionFunction` to the current context of the `FieldExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionFunction` to be applied. This function determines how the
    *          evaluation will transform the current `FieldExpression` into a potential result.
    * @return an `Option` containing an `FieldExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionFunction): Option[FieldExpression] = f match {
    case Negate =>
      Some(One)
    case Reciprocal =>
      Some(this) // TESTME
    case _ =>
      None // TESTME
  }
}

/**
  * Represents the constant number 2 as a case object, extending the `ScalarConstant` class.
  *
  * This object evaluates to the numeric constant `2` within the given context.
  * It can be used in mathematical expressions and operations involving constants.
  */
case object Two extends ScalarConstant(Constants.two, "2") {
  /**
    * Applies the given `ExpressionFunction` to the current context of the `FieldExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionFunction` to be applied. This function determines how the
    *          evaluation will transform the current `FieldExpression` into a potential result.
    * @return an `Option` containing an `FieldExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionFunction): Option[FieldExpression] = f match {
    case Reciprocal =>
      Some(Half)
    case Negate =>
      Some(Literal(-2))
    case _ =>
      None
  }
}

/**
  * ConstPi represents the mathematical constant π (pi) exactly.
  */
case object ConstPi extends ScalarConstant(Constants.pi, "π") {
  /**
    * Applies the given `ExpressionFunction` to the current context of the `FieldExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionFunction` to be applied. This function determines how the
    *          evaluation will transform the current `FieldExpression` into a potential result.
    * @return an `Option` containing an `FieldExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionFunction): Option[FieldExpression] = f match {
    case Sine =>
      Some(Zero)
    case Cosine =>
      Some(MinusOne)
    case _ =>
      None
  }
}

/**
  * The constant e.
  * Yes, this is an exact number.
  */
case object ConstE extends NamedConstant(Constants.e, "e") {
  /**
    * Applies the given `ExpressionFunction` to the current context of the `FieldExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionFunction` to be applied. This function determines how the
    *          evaluation will transform the current `FieldExpression` into a potential result.
    * @return an `Option` containing an `FieldExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionFunction): Option[FieldExpression] = f match {
    case Log =>
      Some(One)
    case _ =>
      None
  }
}

/**
  * The constant i (viz., the square root of 2)
  * Yes, this is an exact number.
  */
case object ConstI extends NamedConstant(Constants.i, "i") { // TESTME
  /**
    * Applies the given `ExpressionFunction` to the current context of the `FieldExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionFunction` to be applied. This function determines how the
    *          evaluation will transform the current `FieldExpression` into a potential result.
    * @return an `Option` containing an `FieldExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionFunction): Option[FieldExpression] = f match {
    case _ =>
      None
  }
}

/**
  * Represents an infinite value in the field of expressions.
  *
  * The `Infinity` object is a special case of `NamedConstant` with a value of positive infinity.
  * It is immutable and serves as a singleton instance to represent the mathematical concept of infinity
  * in calculations or expressions. It overrides certain behaviors of `FieldExpression` to handle
  * operations specific to infinity.
  */
case object Infinity extends NamedConstant(Rational.infinity, "∞") {
  /**
    * Applies the given `ExpressionFunction` to the current context of the `FieldExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionFunction` to be applied. This function determines how the
    *          evaluation will transform the current `FieldExpression` into a potential result.
    * @return an `Option` containing an `FieldExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionFunction): Option[FieldExpression] = f match { // TESTME
    case Reciprocal =>
      Some(Zero)
    case _ =>
      None
  }
}

/**
  * This class represents a monadic function of the given expression.
  *
  * @param x the expression being operated on.
  * @param f the function to be applied to x.
  */
case class Function(x: Expression, f: ExpressionFunction) extends CompositeExpression {

  /**
    * Provides the terms that comprise this `CompositeExpression`.
    *
    * @return a sequence of `Expression` objects representing the individual terms of this `CompositeExpression`.
    */
  def terms: Seq[Expression] = Seq(x) // TESTME

  /**
    * Substitutes the terms of this `CompositeExpression` with the provided sequence of expressions.
    *
    * @param terms the sequence of `Expression` objects to replace the terms of this `CompositeExpression`.
    * @return a new `Expression` where the terms are substituted with the given sequence.
    */
  def substituteTerms(terms: Seq[Expression]): CompositeExpression =
    copy(x = terms.head)

  /**
    * Method to determine the depth of this Expression.
    *
    * @return the 1 + depth of x.
    */
  def depth: Int =
    1 + x.depth

  /**
    * Action to simplify this Expression as a Field.
    *
    * @return the materialized Field.
    */
  def evaluate(context: Context): Option[Field] = x match {
    case AtomicExpression(field) =>
      f.applyExact(field)
    case _ =>
      x.evaluate(context) map f
  }

  /**
    * Provides an approximation of the result of applying the function `f` to the
    * `approximation` of the expression `x`, if one exists.
    *
    * @return an `Option` containing the approximated result as a `Real` if the
    *         approximation is successfully computed; otherwise, `None`.
    */
  def approximation: Option[Real] =
    x.approximation map f match {
      case Some(r: Real) =>
        Some(r)
      case _ =>
        None // TESTME
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
    * Attempts to apply trivial simplifications to this `Expression`.
    * Currently, this method always fails with a message indicating no trivial simplifications are available.
    *
    * @return an `em.AutoMatcher[Expression]` that always fails with a miss case,
    *         as no trivial simplifications are possible.
    */
  def simplifyTrivial: em.AutoMatcher[Expression] =
    em.Matcher("Function: simplifyTrivial") {

      // XXX we check for certain exact literal function results
      case Function(e@FieldExpression(_, _), f) if e.monadicFunction(f).isDefined =>
        em.matchIfDefined(e.monadicFunction(f))(e)

      case expr =>
        em.Miss("Function: simplifyTrivial: no trivial simplifications", expr)
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
      case Function(Function(x, f), g) if em.complementaryMonadic(f, g) =>
        em.Match(x)
      case x: Expression =>
        em.Miss[Expression, Expression]("Function.simplifyComposite: not complementary", x)
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

  /**
    * Simplifies a given `BiFunction` expression where trivial simplifications can be applied.
    * This method identifies patterns in the expression that represent mathematical identities
    * or redundancies and replaces them with their simplified form. Examples of such patterns
    * include removing identities, handling zero and one in products or powers, and other basic
    * algebraic simplifications.
    *
    * The simplifications include:
    * - Removing or simplifying identity elements (e.g., `x * 1 = x`, `x + 0 = x`).
    * - Handling special cases like zeroes in products or powers.
    * - Applying reductions for specific constants or algebraic terms.
    *
    * If no simplification can be applied, the method encapsulates this as a "miss" result.
    * Implementations of this method aim to provide lightweight simplification logic,
    * with more complex cases deferred to other parts of the system.
    *
    * @return an `em.AutoMatcher[Expression]` instance encapsulating the simplified `Expression`
    *         if simplification was possible, or a "miss" if no trivial simplification could be applied.
    */
  def simplifyTrivial: em.AutoMatcher[Expression] =
    em.Matcher[Expression, Expression]("BiFunction: simplifyTrivial") {
      case BiFunction(a, b, f) if matchingIdentity(a, f, left = true).contains(true) =>
        em.Match(b)
      case BiFunction(a, b, f) if matchingIdentity(b, f, left = false).contains(true) =>
        em.Match(a)
      case BiFunction(Zero, _, Product) | BiFunction(_, Zero, Product) =>
        em.Match(Zero)
      case BiFunction(a, One, Product) =>
        em.Match(a)
      case BiFunction(One, b, Product) =>
        em.Match(b)
      case BiFunction(_, Zero, Power) =>
        em.Match(One)
      case BiFunction(a, MinusOne, Product) =>
        em.Match(Function(a, Negate))
      case BiFunction(MinusOne, b, Product) =>
        em.Match(Function(b, Negate))
      case BiFunction(a, b, Sum) if a == b =>
        em.Match(BiFunction(a, Two, Product))
      case BiFunction(a, b, Product) if a == b =>
        em.Match(BiFunction(a, Two, Power))
      case BiFunction(ConstE, Literal(ComplexCartesian(Number.zero, Number.pi), _), Power) =>
        em.Match(MinusOne)
      case BiFunction(ConstE, Literal(ComplexPolar(Number.pi, Number.piBy2, _), _), Power) =>
        em.Match(MinusOne)
      // TODO match on Algebraic_Linear also
      case BiFunction(Literal(Algebraic_Quadratic(_, e1, b1), _), Literal(Algebraic_Quadratic(_, e2, b2), _), f) if e1 == e2 =>
        // XXX Apply Vieta's formula
        f match {
          case Sum if b1 != b2 =>
            em.Match(e1.p.makeNegative)
          case Product if b1 != b2 =>
            em.Match(e1.q)
          case _ =>
            em.Miss[Expression, Expression](s"BiFunction: simplifyTrivial: no trivial simplification for Algebraics and $f", this) // TESTME
        }
      case BiFunction(Literal(a@Algebraic_Quadratic(_, _, _), _), x, f) =>
        modifyQuadratic(a, x, f)
      case BiFunction(x, Literal(a@Algebraic_Quadratic(_, _, _), _), f) =>
        modifyQuadratic(a, x, f)
      case _ =>
        em.Miss[Expression, Expression]("BiFunction: simplifyTrivial: no trivial simplifications", this)
    }

  /**
    * Simplifies a `CompositeExpression` represented as a `BiFunction` by applying various matchers
    * to identify opportunities for simplification. Specifically:
    * - Converts the `BiFunction` into an `Aggregate` for consistent simplification processing.
    * - Attempts to simplify complementary terms within the `BiFunction`.
    * - Applies additional simplification logic as defined by `Expression.simplifyComposite` and `matchSimpler`.
    *
    * If the expression cannot be simplified, the result will indicate the failure.
    *
    * @return an `em.AutoMatcher[Expression]` that encapsulates the logic for simplifying the `BiFunction`.
    *         It provides either the simplified `Expression` or indicates that no simplification was possible.
    */
  def simplifyComposite: em.AutoMatcher[Expression] = em.Matcher[Expression, Expression]("BiFunction: simplifyComposite") {
    case BiFunction(a, Function(b, Negate), Product) if a == b =>
      val xSq = Expression.simplifyConstant(BiFunction(a, Two, Power)).getOrElse(BiFunction(a, Two, Power))   // x²
      em.Match(Function(xSq, Negate))

    case BiFunction(Function(a, Negate), b, Product) if a == b =>  // TESTME
      val xSq = Expression.simplifyConstant(BiFunction(a, Two, Power)).getOrElse(BiFunction(a, Two, Power))
      em.Match(Function(xSq, Negate))

    case b@BiFunction(_, _, _) =>
      ((em.complementaryTermsEliminatorBiFunction |
          em.matchBiFunctionAsAggregate & em.literalsCombiner) &
          em.alt(matchSimpler))(b)
    case b =>
      em.Miss("simplifyComposite", b) // TESTME
  }

  /**
    * Method to determine the depth of this Expression.
    *
    * @return the depth (an atomic expression has depth of 1).
    */
  def depth: Int =
    1 + math.max(a.depth, b.depth)

  /**
    * Action to simplify this Expression as a Field.
    * NOTE that because we need to be able to evaluate this Expression exactly,
    * we need to be sure that the Context passed in to f.evaluate is not None.
    *
    * @return the materialized Field.
    */
  def evaluate(context: Context): Option[Field] =
    context.qualifyingField(f.evaluate(a, b)(context))

  /**
    * Provides the terms that comprise this `CompositeExpression`.
    *
    * @return a sequence of `Expression` objects representing the individual terms of this `CompositeExpression`.
    */
  def terms: Seq[Expression] =
    Seq(a, b) // TESTME

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
  override def toString: String =
    s"BiFunction{$a $f $b}"

  /**
    * Computes the approximation of the `BiFunction` if approximations for both components (`a` and `b`) exist,
    * and applies the function `f` to these approximations. If the resulting value is a valid `Real`, it is returned.
    * Otherwise, `None` is returned.
    *
    * @return an `Option[Real]` representing the computed approximation if possible; otherwise, `None`.
    */
  def approximation: Option[Real] =
    (for (x <- a.approximation; y <- b.approximation) yield f(x, y)) match {
      case Some(r: Real) =>
        Some(r)
      case _ =>
        None // TESTME
    }

  /**
    * Regular hashCode method.
    * TESTME
    *
    * @return an Int depending on f, a, and b.
    */
  override def hashCode(): Int =
    java.util.Objects.hash(f, a, b) // TESTME

  /**
    * An equals method which considers two BiFunctions, which are non-identical but symmetric, to be equal.
    *
    * @param obj the other object.
    * @return true if the values of these two expressions would be the same (without any evaluation).
    */
  override def equals(obj: Any): Boolean = obj match {
    case BiFunction(c, d, g) =>
      f == g && (a == c && b == d | f != Power && a == d && b == c)
    case _ =>
      false
  }

  /**
    * Checks if a given `Expression` matches the identity element of a specific `ExpressionBiFunction`.
    * This method identifies if the evaluated value of the given `Expression` is equal to the identity
    * element provided by the `ExpressionBiFunction`. The search for the identity depends on whether
    * the function operates on the left or right operand.
    *
    * @param exp  the `Expression` to be evaluated and checked against the identity element.
    * @param f    the `ExpressionBiFunction` containing the potential identity elements.
    * @param left a boolean determining whether to check the left operand's identity (`true`) or
    *             the right operand's identity (`false`). If the right identity is not present,
    *             the method will fallback to the left identity.
    * @return an `Option[Boolean]` indicating whether the `Expression` matches the identity:
    *         - `Some(true)` if the match is successful.
    *         - `Some(false)` if the match fails or if the `Expression` is not atomic.
    *         - `None` if no identity element exists in the provided `ExpressionBiFunction`.
    */
  private def matchingIdentity(exp: Expression, f: ExpressionBiFunction, left: Boolean): Option[Boolean] =
    exp match {
      case expression: AtomicExpression =>
        for {
          identity <- if (left) f.maybeIdentityL else f.maybeIdentityR orElse f.maybeIdentityL
          field <- expression.evaluateAsIs
        } yield field == identity
      case _ =>
        Some(false)
    }

  /**
    * Scales a quadratic algebraic term by a given expression and attempts to simplify the operation.
    * If the expression is atomic and reducible (e.g., a rational number), the scaling is simplified.
    * Otherwise, returns a "miss" result indicating that no simplification was possible.
    *
    * @param a the `Algebraic_Quadratic` term that is being scaled.
    * @param x the `Expression` to scale `a` by.
    * @return a `MatchResult[Expression]`, which either contains the simplified scaled expression
    *         or indicates that no simplification was possible.
    */
  private def modifyQuadratic(a: Algebraic_Quadratic, x: Expression, f: ExpressionBiFunction): em.MatchResult[Expression] =
    x match {
      case expr: AtomicExpression =>
        expr.evaluateAsIs match {
          case Some(y: Real) if y.isExact =>
            (y.x.toNominalRational, f) match {
              case (Some(z), Sum) =>
                em.Match(Literal(a.add(z)))
              case (Some(z), Product) =>
                em.Match(Literal(a.scale(z)))
              case (Some(Rational.two), Power) =>
                val expression = Expression(a) * (-a.equation.p)
                val simplified = (expression plus (-a.equation.q)).simplify
                em.Match(simplified)
              case (None, _) =>
                em.Miss[Expression, Expression](s"BiFunction: simplifyTrivial: no trivial simplification for $a $f $x (not Rational)", this) // TESTME
            }
          case None =>
            em.Miss[Expression, Expression](s"BiFunction: simplifyTrivial: no trivial simplification for $a $f $x (not Real)", this) // TESTME
        }
      case _ =>
        em.Miss[Expression, Expression](s"BiFunction: simplifyTrivial: no trivial simplification for $a $f $x (not Atomic)", this) // TESTME
    }
}

object BiFunction {
  /**
    * Constructs a BiFunction instance based on the provided DyadicTriple.
    * The method converts the left side of the DyadicTriple to a tuple and pairs it with the right side,
    * creating a BiFunction with the extracted components.
    *
    * NOTE: this is here mostly for historical purposes.
    * We no longer use DyadicTriple except as a convenience in the unit tests.
    *
    * @param dt the DyadicTriple from which the BiFunction will be initialized.
    *           The left side of the DyadicTriple is expected to provide a tuple,
    *           and the right side will be combined with this tuple to construct the BiFunction.
    * @return a new BiFunction initialized with the components derived from the given DyadicTriple.
    */
  def apply(dt: DyadicTriple): BiFunction = {
    val tuple = (dt.l.asTuple, dt.r)
    new BiFunction(tuple._1._2, tuple._2, tuple._1._1)
  }

  implicit def convertFromDyadicTriple(dt: DyadicTriple): BiFunction = apply(dt)
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
    * Simplifies a given `Expression` by removing trivial components or replacing grouped terms
    * with their corresponding higher-level mathematical function (if applicable).
    *
    * This method uses pattern matching to identify and transform instances of `Aggregate`:
    * - Removes identity elements for the aggregate function.
    * - Groups identical terms and replaces them with a new aggregate expression
    * using the next higher mathematical function.
    *
    * @return an `AutoMatcher` for `Expression` capable of identifying and performing
    *         simplifications of trivial aggregates.
    */
  def simplifyTrivial: em.AutoMatcher[Expression] =
    em.Matcher[Expression, Expression]("BiFunction: simplifyTrivial") {
      // Remove identity values
      case Aggregate(f, xs) =>
        em.Match(Aggregate(f, xs filterNot (x => f.maybeIdentityL.contains(x))))
    }

  /**
    * Simplifies a composite `Expression` by leveraging the `simplifyAggregate` method.
    * This is typically used to reduce composite mathematical expressions into their simplest form when possible.
    *
    * @return an `AutoMatcher` for `Expression` that applies the simplification logic defined in `simplifyAggregate`.
    */
  def simplifyComposite: em.AutoMatcher[Expression] = {
    case t: Aggregate =>
      em.simplifyAggregate(t)
    case x: Expression => // TESTME
      em.Miss[Expression, Expression]("Aggregate.simplifyComposite: not aggregate", x)
  }

  /**
    * Evaluates the given context to produce an optional field, leveraging the aggregate function
    * and associated evaluation logic. The method employs an iterative process to evaluate a sequence
    * of terms within the provided context while considering identity elements and context transformation rules.
    *
    * @param context the context in which the evaluation should take place. This defines the constraints
    *                and qualifications for the terms involved in the evaluation process.
    * @return an `Option[Field]` representing the result of the evaluation. Returns `None`
    *         if the evaluation cannot produce a valid field or if an invalid context is encountered.
    */
  def evaluate(context: Context): Option[Field] = {

    // NOTE we combine the expressions of this `Aggregate` but maintain a context which in general changes as we combine terms.
    // The initial context is determined by the parameter `context` and the function's `leftContext` method.
    // The resulting tuple of optional Field and Context is then matched.
    // If the context is impossible (for example, we multiplied a pure number by a logarithmic number such as `e`,
    // then we cannot evaluate this `Aggregate` exactly.
    xs.foldLeft[(Option[Field], Context)]((function.maybeIdentityL, function.leftContext(context)))(combineExpressions) match {
      case (_, ImpossibleContext) =>
        None
      case (fo, _) =>
        fo
    }
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
    * TESTME
    *
    * @param ys the sequence of expressions to be added
    * @return a new Aggregate instance with the updated list of expressions
    */
  def addAll(ys: Seq[Expression]): Aggregate =
    copy(xs = xs ++ ys) // TESTME

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
    * Attempts to compute an approximate value by applying the aggregate function over its components.
    * If all components can be approximated successfully and the aggregate function produces a valid result,
    * the final approximation is returned. If any component fails to be approximated, the result will be `None`.
    *
    * @return an `Option[Real]` where `Some(Real)` represents the successfully computed approximation,
    *         or `None` if the approximation could not be determined.
    */
  def approximation: Option[Real] = { // TESTME
    val identity: Field = function.maybeIdentityL.getOrElse(Constants.zero) // NOTE should never require the default
    val maybeFields: Seq[Option[Field]] = xs.map(e => e.approximation)
    FP.sequence(maybeFields) map (xs => xs.foldLeft[Field](identity)(function)) match {
      case Some(r: Real) =>
        Some(r)
      case _ =>
        None
    }
  }

  /**
    * Method to render this NumberLike in a presentable manner.
    *
    * @return a String
    */
  override def toString: String =
    xs.mkString(s"Aggregate{${function.toString},", ",", "}")

  /**
    * Combines an accumulator and an expression to produce a new tuple containing an optional field
    * and an updated context.
    * This is the function that is invoked by `foldLeft` in the `evaluate` method.
    *
    * @param accum a tuple consisting of an optional field and the current context. The field represents
    *              a computed value (if any), and the context provides additional information
    *              required for processing.
    * @param x     the expression to be combined with the accumulator. This expression is used to derive
    *              updates to the field and/or context within the tuple.
    * @return a new tuple containing an updated optional field and context after combining
    *         the accumulator and the given expression.
    */
  private def combineExpressions(accum: (Option[Field], Context), x: Expression): (Option[Field], Context) = {
    val (fo, context) = accum
    combineFieldsAndContexts(x, fo, context)
  }

  /**
    * Combines a given `Expression`, an optional `Field`, and a `Context` into a new tuple containing an updated
    * optional field and context. The combination logic evaluates the expression within the given context, applying
    * transformations to the field and context when applicable.
    *
    * @param x       the expression to be evaluated and combined with the field and context.
    * @param fo      an optional field representing a potential starting value or state that may be updated
    *                during the combination process.
    * @param context the current context used for evaluating the expression and determining the resulting
    *                updated context.
    * @return a tuple consisting of an updated optional field and the resulting context after processing the
    *         given expression with the provided field and context.
    */
  private def combineFieldsAndContexts(x: Expression, fo: Option[Field], context: Context): (Option[Field], Context) =
    (for (a <- fo; b <- x.evaluate(context)) yield {
      val field = function(a, b)
      field -> (for (factor <- field.maybeFactor) yield function.rightContext(factor)(context))
    }) match {
      case Some((f, Some(qq))) =>
        Some(f) -> qq
    }
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

  /**
    * Creates an instance of `Aggregate` using the given binary function and sequence of expressions.
    * Throws an `IllegalArgumentException` if the sequence of expressions is empty.
    *
    * @param function the binary function used to compose the `Aggregate`.
    * @param xs       a sequence of `Expression` objects to be aggregated.
    * @return an `Aggregate` instance composed of the binary function and the sequence of expressions.
    * @throws java.lang.IllegalArgumentException if the sequence of expressions is empty.
    */
  def create(function: ExpressionBiFunction, xs: Seq[Expression]): Aggregate =
    if (xs.nonEmpty)
      new Aggregate(function, xs)
    else
      throw new IllegalArgumentException("total requires at least one argument (use empty if necessary)") // TESTME

  /**
    * Constructs an `Aggregate` expression that applies the `Sum` operation
    * to a variable number of input expressions.
    *
    * @param xs a sequence of `Expression` instances that will be aggregated by the `Sum` operation.
    * @return an `Expression` representing the sum of the input expressions.
    */
  def total(xs: Expression*): Aggregate =
    create(Sum, xs)

  /**
    * Constructs an `Aggregate` expression that applies the `Product` operation
    * to a variable number of input expressions.
    *
    * @param xs a sequence of `Expression` instances that will be aggregated by the `Product` operation.
    * @return an `Expression` representing the product of the input expressions.
    */
  def product(xs: Expression*): Aggregate =
    create(Product, xs)
}

/**
  * Represents an exact reduced quadratic root expression derived from a quadratic equation.
  *
  * A `ReducedQuadraticRoot` is defined by the parameters `p`, `q`, and `pos`, which correspond
  * to the coefficients and the branch of the root being represented:
  * - `p` and `q` are the coefficients from the quadratic equation.
  * - `pos` determines which of the two possible roots is represented: the positive or the negative branch.
  *
  * The class enforces invariants:
  * - At least one of `p` or `q` must be non-zero.
  * - The discriminant of the quadratic equation, computed as `p^2 - 4*q`, must be non-negative.
  * These invariants ensure that the instance represents a valid root and corresponds to a real solution.
  *
  * This class extends from `AtomicExpression`, meaning the represented root cannot
  * be further simplified within the context of symbolic computation.
  *
  * NOTE that the development of this class preceded `Algebraic` fields like `Quadratic`.
  * CONSIDER eliminating this class as it no longer serves any unique essential purpose.
  *
  * @param name a string name for the root.
  * @param p    the coefficient representing the linear term in the quadratic equation.
  * @param q    the coefficient representing the constant term in the quadratic equation.
  * @param pos  determines whether this root represents the positive or the negative branch of the solution.
  */
class ReducedQuadraticRoot(val name: String, val p: Int, val q: Int, val pos: Boolean) extends AtomicExpression {
//  require(p != 0 || q != 0, "may not have p and q equal to zero")
  require(discriminant >= 0, "discriminant must not be negative")

  /**
    * For now, we consider a ReducedQuadraticRoot to be exact, even though we can't write it out exactly.
    *
    * @return true if this NumberLike object is exact in the context of No factor, else false.
    */
  override def isExact: Boolean = true

  /**
    * Method to determine what `Factor`, if there is such, this `NumberLike` object is based on.
    *
    * @return an optional `Factor`.
    */
  def maybeFactor: Option[Factor] =
    asAlgebraic.maybeFactor

  /**
    * Represents the reduced quadratic root expression defined within the context of a quadratic equation.
    * The root is computed based on the formula derived from the quadratic equation coefficients,
    * specifically involving the discriminant and a parameter `pos` that defines the root being evaluated.
    *
    * This expression involves:
    * - A literal representing `-p`
    * - An addition operation involving the discriminant's square root, determined by the `pos` parameter
    * - A division by 2 to compute the quadratic root.
    *
    * Note:
    * - The `discriminant` must be non-negative, as the square root operation is applied.
    * - The `pos` parameter determines whether the non-negative branch of the square root
    * (`+discriminant.sqrt`) or the negative branch (`-discriminant.sqrt`) is used in the computation.
    */
  lazy val root: Expression = {
    (Expression(-p) plus {
      // NOTE discriminant is required to be non-negative
      val branch: Expression = discriminant.sqrt
      if (pos) branch else -branch
    }) / 2
  }

  /**
    * Converts the current instance into an optional `Number` object.
    *
    * The method computes a value based on the discriminant of the quadratic expression.
    * If the discriminant is negative, there are no real roots and the result is `None`.
    * If the discriminant is zero, the result is a `Number` representing the single root.
    * Otherwise, attempts to compute a `Number` from the solution.
    *
    * @return an `Option[Number]` representing the numeric value derived from the quadratic root,
    *         or `None` if no valid `Number` can be determined.
    */
  override def asNumber: Option[Number] =
    if (discriminant < 0)
      None // TESTME
    else
      Option.when(discriminant == 0)(Number(Rational(p, 2))) orElse getSolution.asNumber

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
    * Evaluates the given context to determine the resulting field.
    *
    * The method solves a quadratic equation represented by the current context parameters
    * and retrieves the exact solution as a field, if possible.
    *
    * @param context the evaluation context that determines the rules by which the expression is evaluated.
    * @return an `Option[Field]` containing the evaluated field if calculable and exact; otherwise, `None`.
    */
  def evaluate(context: Context): Option[Field] = context match {
    case AnyContext =>
      Some(getSolution.asField)
    case RestrictedContext(PureNumber) =>
      val solution = getSolution
      asOptionalField(solution.branchOffset, solution.base)
    case RestrictedContext(SquareRoot) =>
      val solution = getSolution
      asOptionalField(solution.base, solution.branchOffset)
    case _ =>
      None
  }

  /**
    * Computes and returns an approximate numerical value for this expression.
    * The returned value is based on `asNumber`.
    * TESTME
    *
    * @return if possible, returns a `Real` representing the approximation of this expression.
    */
  def approximation: Option[Real] =
    getSolution.asNumber map (Real(_)) // TESTME

  /**
    * Converts this `ReducedQuadraticRoot` instance into its corresponding algebraic representation.
    *
    * The method constructs an `Algebraic_Quadratic` object using the quadratic expression
    * defined by the parameters `p` and `q` of this instance, along with the `pos` value
    * which denotes the specific branch of the solution set.
    *
    * @return an `Algebraic` object represented by the quadratic equation's algebraic form.
    */
  def asAlgebraic: Algebraic =
    Algebraic_Quadratic(Quadratic(p, q), pos)

  /**
    * Retrieves the solution for the quadratic equation represented by this instance.
    *
    * The method computes the solution using the algebraic representation of the quadratic equation
    * as defined by the instance's parameters. It delegates to the `solve` method of the
    * `Algebraic` instance obtained from `asAlgebraic`.
    *
    * @return a `Solution` object representing the solution of the quadratic equation
    */
  def getSolution: Solution =
    asAlgebraic.solve

  /**
    * Method to render this NumberLike in a presentable manner.
    *
    * @return a String
    */
  def render: String = name // TESTME

  /**
    * Provides a string representation of the object by returning the `name` field.
    *
    * @return a `String` that represents the `name` of this quadratic root instance.
    */
  override def toString: String = name

  /**
    * Determines if the given object can be considered equal to this instance.
    * This method is used to support the implementation of the equality operation.
    *
    * @param other the object to be compared against this instance
    * @return true if the given object is an instance of ReducedQuadraticRoot; false otherwise
    */
  private def canEqual(other: Any): Boolean =
    other.isInstanceOf[ReducedQuadraticRoot]

  /**
    * Compares this `ReducedQuadraticRoot` instance to another object for equality.
    * The comparison checks if the other object is of the same type and has equivalent values
    * for the properties of `p`, `q`, and `pos`.
    *
    * @param other the object to compare with this instance for equality.
    * @return `true` if the provided object is a `ReducedQuadraticRoot` and has the same values
    *         for the relevant properties; otherwise, `false`.
    */
  override def equals(other: Any): Boolean = other match {
    case that: ReducedQuadraticRoot =>
      that.canEqual(this) && // TESTME
          p == that.p &&
          q == that.q &&
          pos == that.pos
    case _ =>
      false
  }

  /**
    * Computes the hash code for this instance based on its state.
    *
    * This method takes into account the values of the fields `p`, `q`, and `pos`,
    * ensuring that the hash code reflects the identity and equality of this object.
    *
    * @return an `Int` representing the hash code of this instance.
    */
  override def hashCode(): Int = { // TESTME
    val state = Seq(p, q, pos)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  /**
    * Converts the given values into an optional `Field` representation.
    *
    * The method checks if the first value is zero, and if so, maps the second value into a `Field`.
    * If the first value is not zero, the result is `None`.
    *
    * @param v1 the first value to be checked if it is zero
    * @param v2 the second value, which may be transformed into a `Field` if `v1` is zero
    * @return an `Option[Field]` containing the resulting field if `v1` is zero; otherwise, `None`
    */
  private def asOptionalField(v1: Value, v2: Value): Option[Field] = {
    val maybeValue: Option[Value] = when(Value.isZero(v1))(v2)
    maybeValue.map(v => new Real(Number.create(v)))
  }
}

/**
  * Companion object for the `ReducedQuadraticRoot` class.
  * Provides an unapply method for pattern matching.
  */
object ReducedQuadraticRoot {
  /**
    * Creates an instance of `ReducedQuadraticRoot` from an `Algebraic` object
    * if the input is quadratic and if the parameters can be successfully converted to integers.
    *
    * @param algebraic the `Algebraic` object containing the data to create the `ReducedQuadraticRoot`
    * @return an `Option[ReducedQuadraticRoot]` containing the created instance if the parameters are valid,
    *         or `None` if the conversion fails
    */
  def create(algebraic: Algebraic): Option[ReducedQuadraticRoot] = algebraic match {
    case aq: Algebraic_Quadratic =>
      for {
        p <- toIntOption(aq.equation.p)
        q <- toIntOption(aq.equation.q)
      } yield new ReducedQuadraticRoot(aq.maybeName.getOrElse("unnamed"), p, q, aq.pos)
    case _ =>
      None
  }

  /**
    * Extractor method for pattern matching on a `ReducedQuadraticRoot` instance.
    * Decomposes the instance into its constituent parts.
    *
    * @param rqr the `ReducedQuadraticRoot` instance to be decomposed
    * @return an `Option` containing a tuple of the name, p value, q value,
    *         and pos flag of the `ReducedQuadraticRoot` instance if it exists
    */
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
object Phi extends ReducedQuadraticRoot("\uD835\uDED7", -1, -1, true)

/**
  * Psi represents the conjugate of [[Phi]].
  */
object Psi extends ReducedQuadraticRoot("\uD835\uDED9", -1, -1, false)

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
  def apply(xs: Expression*): Expression =
    xs.toList match {
      case Nil =>
        throw new IllegalArgumentException("Empty Sequence") // TESTME
      case h :: Nil =>
        h // TESTME
      case h :: j :: Nil =>
        BiFunction(h, j, Sum)
      case _ =>
        Aggregate(Sum, xs) // TESTME
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
    apply(xs map (x => Literal(x, None)): _*) // TESTME
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
abstract class SineCos(sine: Boolean) extends ExpressionFunction(if (sine) "sin" else "cos", x => if (sine) x.sin else x.cos) {
  /**
    * Regardless of the value of `context`, the required `Context` for the parameter is `Radian`.
    *
    * @param context ignored.
    * @return a new `RestrictedContext` instance configured with the `Radian` factor.
    */
  def paramContext(context: Context): Context =
    RestrictedContext(Radian) // TESTME

  /**
    * Applies the sine or cosine function to a given `Field` value if the value matches specific constants.
    * Returns an exact result for known trigonometric values of zero, π/2, π, and 3π/2.
    *
    * @param x the input `Field` value to be evaluated.
    * @return an `Option[Field]` containing the result of the sine or cosine function if the input matches a known constant,
    *         or `None` if the input does not match any predefined constants.
    */
  def applyExact(x: Field): Option[Field] = x match {
    case Constants.zero =>
      Some(if (sine) Constants.zero else Constants.one)
    case Constants.piBy2 =>
      Some(if (sine) Constants.one else Constants.zero)
    case Constants.pi =>
      Some(if (sine) Constants.zero else -Constants.one)
    case Constants.piBy2Times3 =>
      Some(if (sine) -Constants.one else Constants.zero) // TESTME
    case _ =>
      None // TESTME
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
case object Atan extends ExpressionBiFunction("atan", Real.atan, false, None, None) {
  /**
    * Identifies and retrieves a restricted evaluation context suitable for left-hand operations.
    *
    * This method reduces the input `Context` by applying constraints, resulting in a
    * new `RestrictedContext` based on predefined evaluation criteria.
    *
    * @param context the input evaluation `Context` that defines the left-hand context for the operation.
    * @return a `RestrictedContext(PureNumber)` object that represents the constrained left-hand evaluation context.
    */
  def leftContext(context: Context): Context =
    RestrictedContext(PureNumber) or AnyRoot

  /**
    * Retrieves the right-hand evaluation context associated with this function.
    *
    * @param context the input evaluation `Context` that specifies the right-hand context for the operation.
    * @return the same `Context` object passed as input, representing the right-hand evaluation context.
    */
  def rightContext(factor: Factor)(context: Context): Context =
    RestrictedContext(PureNumber) or AnyRoot

  /**
    * Applies a binary operation to the provided `Field` elements `a` and `b`, with stricter evaluation rules,
    * and returns an optional result.
    * The evaluation succeeds only if the operation satisfies specific conditions
    * (e.g., exact representations or mathematical constraints).
    *
    * TODO there are many cases which are unimplemented and will fail.
    * For example, other angles based pi/3.
    * TESTME
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
        Some(Constants.pi) // TESTME
      case (Constants.one, Constants.one) =>
        Some(Constants.piBy4)
      case (Constants.one, Constants.root3) =>
        Some(Constants.piBy3)
      case (Constants.zero, Constants.one) =>
        Some(Constants.piBy2)
      case (Real(ExactNumber(x, PureNumber)), Real(ExactNumber(y, PureNumber))) => // TESTME
        for {
          q <- Value.maybeRational(x)
          p <- Value.maybeRational(y)
          r = p / q
          // TODO test this--I have no idea if this is correct
          d = if (Value.signum(x) == Value.signum(y)) 1 else -1
          v <- Operations.doTransformValueMonadic(Value.fromRational(r))(MonadicOperationAtan(d).functions)
        } yield Real(ExactNumber(v, Radian))
      case _ =>
        None // TESTME
    }
}

/**
  * Represents the natural logarithmic function as an ExpressionFunction.
  *
  * This object provides functionality to compute the natural logarithm (ln) of a given Number.
  * The underlying implementation utilizes the `log` method of the Number type.
  */
case object Log extends ExpressionFunction("log", x => x.log) {
  /**
    * Regardless of the value of `context`, the required `Context` for the parameter is `PureNumber`.
    *
    * @param context the initial `Context` to be modified or restricted.
    * @return a new `Context` object, which is a restricted version of the provided `context`.
    */
  def paramContext(context: Context): Context =
    AnyScalar or AnyLog // CONSIDER should we be allowing Log2 and Log10?  // TESTME

  /**
    * Applies an exact mapping transformation on the given `Field`.
    * The method matches the input `Field` to predefined constants
    * and returns the corresponding result wrapped in an `Option`.
    *
    * @param x the input `Field` to be evaluated.
    * @return an `Option` containing the resulting `Field` if the input matches a predefined constant;
    *         otherwise, `None`.
    */
  def applyExact(x: Field): Option[Field] = x match {
    case Constants.e =>
      Some(Constants.one)
    case Constants.one =>
      Some(Constants.zero)
    case Constants.zero =>
      Some(Constants.negInfinity)
    case _ =>
      None
  }
}

/**
  * Represents a mathematical exponential function, exp(x), where e is the base of natural logarithms.
  * This case object extends ExpressionFunction and applies the exp operation on a given number.
  * It defines the exponential operation for transformation or evaluation within expressions.
  */
case object Exp extends ExpressionFunction("exp", x => x.exp) {
  /**
    * Ignores the provided `context` and returns `AnyScalar`.
    *
    * @param context ignored.
    * @return a new or modified `Context` after applying the associated operation
    */
  def paramContext(context: Context): Context =
    AnyScalar // TESTME

  /**
    * Computes the result of applying the exponential function to a specific `Field` value.
    * This method provides predefined results for certain input cases:
    * - Negative infinity maps to zero.
    * - Zero maps to one.
    * - One maps to the mathematical constant `e`.
    * - For all other inputs, no result is computed.
    *
    * @param x the input `Field` value on which the exact exponential operation is applied.
    * @return `Some(Field)` if the input matches a predefined case, or `None` otherwise.
    */
  def applyExact(x: Field): Option[Field] = x match {
    case Constants.negInfinity =>
      Some(Constants.zero)
    case Constants.zero =>
      Some(Constants.one) // TESTME
    case Constants.one =>
      Some(Constants.e)
    case _ =>
      None
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
case object Negate extends ExpressionFunction("-", x => -x) {
  /**
    * Ignores the specified `context` and returns `AnyScalar`.
    *
    * @param context ignored.
    * @return a `Context` object derived from or related to the provided `context`.
    */
  def paramContext(context: Context): Context = AnyScalar // TESTME

  /**
    * Applies an exact mathematical operation to negate certain types of exact numeric fields.
    * This method specifically handles cases where the input is a `Real` containing an `ExactNumber`
    * with either a `PureNumber` or `Radian` factor.
    *
    * @param x the input field to which the exact operation is applied.
    *          Only fields matching predefined patterns are processed; others return `None`.
    * @return an `Option[Field]` containing the negated `Field` if the input matches the expected pattern,
    *         otherwise `None`.
    */
  def applyExact(x: Field): Option[Field] = x match {
    case Real(ExactNumber(v, f@PureNumber)) =>
      Some(Real(ExactNumber(Value.negate(v), f)))
    case Real(ExactNumber(v, f@Radian)) =>
      Some(Real(ExactNumber(Value.negate(v), f)))
    case _ =>
      None
  }
}

/**
  * `Reciprocal` is an `ExpressionFunction` representing the mathematical reciprocal operation.
  * The function takes a numeric input `x` and computes `1 / x`.
  * It is identified by the name "rec".
  *
  * The operation is performed lazily and adheres to the behavior defined in its parent class.
  */
case object Reciprocal extends ExpressionFunction("rec", x => x.invert) {
  /**
    * Attempts to compute the reciprocal (exact inverse) of the given `Field`.
    * For specific cases of `Real` representations, such as `ExactNumber` with pure, logarithmic, or root factors,
    * the method determines the inverse or applies negation as appropriate.
    * Complex or unsupported `Field` cases are ignored, and `None` is returned.
    *
    * @param x the input `Field` to which the exact reciprocal operation should be applied.
    * @return an `Option[Field]` containing the exact reciprocal if it can be computed, otherwise `None`.
    */
  def applyExact(x: Field): Option[Field] = x match {
    case Real(ExactNumber(v, f@PureNumber)) =>
      Value.inverse(v) map (x =>
        // NOTE: experimental code. If it works well, we could use it elsewhere.
        Constants.pureConstants.getOrElse(x,
          Real(ExactNumber(x, f))))
    case Real(ExactNumber(v, f@Logarithmic(_))) =>
      Some(Real(ExactNumber(Value.negate(v), f))) // TESTME
    case Real(ExactNumber(v, f@Root(_))) =>
      Value.inverse(v) map (x =>
        Real(ExactNumber(x, f)))
    case _ =>
      None
  }

  /**
    * Ignores the provided context and returns contexts that can be exactly inverted.
    *
    * @param context ignored.
    * @return a new context resulting from the combination of the `RestrictedContext` defined with `PureNumber`
    *         and the `AnyLog` condition using the logical "or" operation.
    */
  def paramContext(context: Context): Context =
    RestrictedContext(PureNumber) or AnyLog // TESTME
}

/**
  * Represents a sum operation as a binary function that adds two `Field` values.
  *
  * This object extends `ExpressionBiFunction` by defining its operation as addition (`add`)
  * with the corresponding symbol "+" and is flagged as not always exact (`isExact = false`).
  */
case object Sum extends ExpressionBiFunction("+", (x, y) => x add y, isExact = false, Some(Constants.zero), maybeIdentityR = None) {
  /**
    * Defines the `Context` appropriate for evaluating the left-hand parameter of this function.
    *
    * @param context the `Context` typically based on the context for the evaluation of the whole function.
    * @return the left-hand `Context` of the binary function.
    */
  def leftContext(context: Context): Context =
    context

  /**
    * Retrieves the right-hand evaluation `Context` appropriate for this function.
    *
    * @param context the `Context` typically based on the value of the left-hand parameter.
    * @return the right-hand `Context` of the binary function.
    */
  def rightContext(factor: Factor)(context: Context): Context =
    context

  /**
    * Applies a binary operation to the provided `Field` elements `a` and `b`, with stricter evaluation rules,
    * and returns an optional result.
    * The evaluation succeeds only if the operation satisfies specific conditions
    * (e.g., exact representations or mathematical constraints).
    * In this case, the evaluation succeeds only if the factors of each parameter are compatible.
    * Otherwise, the result is `None`. TODO CHECK
    *
    * @param a the first operand, an exact `Field` instance.
    * @param b the second operand, an exact `Field` instance.
    * @return an `Option[Field]` containing the result of the operation if it can be computed exactly,
    *         or `None` if the operation fails to meet exactness requirements.
    */
  def applyExact(a: Field, b: Field): Option[Field] =
    Some(a add b)
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
case object Product extends ExpressionBiFunction("*", (x, y) => x multiply y, isExact = true, Some(Constants.one), maybeIdentityR = None) {
  /**
    * Evaluates two `Field` instances under certain trivial conditions and determines the result.
    *
    * This method returns `Some(Constants.zero)` if either of the input `Field` instances is
    * equal to `Constants.zero`. Otherwise, it returns `None`.
    *
    * @param a the first `Field` instance to evaluate.
    * @param b the second `Field` instance to evaluate.
    * @return an `Option[Field]` containing `Constants.zero` if trivial conditions are met;
    *         otherwise, `None`.
    */
  override def trivialEvaluation(a: Field, b: Field): Option[Field] = (a, b) match {
    case (Constants.zero, _) | (_, Constants.zero) =>
      Some(Constants.zero)
    case _ =>
      None
  }

  /**
    * Retrieves the left-hand evaluation context associated with this function.
    *
    * @return None.
    */
  def leftContext(context: Context): Context = context

  /**
    * Determines the right-hand evaluation context for a given `Context`.
    *
    * The method evaluates the provided context and returns a new `Context` that is derived
    * based on the specific rules for different types of contexts.
    *
    * CONSIDER reworking this method to take a Factor (from the left-hand parameter) and a Context (the overall context).
    * OTherwise, I don't think it's going to work properly.
    *
    * @param context the initial `Context` to be evaluated and transformed.
    * @return the resulting `Context` after applying the transformation logic.
    */
  def rightContext(factor: Factor)(context: Context): Context = context match {
    case AnyScalar | AnyContext =>
      context or RestrictedContext(PureNumber) // TESTME
    case AnyLog =>
      context // TESTME
    case AnyRoot =>
      context or RestrictedContext(PureNumber) // TESTME
    case r@RestrictedContext(SquareRoot) =>
      r or RestrictedContext(PureNumber)
    case r@RestrictedContext(Radian) =>
      r or RestrictedContext(PureNumber)
    case r@RestrictedContext(_) =>
      r
    case _ =>
      ImpossibleContext // TESTME
  }

  /**
    * Multiplies two Field instances under specific conditions and returns the result as an optional Field.
    *
    * The method checks the type and characteristics of the second operand `b` (the multiplier)
    * and applies an exact mathematical operation to the first operand `a` (the multiplicand)
    * if certain criteria are met. If no conditions are satisfied, it returns `None`.
    *
    * The final check on `isExact` should be redundant, but it's here to be safe.
    *
    * @param a the first operand, a Field instance serving as the multiplicand.
    * @param b the second operand, a Field instance serving as the multiplier. This operand is evaluated
    *          to determine the applicability of exact computations.
    * @return an `Option[Field]` containing the resulting Field if the operation is valid and applicable,
    *         or `None` if the conditions for exact multiplication are not met.
    */
  def applyExact(a: Field, b: Field): Option[Field] =
    Option.when(a.isExact && b.isExact)(a multiply b) filter (_.isExact)
}

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
case object Power extends ExpressionBiFunction("^", (x, y) => x.power(y), isExact = false, None, Some(Constants.one)) {
  /**
    * Evaluates two `Field` instances and determines a trivial result based on predefined conditions.
    * Specifically, it checks if the first `Field` instance is equivalent to the constant `zero`.
    *
    * @param a the first operand, a `Field` instance, which is evaluated to see if it's `zero`.
    * @param b the second operand, a `Field` instance, which is ignored in this implementation.
    * @return an `Option[Field]`, where `Some(a)` is returned if `a` is `zero`; otherwise, `None`.
    */
  override def trivialEvaluation(a: Field, b: Field): Option[Field] = a match {
    case Constants.zero =>
      Some(a) // TESTME
    case _ =>
      None
  }

  /**
    * Determines the left-hand evaluation context for a given input context.
    * This method evaluates the provided context to compute the appropriate left
    * evaluation context based on specific rules or conditions.
    *
    * @param context the input `Context` to evaluate.
    * @return None.
    */
  def leftContext(context: Context): Context = context

  /**
    * Determines the right-hand evaluation context for a given input context.
    * This method ignores the provided context and determines whether the `PureNumber` context
    * should be included or excluded based on the evaluation conditions.
    *
    * @param context the input context to evaluate.
    * @return the updated `Context` after applying the evaluation logic. Returns `None` if the input
    *         context matches `Some(PureNumber)`, otherwise returns `Some(PureNumber)`.
    */
  def rightContext(factor: Factor)(context: Context): Context =
    AnyScalar // ignore both parameters

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
  def applyExact(a: Field, b: Field): Option[Field] = (a, b) match {
    case (Real(x@ExactNumber(_, _)), Real(y@ExactNumber(_, PureNumber))) =>
      val result: Number = x.doPower(y)
      when(result.isExact)(Real(result))
    case _ =>
      None // TESTME
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
abstract class ExpressionFunction(val name: String, val f: Field => Field) extends (Field => Field) {

  /**
    * Specifies the context in which the parameter of the function `f` must be evaluated.
    *
    * @param context the `Context` in which this `ExpressionFunction` must be evaluated.
    * @return the `Context` to be used when evaluating the parameter.
    */
  def paramContext(context: Context): Context

  /**
    * Attempts to evaluate the given `Field` exactly using this `ExpressionFunction`.
    * If the operation can be performed exactly, it returns the resulting `Field` wrapped
    * in an `Option`. If the operation cannot be performed exactly, it returns `None`.
    *
    * @param x the input parameter of type `Field` to be evaluated.
    * @return an `Option` containing the exact result as a `Field` if the evaluation succeeds, or `None` if it does not.
    */
  def applyExact(x: Field): Option[Field]

  /**
    * Evaluates an `Expression` within a given `Context`, performing operations as defined
    * by the `ExpressionFunction`. The method attempts to apply the function exactly if
    * possible. If the evaluation fails at any step, it returns `None`.
    *
    * TESTME this is never used.
    *
    * @param x       the `Expression` to be evaluated.
    * @param context the `Context` within which the evaluation occurs.
    * @return an `Option[Field]` containing the result if the evaluation succeeds, or `None` if it fails.
    */
  def evaluate(x: Expression)(context: Context): Option[Field] = // TESTME
    for {
      a <- x.evaluate(paramContext(context))
      z <- applyExact(a)
    } yield z

  /**
    * Evaluate this function on Field x.
    *
    * @param x the parameter to the function.
    * @return the result of f(x).
    */
  def apply(x: Field): Field = f(x)

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
  def unapply(arg: ExpressionFunction): Option[(String, Field => Field)] =
    Some(arg.name, arg.f) // TESTME
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
abstract class ExpressionBiFunction(
                                       val name: String,
                                       val f: (Field, Field) => Field,
                                       val isExact: Boolean,
                                       val maybeIdentityL: Option[Field],
                                       val maybeIdentityR: Option[Field]
                                   ) extends ((Field, Field) => Field) {

  /**
    * Indicates whether the binary operation represented by this instance commutes,
    * meaning the result remains unchanged for arbitrary swapping of inputs.
    * This is determined based on the absence of a right-hand identity (`maybeIdentityR`).
    */
  lazy val commutes: Boolean = maybeIdentityR.isEmpty

  /**
    * Applies a trivial binary function to the provided `Field` elements `a` and `b`.
    * Typically returns a default or neutral result without performing any meaningful operation.
    *
    * @param a the first operand, a `Field` instance.
    * @param b the second operand, a `Field` instance.
    * @return an `Option[Field]` containing the result of the trivial operation,
    *         or `None` to signify no computation or transformation.
    */
  def trivialEvaluation(a: Field, b: Field): Option[Field] = None

  /**
    * Evaluate this function on x.
    *
    * @param a the first parameter to the function.
    * @param b the second parameter to the function.
    * @return the result of f(x).
    */
  def apply(a: Field, b: Field): Field = f(a, b)

  /**
    * Defines the `Context` appropriate for evaluating the left-hand parameter of this function.
    *
    * @param context the `Context` typically based on the context for the evaluation of the whole function.
    * @return the left-hand `Context` of the binary function.
    */
  def leftContext(context: Context): Context

  /**
    * Defines the `Context` appropriate for evaluating the right-hand parameter of this function
    * based on the provided `Factor`.
    *
    * CONSIDER eliminating context as a parameter, or maybe changing it to the overall context
    * as used by leftContext.
    *
    * @param factor  the `Factor` used to determine the specific right-hand `Context`.
    * @param context the initial `Context` typically derived from the evaluation of the left-hand parameter.
    * @return the updated right-hand `Context` for use in further evaluations.
    */
  def rightContext(factor: Factor)(context: Context): Context

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
    * Evaluates two expressions `x` and `y` in a given context and determines the resulting `Field` based on specific identity and evaluation rules.
    * Trivial identities are recognized and evaluated appropriately.
    *
    * @param x       the first expression to be evaluated.
    * @param y       the second expression to be evaluated.
    * @param context the evaluation context providing the necessary environment for resolving expressions.
    * @return an `Option[Field]` containing the result of the evaluation if successful, or `None` if evaluation fails.
    */
  def evaluate(x: Expression, y: Expression)(context: Context): Option[Field] = (x.evaluateAsIs, y.evaluateAsIs) match {
    case (Some(a), _) if maybeIdentityL contains a =>
      y.evaluate(context)
    case (_, Some(b)) if maybeIdentityR contains b =>
      x.evaluate(context)
    case (Some(a), Some(b)) if trivialEvaluation(a, b).isDefined =>
      trivialEvaluation(a, b)
    case _ =>
      val xy = doEvaluate(x, y)(context)
      val yx = FP.whenever(commutes)(doEvaluate(y, x)(context))
      context.qualifyingField(xy orElse yx)
  }

  /**
    * Evaluates two expressions `x` and `y` using their respective contexts, combines the evaluated results
    * through an exact binary operation, and returns the output if all operations are successful.
    *
    * This method performs a sequential evaluation where:
    * 1. The `x` expression is evaluated in the left-hand context to produce an intermediate result.
    * 2. The intermediate result is used to derive a right-hand context, in which the `y` expression is evaluated.
    * 3. If both expressions are successfully evaluated, their results are combined using a strict binary operation.
    *
    * @param x the first expression to be evaluated in the left-hand context.
    * @param y the second expression to be evaluated in the right-hand context derived from the result of `x`.
    * @return an `Option[Field]` containing the result of the exact binary operation on the evaluated results
    *         of `x` and `y`, or `None` if any step in the process fails.
    */
  private def doEvaluate(x: Expression, y: Expression)(context: Context): Option[Field] =
    for {
      a <- x.evaluate(leftContext(context))
      f <- a.maybeFactor
      b <- y.evaluate(rightContext(f)(RestrictedContext(f)))
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
    case e: ExpressionBiFunction =>
      Some(e.f, e.name, e.maybeIdentityL, e.maybeIdentityR)
    case _ =>
      None // TESTME
  }
}

/**
  * A custom exception that represents errors related to expressions.
  * TESTME (unused)
  *
  * @param str The error message providing details about the expression error.
  */
case class ExpressionException(str: String) extends Exception(str)