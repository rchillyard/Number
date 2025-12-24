/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.Valuable.valuableToMaybeField
import com.phasmidsoftware.number.algebra.{Angle, CanAddAndSubtract, CanMultiplyAndDivide, CanNormalize, Complex, Context, Eager, Nat, NatLog, Number, RationalNumber, Real, Scalar, Valuable, WholeNumber}
import com.phasmidsoftware.number.core.inner.{Factor, Rational}
import com.phasmidsoftware.number.core.numerical
import com.phasmidsoftware.number.core.numerical.{Constants, Field}
import com.phasmidsoftware.number.expression.expr.Expression.em
import scala.language.implicitConversions

/**
  * An Expression that is based on one simple constant value.
  */
trait AtomicExpression extends Expression {

  /**
    * Indicates whether the expression is atomic.
    *
    * @return true, as this expression is atomic and cannot be further simplified.
    */
  def isAtomic: Boolean = true

  /**
    * @return 1.
    */
  def depth: Int = 1

  /**
    * Attempts to simplify an atomic expression, for example,
    * we replace `Literal(Valuable.pi)` with `ConstPi`.
    *
    * @return an `em.AutoMatcher[Expression]` representing
    *         the process of handling or matching the atomic expression.
    */
  def simplifyAtomic: em.AutoMatcher[Expression]
}

/**
  * Companion object for the `AtomicExpression` trait.
  * Provides utility methods for working with instances of `AtomicExpression`.
  *
  * The `unapply` method enables pattern matching on `AtomicExpression` instances, providing an
  * optional `Valuable` as a result based on the type of the expression. The behavior of this method
  * varies for different subtypes of `AtomicExpression`, such as `Complex`, `ValueExpression`,
  * `Literal`, `Valuable`, `Noop`, and `ReducedQuadraticRoot`.
  *
  * Notes:
  * - For `Complex`, the method directly returns the `Complex` instance as a `Valuable`.
  * - For `ValueExpression` and `Literal`, the value of the Valuable is returned, but the name
  * might be discarded during extraction.
  * - For `Valuable`, the method directly returns the Valuable.
  * - For `ReducedQuadraticRoot`, the method attempts to evaluate it "as-is."
  * - For `Noop`, the method returns `None`.
  *
  * Considerations:
  * - The use of `Complex` and `Valuable` directly in the extraction may warrant re-evaluation.
  * - The potential loss of the name in `ValueExpression` and `Literal` is noted as a trade-off.
  */
object AtomicExpression {
  /**
    * Extracts an optional `Valuable` from an `AtomicExpression` instance based on its type.
    * This method provides a mechanism for pattern matching on subtypes of `AtomicExpression`,
    * returning a `Valuable` where applicable.
    *
    * @param arg the `AtomicExpression` instance from which the `Valuable` is to be extracted.
    *            This can be one of the subtypes such as `Complex`, `ValueExpression`, `Literal`,
    *            `Valuable`, `Noop`, or `ReducedQuadraticRoot`.
    * @return an `Option` containing the extracted `Valuable` if one can be determined based on the
    *         type of `arg`. Returns `None` if no `Valuable` can be extracted, e.g., in the case of `Noop`.
    */
  def unapply(arg: AtomicExpression): Option[Valuable] = arg match {
    case c: Complex =>
      Some(c) // CONSIDER eliminate this?  // TESTME
    case ValueExpression(x, _) =>
      Some(x) // NOTE we lose the name here.
    case Literal(x, _) =>
      Some(x) // NOTE we lose the name here. // TESTME
    case r: Root =>
      r.evaluateAsIs // TESTME
    case _ => // TESTME
      None
  }
}

/**
  * The `Noop` object is an atomic expression that represents a no-operation placeholder in an expression tree.
  * It cannot be evaluated, simplified, or associated with any specific factor. It is a concrete implementation
  * of the `AtomicExpression` trait.
  */
case class Noop(w: String) extends AtomicExpression {
  /**
    * Determines whether this `Valuable` is exact, i.e., has no approximation.
    *
    * CONSIDER it may be possible that there are non-approximatable entities that are not exact either.
    *
    * The method returns `true` if there is no approximate representation
    * available (i.e., `approximation` is `None`), indicating that the
    * entity is exact. Otherwise, it returns `false`.
    *
    * @return a `Boolean` indicating whether the entity is exact (`true`)
    *         or has an approximation (`false`).
    */
  def isExact: Boolean = false

  /**
    * Method to determine what `Factor`, if there is such, this `Structure` object is based on.
    *
    * @return an optional `Factor`.
    */
  def maybeFactor(context: Context): Option[Factor] = None

  /**
    * Action to evaluate this `Expression` as a `Valuable`,
    * NOTE: no simplification occurs here.
    *
    * @return a `Valuable`.
    */
  def evaluate(context: Context): Option[Eager] =
    throw new UnsupportedOperationException(s"Can''t evaluate: $this")

  /**
    * Method to render this Structure in a presentable manner.
    *
    * @return a String
    */
  def render: String = toString

  /**
    * If this `Valuable` is exact, it returns the exact value as a `Double`.
    * Otherwise, it returns `None`.
    * NOTE: do NOT implement this method to return a Double for a fuzzy Real--only for exact numbers.
    *
    * @return Some(x) where x is a Double if this is exact, else None.
    */
  def maybeDouble: Option[Double] = None

  /**
    *
    */
  def simplifyAtomic: em.AutoMatcher[Expression] = em.Matcher[Expression, Expression]("simplifyAtomic")(
    _ =>
      em.Miss[Expression, Expression]("simplifyAtomic: ", this)
  )

  /**
    * Provides an approximation of this number, if applicable.
    *
    * This method attempts to compute an approximate representation of the number
    * in the form of a `Real`, which encapsulates uncertainty or imprecision
    * in its value. If no meaningful approximation is possible for the number, it
    * returns `None`.
    *
    * @return an `Option[Real]` containing the approximate representation
    *         of this `Number`, or `None` if no approximation is available.
    */
  def approximation(force: Boolean): Option[Real] = None

  override def toString: String = s"Noop: not an Expression: $w"
}
//
///**
//  * Converts a new `Real` type to an old `Real` type by first transforming
//  * it into a valuable field representation and then casting it to the core `Real`.
//  *
//  * TESTME: this is a hack.
//  *
//  * @param r The new `Real` type that needs to be converted to the old `Real` type.
//  */
//def newRealToOldReal(r: Real) =
//  // TODO asInstanceOf
//  valuableToMaybeField(r).get.asInstanceOf[numerical.Real]

/**
  * Represents an abstract expression for a Valuable that can optionally be associated with a name.
  *
  * A `ValueExpression` provides methods for evaluating its qualifications within
  * a given context, rendering its representation, and determining equality or hashing.
  * It extends the `AtomicExpression` trait, allowing it to be used wherever atomic
  * expressions are valid.
  *
  * CONSIDER placing maybeName in the second parameter list.
  *
  * @param value     the `Valuable` associated with the expression
  * @param maybeName an optional name for the Valuable expression
  */
sealed abstract class ValueExpression(val value: Eager, val maybeName: Option[String] = None) extends AtomicExpression {
  /**
    * Method to determine what `Factor`, if there is such, this `Structure` object is based on.
    *
    * @return an optional `Factor`.
    */
  def maybeFactor(context: Context): Option[Factor] = value.maybeFactor(context)

  /**
    * Method to determine if this Structure object is exact.
    * For instance, `Number.pi` is exact, although if you converted it into a PureNumber, it would no longer be exact.
    *
    * @return true if this Structure object is exact in the context of No factor, else false.
    */
  override def isExact: Boolean = value.isExact

  /**
    * If this `Valuable` is exact, it returns the exact value as a `Double`.
    * Otherwise, it returns `None`.
    * NOTE: do NOT implement this method to return a Double for a fuzzy Real--only for exact numbers.
    *
    * @return Some(x) where x is a Double if this is exact, else None.
    */
  def maybeDouble: Option[Double] = value.maybeDouble

  /**
    * Applies the given `ExpressionMonoFunction` to the current context of the `ValueExpression`
    * and attempts to produce an atomic expression as its result.
    * NOTE that, if there is a result defined, it is exact and, preferably, a `ValueExpression`.
    *
    * @param f the `ExpressionMonoFunction` to be applied. This function determines how the
    *          evaluation will transform the current `ValueExpression` into a potential result.
    * @return an `Option` containing a `ValueExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionMonoFunction): Option[ValueExpression]

  /**
    * Evaluates the current Valuable expression within the given context and determines
    * if the Valuable qualifies based on the context's rules.
    *
    * NOTE: we now have Valuable values that may not have a unique factor (Algebraics).
    * That's why we need to check first if value has a unique factor.
    *
    * If the Valuable meets the qualifications specified by the context, the method
    * returns an `Option` containing the Valuable. Otherwise, it returns `None`.
    *
    * @param context the context in which the Valuable is evaluated. It defines the
    *                qualification rules for determining whether the Valuable is valid.
    * @return `Some(Valuable)` if the Valuable qualifies within the given context, otherwise `None`.
    */
  def evaluate(context: Context): Option[Eager] =
    if (context.valuableQualifies(value))
      Some(value)
    // NOTE this seems bizarre but if we include the following fragment, many unit tests fail...
//    else if (value.maybeName.isDefined) // NOTE: this value is a "named" constant.
//      Some(value)
    else
      value match {
        case nat: Nat =>
          Some(nat)
        case normalizable: CanNormalize[?] =>
          // CONSIDER putting the conditional in the pattern
          Option.when(normalizable.maybeFactor(context).isDefined && context.valuableQualifies(normalizable))(normalizable.normalize)
        case eager: Eager =>
          // CONSIDER putting the conditional in the pattern
          Option.when(eager.maybeFactor(context).isDefined && context.valuableQualifies(eager))(value)
      }

  /**
    * Provides an approximation of this number, if applicable.
    *
    * This method attempts to compute an approximate representation of the number
    * in the form of a `Real`, which encapsulates uncertainty or imprecision
    * in its value. If no meaningful approximation is possible for the number, it
    * returns `None`.
    *
    * @return an `Option[Real]` containing the approximate representation
    *         of this `Number`, or `None` if no approximation is available.
    */
  def approximation(force: Boolean): Option[Real] = value match {
    case r: Real =>
      Some(r)
    case eager =>
      eager.approximation(force)
  }

  /**
    * Method to render this Structure in a presentable manner.
    *
    * @return a String
    */
  def render: String = maybeName getOrElse value.render // TESTME
//
//  /**
//    * Generate a String for debugging purposes.
//    *
//    * @return a String representation of this Literal.
//    */
//  override def toString: String =
//    maybeName getOrElse value.toString

  /**
    * Compares this `ValueExpression` with another object for equality.
    * The comparison considers the `value` Valuable and whether the other object can
    * be equal to this instance.
    *
    * @param other the object to compare for equality with this instance.
    * @return true if the provided object is equal to this instance, false otherwise.
    */
  override def equals(other: Any): Boolean = other match {
    case that: ValueExpression =>
      that.canEqual(this) &&
          value == that.value
    case _ =>
      false
  }

  /**
    * Computes the hash code for this `ValueExpression` instance.
    *
    * The hash code is derived from the hash code of the `value` Valuable, ensuring
    * that the behavior adheres to the contract of the `hashCode` method,
    * particularly in relation to the `equals` method.
    *
    * @return an integer hash code that represents this `ValueExpression` instance.
    */
  override def hashCode(): Int = value.hashCode()

  /**
    * Determines whether the provided object can be considered equal to an instance of `ValueExpression`.
    *
    * @param other the object to compare with this instance.
    * @return true if the provided object is an instance of `ValueExpression`, false otherwise.
    */
  private def canEqual(other: Any): Boolean =
    other.isInstanceOf[ValueExpression]
}

/**
  * The `ValueExpression` companion object provides a mechanism to extract elements from a `ValueExpression` instance.
  * It defines the `unapply` method to enable pattern matching and decomposition of `ValueExpression` objects.
  *
  * The extraction operation retrieves the underlying `Eager` and an optional name, if available.
  * It handles two main cases:
  * 1. For instances of `Literal`, it extracts the `Eager` value along with `None` for the optional name.
  * 2. For other cases, it extracts the `Eager` value along with its associated optional name.
  *
  * @see ValueExpression
  * @see Literal
  */
object ValueExpression {

  /**
    * Creates a `ValueExpression` instance by wrapping the provided `Valuable` object.
    *
    * @param v the `Eager` instance to be encapsulated within a `ValueExpression`.
    * @return a `ValueExpression` representing the provided `Eager` object, with its rendered representation.
    */
  def apply(v: Eager): ValueExpression = Literal(v, Some(v.render))

  /**
    * Creates a Literal instance using an integer value.
    *
    * @param x the integer value to be used for constructing the Literal.
    * @return a Literal instance wrapping the provided integer value as a Real number.
    */
  def apply(x: Int): ValueExpression = x match {
    case 0 => Zero
    case 1 => One
    case -1 => MinusOne
    case 2 => Two
    case _ => ValueExpression(WholeNumber(x))
  }

  /**
    * Extracts components from a `ValueExpression` instance, enabling pattern matching.
    *
    * The `unapply` method decomposes a `ValueExpression` into its constituent `Valuable` and an
    * optional name. For instances of `Literal`, the optional name is always `None`.
    *
    * @param f the `ValueExpression` to be decomposed.
    * @return an `Option` containing a tuple of the `Valuable` and an optional name derived
    *         from the given `ValueExpression`. If the input cannot be decomposed, returns `None`.
    */
  def unapply(f: ValueExpression): Option[(Eager, Option[String])] = f match {
    case Literal(x, _) =>
      Some((x, None))
    case _ =>
      Some((f.value, f.maybeName))
  }
}

/**
  * A `ValueExpression` based on a literal `Valuable`.
  *
  * @param value     the `Valuable`.
  * @param maybeName an optional name (typically this will be None).
  */
case class Literal(override val value: Eager, override val maybeName: Option[String] = None) extends ValueExpression(value, maybeName) {

  /**
    * Attempts to simplify literal expressions within the `Expression` context by matching against predefined constants.
    * The method pattern matches on the literal values and maps them to their corresponding simplified representations:
    * - `Valuable.zero` maps to `Zero`
    * - `Valuable.one` maps to `One`
    * - `Valuable.minusOne` maps to `MinusOne`
    * - `Valuable.two` maps to `Two`
    * - `Valuable.half` maps to `Half`
    * - `Valuable.pi` maps to `ConstPi`
    * - `Valuable.e` maps to `ConstE`
    * - `Constants.i` maps to `ConstI`
    * - `Valuable.infinity` maps to `Infinity`
    *
    * If the input does not match any supported constants, it returns a miss indicating the inability to simplify.
    *
    * @return an `em.AutoMatcher[Expression]` that simplifies literal constants to their predefined values
    *         or returns a miss if no simplification is applicable.
    */
  def simplifyAtomic: em.AutoMatcher[Expression] = em.Matcher[Expression, Expression]("simplifyAtomic") {
    case Literal(Eager.zero, _) =>
      em.Match(Zero)
    case Literal(Eager.one, _) =>
      em.Match(One)
    case Literal(Eager.minusOne, _) =>
      em.Match(MinusOne)
    case Literal(Eager.two, _) =>
      em.Match(Two)
    case Literal(Eager.half, _) =>
      em.Match(Half)
    case Literal(Eager.pi, _) | Literal(Angle.pi, _) =>
      em.Match(ConstPi)
    case Literal(Eager.e, _) =>
      em.Match(ConstE)
    case Literal(RationalNumber(r, _), _) if r.isWhole =>
      em.Match(Literal(WholeNumber(r.toBigInt)))
    // TODO reinstate this match...
    //    case Literal(Eager.i, _) =>
    //      em.Match(ConstI)
    case Literal(Eager.infinity, _) =>
      em.Match(Infinity)
    case x =>
      em.Miss("simplifyAtomic: cannot be simplified", x)
  }

  /**
    * Applies the given `ExpressionMonoFunction` to the current context of the `ValueExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionMonoFunction` to be applied. This function determines how the
    *          evaluation will transform the current `ValueExpression` into a potential result.
    * @return an `Option` containing an `ValueExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionMonoFunction): Option[ValueExpression] = Literal.someLiteral(doMonoFunction(f))

  private def doMonoFunction(f: ExpressionMonoFunction): Eager = (f, value) match {
    case (Negate, r: CanAddAndSubtract[?, ?]) =>
      -r
    case (Reciprocal, r: CanMultiplyAndDivide[Number] @unchecked) =>
      import Number.NumberIsMultiplicativeGroup
      r.reciprocal
    //      case (Reciprocal, a: Algebraic) =>
    //            (a.invert)
    case (Reciprocal, c: Complex) =>
      // TODO asInstanceOf
      Complex(c.complex.invert.asInstanceOf[numerical.Complex])
    case (function, q) =>
      function(q)
  }
}

/**
  * Companion object for the `Literal` class, providing factory methods and pattern matching support.
  */
object Literal {

  /**
    * Converts the given integer value into a `Literal` object with a `WholeNumber` representation.
    *
    * @param x the integer value to be converted
    * @return a `Literal` containing the `WholeNumber` representation of the input integer
    */
  def apply(x: Int): Literal = Literal(WholeNumber(x))

  /**
    * Creates a `Literal` instance from an `Eager` value.
    *
    * @param x the `Eager` value to be wrapped within the `Literal`.
    * @return a new `Literal` instance containing the provided `Eager` value and its rendered representation as an optional name.
    */
  def apply(x: Eager): Literal = Literal(x, Some(x.render))

  /**
    * Extracts a Valuable value from a Literal instance.
    * CONSIDER this may never be invoked.
    *
    * @param arg the Literal instance to extract the Valuable from.
    * @return an Option containing the extracted Valuable, or None if extraction is not possible.
    */
  def unapply(arg: Literal): Option[(Eager, Option[String])] =
    Some(arg.value, arg.maybeName)

  /**
    * Creates a Literal instance from a Rational value.
    *
    * @param x the Rational value to be wrapped in a Literal
    * @return a Literal instance containing the given Rational value encapsulated in a Real
    */
  def apply(x: Rational): Expression =
    apply(RationalNumber(x))

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
  def apply(x: numerical.Number): Expression = x match {
    case numerical.Number.one =>
      One
    case numerical.Number.zero =>
      Zero
    case numerical.Number.pi =>
      ConstPi
    case numerical.Number.two =>
      Two
    case numerical.Number.negOne =>
      MinusOne
    case numerical.Number.half =>
      Half
    case numerical.Number.e =>
      ConstE
    case _ =>
      ValueExpression(Scalar(x))
  }

  /**
    * Creates a `Literal` instance wrapping the provided `Valuable` object.
    *
    * @param x the `Valuable` instance to be wrapped within an optional `Literal`.
    * @return an `Option[Literal]` containing the created `Literal` if successful, or `None` otherwise.
    */
  def someLiteral(x: Eager): Option[Literal] = Some(Literal(x, Some(x.render)))
}

/**
  * Represents a specific constant whose value is a `Valuable` with an associated name.
  *
  * This abstract class extends [[ValueExpression]], allowing for named representation
  * of constants in mathematical expressions, while being tied to a specific Valuable type.
  *
  * @constructor Creates a named constant within the context of the provided Eager.
  * @param x    the mathematical Valuable to which this constant belongs.
  * @param name the name associated with this constant.
  */
sealed abstract class NamedConstant(x: Eager, name: String) extends ValueExpression(x, Some(name)) {
  def simplifyAtomic: em.AutoMatcher[Expression] =
    em.Matcher[Expression, Expression]("simplifyAtomic")(
      _ =>
        em.Miss[Expression, Expression]("AtomicExpression: simplifyAtomic: NamedConstant", this)
    )
}

/**
  * An abstract representation of a scalar constant in a specific mathematical Valuable,
  * for example, π (pi), 1, 0, but not `e`.
  *
  * This class extends `NamedConstant`, tying the scalar constant to a particular
  * `Valuable` instance and associating it with a specific name. It is designed to
  * represent immutable scalar constants in mathematical expressions, leveraging
  * the properties and operations of the `Valuable`.
  *
  * @param x    the `Valuable` instance representing the value of the scalar constant.
  * @param name the name associated with the scalar constant.
  */
sealed abstract class ScalarConstant(x: Eager, name: String) extends NamedConstant(x, name)

/**
  * Represents the mathematical constant zero.
  * This object extends the `ScalarConstant` class, providing implementation specific to zero.
  */
case object Zero extends ScalarConstant(WholeNumber.zero, "0") {
  /**
    * Applies the given `ExpressionMonoFunction` to the current context of the `ValueExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionMonoFunction` to be applied. This function determines how the
    *          evaluation will transform the current `ValueExpression` into a potential result.
    * @return an `Option` containing an `ValueExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionMonoFunction): Option[ValueExpression] = f match {
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
case object Half extends ScalarConstant(RationalNumber.half, "\u00BD") {
  /**
    * Applies the given `ExpressionMonoFunction` to the current context of the `ValueExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionMonoFunction` to be applied. This function determines how the
    *          evaluation will transform the current `ValueExpression` into a potential result.
    * @return an `Option` containing an `ValueExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionMonoFunction): Option[ValueExpression] = f match {
    case Negate =>
      Literal.someLiteral(-RationalNumber.half) // TESTME
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
case object One extends ScalarConstant(WholeNumber.one, "1") {
  /**
    * Applies the given `ExpressionMonoFunction` to the current context of the `ValueExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionMonoFunction` to be applied. This function determines how the
    *          evaluation will transform the current `ValueExpression` into a potential result.
    * @return an `Option` containing an `ValueExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionMonoFunction): Option[ValueExpression] = f match {
    case Negate =>
      Some(MinusOne)
    case Reciprocal =>
      Some(this)
    case Exp =>
      Some(ConstE)
    case Ln =>
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
  * This constant can be used in mathematical expressions involving Valuables and
  * supports operations defined in the `Valuable` trait.
  */
case object MinusOne extends ScalarConstant(-WholeNumber.one, "-1") {
  /**
    * Applies the given `ExpressionMonoFunction` to the current context of the `ValueExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionMonoFunction` to be applied. This function determines how the
    *          evaluation will transform the current `ValueExpression` into a potential result.
    * @return an `Option` containing an `ValueExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionMonoFunction): Option[ValueExpression] = f match {
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
case object Two extends ScalarConstant(WholeNumber.two, "2") {
  /**
    * Applies the given `ExpressionMonoFunction` to the current context of the `ValueExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionMonoFunction` to be applied. This function determines how the
    *          evaluation will transform the current `ValueExpression` into a potential result.
    * @return an `Option` containing an `ValueExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionMonoFunction): Option[ValueExpression] = f match {
    case Reciprocal =>
      Some(Half)
    case Negate =>
      Some(ValueExpression(-2))
    case _ =>
      None
  }
}

/**
  * ConstPi represents the mathematical constant π (pi) exactly.
  */
case object ConstPi extends ScalarConstant(Angle.pi, "π") {
  /**
    * Applies the given `ExpressionMonoFunction` to the current context of the `ValueExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionMonoFunction` to be applied. This function determines how the
    *          evaluation will transform the current `ValueExpression` into a potential result.
    * @return an `Option` containing an `ValueExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionMonoFunction): Option[ValueExpression] = f match {
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
case object ConstE extends NamedConstant(NatLog.e, "e") {
  /**
    * Applies the given `ExpressionMonoFunction` to the current context of the `ValueExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionMonoFunction` to be applied. This function determines how the
    *          evaluation will transform the current `ValueExpression` into a potential result.
    * @return an `Option` containing an `ValueExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionMonoFunction): Option[ValueExpression] = f match {
    case Ln =>
      Some(One)
    case _ =>
      None
  }
}

/**
  * The constant i (viz., the square root of 2)
  * Yes, this is an exact number.
  */
case object ConstI extends NamedConstant(Eager(Constants.i), "i") { // TESTME
  /**
    * Applies the given `ExpressionMonoFunction` to the current context of the `ValueExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionMonoFunction` to be applied. This function determines how the
    *          evaluation will transform the current `ValueExpression` into a potential result.
    * @return an `Option` containing an `ValueExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionMonoFunction): Option[ValueExpression] = f match {
    case _ =>
      None
  }
}

/**
  * Represents an infinite value in the Valuable of expressions.
  *
  * The `Infinity` object is a special case of `NamedConstant` with a value of positive infinity.
  * It is immutable and serves as a singleton instance to represent the mathematical concept of infinity
  * in calculations or expressions. It overrides certain behaviors of `ValueExpression` to handle
  * operations specific to infinity.
  */
case object Infinity extends NamedConstant(Eager(Rational.infinity), "∞") {
  /**
    * Applies the given `ExpressionMonoFunction` to the current context of the `ValueExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionMonoFunction` to be applied. This function determines how the
    *          evaluation will transform the current `ValueExpression` into a potential result.
    * @return an `Option` containing an `ValueExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionMonoFunction): Option[ValueExpression] = f match { // TESTME
    case Reciprocal =>
      Some(Zero)
    case Exp =>
      Some(Infinity)
    case _ =>
      None
  }
}
