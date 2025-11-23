/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.Valuable.valuableToMaybeField
import com.phasmidsoftware.number.algebra.misc.FP
import com.phasmidsoftware.number.algebra.{Angle, AnyContext, CanAddAndSubtract, CanMultiplyAndDivide, CanNormalize, Complex, Eager, ExpressionContext, Monotone, Nat, NatLog, Number, RationalNumber, Real, Scalar, Structure, Valuable, WholeNumber}
import com.phasmidsoftware.number.core
import com.phasmidsoftware.number.core.Constants.gamma
import com.phasmidsoftware.number.core.algebraic.*
import com.phasmidsoftware.number.core.algebraic.Algebraic.{phi, psi}
import com.phasmidsoftware.number.core.inner.{Factor, PureNumber, Rational, Value}
import com.phasmidsoftware.number.core.{Constants, Field, NumberException, inner}
import com.phasmidsoftware.number.expression.expr.Expression.em
import com.phasmidsoftware.number.expression.expr.{CompositeExpression, UniFunction}
import java.util.Objects
import scala.language.implicitConversions

/**
  * An Expression that is based on one simple constant value.
  */

/** sealed */
sealed trait AtomicExpression extends Expression {

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

  def value: Valuable =
    throw new UnsupportedOperationException("Not a Valuable: $w")

  /**
    * Method to determine what `Factor`, if there is such, this `Structure` object is based on.
    *
    * @return an optional `Factor`.
    */
  def maybeFactor(context: ExpressionContext): Option[Factor] = None

  /**
    * Action to evaluate this `Expression` as a `Valuable`,
    * NOTE: no simplification occurs here.
    *
    * @return a `Valuable`.
    */
  def evaluate(context: ExpressionContext): Option[Eager] =
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

/**
  * Converts a new `Real` type to an old `Real` type by first transforming
  * it into a valuable field representation and then casting it to the core `Real`.
  *
  * TESTME: this is a hack.
  *
  * @param r The new `Real` type that needs to be converted to the old `Real` type.
  */
def newRealToOldReal(r: Real) =
  valuableToMaybeField(r).get.asInstanceOf[core.Real] // TODO fix this

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
  def maybeFactor(context: ExpressionContext): Option[Factor] = value.maybeFactor(context)

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
  def evaluate(context: ExpressionContext): Option[Eager] = value match {
    case nat: Nat =>
      Some(nat)
    case normalizable: CanNormalize[?] =>
      Option.when(normalizable.maybeFactor(context).isDefined && context.valuableQualifies(normalizable))(normalizable.normalize)
    case structure: Structure =>
      Option.when(structure.maybeFactor(context).isDefined && context.valuableQualifies(structure))(value)
    case _ =>
      throw NumberException(s"evaluate: cannot evaluate $value in $context")
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
      Some(r) // TESTME
    case m: Monotone =>
      m.approximation(force)
    case algebraic: Algebraic =>
      algebraic.solve.asField match {
        case r: core.Real =>
          Some(Eager(r).asInstanceOf[Real])
        case _ =>
          None // TESTME
      }
    case _ =>
      None // TESTME
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
  * The extraction operation retrieves the underlying `Valuable` and an optional name, if available.
  * It handles two main cases:
  * 1. For instances of `Literal`, it extracts the `Valuable` value along with `None` for the optional name.
  * 2. For other cases, it extracts the `Valuable` value along with its associated optional name.
  *
  * @see ValueExpression
  * @see Literal
  */
object ValueExpression {

  /**
    * Creates a `ValueExpression` instance by wrapping the provided `Valuable` object.
    *
    * @param v the `Valuable` instance to be encapsulated within a `ValueExpression`.
    * @return a `ValueExpression` representing the provided `Valuable` object, with its rendered representation.
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
    case Literal(Valuable.zero, _) =>
      em.Match(Zero)
    case Literal(Valuable.one, _) =>
      em.Match(One)
    case Literal(Valuable.minusOne, _) =>
      em.Match(MinusOne)
    case Literal(Valuable.two, _) =>
      em.Match(Two)
    case Literal(Valuable.half, _) =>
      em.Match(Half)
    case Literal(Valuable.pi, _) | Literal(Angle.pi, _) =>
      em.Match(ConstPi)
    case Literal(Valuable.e, _) =>
      em.Match(ConstE)
    // TODO reinstate this match...
    //    case Literal(Valuable.i, _) =>
    //      em.Match(ConstI)
    case Literal(Valuable.infinity, _) =>
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

  private def doMonoFunction(f: ExpressionMonoFunction): Eager = {
    (f, value) match {
      case (Negate, r: CanAddAndSubtract[?, ?]) =>
        (-r).asInstanceOf[Eager]
      case (Reciprocal, r: CanMultiplyAndDivide[Number] @unchecked) =>
        import Number.NumberIsMultiplicativeGroup
        r.reciprocal
      //      case (Reciprocal, a: Algebraic) =>
      //            (a.invert)
      case (Reciprocal, c: Complex) =>
        Complex(c.complex.invert.asInstanceOf[core.Complex])
      case (function, q) =>
        function(q)
      case _ =>
        // TODO change the expected result to Option[Valuable] and return None here
        throw NumberException(s"monadicFunction: cannot apply $f to $value")
    }
  }
}

/**
  * Companion object for the `Literal` class, providing factory methods and pattern matching support.
  */
object Literal {

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
  def apply(x: core.Number): Expression = x match {
    case core.Number.one =>
      One
    case core.Number.zero =>
      Zero
    case core.Number.pi =>
      ConstPi
    case core.Number.two =>
      Two
    case core.Number.negOne =>
      MinusOne
    case core.Number.half =>
      Half
    case core.Number.e =>
      ConstE
    case _ =>
      ValueExpression(Scalar(x))
  }

  /**
    * Applies the given Algebraic instance to create a corresponding Expression.
    *
    * @param x the Algebraic instance to be converted into an Expression
    * @return an Expression instance representing the given Algebraic input
    */
  def apply(x: Algebraic): Expression = Literal(Eager(x))

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
  * @constructor Creates a named constant within the context of the provided Valuable.
  * @param x    the mathematical Valuable to which this constant belongs.
  * @param name the name associated with this constant.
  */
abstract class NamedConstant(x: Eager, name: String) extends ValueExpression(x, Some(name)) {
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
abstract class ScalarConstant(x: Eager, name: String) extends NamedConstant(x, name)

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

/**
  * Represents a type of `AtomicExpression` that embodies a transcendental entity.
  * A transcendental entity typically includes non-algebraic constants or functions
  * that cannot arise from finite polynomial equations with rational coefficients.
  *
  * This trait extends the characteristics of `AtomicExpression` by enabling the
  * application of a transformation or computation defined as an `ExpressionMonoFunction`.
  */
trait Transcendental extends AtomicExpression {

  /**
    * Applies the provided `ExpressionMonoFunction` to this `Transcendental` entity.
    *
    * @param f the `ExpressionMonoFunction` to be applied, defining a transformation or operation on this `Transcendental`.
    * @return a new `Transcendental` instance representing the result of applying the function.
    */
  def function(f: ExpressionMonoFunction): Transcendental
}

/**
  * An abstract class representing a transcendental mathematical entity, extending the `Transcendental` trait.
  * This class encapsulates a human-readable name and an `Expression` representing its value.
  *
  * @constructor Creates a new instance of `AbstractTranscendental`.
  * @param name       A `String` representing the name of the transcendental entity.
  * @param expression An `Expression` representing the mathematical definition or value of the transcendental entity.
  */
abstract class AbstractTranscendental(val name: String, val expression: Expression) extends Transcendental {

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
  def isExact: Boolean = expression.isExact

  /**
    * If this `Valuable` is exact, it returns the exact value as a `Double`.
    * Otherwise, it returns `None`.
    * NOTE: do NOT implement this method to return a Double for a fuzzy Real--only for exact numbers.
    *
    * @return Some(x) where x is a Double if this is exact, else None.
    */
  def maybeDouble: Option[Double] = expression.maybeDouble

  /**
    * Attempts to simplify an atomic expression, for example,
    * we replace `Literal(Valuable.pi)` with `ConstPi`.
    *
    * @return an `em.AutoMatcher[Expression]` representing
    *         the process of handling or matching the atomic expression.
    */
  def simplifyAtomic: em.AutoMatcher[Expression] =
    expression match {
      case atomicExpression: AtomicExpression =>
        atomicExpression.simplifyAtomic
      case compositeExpression: CompositeExpression =>
        compositeExpression.simplifyComposite
      case _ =>
        throw ExpressionException("AbstractTranscendental.simplifyAtomic: impossible case (all Expressions are either Atomic or Composite)")
    }

  /**
    * Applies a given `ExpressionMonoFunction` to create a new instance of `Transcendental`.
    *
    * @param f the `ExpressionMonoFunction` to be applied, representing a lazy monadic operation.
    * @return a new `Transcendental` instance that encapsulates the applied function and updated expression.
    */
  def function(f: ExpressionMonoFunction): Transcendental =
    new AbstractTranscendental(s"${f.name}($name)", UniFunction(expression, f).simplify) {}

  /**
    * Method to determine what `Factor`, if there is such, this `Structure` object is based on.
    * Unlike context, a `None` result is not permissive.
    *
    * @return an optional `Factor`.
    */
  def maybeFactor(context: ExpressionContext): Option[Factor] = expression.maybeFactor(context)

  /**
    * Method to render this Structure in a presentable manner.
    *
    * @return a String
    */
  def render: String = name

  /**
    * Action to evaluate this `Expression` as a `Valuable`, if possible.
    * NOTE: no simplification or factor-based conversion occurs here.
    *
    * @return an optional `Valuable`.
    */
  def evaluate(context: ExpressionContext): Option[Eager] = expression.evaluate(context)

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
  def approximation(force: Boolean): Option[Real] = expression.approximation(force)

  /**
    * Determines if the provided object is equal to the current instance.
    * The comparison considers the object's type and specific attributes.
    *
    * @param other the object to be compared with the current instance
    * @return true if the provided object is an instance of `AbstractTranscendental`,
    *         can be equal to the current instance, and has the same `expression` value; false otherwise
    */
  override def equals(other: Any): Boolean = other match {
    case that: AbstractTranscendental =>
      that.canEqual(this) && expression == that.expression
    case _ =>
      false
  }

  /**
    * Computes the hash code for this instance of `AbstractTranscendental`.
    *
    * @return an integer representing the hash code of the `expression` Valuable.
    */
  override def hashCode(): Int = expression.hashCode()

  /**
    * Determines if the provided object can be considered equal to the current instance.
    *
    * @param other the object to be compared with the current instance
    * @return true if the provided object is an instance of `AbstractTranscendental`, false otherwise
    */
  private def canEqual(other: Any): Boolean =
    other.isInstanceOf[AbstractTranscendental]

}

/**
  * Pi is a case object representing the mathematical constant π (pi).
  * It extends the AbstractTranscendental class, with a symbolic name "π" and ConstPi as its exact value.
  *
  * This object provides an exact representation of π and inherits capabilities
  * for evaluation, materialization, and comparison from its abstract superclass.
  */
case object Pi extends AbstractTranscendental("\uDED1", ConstPi)

/**
  * Case object representing the transcendental constant `e`.
  *
  * Extends the `AbstractTranscendental` class, providing implementations specific to
  * the mathematical constant `e` (Euler's number), known for its importance in the Valuable
  * of mathematics, particularly in calculus and exponential growth behavior.
  *
  * The `name` parameter is set to "xD835DF00", which represents a unique identifier for
  * this transcendental, and its `expression` is given by the constant `ConstE`.
  *
  * The `E` object inherits all methods and properties from `AbstractTranscendental`, allowing
  * it to be treated as an atomic and exact mathematical expression with various evaluative
  * and comparison capabilities. It also ensures consistency in rendering and context-based
  * operations.
  */
case object E extends AbstractTranscendental("\uD835\uDF00", ConstE)

/**
  * Represents the natural logarithm of 2 as a transcendental constant.
  * NOTE that L2 evaluates to None because generating a Double from L2 would lose precision.
  *
  * This is a case object extending the `AbstractTranscendental` class, encapsulating
  * the mathematical expression for the natural log of 2 (`ln(2)`) and the corresponding
  * expression (`Two.log`).
  *
  * The `L2` object is defined as a named transcendental entity and can be used
  * in operations or expressions involving transcendental numbers.
  */
case object L2 extends AbstractTranscendental("ln(2)", Two.ln)

/**
  * Represents the natural logarithm of 2 as a transcendental constant.
  * NOTE that L2 evaluates to None because generating a Double from L2 would lose precision.
  *
  * This is a case object extending the `AbstractTranscendental` class, encapsulating
  * the mathematical expression for the natural log of 2 (`ln(2)`) and the corresponding
  * expression (`Two.log`).
  *
  * The `L2` object is defined as a named transcendental entity and can be used
  * in operations or expressions involving transcendental numbers.
  */
case object LgE extends AbstractTranscendental("log2e", Two.ln.reciprocal.simplify)

/**
  * Singleton object representing the Euler-Mascheroni constant (𝛾), a fundamental mathematical constant.
  * It extends `AbstractTranscendental` to encapsulate its symbolic representation and mathematical definition.
  *
  * The Euler-Mascheroni constant is a transcendental entity commonly used in number theory and analysis.
  */
case object EulerMascheroni extends AbstractTranscendental("𝛾", Literal(Eager(gamma)))

/**
  * The `Root` trait represents a mathematical root derived from a specific equation.
  * It corresponds to a solution of a multivalued mathematical expression
  * that is typically associated with a monic polynomial equation.
  * Each root is uniquely identified by its underlying equation and a branch index
  * that represents a specific solution when multiple solutions are possible.
  */
trait Root extends AtomicExpression {
  /**
    * Retrieves the `Equation` associated with this `Root`.
    *
    * @return an `Equation`, which represents a mathematical relationship, typically
    *         defined by a monic polynomial and may have multiple branches of solutions.
    */
  def equation: Equation

  /**
    * Retrieves the branch index for this `Root`. A branch represents a specific solution
    * or interpretation of an associated multivalued mathematical expression, typically
    * derived from an underlying `Equation`.
    *
    * @return the index of the branch as an integer. This value is typically
    *         within the range `0` to `branches - 1`, where `branches` is the
    *         total number of solution branches for the corresponding `Equation`.
    */
  def branch: Int

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
  def isExact: Boolean = true

  /**
    * Adds another `Root` to this `Root`, resulting in a new `Root` that
    * represents the sum of the two roots. If the addition is not valid or
    * cannot be performed, returns `None`.
    *
    * @param other the `Root` to be added to the current `Root`.
    *              This parameter represents another mathematical root to
    *              combine with the current `Root`.
    * @return an `Option[Root]` containing the resulting `Root` if the addition
    *         is successful, or `None` if the addition is not valid.
    */
  def add(other: Root): Option[Root]

  /**
    * Retrieves an optional value of type `Valuable` associated with this `Root`.
    * The result will be defined if either the base or the offset is zero (in the case of a quadratic root).
    *
    * @return an `Option[Valuable]` that may contain the value. If no value is associated, returns `None`.
    */
  def maybeValue: Option[Field]

  /**
    * Computes the result of raising the current `Root` to the power of the provided `Rational` value.
    *
    * @param r the `Rational` exponent to which the current `Root` is raised.
    *          It represents the power operation to apply to the `Root`.
    * @return an `Expression` representing the result of the operation,
    *         where the current `Root` is raised to the specified `Rational` power.
    */
  def power(r: Rational): Expression

  /**
    * Computes the square of the current `Expression`.
    * If the current equation is quadratic, it computes the result of the operation: this * -p + q.
    * If the current equation is linear, it computes the result of the operation: this * -r.
    * Otherwise, it returns the square of this expression by performing this * this.
    *
    * @return the result of squaring the current `Expression`, evaluated according to the type of the equation.
    */
  def reciprocal: Expression

  /**
    * Creates a new `Expression` that represents the negation of the current `Expression`.
    * This operation is equivalent to multiplying the current `Expression` by -1.
    *
    * @return an `Expression` representing the negation of the current `Expression`.
    */
  def negate: Expression

  /**
    * Computes the square root of this `Root`.
    * For a `Quadratic` equation, this method calculates one of its roots based on the specified parameter.
    * If the `Expression` is not quadratic, an `ExpressionException` is thrown.
    *
    * @param plus a boolean value that determines which square root (positive or negative root) to compute:
    *             if true, compute the positive root; if false, compute the negative root.
    * @return an `Expression` representing the computed square root of the current `Expression`.
    * @throws ExpressionException if the square root computation is not supported for the current `Expression`.
    */
  def squareRoot(plus: Boolean): Expression

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
  def approximation(force: Boolean): Option[Real] = None // TODO Implement me
}

/**
  * Represents a root of a quadratic equation with a specified branch.
  * This class is a case class extending the `AbstractRoot`, which models the root of an equation
  * and provides required functionality for specific implementations.
  *
  * The `QuadraticRoot` works specifically with quadratic equations, which have two branches corresponding
  * to their solutions. Each instance of `QuadraticRoot` is associated with one branch of the equation.
  *
  * @param equ    the equation whose root is being represented. It must be an instance of the `Equation` trait.
  * @param branch the branch index indicating the solution branch of the equation. This value should be within
  *               the valid range of branches supported by the equation, typically `0` or `1` for quadratic equations.
  */
case class QuadraticRoot(equ: Equation, branch: Int) extends AbstractRoot(equ, branch) {

  /**
    * Constructs a `Root` for a given quadratic equation and its specific solution branch.
    *
    * @param equ    the quadratic equation for which the root is to be constructed. It must be
    *               an instance of the `Equation` trait.
    * @param branch the branch index corresponding to the desired root of the equation.
    *               Typically, for quadratic equations, this value is 0 or 1.
    * @return a `Root` representing the solution branch of the specified quadratic equation.
    */
  def pure(equ: Equation, branch: Int): Root = QuadraticRoot(equ, branch)

  /**
    * Adds another `Root` to this `Root`, yielding a new `Root` as a result.
    *
    * @param other the `Root` to be added to the current `Root`.
    *              This represents the operand added to this `Root`.
    * @return a new `Root` which is the sum of this `Root` and the provided `other` `Root`.
    */
  infix def add(other: Root): Option[Root] = other match {
    case q: QuadraticRoot =>
      Some(QuadraticRoot(algebraic add q.algebraic))
    case _ =>
      None
  }

  /**
    * Returns the associated `Quadratic` equation for this instance.
    *
    * @return the `Quadratic` equation represented by this root.
    */
  def equation: Quadratic = equ.asInstanceOf[Quadratic]

  /**
    * Produces a string representation of this `QuadraticRoot` instance.
    * Depending on the equation and branch, the result may represent specific
    * notable constants (e.g., the golden ratio and its conjugate) or a general
    * description of the quadratic root.
    *
    * @return a string representation of the `QuadraticRoot` based on its equation
    *         and branch index.
    */
  override def toString: String = (equ, branch) match {
    case (Quadratic.goldenRatioEquation, 0) =>
      "\uD835\uDED7"
    case (Quadratic.goldenRatioEquation, 1) =>
      "\uD835\uDED9"
    case _ =>
      s"QuadraticRoot($equ, $branch)"
  }
}

/**
  * The `QuadraticRoot` object provides functionality to construct and manipulate roots of quadratic equations.
  *
  * This object acts as a factory for creating instances of `QuadraticRoot` using algebraic data,
  * and offers methods for working with quadratic equations and their solutions.
  */
object QuadraticRoot {
  /**
    * Creates a new instance of `QuadraticRoot` based on the provided algebraic equation and branch.
    *
    * @param algebraic an instance of `Algebraic` that provides the equation and branch
    *                  used to construct the `QuadraticRoot`
    * @return a new instance of `QuadraticRoot` created from the specified algebraic data
    */
  def apply(algebraic: Algebraic): QuadraticRoot =
    QuadraticRoot(algebraic.equation, algebraic.branch)
}

/**
  * Represents the linear root of a given equation. A `LinearRoot` is an extension
  * of the `AbstractRoot` class, specialized for cases where the root corresponds
  * to a degree-1 (linear) polynomial equation.
  *
  * This implementation assumes the linear root is unique and thus corresponds
  * to the only solution available for a first-degree equation.
  *
  * @param equ the `Equation` instance associated with this root.
  *            Assumes the equation is linear (degree 1).
  */
case class LinearRoot(equ: Equation) extends AbstractRoot(equ, 0) {
  /**
    * @return 0.
    */
  def branch: Int = 0

  /**
    * Creates a new root instance corresponding to the specified equation and branch.
    * This method is used to produce a mathematical solution associated with the given input parameters.
    *
    * @param equ    the equation from which the root is derived. The equation represents a
    *               mathematical relationship, typically a monic polynomial.
    * @param branch the branch index associated with this root. It specifies a particular
    *               solution for the equation if multiple solutions exist.
    * @return a new `Root` instance corresponding to the provided equation and branch.
    */
  def pure(equ: Equation, branch: Int): Root = LinearRoot(equ)

  /**
    * Retrieves the `LinearEquation` associated with this `Root`.
    *
    * @return an `Equation`, which represents a mathematical relationship, typically
    *         defined by a monic polynomial and may have multiple branches of solutions.
    */
  def equation: LinearEquation = equ.asInstanceOf[LinearEquation]

  /**
    * Adds another `Root` to this `Root`, resulting in a new `Root` that
    * represents the sum of the two roots. If the addition is not valid or
    * cannot be performed, returns `None`.
    *
    * @param other the `Root` to be added to the current `Root`.
    *              This parameter represents another mathematical root to
    *              combine with the current `Root`.
    * @return an `Option[Root]` containing the resulting `Root` if the addition
    *         is successful, or `None` if the addition is not valid.
    */
  def add(other: Root): Option[Root] = other match {
    case l: LinearRoot =>
      val result = algebraic add l.algebraic
      Some(LinearRoot(result.equation))
    case _ =>
      None
  }
}

/**
  * Represents the root of an equation, associated with a specific branch.
  *
  * This class models the concept of a mathematical root, where an equation
  * is solved for a specific branch index, yielding a solution that adheres to
  * the constraints defined in the `Solution` trait. It extends `AtomicExpression`
  * to integrate with the broader mathematical expression framework.
  *
  * @param equ    the mathematical equation whose solution is represented by this root
  * @param branch the branch index used to solve the equation
  */
abstract class AbstractRoot(equ: Equation, branch: Int) extends Root {

  /**
    * Computes and returns a `Root` associated with the given `Equation` and branch.
    *
    * @param equ    the `Equation` for which the `Root` is to be computed.
    * @param branch an integer value representing the specific branch of the `Equation`.
    * @return the `Root` corresponding to the provided `Equation` and branch.
    */
  def pure(equ: Equation, branch: Int): Root

  /**
    * Represents an algebraic instance derived from the associated equation and branch.
    * This value is lazily evaluated and used within the context of symbolic or algebraic computations.
    */
  lazy val algebraic: Algebraic = Algebraic(equ, branch)

  /**
    * Represents the solution of an equation for a specific branch in the context of a mathematical expression.
    * The solution is computed by invoking the `solve` method of the `equation` with the provided branch index.
    * The resulting solution is exact and adheres to the constraints described in the `Solution` trait.
    */
  lazy val solution: Solution = algebraic.solve

  /**
    * Represents an optional value for the current `AbstractRoot` instance, resulting from a numerical
    * transformation of its associated solution if the solution is exact.
    *
    * The value is computed by invoking the `whenever` method with a predicate that checks whether
    * the associated solution is exact (`solution.isExact`). If the predicate is true, the `asField` method
    * of the solution is accessed, and its value is converted to a real number and subsequently mapped to a
    * `Double`. If the predicate is false */
  lazy val maybeDouble: Option[Double] =
    FP.whenever(solution.isExact)(solution.asField.asReal.map(_.toDouble))

  /**
    * Lazily computes an optional `Valuable` value (`maybeValue`) based on the type of the solution
    * and specific cases associated with the given `solution` and `equation`.
    *
    * The computation involves pattern matching on the type and properties of the `solution`.
    *
    * - For a `LinearSolution`, a `Valuable` is created using the solution's value and the `PureNumber` factor.
    * - For a `QuadraticSolution`, specific conditions are checked (e.g., offset is zero, base and factor conditions),
    * to determine how the corresponding `Valuable` is computed, which may include a calculation with a branch.
    * - For special cases related to the `Quadratic` golden ratio equation, named constants `phi` or `psi` are returned,
    * depending on the branch.
    * - Returns `None` if none of the conditions match.
    *
    * The computation incorporates the logic for determining whether the solution is linear or quadratic
    * and evaluates branches of quadratic solutions based on specific properties.
    */
  lazy val maybeValue: Option[Field] = solution match {
    case LinearSolution(x) =>
      Some(Field(x, PureNumber))
    case QuadraticSolution(base, offset, _, _) if Value.isZero(offset) =>
      Some(Field(base, PureNumber))
    case QuadraticSolution(base, offset, factor, branch) if Value.isZero(base) && (factor == PureNumber || branch == 0) =>
      val radicalTerm = if branch == 0 then offset else Value.negate(offset)
      Some(core.Real(core.Number.one.make(radicalTerm, factor)))
    case _ =>
      (equ, branch) match {
        case (Quadratic.goldenRatioEquation, 0) =>
          Some(phi)
        case (Quadratic.goldenRatioEquation, 1) =>
          Some(psi)
        case _ =>
          None
      }
  }

  /**
    * Method to determine what `Factor`, if there is such, this `Structure` object is based on.
    *
    * @return an optional `Factor`.
    */
  override def maybeFactor(context: ExpressionContext): Option[Factor] = solution match {
    case LinearSolution(_) =>
      Some(PureNumber)
    case QuadraticSolution(Value.zero, offset, _, _) if Value.isZero(offset) =>
      Some(PureNumber)
    case QuadraticSolution(Value.zero, _, factor, _) =>
      Some(factor)
    case QuadraticSolution(_, Value.zero, _, _) =>
      Some(PureNumber)
    case _ =>
      None
  }

  /**
    * Attempts to simplify an atomic expression, for example,
    * we replace `Literal(Valuable.pi)` with `ConstPi`.
    *
    * @return an `em.AutoMatcher[Expression]` representing
    *         the process of handling or matching the atomic expression.
    */
  def simplifyAtomic: em.AutoMatcher[Expression] =
    em.Matcher[Expression, Expression]("Root.simplifyAtomic") {
      case r: AbstractRoot =>
        // TODO refactor maybeValue so that it yields Option[Valuable]
        em.matchIfDefined(r.maybeValue)(r).flatMap {
          x => matchAndSimplify(Eager(x))
        }
    }

  /**
    * Action to evaluate this `Expression` as a `Valuable`, if possible.
    * NOTE: no simplification or factor-based conversion occurs here.
    *
    * @return an optional `Valuable`.
    */
  def evaluate(context: ExpressionContext): Option[Eager] =
    maybeValue match {
      case x@Some(value) if context.valuableQualifies(Eager(value)) =>
        Some(Eager(value))
      case _ =>
        Option.when(context == AnyContext)(Eager(algebraic))
    }

  /**
    * Computes and returns an approximate numerical value for this Approximatable.
    * All Valuables, PowerSeries and Expressions that implement this method should work except for complex quantities.
    *
    * @return if possible, returns a `Real` representing the approximation of this expression.
    */
  def approximation: Option[core.Real] =
    maybeValue match {
      case Some(value) =>
        value.approximation
      case None =>
        solution.asNumber map (core.Real(_))
    }

  /**
    * Method to render this Structure in a presentable manner.
    *
    * @return a String
    */
  def render: String = algebraic.render

  /**
    * Computes the power of the current `Expression` raised to the specified `Rational` exponent.
    *
    * @param r the `Rational` exponent to which the current `Expression` is to be raised.
    *          If the exponent is negative, the method computes the reciprocal of the positive power.
    *          If the exponent is zero, the result is the identity `One`.
    *          Otherwise, the computation involves recursive calls to this method.
    * @return an `Expression` representing the result of raising the current `Expression` to the power of `r`.
    */
  def power(r: Rational): Expression = r match {
    case Rational.zero =>
      One
    case Rational.one =>
      this
    case x if x < 0 =>
      power(-x).reciprocal
    case Rational.half =>
      squareRoot(branch == 0)
    case x if x >= 2 =>
      import com.phasmidsoftware.number.expression.expr.Expression.ExpressionOps
      squared * power(x - 2)
    case _ =>
      throw ExpressionException(s"power: unable to compute power of $this to $r")
  }

  /**
    * Computes the reciprocal of the current `Expression`.
    *
    * For a quadratic equation in the form Quadratic(p, q), the reciprocal is computed as:
    * (this / Literal(-q)) + Literal(-p / q).
    * Otherwise, the reciprocal is calculated as `One / this`.
    *
    * @return an `Expression` representing the reciprocal of the current `Expression`.
    */
  def reciprocal: Expression = equation match {
    case Quadratic(p, q) =>
      import com.phasmidsoftware.number.expression.expr.Expression.ExpressionOps
      this / Literal(-q) + Literal(-p / q)
    case _ =>
      One / this
  }

  /**
    * Creates a new `Expression` that represents the negation of the current `Expression`.
    * This operation is equivalent to multiplying the current `Expression` by -1.
    *
    * @return an `Expression` representing the negation of the current `Expression`.
    */
  def negate: Expression = {
    val result: Algebraic = Algebraic(algebraic.negate.solve)
    Root(result.equation, result.branch)
  }

  /**
    * Computes the square root of this `Root`.
    * For a `Quadratic` equation, this method calculates one of its roots based on the specified parameter.
    * If the `Expression` is not quadratic, an `ExpressionException` is thrown.
    *
    * @param plus a boolean value that determines which square root (positive or negative root) to compute:
    *             if true, compute the positive root; if false, compute the negative root.
    * @return an `Expression` representing the computed square root of the current `Expression`.
    * @throws ExpressionException if the square root computation is not supported for the current `Expression`.
    */
  def squareRoot(plus: Boolean): Expression = equation match {
    case Quadratic(p, q) =>
      pure(Quadratic(-p.invert, -q / p), if plus then 0 else 1)
    case _ =>
      throw ExpressionException(s"squareRoot: cannot compute square root of $this")
  }

  /**
    * Compares this `AbstractRoot` instance with another object for equality.
    * The method checks if the other object is of a compatible type and
    * whether all relevant Valuables of both objects are equal.
    *
    * @param other the object to compare for equality with this instance
    * @return true if the given object is an instance of `AbstractRoot`,
    *         has `canEqual` compatibility with this instance, and
    *         if all relevant Valuables are equal; otherwise, false
    */
  override def equals(other: Any): Boolean =
    other match {
      case that: AbstractRoot =>
        that.canEqual(this) &&
            equ == that.equation &&
            branch == that.branch
      case _ =>
        false
    }

  /**
    * Generates a hash code for the instance based on its `equ` and `branch` Valuables.
    *
    * @return an integer hash code value obtained by hashing the `equ` and `branch` Valuables.
    */
  override def hashCode(): Int =
    Objects.hash(equ, branch)

  /**
    * Determines if the given object is of a type that can be compared for equality with this instance.
    *
    * @param other the object to compare with this instance
    * @return true if the given object is an instance of AbstractRoot, false otherwise
    */
  private def canEqual(other: Any): Boolean =
    other.isInstanceOf[AbstractRoot]

  /**
    * Computes the square of the current `Expression`.
    * If the current equation is quadratic, it computes the result of the operation: this * -p + q.
    * If the current equation is linear, it computes the result of the operation: this * -r.
    * Otherwise, it returns the square of this expression by performing this * this.
    *
    * @return the result of squaring the current `Expression`, evaluated according to the type of the equation.
    */
  private def squared: Expression = equation match {
    case Quadratic(p, q) =>
      import com.phasmidsoftware.number.expression.expr.Expression.ExpressionOps
      this * Literal(-p) + Literal(-q)
    case LinearEquation(r) =>
      this * Literal(-r)
    case _ =>
      this * this
  }

  /**
    * Matches the given `Valuable` instance and attempts to simplify its representation
    * into an `Expression`. This involves wrapping the `Valuable` in a `Literal` and
    * applying atomic simplification transformations.
    *
    * @param eager the `Valuable` to match and simplify.
    * @return a `MatchResult` containing the resulting `Expression`.
    */
  private def matchAndSimplify(eager: Eager): em.MatchResult[Expression] =
    em.Match(Literal(eager)).flatMap(simplifyAtomic)
}

/**
  * The `Root` object provides predefined mathematical constants or solutions to
  * well-known equations modeled as roots. These constants are instances of `QuadraticRoot`
  * or `LinearRoot` corresponding to specific root solutions of their respective equations.
  */
object Root {
  /**
    * Applies the given `Equation` instance and a branch index to determine the corresponding `Root`.
    * Depending on the type of the equation, the method computes and returns an appropriate `Root` representation.
    *
    * @param equation the `Equation` instance to process, which represents a mathematical relationship.
    *                 It may include multiple solutions or branches (e.g., quadratic or linear equations).
    * @param branch   an integer representing the branch index to compute. The branch identifies
    *                 a specific solution for equations with multiple branches.
    * @return a `Root` instance that corresponds to the given `Equation` and branch index.
    *         The returned `Root` is specialized for `Quadratic` or `LinearEquation` types.
    */
  def apply(equation: Equation, branch: Int): Root = equation match {
    case q: Quadratic =>
      QuadraticRoot(q, branch)
    case l: LinearEquation =>
      LinearRoot(l)
  }

  /**
    * Represents the mathematical constant φ (phi), also known as the golden ratio.
    * The golden ratio is defined as the positive root of the quadratic equation `x² + x - 1 = 0`.
    * It is an irrational number approximately equal to 1.6180339887498948.
    *
    * This value is modeled using a `QuadraticRoot` instance, which takes the predefined golden ratio quadratic equation
    * (`Quadratic.goldenRatioEquation`) and specifies the branch `0`, representing the positive root.
    */
  val phi = QuadraticRoot(Quadratic.goldenRatioEquation, 0)
  /**
    * Represents the conjugate root of the golden ratio equation (`x² + x - 1 = 0`),
    * commonly referred to as ψ (psi). This value is the second root of the quadratic equation,
    * distinct from the golden ratio (φ, phi). It is calculated using the `QuadraticRoot` constructor,
    * with the golden ratio equation as its basis and branch index set to 1.
    */
  val psi = QuadraticRoot(Quadratic.goldenRatioEquation, 1)
  /**
    * Represents the constant root `1` of a quadratic equation.
    * This value is a particular solution of the quadratic equation `-2x + 1 = 0` on branch `0`.
    */
  val one = QuadraticRoot(Quadratic(-2, 1), 0)
  /**
    * Represents the quadratic root when both the quadratic coefficients and the branch index are zero.
    * This constant corresponds to the simplest quadratic equation with all coefficients as zero and the solution at branch zero.
    */
  val zero = QuadraticRoot(Quadratic(0, 0), 0)
  /**
    * Represents the root solution corresponding to the quadratic equation for ±√2.
    *
    * The `rootTwo` value is an instance of `QuadraticRoot` that is initialized with the predefined
    * quadratic equation `x² - 2 = 0` (`Quadratic.rootTwoEquation`) and specifies the primary root (`branch 0`).
    *
    * This root is a well-known mathematical constant (the square root of 2), widely used in
    * geometry, algebra, and various other mathematical applications.
    */
  val rootTwo = QuadraticRoot(Quadratic.rootTwoEquation, 0)
  /**
    * Represents the negative root of the quadratic equation `x² - 2 = 0`.
    * This equation defines the square root of 2, with the two roots being `±√2`.
    * The `branch` parameter set to 1 in this instance corresponds to the negative root (i.e., `-√2`).
    */
  val negRootTwo = QuadraticRoot(Quadratic.rootTwoEquation, 1)
  /**
    * Represents the value one-half as a linear root, constructed from a linear equation
    * with a negated half-rational coefficient.
    */
  val half = LinearRoot(LinearEquation(Rational.half.negate))
}