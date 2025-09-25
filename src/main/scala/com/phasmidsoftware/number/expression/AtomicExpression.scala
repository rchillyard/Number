/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression

import com.phasmidsoftware.number.core.Constants.gamma
import com.phasmidsoftware.number.core.Number.convertInt
import com.phasmidsoftware.number.core.algebraic.{Algebraic, Algebraic_Quadratic, Quadratic, Solution}
import com.phasmidsoftware.number.core.inner.Rational.toIntOption
import com.phasmidsoftware.number.core.inner._
import com.phasmidsoftware.number.core.{Complex, Constants, ExactNumber, Field, Number, Real}
import com.phasmidsoftware.number.expression.Expression.em
import scala.Option.when
import scala.language.implicitConversions

/**
  * An Expression that is based on one simple constant value.
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

  /**
    * Attempts to simplify an atomic expression, for example,
    * we replace `Literal(Constants.pi)` with `ConstPi`.
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

  /**
    *
    */
  def simplifyAtomic: em.AutoMatcher[Expression] = em.Matcher[Expression, Expression]("simplifyAtomic")(
    _ =>
      em.Miss[Expression, Expression]("AtomicExpression: simplifyAtomic: Noop", this)
  )
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
sealed abstract class FieldExpression(val value: Field, val maybeName: Option[String] = None) extends AtomicExpression {

  /**
    * Applies the given `ExpressionMonoFunction` to the current context of the `FieldExpression`
    * and attempts to produce an atomic expression as its result.
    * NOTE that, if there is a result defined, it is exact and, preferably, a `FieldExpression`.
    *
    * @param f the `ExpressionMonoFunction` to be applied. This function determines how the
    *          evaluation will transform the current `FieldExpression` into a potential result.
    * @return an `Option` containing a `FieldExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionMonoFunction): Option[FieldExpression]


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
    * Attempts to simplify literal expressions within the `Expression` context by matching against predefined constants.
    * The method pattern matches on the literal values and maps them to their corresponding simplified representations:
    * - `Constants.zero` maps to `Zero`
    * - `Constants.one` maps to `One`
    * - `Constants.minusOne` maps to `MinusOne`
    * - `Constants.two` maps to `Two`
    * - `Constants.half` maps to `Half`
    * - `Constants.pi` maps to `ConstPi`
    * - `Constants.e` maps to `ConstE`
    * - `Constants.i` maps to `ConstI`
    * - `Constants.infinity` maps to `Infinity`
    *
    * If the input does not match any supported constants, it returns a miss indicating the inability to simplify.
    *
    * @return an `em.AutoMatcher[Expression]` that simplifies literal constants to their predefined values
    *         or returns a miss if no simplification is applicable.
    */
  def simplifyAtomic: em.AutoMatcher[Expression] = em.Matcher[Expression, Expression]("simplifyAtomic") {
    case Literal(Constants.zero, _) =>
      em.Match(Zero)
    case Literal(Constants.one, _) =>
      em.Match(One)
    case Literal(Constants.minusOne, _) =>
      em.Match(MinusOne)
    case Literal(Constants.two, _) =>
      em.Match(Two)
    case Literal(Constants.half, _) =>
      em.Match(Half)
    case Literal(Constants.pi, _) | Literal(Real(Number.pi), _) =>
      em.Match(ConstPi)
    case Literal(Constants.e, _) =>
      em.Match(ConstE)
    case Literal(Constants.i, _) =>
      em.Match(ConstI)
    case Literal(Constants.infinity, _) =>
      em.Match(Infinity)
    case x =>
      em.Miss("simplifyAtomic: cannot be simplified", x)
  }

  /**
    * Applies the given `ExpressionMonoFunction` to the current context of the `FieldExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionMonoFunction` to be applied. This function determines how the
    *          evaluation will transform the current `FieldExpression` into a potential result.
    * @return an `Option` containing an `FieldExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionMonoFunction): Option[FieldExpression] = f match {
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
  * Represents a specific constant whose value is a `Field` with an associated name.
  *
  * This abstract class extends [[FieldExpression]], allowing for named representation
  * of constants in mathematical expressions, while being tied to a specific field type.
  *
  * @constructor Creates a named constant within the context of the provided field.
  * @param x    the mathematical field to which this constant belongs.
  * @param name the name associated with this constant.
  */
abstract class NamedConstant(x: Field, name: String) extends FieldExpression(x, Some(name)) {
  def simplifyAtomic: em.AutoMatcher[Expression] =
    em.Matcher[Expression, Expression]("simplifyAtomic")(
      _ =>
        em.Miss[Expression, Expression]("AtomicExpression: simplifyAtomic: NamedConstant", this)
    )
}

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
    * Applies the given `ExpressionMonoFunction` to the current context of the `FieldExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionMonoFunction` to be applied. This function determines how the
    *          evaluation will transform the current `FieldExpression` into a potential result.
    * @return an `Option` containing an `FieldExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionMonoFunction): Option[FieldExpression] = f match {
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
    * Applies the given `ExpressionMonoFunction` to the current context of the `FieldExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionMonoFunction` to be applied. This function determines how the
    *          evaluation will transform the current `FieldExpression` into a potential result.
    * @return an `Option` containing an `FieldExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionMonoFunction): Option[FieldExpression] = f match {
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
    * Applies the given `ExpressionMonoFunction` to the current context of the `FieldExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionMonoFunction` to be applied. This function determines how the
    *          evaluation will transform the current `FieldExpression` into a potential result.
    * @return an `Option` containing an `FieldExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionMonoFunction): Option[FieldExpression] = f match {
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
  * This constant can be used in mathematical expressions involving fields and
  * supports operations defined in the `Field` trait.
  */
case object MinusOne extends ScalarConstant(Constants.minusOne, "-1") {
  /**
    * Applies the given `ExpressionMonoFunction` to the current context of the `FieldExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionMonoFunction` to be applied. This function determines how the
    *          evaluation will transform the current `FieldExpression` into a potential result.
    * @return an `Option` containing an `FieldExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionMonoFunction): Option[FieldExpression] = f match {
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
    * Applies the given `ExpressionMonoFunction` to the current context of the `FieldExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionMonoFunction` to be applied. This function determines how the
    *          evaluation will transform the current `FieldExpression` into a potential result.
    * @return an `Option` containing an `FieldExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionMonoFunction): Option[FieldExpression] = f match {
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
    * Applies the given `ExpressionMonoFunction` to the current context of the `FieldExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionMonoFunction` to be applied. This function determines how the
    *          evaluation will transform the current `FieldExpression` into a potential result.
    * @return an `Option` containing an `FieldExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionMonoFunction): Option[FieldExpression] = f match {
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
    * Applies the given `ExpressionMonoFunction` to the current context of the `FieldExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionMonoFunction` to be applied. This function determines how the
    *          evaluation will transform the current `FieldExpression` into a potential result.
    * @return an `Option` containing an `FieldExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionMonoFunction): Option[FieldExpression] = f match {
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
case object ConstI extends NamedConstant(Constants.i, "i") { // TESTME
  /**
    * Applies the given `ExpressionMonoFunction` to the current context of the `FieldExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionMonoFunction` to be applied. This function determines how the
    *          evaluation will transform the current `FieldExpression` into a potential result.
    * @return an `Option` containing an `FieldExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionMonoFunction): Option[FieldExpression] = f match {
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
    * Applies the given `ExpressionMonoFunction` to the current context of the `FieldExpression`
    * and attempts to produce an atomic result.
    *
    * @param f the `ExpressionMonoFunction` to be applied. This function determines how the
    *          evaluation will transform the current `FieldExpression` into a potential result.
    * @return an `Option` containing an `FieldExpression` if the evaluation succeeds,
    *         or `None` if the evaluation fails.
    */
  def monadicFunction(f: ExpressionMonoFunction): Option[FieldExpression] = f match { // TESTME
    case Reciprocal =>
      Some(Zero)
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
    * Attempts to simplify an atomic expression, for example,
    * we replace `Literal(Constants.pi)` with `ConstPi`.
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
    new AbstractTranscendental(s"${f.name}($name)", com.phasmidsoftware.number.expression.UniFunction(expression, f).simplify) {}

  /**
    * Method to determine what `Factor`, if there is such, this `NumberLike` object is based on.
    * Unlike context, a `None` result is not permissive.
    *
    * @return an optional `Factor`.
    */
  def maybeFactor: Option[Factor] = expression.maybeFactor

  /**
    * Method to render this NumberLike in a presentable manner.
    *
    * @return a String
    */
  def render: String = name

  /**
    * Action to evaluate this `Expression` as a `Field`, if possible.
    * NOTE: no simplification or factor-based conversion occurs here.
    *
    * @return an optional `Field`.
    */
  def evaluate(context: Context): Option[Field] = expression.evaluate(context)

  /**
    * Computes and returns an approximate numerical value for this Approximatable.
    * All Fields, PowerSeries and Expressions that implement this method should work except for complex quantities.
    *
    * @return if possible, returns a `Real` representing the approximation of this expression.
    */
  def approximation: Option[Real] = expression.approximation
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
  * the mathematical constant `e` (Euler's number), known for its importance in the field
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
case object EulerMascheroni extends AbstractTranscendental("𝛾", gamma)

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

  def simplifyAtomic: em.AutoMatcher[Expression] = em.Matcher[Expression, Expression]("simplifyAtomic")(
    _ =>
      em.Miss[Expression, Expression]("AtomicExpression: simplifyAtomic: ReducedQuadraticRoot", this)
  )
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
