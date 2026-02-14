/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.core.Context
import com.phasmidsoftware.number.algebra.eager.{Eager, Real}
import com.phasmidsoftware.number.core.inner.Factor
import com.phasmidsoftware.number.expression.expr.Expression.em
import com.phasmidsoftware.number.expression.expr.UniFunction

import scala.language.implicitConversions

/**
  * Represents a type of `AtomicExpression` that embodies a transcendental entity.
  * A transcendental entity typically includes non-algebraic constants or functions
  * that cannot arise from finite polynomial equations with rational coefficients.
  *
  * This trait extends the characteristics of `AtomicExpression` by enabling the
  * application of a transformation or computation defined as an `ExpressionMonoFunction`.
  */
sealed trait Transcendental extends AtomicExpression {

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
sealed abstract class AbstractTranscendental(val name: String, val expression: Expression) extends Transcendental {

  /**
    * This ensures that all `Transcendental` instances are protected from evaluation during simplification.
    * Such transcendentals as the Euler-Mascheroni constant are kept as is during simplification.
    */
  override val protectedName: Boolean = true

  /**
    * Retrieves an optional name associated with the implementing class.
    *
    * The method returns an `Option` wrapping a `String`. If a name is available, it will be 
    * contained within the `Option` as `Some(name)`. If no name is present, the method will return `None`.
    *
    * @return an `Option[String]` representing the optional name
    */
  val maybeName: Option[String] = Some(name)

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
    * Determines if the current number is equal to zero.
    *
    * @return true if the number is zero, false otherwise
    */
  def isZero: Boolean = expression.isZero

  /**
    * Determines whether this object represents unity.
    *
    * @return true if the object represents unity, false otherwise
    */
  def isUnity: Boolean = expression.isUnity

  /**
    * Determines the sign of the Structure value represented by this instance.
    * Returns an integer indicating whether the value is positive, negative, or zero.
    *
    * @return 1 if the value is positive, -1 if the value is negative, and 0 if the value is zero
    */
  def signum: Int = expression.signum

  /**
    * Attempts to simplify an atomic expression, for example,
    * we replace `Literal(Eager.pi)` with `Pi`.
    *
    * @return an `em.AutoMatcher[Expression]` representing
    *         the process of handling or matching the atomic expression.
    */
  def simplifyAtomic: em.AutoMatcher[Expression] =
    em.Matcher[Expression, Expression]("simplifyAtomic")(x =>
      em.Miss[Expression, Expression]("AbstractTranscendental.simplifyAtomic: ", x))

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
  def maybeFactor(context: Context): Option[Factor] = expression.maybeFactor(context)

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
  def evaluate(context: Context): Option[Eager] = expression.evaluate(context)

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
    * @return an integer representing the hash code of the `expression` Eager.
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
  * PiTranscendental is a case object representing the mathematical constant π (pi).
  * It extends the AbstractTranscendental class, with a symbolic name "π" and Pi as its exact value.
  *
  * This object provides an exact representation of π and inherits capabilities
  * for evaluation, materialization, and comparison from its abstract superclass.
  *
  * @deprecated use Pi instead.
  */
@deprecated
case object PiTranscendental extends AbstractTranscendental("\uD835\uDED1", Pi)

/**
  * Case object representing the transcendental constant `e`.
  *
  * Extends the `AbstractTranscendental` class, providing implementations specific to
  * the mathematical constant `e` (Euler's number), known for its importance in the Valuable
  * of mathematics, particularly in calculus and exponential growth behavior.
  *
  * The `name` parameter is set to "xD835DF00", which represents a unique identifier for
  * this transcendental, and its `expression` is given by the constant `E`.
  *
  * The `E` object inherits all methods and properties from `AbstractTranscendental`, allowing
  * it to be treated as an atomic and exact mathematical expression with various evaluative
  * and comparison capabilities. It also ensures consistency in rendering and context-based
  * operations.
  */
@deprecated("use E instead.", "0.1.0")
case object ETranscendental extends AbstractTranscendental("\uD835\uDF00", E)

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
case object EulerMascheroni extends AbstractTranscendental("𝛾", Literal(Real.𝛾))
