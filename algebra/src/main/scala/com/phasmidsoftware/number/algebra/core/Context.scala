/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.core

import com.phasmidsoftware.number.algebra.eager.{Eager, Nat}
import com.phasmidsoftware.number.core.inner
import com.phasmidsoftware.number.core.inner.*

/**
  * Represents a trait for specifying in which evaluation-contexts a particular expression may be evaluated.
  *
  * TODO rename this as `Context`
  *
  * The `Context` trait provides an abstraction for determining whether a particular
  * factor or field qualifies within the scope of a specific context. It supports
  * logical operations (`or`, `and`, `not`) to compose complex contexts from simpler ones.
  */
trait Context extends CoreContext {

  /**
    * Determines whether a given `HasValue` qualifies in the current `Context`.
    *
    * This method evaluates the provided `HasValue` object to check if it meets
    * the criteria defined by the `Context`. The qualification logic is based
    * on the specific type of `HasValue`. Natural numbers (`Nat`) automatically
    * qualify, whereas scalar values (`Scalar`) qualify only if their associated
    * factor meets the required conditions.
    *
    * @param v the `HasValue` object to check for qualification
    * @return `true` if the `HasValue` qualifies; otherwise, throws a `AlgebraException` for unsupported types
    */
  def valuableQualifies(v: Valuable): Boolean = (this, v) match {
    case (_, nat: Nat) =>
      true
    case (AnyContext, _) => true
    case _ =>
      v.maybeFactor(this).exists(factorQualifies)
  }

  /**
    * Determines if a given optional `Scalar` is qualified within this context.
    *
    * The method evaluates the provided optional `Scalar`'s associated factor,
    * and checks if the factor satisfies the qualification criteria defined
    * in this context. If the factor qualifies, the `Scalar` itself is returned
    * wrapped in an `Option`. Otherwise, the result is `None`.
    *
    * @param so an optional `Scalar` to evaluate; if `None` is provided, the method directly returns `None`
    * @return an optional `Scalar` that qualifies, or `None` if either the input is `None`
    *         or the factor does not meet the qualification criteria
    */
  def qualifyingEagerValue(so: Option[Eager]): Option[Eager] =
    for {
      s <- so
      value <- s.maybeFactor(this) if factorQualifies(value)
    } yield s

  /**
    * Combines this `Context` with another `Context` using a logical OR operation.
    * The resulting `Context` qualifies a factor if it qualifies in either the current `Context`
    * or the provided `Context`.
    *
    * @param that the other `Context` to combine with this `Context`.
    * @return a new `Context` that represents the logical OR of the two contexts.
    */
  override def or(that: CoreContext): Context =
    (f: Factor) => this.factorQualifies(f) || that.factorQualifies(f)

  /**
    * Produces a new context that requires both this context and the provided context to qualify a factor.
    * The resulting context effectively applies a logical AND operation between this context and the given context.
    *
    * @param that the second context that will be combined with this context using a logical AND operation.
    * @return a new `Context` that qualifies a factor only if it qualifies in both this context and the given context.
    */
  override def and(that: CoreContext): Context =
    (f: Factor) => this.factorQualifies(f) && that.factorQualifies(f)

  /**
    * Negates the qualification of a factor in the current context.
    *
    * @return A new context that inverts the qualification logic for factors.
    */
  override def not: Context =
    (f: Factor) => !this.factorQualifies(f)
}

/**
  * A case class that represents a restricted evaluation context for a specific `Factor`.
  *
  * The `RestrictedContext` is a specialized `Context` implementation that confines its
  * qualifications to a single `Factor`. It determines whether a given factor matches
  * the specific factor (`context`) it was instantiated with. This ensures that only
  * evaluations respecting this restricted context are allowed.
  *
  * @constructor Creates a `RestrictedContext` with a predefined `Factor` to enforce context-based restrictions.
  * @param factor the specific factor that defines the restrictive context. Only factors matching this will qualify.
  */
case class RestrictedContext(factor: Factor) extends Context {
  /**
    * Determines whether the given factor qualifies for this `Context`.
    *
    * TODO this needs to be checked!
    *
    * @param f the factor.
    * @return true if the factor qualifies; false otherwise.
    */
  def factorQualifies(f: Factor): Boolean = (f, factor) match {
    case (a, b) if a == b =>
      true
    case _ =>
      false
  }

  override def toString: String = s"RestrictedContext($factor)"
}

/**
  * The `AnyContext` object represents a special `Context` that qualifies all factors without exception.
  *
  * This context essentially overrides all filtering or conditional logic
  * by always returning `true` for any given factor. It is useful as a
  * catch-all context where no restrictions are applied.
  */
case object AnyContext extends Context {
  /**
    * Determines whether the given factor qualifies for this `AnyContext`.
    *
    * @param f the factor to be evaluated for qualification (itd).
    * @return true.
    */
  def factorQualifies(f: Factor): Boolean = true

  override def toString: String = "AnyContext"
}

/**
  * A `Context` that rejects all `Factor` instances without exception.
  *
  * `ImpossibleContext` is a specific instance of the `Context` trait
  * that always returns `false` for any factor, indicating that no factor
  * qualifies within this context.
  *
  * This context acts as the logical negation of all other possible contexts,
  * prohibiting any factor from being considered qualifying.
  */
case object ImpossibleContext extends Context {
  /**
    * Determines whether the given factor qualifies for this `ImpossibleContext`.
    *
    * @param f the factor to be evaluated for qualification (itd).
    * @return false.
    */
  def factorQualifies(f: Factor): Boolean = false

  override def toString: String = "ImpossibleContext"
}

/**
  * Companion object for the `Context` class.
  *
  * Provides predefined contexts for evaluating and qualifying factors or fields
  * under specific mathematical or functional conditions. This includes contexts
  * for scalars, logarithms, and roots, along with utility functions for determining qualification.
  */
object Context {
  /**
    * Represents a context that supports either purely numerical scalars (`PureNumber`)
    * or angular units represented in x (`Radian`).
    *
    * This combined context allows for operations or evaluations that are valid
    * for both pure numerical values and values with radian factors.
    *
    * Defined by combining `RestrictedContext(PureNumber)` and `RestrictedContext(Radian)`
    * using the `or` method, which permits qualification within either of these contexts.
    */
  val AnyScalar: Context = RestrictedContext(PureNumber) or RestrictedContext(Radian)
  /**
    * A combined `Context` that represents any type of logarithmic base.
    *
    * This context is composed of three restricted contexts:
    * - Natural Logarithm (NatLog)
    * - Base 2 Logarithm (Log2)
    * - Base 10 Logarithm (Log10)
    *
    * A value qualifies for this context if it qualifies for any of the three individual
    * restricted logarithmic contexts.
    */
  val AnyLog: Context = RestrictedContext(inner.NatLog) or RestrictedContext(inner.Log2) or RestrictedContext(Log10) or RestrictedContext(Euler)
  /**
    * A `Context` that qualifies factors as either square roots (`SquareRoot`) or cube roots (`CubeRoot`).
    * Combines the qualification conditions of `SquareRoot` and `CubeRoot` contexts using logical OR.
    */
  val AnyRoot: Context = RestrictedContext(SquareRoot) or RestrictedContext(CubeRoot)
}
