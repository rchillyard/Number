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
  * The `Context` trait provides an abstraction for determining whether a particular
  * representation or field qualifies within the scope of a specific context. It supports
  * logical operations (`or`, `and`, `not`) to compose complex contexts from simpler ones.
  */
trait Context extends CoreContext {

  self =>

  /**
    * Determines whether a given `HasValue` qualifies in the current `Context`.
    *
    * This method evaluates the provided `HasValue` object to check if it meets
    * the criteria defined by the `Context`. The qualification logic is based
    * on the specific type of `HasValue`. Natural numbers (`Nat`) automatically
    * qualify, whereas scalar values (`Scalar`) qualify only if their associated
    * representation meets the required conditions.
    *
    * @param v the `HasValue` object to check for qualification
    * @return `true` if the `HasValue` qualifies; otherwise, throws a `AlgebraException` for unsupported types
    */
  def valuableQualifies(v: Valuable): Boolean = (this, v) match {
    case (_, _: Nat) =>
      true
    case (AnyContext, _) =>
      true
    case _ =>
      v.maybeRepresentation(this).exists(representationQualifies)
  }

  /**
    * Determines if a given optional `Scalar` is qualified within this context.
    *
    * The method evaluates the provided optional `Scalar`'s associated representation,
    * and checks if the representation satisfies the qualification criteria defined
    * in this context. If the representation qualifies, the `Scalar` itself is returned
    * wrapped in an `Option`. Otherwise, the result is `None`.
    *
    * @param so an optional `Scalar` to evaluate; if `None` is provided, the method directly returns `None`
    * @return an optional `Scalar` that qualifies, or `None` if either the input is `None`
    *         or the representation does not meet the qualification criteria
    */
  def qualifyingEagerValue(so: Option[Eager]): Option[Eager] =
    for {
      s <- so
      value <- s.maybeRepresentation(this) if representationQualifies(value)
    } yield s

  /**
    * Combines this `Context` with another `Context` using a logical OR operation.
    * The resulting `Context` qualifies a representation if it qualifies in either the current `Context`
    * or the provided `Context`.
    *
    * @param that the other `Context` to combine with this `Context`.
    * @return a new `Context` that represents the logical OR of the two contexts.
    */
  override def or(that: CoreContext): Context = new Context {
    /**
      * Determines whether the given representation is acceptable in this `Context`.
      *
      * @param f the representation.
      * @return true if the representation qualifies; false otherwise.
      */
    def representationQualifies(f: Representation): Boolean = self.representationQualifies(f) || that.representationQualifies(f)

    override def toString: String = s"Context allowing ($self) or ($that)"
  }

  /**
    * Produces a new context that requires both this context and the provided context to qualify a representation.
    * The resulting context effectively applies a logical AND operation between this context and the given context.
    *
    * @param that the second context that will be combined with this context using a logical AND operation.
    * @return a new `Context` that qualifies a representation only if it qualifies in both this context and the given context.
    */
  override def and(that: CoreContext): Context =
    (f: Representation) => this.representationQualifies(f) && that.representationQualifies(f)

  /**
    * Negates the qualification of a representation in the current context.
    *
    * @return A new context that inverts the qualification logic for representations.
    */
  override def not: Context =
    (f: Representation) => !this.representationQualifies(f)
}

/**
  * A case class that represents a restricted evaluation context for a specific `Representation`.
  *
  * The `RestrictedContext` is a specialized `Context` implementation that confines its
  * qualifications to a single `Representation`. It determines whether a given representation matches
  * the specific representation (`context`) it was instantiated with. This ensures that only
  * evaluations respecting this restricted context are allowed.
  *
  * @constructor Creates a `RestrictedContext` with a predefined `Representation` to enforce context-based restrictions.
  * @param representation the specific representation that defines the restrictive context. Only representations matching this will qualify.
  */
case class RestrictedContext(representation: Representation) extends Context {
  /**
    * Determines whether the given representation qualifies for this `Context`.
    *
    * @param f the representation.
    * @return true if the representation qualifies; false otherwise.
    */
  def representationQualifies(f: Representation): Boolean = (f, representation) match {
    case (a, b) if a == b =>
      true
    case (PureNumber, Composite) => // Special case to allow PureNumber to be evaluated in Composite context.
      true
    case _ =>
      false
  }

  override def toString: String = s"RestrictedContext($representation)"

  /**
    * Deprecated: use `representation` instead (renamed in version 1.11.0).
    */
  @deprecated("Use representation", since = "1.11.0")
  def factor: Representation = representation
}

/**
  * The `AnyContext` object represents a special `Context` that qualifies all representations without exception.
  *
  * This context essentially overrides all filtering or conditional logic
  * by always returning `true` for any given representation. It is useful as a
  * catch-all context where no restrictions are applied.
  */
case object AnyContext extends Context {
  /**
    * Determines whether the given representation qualifies for this `AnyContext`.
    *
    * @param f the representation to be evaluated for qualification (itd).
    * @return true.
    */
  def representationQualifies(f: Representation): Boolean = true

  override def toString: String = "AnyContext"
}

/**
  * A `Context` that rejects all `Representation` instances without exception.
  *
  * `ImpossibleContext` is a specific instance of the `Context` trait
  * that always returns `false` for any representation, indicating that no representation
  * qualifies within this context.
  *
  * This context acts as the logical negation of all other possible contexts,
  * prohibiting any representation from being considered qualifying.
  */
case object ImpossibleContext extends Context {
  /**
    * Determines whether the given representation qualifies for this `ImpossibleContext`.
    *
    * @param f the representation to be evaluated for qualification (itd).
    * @return false.
    */
  def representationQualifies(f: Representation): Boolean = false

  override def toString: String = "ImpossibleContext"
}

/**
  * Companion object for the `Context` class.
  *
  * Provides predefined contexts for evaluating and qualifying representations or fields
  * under specific mathematical or functional conditions. This includes contexts
  * for scalars, logarithms, and roots, along with utility functions for determining qualification.
  */
object Context {
  /**
    * Represents a context that supports either purely numerical scalars (`PureNumber`)
    * or angular units represented in x (`Radian`).
    *
    * This combined context allows for operations or evaluations that are valid
    * for both pure numerical values and values with radian representations.
    *
    * Defined by combining `RestrictedContext(PureNumber)` and `RestrictedContext(Radian)`
    * using the `or` method, which permits qualification within either of these contexts.
    */
  val AnyScalar: Context = RestrictedContext(PureNumber) `or` RestrictedContext(Radian)
  /**
    * A combined `Context` that represents any type of logarithmic base.
    *
    * This context is composed of three restricted contexts:
    * - Natural Exponential (inner.NatLog)
    * - Binary Exponential (inner.Log2)
    * - Base 10 Exponential (inner.Log10)
    *
    * A value qualifies for this context if it qualifies for any of the three individual
    * restricted logarithmic contexts.
    */
  val AnyLog: Context = RestrictedContext(inner.NatLog) `or` RestrictedContext(inner.Log2) `or` RestrictedContext(Log10) `or` RestrictedContext(Euler)
  /**
    * A `Context` that qualifies representations as either square roots (`SquareRoot`) or cube roots (`CubeRoot`).
    * Combines the qualification conditions of `SquareRoot` and `CubeRoot` contexts using logical OR.
    */
  val AnyRoot: Context = RestrictedContext(SquareRoot) `or` RestrictedContext(CubeRoot)
}
