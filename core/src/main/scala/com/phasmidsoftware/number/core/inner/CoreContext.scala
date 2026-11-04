/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.inner

import com.phasmidsoftware.number.core.numerical.Field

/**
  * Represents a trait for specifying in which evaluation-contexts a particular expression may be evaluated.
  *
  * The `Context` trait provides an abstraction for determining whether a particular
  * representation or field qualifies within the scope of a specific context. It supports
  * logical operations (`or`, `and`, `not`) to compose complex contexts from simpler ones.
  */
trait CoreContext {

  /**
    * Determines whether the given representation is acceptable in this `Context`.
    *
    * @param rep the representation.
    * @return true if the representation qualifies; false otherwise.
    */
  def repQualifies(rep: Representation): Boolean

  /**
    * Deprecated: use `repQualifies` instead (renamed in version 1.11.0).
    */
  @deprecated("Use repQualifies", since = "1.11.0")
  def factorQualifies(f: Representation): Boolean = repQualifies(f)

  /**
    * Checks if the given `Field` qualifies based on its associated representation, if any.
    *
    * CONSIDER moving Field into inner package.
    *
    * @param f the field to evaluate.
    * @return true if the field's associated representation qualifies, or if no representation is associated; false otherwise.
    */
  def fieldQualifies(f: Field): Boolean =
    f.maybeRep.exists(repQualifies)

  /**
    * Determines whether a given optional field qualifies based on its associated representation.
    *
    * This method evaluates an optional `Field` to determine if it can be considered
    * a qualifying field within the given context. The `Field` must have an associated
    * representation, and that representation must qualify based on the `fieldQualifies` logic.
    *
    * @param fo an optional `Field` to be evaluated.
    * @return an optional `Field` if it qualifies; otherwise, `None`.
    */
  def qualifyingField(fo: Option[Field]): Option[Field] =
    for (f <- fo; value <- f.maybeRep if repQualifies(value)) yield f

  /**
    * Combines this `Context` with another `Context` using a logical OR operation.
    * The resulting `Context` qualifies a representation if it qualifies in either the current `Context`
    * or the provided `Context`.
    *
    * @param that the other `Context` to combine with this `Context`.
    * @return a new `Context` that represents the logical OR of the two contexts.
    */
  def or(that: CoreContext): CoreContext =
    (rep: Representation) => CoreContext.this.repQualifies(rep) || that.repQualifies(rep)

  /**
    * Produces a new context that requires both this context and the provided context to qualify a representation.
    * The resulting context effectively applies a logical AND operation between this context and the given context.
    *
    * @param that the second context that will be combined with this context using a logical AND operation.
    * @return a new `Context` that qualifies a representation only if it qualifies in both this context and the given context.
    */
  def and(that: CoreContext): CoreContext =
    (rep: Representation) => CoreContext.this.repQualifies(rep) && that.repQualifies(rep)

  /**
    * Negates the qualification of a representation in the current context.
    *
    * @return A new context that inverts the qualification logic for representations.
    */
  def not: CoreContext =
    (rep: Representation) => !CoreContext.this.repQualifies(rep)
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
  * @param context the specific representation that defines the restrictive context. Only representations matching this will qualify.
  */
case class RestrictedContext(context: Representation) extends CoreContext {
  /**
    * Determines whether the given representation qualifies for this `Context`.
    *
    * TODO this needs to be checked!
    *
    * @param rep the representation.
    * @return true if the representation qualifies; false otherwise.
    */
  def repQualifies(rep: Representation): Boolean = (rep, context) match {
    case (a, b) if a == b =>
      true
    case _ =>
      false
  }

  override lazy val toString: String = s"RestrictedContext($context)"
}

/**
  * The `AnyContext` object represents a special `Context` that qualifies all representations without exception.
  *
  * This context essentially overrides all filtering or conditional logic
  * by always returning `true` for any given representation. It is useful as a
  * catch-all context where no restrictions are applied.
  */
case object AnyContext extends CoreContext {
  /**
    * Determines whether the given representation qualifies for this `AnyContext`.
    *
    * @param rep the representation to be evaluated for qualification (ignored).
    * @return true.
    */
  def repQualifies(rep: Representation): Boolean = true

  override lazy val toString: String = "AnyContext"

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
case object ImpossibleContext extends CoreContext {
  /**
    * Determines whether the given representation qualifies for this `ImpossibleContext`.
    *
    * @param rep the representation to be evaluated for qualification (ignored).
    * @return false.
    */
  def repQualifies(rep: Representation): Boolean = false

  override lazy val toString: String = "ImpossibleContext"

}

/**
  * Companion object for the `Context` class.
  *
  * Provides predefined contexts for evaluating and qualifying representations or fields
  * under specific mathematical or functional conditions. This includes contexts
  * for scalars, logarithms, and roots, along with utility functions for determining qualification.
  */
object CoreContext {
  /**
    * Represents a context that supports either purely numerical scalars (`PureNumber`)
    * or angular units represented in radians (`Radian`).
    *
    * This combined context allows for operations or evaluations that are valid
    * for both pure numerical values and values with radian representations.
    *
    * Defined by combining `RestrictedContext(PureNumber)` and `RestrictedContext(Radian)`
    * using the `or` method, which permits qualification within either of these contexts.
    */
  val AnyScalar: CoreContext = RestrictedContext(PureNumber) `or` RestrictedContext(Radian)
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
  val AnyLog: CoreContext = RestrictedContext(NatLog) `or` RestrictedContext(Log2) `or` RestrictedContext(Log10) `or` RestrictedContext(Euler)
  /**
    * A `Context` that qualifies representations as either square roots (`SquareRoot`) or cube roots (`CubeRoot`).
    * Combines the qualification conditions of `SquareRoot` and `CubeRoot` contexts using logical OR.
    */
  val AnyRoot: CoreContext = RestrictedContext(SquareRoot) `or` RestrictedContext(CubeRoot)
}
