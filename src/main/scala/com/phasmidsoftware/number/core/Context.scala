/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

/**
  * Represents a trait for specifying in which evaluation-contexts a particular expression may be evaluated.
  *
  * The `Context` trait provides an abstraction for determining whether a particular
  * factor or field qualifies within the scope of a specific context. It supports
  * logical operations (`or`, `and`, `not`) to compose complex contexts from simpler ones.
  */
trait Context {

  /**
    * Determines whether the given factor is acceptable in this `Context`.
    *
    * @param f the factor.
    * @return true if the factor qualifies; false otherwise.
    */
  def factorQualifies(f: Factor): Boolean

  /**
    * Checks if the given `Field` qualifies based on its associated factor, if any.
    *
    * @param f the field to evaluate.
    * @return true if the field's associated factor qualifies, or if no factor is associated; false otherwise.
    */
  def fieldQualifies(f: Field): Boolean =
    f.maybeFactor.forall(factorQualifies)

  /**
    * Determines whether a given optional field qualifies based on its associated factor.
    *
    * This method evaluates an optional `Field` to determine if it can be considered
    * a qualifying field within the given context. The `Field` must have an associated
    * factor, and that factor must qualify based on the `fieldQualifies` logic.
    *
    * @param fo an optional `Field` to be evaluated.
    * @return an optional `Field` if it qualifies; otherwise, `None`.
    */
  def qualifyingField(fo: Option[Field]): Option[Field] =
    for (f <- fo; value <- f.maybeFactor if factorQualifies(value)) yield f

  /**
    * Combines this `Context` with another `Context` using a logical OR operation.
    * The resulting `Context` qualifies a factor if it qualifies in either the current `Context`
    * or the provided `Context`.
    *
    * @param that the other `Context` to combine with this `Context`.
    * @return a new `Context` that represents the logical OR of the two contexts.
    */
  def or(that: Context): Context =
    (f: Factor) => Context.this.factorQualifies(f) || that.factorQualifies(f)

  /**
    * Produces a new context that requires both this context and the provided context to qualify a factor.
    * The resulting context effectively applies a logical AND operation between this context and the given context.
    *
    * @param that the second context that will be combined with this context using a logical AND operation.
    * @return a new `Context` that qualifies a factor only if it qualifies in both this context and the given context.
    */
  def and(that: Context): Context =
    (f: Factor) => Context.this.factorQualifies(f) && that.factorQualifies(f)

  /**
    * Negates the qualification of a factor in the current context.
    *
    * @return A new context that inverts the qualification logic for factors.
    */
  def not: Context =
    (f: Factor) => !Context.this.factorQualifies(f)
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
  * @param context the specific factor that defines the restrictive context. Only factors matching this will qualify.
  */
case class RestrictedContext(context: Factor) extends Context {
  /**
    * Determines whether the given factor qualifies for this `Context`.
    *
    * TODO this needs to be checked!
    *
    * @param f the factor.
    * @return true if the factor qualifies; false otherwise.
    */
  def factorQualifies(f: Factor): Boolean = (f, context) match {
    case (a, b) if a == b => true
    case _ => false
  }
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
    * @param f the factor to be evaluated for qualification (ignored).
    * @return true.
    */
  def factorQualifies(f: Factor): Boolean = true
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
    * @param f the factor to be evaluated for qualification (ignored).
    * @return false.
    */
  def factorQualifies(f: Factor): Boolean = false
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
    * or angular units represented in radians (`Radian`).
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
  val AnyLog: Context = RestrictedContext(NatLog) or RestrictedContext(Log2) or RestrictedContext(Log10)
  /**
    * A `Context` that qualifies factors as either square roots (`SquareRoot`) or cube roots (`CubeRoot`).
    * Combines the qualification conditions of `SquareRoot` and `CubeRoot` contexts using logical OR.
    */
  val AnyRoot: Context = RestrictedContext(SquareRoot) or RestrictedContext(CubeRoot)
}
