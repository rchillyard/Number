/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.inner.Factor
import com.phasmidsoftware.number.expression._

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
  * Represents the transcendental function `natural log of 2` as a singleton object.
  * Unlike `Pi` and `E`, there is currently no way to represent this quantity exactly (and eagerly)
  * in the `Number` library.
  *
  * We use the notation "l2" because that's what Euler would have used.
  * Besides, I hate the notation "ln" for natural log.
  * I once refused to use the "ln" notation in an exam.
  * I thought the examiner would be able to figure out
  * that I knew what I was doing.
  * That was naive!
  *
  * This object extends the `AbstractTranscendental` class, inheriting its behavior and
  * properties. The `l2` object encapsulates the mathematical function `ln(2)`,
  * with its name being `"l2"` and its `Expression` representation being based on `Two.log`.
  *
  * The `Expression` associated with `l2` can be evaluated, materialized, and rendered,
  * adhering to the behavior defined in `AbstractTranscendental`. It is considered atomic
  * and exact in mathematical contexts.
  */
case object L2 extends AbstractTranscendental("l2", Two.log)

/**
  * Trait representing a mathematical transcendental entity.
  * A transcendental entity is typically a mathematical quantity that cannot be expressed as a root
  * of a polynomial equation with rational coefficients. This trait serves as a base for such entities
  * within a symbolic computation framework.
  *
  * NOTE The purpose of this trait in `Number` is to represent transcendental quantities that cannot be
  * expressed exactly any other way.
  * We include pi and e simply for completeness, although we can and do represent those quantities exactly.
  *
  * A `Transcendental` is characterized by:
  *  - A human-readable name of the transcendental entity.
  *  - An `Expression` representing its lazy value, i.e., a mathematical expression.
  */
trait Transcendental extends NumberLike {
  def name: String

  def expression: Expression

  def function(f: ExpressionFunction): Transcendental
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
    * Applies a given `ExpressionFunction` to create a new instance of `Transcendental`.
    *
    * @param f the `ExpressionFunction` to be applied, representing a lazy monadic operation.
    * @return a new `Transcendental` instance that encapsulates the applied function and updated expression.
    */
  def function(f: ExpressionFunction): Transcendental =
    new AbstractTranscendental(s"${f.name}($name)", com.phasmidsoftware.number.expression.Function(expression, f)) {}

  /**
    * Action to evaluate this `Expression` as a `Field`, if possible.
    * NOTE: no simplification or factor-based conversion occurs here.
    *
    * @return an optional `Field`.
    */
  def evaluate: Option[Field] = expression.evaluateAsIs

  /**
    * Method to determine if the materialized value of this `Expression` is defined and corresponds to a `Number`.
    * If this expression is exact, then evaluate as is and convert the result to a `Number`.
    * Otherwise, we simply materialize this expression and convert the result to a `Number`.
    *
    * @return a `Some(x)` if this materializes as a `Number`; otherwise `None`.
    */
  def asNumber: Option[Number] = expression.asNumber

  /**
    * Method to determine what `Factor`, if there is such, this `NumberLike` object is based on.
    * Unlike context, a `None` result is not permissive.
    *
    * @return an optional `Factor`.
    */
  def maybeFactor: Option[Factor] = expression.maybeFactor

  /**
    * Method to determine if this NumberLike object is exact.
    * For instance, Number.pi is exact, although if you converted it into a PureNumber, it would no longer be exact.
    *
    * @return true if this NumberLike object is exact in the context of No factor, else false.
    */
  def isExact: Boolean = true

  /**
    * Method to render this NumberLike in a presentable manner.
    *
    * @return a String
    */
  def render: String = name
}
