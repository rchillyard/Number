/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Constants.gamma
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
case object L2 extends AbstractTranscendental("ln(2)", Two.log)

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
case object LgE extends AbstractTranscendental("log2e", Two.log.reciprocal.simplify)

/**
  * Singleton object representing the Euler-Mascheroni constant (𝛾), a fundamental mathematical constant.
  * It extends `AbstractTranscendental` to encapsulate its symbolic representation and mathematical definition.
  *
  * The Euler-Mascheroni constant is a transcendental entity commonly used in number theory and analysis.
  */
case object EulerMascheroni extends AbstractTranscendental("𝛾", gamma)

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
  *
  * CONSIDER should Transcendental extend Expression instead of NumberLike? It is very much like CompositeExpression or AtomicExpression.
  */
trait Transcendental extends NumberLike {
  /**
    * Retrieves the name as a string.
    *
    * @return the name in string format
    */
  def name: String

  /**
    * Retrieves the lazily evaluated mathematical expression representing this transcendental entity.
    *
    * @return the `Expression` associated with this transcendental entity
    */
  def expression: Expression

  /**
    * Applies the provided `ExpressionFunction` to this `Transcendental` entity.
    *
    * @param f the `ExpressionFunction` to be applied, defining a transformation or operation on this `Transcendental`.
    * @return a new `Transcendental` instance representing the result of applying the function.
    */
  def function(f: ExpressionFunction): Transcendental

  /**
    * Action to evaluate this `Expression` as a `Field`, if possible.
    * NOTE: no simplification or factor-based conversion occurs here.
    *
    * @return an optional `Field`.
    */
  def evaluate: Option[Field] = expression.evaluateAsIs
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
    new AbstractTranscendental(s"${f.name}($name)", com.phasmidsoftware.number.expression.Function(expression, f).simplify) {}

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
