/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.core

/**
  * A trait representing the concept of exactness in a value or entity.
  *
  * The `Exactitude` trait provides a contract for determining whether
  * an entity is represented exactly or as an approximation. This trait
  * can be implemented by classes or objects that wish to encapsulate
  * the notion of exactness and approximate values.
  */
trait Exactitude {

  /**
    * Determines whether this object is exact, i.e., is not any kind of approximation.
    *
    * An object is exact if THERE EXISTS AT LEAST ONE CONTEXT IN WHICH IT CAN BE EXACTLY RENDERED.
    * Note that this is an existential over contexts, not a claim about any particular one: the
    * square root of two is exact because the `SquareRoot` representation renders it exactly, even
    * though no `PureNumber` can, and 𝛑 is exact because the `Radian` representation renders it
    * exactly, even though its decimal expansion never will. A method documented as "exact in the
    * context of No representation" is making the narrower claim, which is not this one (#615).
    *
    * CONSIDER it may be possible that there are non-approximatable entities that are not exact either. (#498)
    *
    * Examples: `WholeNumber(1)` and `Succ(NatZero)` are exact, as is `InversePower(2,2)`, the square
    * root of two, and `Angle(WholeNumber.one)`, which is 𝛑. Exactness is a property of the available
    * representations, not of any machine format: whether a value happens to fit an IEEE 754 double
    * has no bearing on it.
    *
    * When we are forced to approximate a value, for example when we want to print the decimal value of 𝛗, the Golden Ratio, 
    * then we mark such a representation as approximate, i.e., this method returns `false`.
    *
    * NOTE a fuzzy value is exact in no context at all. `Real(4.0)` is not exact, because a
    * one-decimal literal carries ±0.05 of fuzz; `Real(4)` is (#551).
    *
    * @return a `Boolean` indicating whether the entity is exact (`true`)
    *         or is an approximation (`false`).
    */
  def isExact: Boolean
}