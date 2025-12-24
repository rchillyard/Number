package com.phasmidsoftware.number.algebra.misc

/**
  * Trait representing a renderable entity that can be presented to the user as a string.
  * Any class or trait that extends `Renderable` should provide an implementation of `render`
  * to define how the entity will be rendered.
  * TODO move this into the core module.
  *
  * NOTE that `render` differs from `toString` in that the latter is intended to be used for debugging
  * purposes, whereas the former is intended to be used for presentation to the user.
  */
trait Renderable {

  /**
    * Method to render this `Valuable` for presentation to the user.
    *
    * @return a String
    */
  def render: String
}

/**
  * A trait representing an abstraction for optionally holding a numeric value.
  *
  * Classes or objects implementing this trait should provide a definition 
  * of how the optional numeric value is derived or maintained.
  */
trait Numeric {

  /**
    * If this is exact, it returns the exact value as a `Double`.
    * Otherwise, it returns `None`.
    * NOTE: do NOT implement this method to return a Double for a fuzzy Real--only for exact numbers.
    *
    * @return Some(x) where x is a Double if this is exact, else None.
    */
  def maybeDouble: Option[Double]
}

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
    * CONSIDER it may be possible that there are non-approximatable entities that are not exact either.
    *
    * The method returns `true` if the value is represented exactly
    * in IEEE 754 format, or Two's complement _or_ the exact value must be represented symbolically.
    * Examples of the first type include the number one (represented here as either `WholeNumber(1)` or `Succ(NatZero)`.
    * Examples of the second type include 1.5 or the square root of two (represented here as `InversePower(2,2)`).
    *
    * When we are forced to approximate a value, for example when we want to print the decimal value of 𝛗, the Golden Ratio, 
    * then we mark such a representation as approximate, i.e., this method returns `false`.
    *
    * @return a `Boolean` indicating whether the entity is exact (`true`)
    *         or is an approximation (`false`).
    */
  def isExact: Boolean
}