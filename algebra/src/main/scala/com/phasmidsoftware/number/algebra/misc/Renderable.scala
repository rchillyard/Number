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