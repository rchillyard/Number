/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.core

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
