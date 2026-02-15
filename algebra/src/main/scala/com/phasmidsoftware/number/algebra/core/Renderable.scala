/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.core

/**
  * Trait representing a renderable entity that can be presented to the user as a string.
  * Any class or trait that extends `Renderable` should provide an implementation of `render`
  * to define how the entity will be rendered.
  *
  * NOTE that `render` differs from `toString` in that the latter is intended to be used for debugging
  * purposes, whereas the former is intended to be used for presentation to the user.
  *
  * NOTE that there is also a `render` method declared in `NumberLike` (`core` module).
  */
trait Renderable {

  /**
    * Method to render this `Valuable` for presentation to the user.
    * NOTE to implementors: never ever invoke `this.toString` in your implementation lest you get a StackOverflow.
    *
    * @return a String
    */
  def render: String

  /**
    * MORE IMPORTANT NOTE: it really doesn't work to do this. It makes debugging almost useless.
    * You really do need to avoid having toString invoke render (or show).
    *
    * @return the rendered string representation of the object
    */
  //  override def toString: String = render
}
