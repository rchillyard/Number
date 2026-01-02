/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.core

/**
  * The typeclass `Convertible` represents a generic abstraction for transforming an instance of type `U`
  * into type `T` by applying a specific operation defined by the `convert` method.
  *
  * @tparam T the target type to which an instance of type `U` can be converted
  * @tparam U the source type that can be transformed into type `T`
  */
trait Convertible[T, U] {

  /**
    * Transforms an instance of type `U` into type `T` by applying a specific operation,
    * optionally using an additional parameter of type `T`.
    *
    * @param t a template object of type `T` that might assist in the conversion
    * @param u the source object of type `U` to be converted
    * @return the converted object of type `T`
    */
  def convert(t: T, u: U): T
}
