package com.phasmidsoftware.number.algebra.eager

import com.phasmidsoftware.number.algebra.core.{Q, Z}

/**
  * The Extractors object provides utility methods and patterns for extracting
  * or parsing data from complex structures. It is designed to streamline
  * the process of retrieving specific information by encapsulating common
  * extraction logic.
  *
  * This object is typically used when working with pattern matching,
  * enabling concise and readable code for deconstructing data structures.
  */
object Extractors

/**
  * Provides an extractor for identifying whether an `Eager` instance satisfies
  * the `isZero` condition.
  */
object IsZero {
  /**
    * Extractor method for determining if an `Eager` instance satisfies the `isZero` condition.
    *
    * @param eager The `Eager` instance to be evaluated.
    * @return An `Option` containing the `Eager` instance if it satisfies the `isZero` condition; otherwise, `None`.
    */
  def unapply(eager: Eager): Option[Eager] =
    Option.when(eager.isZero)(eager)
}

/**
  * Companion object containing a custom extractor for identifying instances of the `Eager` class
  * that satisfy a specific condition defined by the `isUnity` method.
  */
object IsUnity {
  /**
    * Extractor method to check if the given `eager` instance satisfies the `isUnity` condition.
    *
    * @param eager An instance of the `Eager` class to be checked.
    * @return An `Option` containing the `eager` instance if the `isUnity` condition is met, otherwise `None`.
    */
  def unapply(eager: Eager): Option[Eager] =
    Option.when(eager.isUnity)(eager)
}

/**
  * Object containing utility methods for working with integers in the context of the Eager type,
  * including a pattern matcher to extract integer values.
  */
object IsInteger {
  /**
    * Extracts an integer value from the given Eager instance if possible.
    *
    * @param eager The Eager instance to be tested and potentially decomposed.
    * @return An Option containing the integer value if the input can be converted
    *         to an integer, or None otherwise.
    */
  def unapply(eager: Eager): Option[Int] = eager match {
    case z: Z =>
      Some(z.toInt)
    case q: Q =>
      q.toRational.maybeInt
    case _ =>
      None
  }
}
