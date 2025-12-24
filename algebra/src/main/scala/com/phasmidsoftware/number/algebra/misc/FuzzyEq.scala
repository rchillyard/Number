/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.misc

/**
  * Trait representing fuzzy equality for a type `A`.
  *
  * Fuzzy equality allows determining whether two instances of type `A`
  * are approximately equal based on a given probability `p`.
  *
  * @tparam A The type for which fuzzy equality is defined.
  */
trait FuzzyEq[A]:
  def eqv(x: A, y: A, p: Double): Boolean
// CONSIDER the following alternative signature:
//  def eqv(p: Double)(x: A, y: A): Boolean

object FuzzyEq:
  /**
    * Retrieves the implicit `FuzzyEq` instance for the type `A`.
    *
    * @param fe The implicit `FuzzyEq` instance for the type `A`.
    * @return The `FuzzyEq` instance for the type `A`.
    */
  def apply[A](using fe: FuzzyEq[A]): FuzzyEq[A] = fe

  /**
    * Provides extension methods for performing fuzzy equality comparisons on values of type `A`.
    *
    * The methods rely on an implicit `FuzzyEq[A]` instance to define the fuzzy equality logic.
    * This allows approximate equality checks with a default or specified probability threshold.
    *
    * @param x  The value of type `A` on which the extension methods are invoked.
    * @param fe The implicit `FuzzyEq[A]` instance that defines the equality behavior.
    * @tparam A The type for which fuzzy equality is performed.
    */
  extension [A](x: A)(using fe: FuzzyEq[A])
    /**
      * Compares the current value with another value using fuzzy equality logic.
      * The comparison allows for an approximate equality check with a default probability threshold.
      *
      * @param y The value of type `A` to compare with the current value.
      * @return `true` if the values are considered approximately equal within the default threshold, otherwise `false`.
      */
    infix def ~=(y: A): Boolean = fe.eqv(x, y, 0.5)
    /**
      * Compares the current value with another value using fuzzy equality logic.
      * The comparison allows for an approximate equality check with a specified probability threshold.
      *
      * @param y The value of type `A` to compare with the current value.
      * @param p The probability threshold for determining approximate equality.
      *          A higher value makes the comparison stricter, while a lower value allows more flexibility.
      * @return `true` if the values are considered approximately equal within the specified threshold, otherwise `false`.
      */
    def ~=(y: A, p: Double): Boolean = fe.eqv(x, y, p)
    /**
      * Compares the current value with another value using fuzzy equality logic.
      * The comparison allows for an approximate equality check with a specified probability threshold.
      *
      * @param y The value of type `A` to compare with the current value.
      * @param p The probability threshold for determining approximate equality. Defaults to 0.5.
      *          A higher value makes the comparison stricter, while a lower value allows more flexibility.
      * @return `true` if the values are considered approximately equal within the specified threshold, otherwise `false`.
      */
    def fuzzyEqv(y: A, p: Double = 0.5): Boolean = fe.eqv(x, y, p)

  /**
    * Creates a new `FuzzyEq` instance for the type `A` using the provided comparison function.
    *
    * @param f A function that defines the fuzzy equality logic for values of type `A`.
    *          The function takes two values of type `A` and a probability threshold of type `Double`,
    *          returning `true` if the values are considered approximately equal within the given threshold,
    *          otherwise `false`.
    * @return A `FuzzyEq[A]` instance that uses the provided function for fuzzy equality checks.
    */
  def instance[A](f: (A, A, Double) => Boolean): FuzzyEq[A] =
    (x: A, y: A, p: Double) => f(x, y, p)