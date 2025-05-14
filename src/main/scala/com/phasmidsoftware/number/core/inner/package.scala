/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

import scala.util.Try

package object inner {

  /**
    * This is the type of the nominalValue parameter of a GeneralNumber.
    *
    * There is a companion object to this type in module Value.scala.
    */
  type Value = Either[Either[Option[Double], Rational], Int]

  /**
    * Represents an intermediate numerical form, consisting of:
    * - A `Value`, representing the numerical component.
    * - A `Factor`, providing the contextual framework for interpretation (e.g., units, dimensions, basis of representation).
    * - An optional `Fuzziness[Double]`, describing any associated uncertainty or imprecision.
    *
    * This type is used throughout various computations and transformations to encapsulate numeric quantities,
    * their interpretive factors, and any associated tolerance or fuzziness.
    */
  type ProtoNumber = (Value, Factor, Option[Fuzziness[Double]])

  /**
    * Type alias for the dyadic functions, a tuple of three functions, corresponding to the functions for Int, Rational and Double representations.
    */
  type DyadicFunctions = ((Int, Int) => Try[Int], (Rational, Rational) => Try[Rational], (Double, Double) => Try[Double])

  /**
    * Type alias for the monadic functions, a tuple of three functions, corresponding to the functions for Int, Rational and Double representations.
    */
  type MonadicFunctions = (Int => Try[Int], Rational => Try[Rational], Double => Try[Double])

  /**
    * Type alias for the Boolean query functions.
    */
  type BooleanQueryFunctions = QueryFunctions[Boolean]

  /**
    * Type alias for the Int query functions.
    */
  type IntQueryFunctions = QueryFunctions[Int]

}
