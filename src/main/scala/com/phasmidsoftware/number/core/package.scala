/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number

import scala.util.{Either, Try}

package object core {

  /**
    * This is the type of the value parameter of a GeneralNumber.
    *
    * There is a companion object to this type in module Value.scala.
    */
  type Value = Either[Either[Option[Double], Rational], Int]

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

  /**
    * This is the (approximate) error bound on double precision numbers.
    * Not all 64-bit floating point numbers will have this much error, and some might have a bit more.
    * The value of 2 to the power of -53 is 1.11E-16 (this is half of the "official value" because we represent
    * error bounds in the form of plus or minus the error).
    *
    * For more detail see:
    * (1) https://en.wikipedia.org/wiki/IEEE_754
    * (2) https://stackoverflow.com/questions/4317988/ieee-754-floating-point-precision-how-much-error-is-allowed
    * which quotes a slightly lower value in one particular case.
    * (3) http://cs.boisestate.edu/~alark/cs354/lectures/ieee754.pdf
    * (4) https://babbage.cs.qc.cuny.edu/IEEE-754.old/64bit.html
    * (5) http://www.h-schmidt.net/FloatConverter/IEEE754.html (single-precision converter)
    *
    * NOTE also: in IEEE 754 binary, pi is 400921fb54442d18
    * this is equivalent to:                             3.141592653589793
    * The true value is:                                 3.1415926535897932384626433...
    * Varying the lowest-order bit down by one gives us: 3.1415926535897927
    * Varying the lowest-order bit up by one gives us:   3.1415926535897936
    * Thus, the absolute d-p error tolerance for Pi should be: +- 0.0000000000000009, i.e. 5E-16
    * Thus, the relative d-p error tolerance for Pi should be: +- 1.6E-16
    */
  val DoublePrecisionTolerance: Double = 1.6E-16
}
