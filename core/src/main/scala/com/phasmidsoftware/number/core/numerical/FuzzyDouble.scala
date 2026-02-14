/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.numerical

import scala.language.implicitConversions


/**
  * Represents a numeric value with an optional level of fuzziness or uncertainty.
  * Fuzziness quantifies the margin of error or uncertainty associated with the value.
  * NOTE that this class has no obvious "raison d'etre."
  * It is structurally the same as `Real` in the algebra module, but missing a lot of functionality.
  * CONSIDER eliminating this class.
  *
  * @param x    The numeric value represented by this instance.
  * @param fuzz An optional fuzziness object that describes the level and type of uncertainty.
  */
case class FuzzyDouble(x: Double, fuzz: Option[Fuzziness[Double]]) extends Fuzz[Double] {
  /**
    * Adds the provided fuzziness to the current `Fuzz[T]` and returns the resulting value.
    * This method may utilize the fuzziness parameter for calculations where applicable.
    * TESTME
    *
    * @param f the fuzziness to add, represented as a Fuzziness[T].
    * @return a Number that represents the result of adding the provided fuzziness.
    */
  def addFuzz(f: Fuzziness[Double]): Fuzz[Double] =
    copy(fuzz = Fuzziness.combine(x, 0.0, f.style, independent = false)(fuzz, Some(f)))
}

object FuzzyDouble {
  def apply(x: Double): FuzzyDouble = FuzzyDouble(x, None)

  implicit def double2FuzzyDouble(x: Double): FuzzyDouble = FuzzyDouble(x)

  implicit def int2FuzzyDouble(x: Int): FuzzyDouble = FuzzyDouble(x)

  trait FuzzyDoubleIsNumeric extends Numeric[FuzzyDouble] {
    def plus(x: FuzzyDouble, y: FuzzyDouble): FuzzyDouble = new FuzzyDouble(x.x + y.x, Some(x.fuzz.getOrElse(Fuzziness.doublePrecision).*(y.fuzz.getOrElse(Fuzziness.doublePrecision), independent = true)))

    def times(x: FuzzyDouble, y: FuzzyDouble): FuzzyDouble = new FuzzyDouble(x.x * y.x, Some(x.fuzz.getOrElse(Fuzziness.doublePrecision).*(y.fuzz.getOrElse(Fuzziness.doublePrecision), independent = true)))

    def negate(x: FuzzyDouble): FuzzyDouble = FuzzyDouble(-x.x, x.fuzz)

    def fromInt(x: Int): FuzzyDouble = FuzzyDouble(x)

    def toInt(x: FuzzyDouble): Int = x.x.toInt

    def toLong(x: FuzzyDouble): Long = x.x.toLong

    def toFloat(x: FuzzyDouble): Float = x.x.toFloat

    def toDouble(x: FuzzyDouble): Double = x.x

    def compare(x: FuzzyDouble, y: FuzzyDouble): Int = // TEST
      val diff = plus(x, negate(y))
      val difference = math.abs(diff.x)
      if (diff.x < difference) 0 else diff.x.sign.toInt

    def minus(x: FuzzyDouble, y: FuzzyDouble): FuzzyDouble =
      plus(x, negate(y))

    def parseString(str: String): Option[FuzzyDouble] = // CONSIDER adding double-precision fuzziness
      str.toDoubleOption map (x => FuzzyDouble(x, None))
  }

  implicit object FuzzyDoubleIsNumeric extends FuzzyDoubleIsNumeric
}