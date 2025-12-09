/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.numerical

import scala.language.implicitConversions


case class FuzzyDouble(x: Double, fuzz: Option[Fuzziness[Double]]) extends Fuzz[Double] {
  /**
    * Adds the provided fuzziness to the current `Fuzz[T]` and returns the resulting value.
    * This method may utilize the fuzziness parameter for calculations where applicable.
    *
    * @param f the fuzziness to add, represented as a Fuzziness[T].
    * @return a Number that represents the result of adding the provided fuzziness.
    */
  def addFuzz(f: Fuzziness[Double]): Number = ??? // TODO implement me
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

    def toInt(x: FuzzyDouble): Int = ??? // TODO implement me

    def toLong(x: FuzzyDouble): Long = ??? // TODO implement me

    def toFloat(x: FuzzyDouble): Float = ??? // TODO implement me

    def toDouble(x: FuzzyDouble): Double = x.x

    def compare(x: FuzzyDouble, y: FuzzyDouble): Int = ??? // TODO implement me

    def minus(x: FuzzyDouble, y: FuzzyDouble): FuzzyDouble = ??? // TODO implement me

    def parseString(str: String): Option[FuzzyDouble] = ??? // TODO implement me
  }

  implicit object FuzzyDoubleIsNumeric extends FuzzyDoubleIsNumeric
}