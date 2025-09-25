/*
 * Cats Kernel instances for Number library
 */

package com.phasmidsoftware.number.cats

import cats.kernel.{Eq, Order, PartialOrder}
import cats.Show
import com.phasmidsoftware.number.core.{ExactNumber, Number, GeneralNumber}
import com.phasmidsoftware.number.core.inner.Rational

/**
  * Centralized Cats Kernel instances, kept out of core companion objects
  * to avoid forcing a Cats dependency on core types at definition sites.
  *
  * Import usage:
  *   import com.phasmidsoftware.number.instances.catsKernel._
  */
trait CatsKernelInstances {

  // Rational
  implicit val rationalEq: Eq[Rational] = Eq.instance((x, y) => x.compare(y) == 0)
  implicit val rationalOrder: Order[Rational] = Order.from((x, y) => x.compare(y))
  implicit val rationalShow: Show[Rational] = Show.show(_.render)

  // ExactNumber
  implicit val exactNumberEq: Eq[ExactNumber] = Eq.instance(_ == _)
  implicit val exactNumberOrder: Order[ExactNumber] = Order.from((x, y) => Number.doCompare(x, y))
  implicit val exactNumberShow: Show[ExactNumber] = Show.show(_.render)

  //implicit val fuzzyNumberIsFuzzy: Fuzzy[Number] = FuzzyNumber.NumberIsFuzzy

  // Number: prefer PartialOrder to reflect fuzzy/NaN comparability limits
  implicit val numberPartialOrder: PartialOrder[Number] = new PartialOrder[Number] {
    def partialCompare(x: Number, y: Number): Double = {
      // 1) Consistent with Eq: Structural equivalence -> 0
      if (numberEq.eqv(x, y)) 0.0
      // 2) NaN handling
      else if (x == Number.NaN || y == Number.NaN) Double.NaN
      // 3) Fuzzy "indistinguishable" -> incomparable (NaN)
      //else if (implicitly[Fuzzy[Number]].same(0.5)(x, y)) Double.NaN
      // 4) Otherwise gives a fixed ±1 via structural key ordering (never 0)
      else (x, y) match {
        case (a: GeneralNumber, b: GeneralNumber) =>
          val c1 = a.factor.toString.compare(b.factor.toString)
          if (c1 != 0) c1.toDouble
          else {
            val c2 = a.nominalValue.toString.compare(b.nominalValue.toString)
            if (c2 != 0) c2.toDouble
            else a.fuzz.toString.compare(b.fuzz.toString).toDouble
          }
        case _ =>
          // Fallback: consistent non-zero using class name
          x.getClass.getName.compare(y.getClass.getName).toDouble
      }
    }
    override def eqv(x: Number, y: Number): Boolean =
      numberEq.eqv(x, y)
  }

  // A strict Eq for Number: structural or zero-difference equality, excluding NaN
  implicit val numberEq: Eq[Number] = Eq.instance {
  case (a: GeneralNumber, b: GeneralNumber) =>
    a.nominalValue == b.nominalValue &&
    a.factor       == b.factor &&
    a.fuzz         == b.fuzz
}

  implicit val numberShow: Show[Number] = Show.show(_.render)
}

object catsKernel extends CatsKernelInstances


