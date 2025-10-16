/*
 * Cats Kernel instances for Number library
 */

package com.phasmidsoftware.number.cats

import cats.Show
import cats.implicits.catsSyntaxEq
import cats.kernel.{Eq, Order, PartialOrder}
import com.phasmidsoftware.number.core.inner.{Factor, Rational, Value}
import com.phasmidsoftware.number.core.{AbsoluteFuzz, Complex, ComplexCartesian, ComplexPolar, ExactNumber, Field, GeneralNumber, Number, Real, RelativeFuzz}
import com.phasmidsoftware.number.expression.Expression

/**
  * Centralized Cats Kernel instances, kept out of core companion objects
  * to avoid forcing a Cats dependency on core types at definition sites.
  *
  * Import usage:
  *   import com.phasmidsoftware.number.instances.catsKernel._
  */
trait CatsKernelInstances {

  // Expression
  implicit val expressionEq: Eq[Expression] = Eq.instance {
    (x, y) => x == y || x.simplify == y.simplify || x.evaluateAsIs === y.evaluateAsIs
  }
  implicit val expressionOrder: PartialOrder[Expression] =
    (x: Expression, y: Expression) =>
      if (x == y || x.simplify == y.simplify || x.evaluateAsIs === y.evaluateAsIs)
        0.0
      else
        (for (xx <- x.approximation; yy <- y.approximation) yield fieldPartialOrder.partialCompare(xx, yy)).getOrElse(Double.NaN)
  implicit val expressionShow: Show[Expression] = Show.show(_.render)

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

    private def factorId(f: Factor): String = f.getClass.getName

    private def compareBigInt(a: BigInt, b: BigInt): Int =
      if (a == b) 0 else if (a < b) -1 else 1

    private def compareLong(a: Long, b: Long): Int =
      java.lang.Long.compare(a, b)

    private def nominalKeyCompare(a: Value, b: Value): Int = {
      // Prefer exact rational comparison
      (Value.maybeRational(a), Value.maybeRational(b)) match {
        case (Some(ra), Some(rb)) =>
          val num = compareBigInt(ra.n, rb.n)
          if (num != 0) num else compareBigInt(ra.d, rb.d)
        case _ =>
          // Fall back to integer if available
          (Value.maybeInt(a), Value.maybeInt(b)) match {
            case (Some(x), Some(y)) => java.lang.Integer.compare(x, y)
            case _ =>
              // Finally fall back to raw double bits
              // CONSIDER: this looks wrong to me--shouldn't you be using Double.compare instead of Long.compare?
              // (Also, I see you're using doubleToRawLongBits, but I don't see any use of doubleToLongBits. elsewhere, too)
              (Value.maybeDouble(a), Value.maybeDouble(b)) match {
                case (Some(x), Some(y)) => java.lang.Double.compare(x, y)
                case (Some(_), None)    => 1
                case (None, Some(_))    => -1
                case _                  => 0
              }
          }
      }
    }

    private def fuzzKeyCompare(fo: Option[com.phasmidsoftware.number.core.Fuzziness[Double]],
                               go: Option[com.phasmidsoftware.number.core.Fuzziness[Double]]): Int = (fo, go) match {
      case (None, None)       => 0
      case (None, Some(_))    => -1
      case (Some(_), None)    => 1
      case (Some(f), Some(g)) =>
        // kind tag: Absolute=0, Relative=1, Other=2
        val kindF = f match {
          case _: AbsoluteFuzz[_] => 0
          case _: RelativeFuzz[_] => 1
          case _                  => 2
        }
        val kindG = g match {
          case _: AbsoluteFuzz[_] => 0
          case _: RelativeFuzz[_] => 1
          case _                  => 2
        }
        val cKind = java.lang.Integer.compare(kindF, kindG)
        if (cKind != 0) cKind
        else {
          // shape tag: Box=0, Gaussian=1, else 2
          val shapeF = f match {
            case AbsoluteFuzz(_: Double, com.phasmidsoftware.number.core.Box)      => 0
            case AbsoluteFuzz(_: Double, com.phasmidsoftware.number.core.Gaussian) => 1
            case RelativeFuzz(_: Double, com.phasmidsoftware.number.core.Box)      => 0
            case RelativeFuzz(_: Double, com.phasmidsoftware.number.core.Gaussian) => 1
            case _                                                                 => 2
          }
          val shapeG = g match {
            case AbsoluteFuzz(_: Double, com.phasmidsoftware.number.core.Box)      => 0
            case AbsoluteFuzz(_: Double, com.phasmidsoftware.number.core.Gaussian) => 1
            case RelativeFuzz(_: Double, com.phasmidsoftware.number.core.Box)      => 0
            case RelativeFuzz(_: Double, com.phasmidsoftware.number.core.Gaussian) => 1
            case _                                                                 => 2
          }
          val cShape = java.lang.Integer.compare(shapeF, shapeG)
          if (cShape != 0) cShape
          else {
            // magnitude bits
            val magF = f match {
              case AbsoluteFuzz(m: Double, _) => java.lang.Double.doubleToRawLongBits(m)
              case RelativeFuzz(m: Double, _) => java.lang.Double.doubleToRawLongBits(m)
              case _                          => 0L
            }
            val magG = g match {
              case AbsoluteFuzz(m: Double, _) => java.lang.Double.doubleToRawLongBits(m)
              case RelativeFuzz(m: Double, _) => java.lang.Double.doubleToRawLongBits(m)
              case _                          => 0L
            }
            val cMag = compareLong(magF, magG)
            if (cMag != 0) cMag
            else f.getClass.getName.compareTo(g.getClass.getName)
          }
        }
    }
    def partialCompare(x: Number, y: Number): Double = {
      // 1) Consistent with Eq: Structural equivalence -> 0
      if (numberEq.eqv(x, y)) 0.0
      // 2) NaN handling
      else if (x == Number.NaN || y == Number.NaN) Double.NaN
      // 3) Otherwise gives a fixed ±1 via structural key ordering (never 0)
      else (x, y) match {
        case (a: GeneralNumber, b: GeneralNumber) =>
          val c1 = factorId(a.factor).compareTo(factorId(b.factor))
          if (c1 != 0) c1.toDouble
          else {
            val c2 = nominalKeyCompare(a.nominalValue, b.nominalValue)
            if (c2 != 0) c2.toDouble
            else {
              val c3 = fuzzKeyCompare(a.fuzz, b.fuzz)
              if (c3 != 0) c3.toDouble
              else x.getClass.getName.compareTo(y.getClass.getName).toDouble
            }
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

  // Real: delegate to underlying Number semantics
  implicit val realEq: Eq[Real] = Eq.instance((a, b) => numberEq.eqv(a.x, b.x))

  implicit val realPartialOrder: PartialOrder[Real] =
    PartialOrder.by[Real, Number](_.x)(numberPartialOrder)

  implicit val realShow: Show[Real] = Show.show(_.render)

  // Field: conservative, law-abiding semantics
  implicit val fieldShow: Show[Field] = Show.show(_.render)

  implicit val complexEq: Eq[Complex] = Eq.instance {
    case (ComplexCartesian(x1, y1), ComplexCartesian(x2, y2)) =>
      numberEq.eqv(x1, x2) && numberEq.eqv(y1, y2)
    case (ComplexPolar(r1, t1, n1), ComplexPolar(r2, t2, n2)) =>
      numberEq.eqv(r1, r2) && numberEq.eqv(t1, t2) && n1 == n2
    case _ => false
  }

  implicit val complexShow: Show[Complex] = Show.show(_.render)

  // Re-introduce Eq[Field] with strict structural semantics
  implicit val fieldEq: Eq[Field] = Eq.instance {
    case (Real(x), Real(y)) => numberEq.eqv(x, y)
    case (a: Complex, b: Complex) => complexEq.eqv(a, b)
    case _ => false
  }

  implicit val fieldPartialOrder: PartialOrder[Field] = new PartialOrder[Field] {
    def partialCompare(a: Field, b: Field): Double = (a, b) match {
      case (Real(x), Real(y)) => numberPartialOrder.partialCompare(x, y)
      case (a: Complex, b: Complex) => if (complexEq.eqv(a, b)) 0.0 else Double.NaN
      case _ => Double.NaN
    }
    override def eqv(a: Field, b: Field): Boolean = (a, b) match {
      case (Real(x), Real(y)) => numberEq.eqv(x, y)
      case (a: Complex, b: Complex) => complexEq.eqv(a, b)
      case _ => false
    }
  }
}

object CatsKernel extends CatsKernelInstances


