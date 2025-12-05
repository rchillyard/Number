/*
 * Cats Kernel instances for Number library
 */

package com.phasmidsoftware.number.cats

import cats.Show
import cats.implicits.catsSyntaxEq
import cats.kernel.{Eq, Order, PartialOrder}
import com.phasmidsoftware.number.core.algebraic.Algebraic
import com.phasmidsoftware.number.core.expression.Expression
import com.phasmidsoftware.number.core.inner.{Rational, Value}
import com.phasmidsoftware.number.core.{Complex, ComplexCartesian, ComplexPolar, ExactNumber, Field, GeneralNumber, Number, Real}
import scala.annotation.tailrec

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

  // ===== Fuzziness equality/ordering (used by Number partial order fallback) =====
  import com.phasmidsoftware.number.core.{Fuzziness, AbsoluteFuzz => Abs, RelativeFuzz => Rel}

  // Structural Eq for Fuzziness[Double]: kind/shape/magnitude
  implicit val fuzzEq: Eq[Fuzziness[Double]] = Eq.instance { (f, g) =>
    (f, g) match {
      case (Abs(m1: Double, s1), Abs(m2: Double, s2)) => m1 == m2 && s1 == s2
      case (Rel(m1: Double, s1), Rel(m2: Double, s2)) => m1 == m2 && s1 == s2
      case _ => false
    }
  }

  // Fuzziness ordering is delegated to PartialOrder instances below
  implicit val fuzzPartialOrder: PartialOrder[Fuzziness[Double]] = new PartialOrder[Fuzziness[Double]] {
    def partialCompare(f: Fuzziness[Double], g: Fuzziness[Double]): Double = {
      // kind: Abs < Rel; other kinds treated equal
      val cKind = (f, g) match {
        case (_: Abs[_], _: Abs[_]) => 0
        case (_: Rel[_], _: Rel[_]) => 0
        case (_: Abs[_], _: Rel[_]) => -1
        case (_: Rel[_], _: Abs[_]) => 1
        case _ => 0
      }
      if (cKind != 0) cKind.toDouble
      else {
        // shape: Box < Gaussian; others equal
        val cShape = (f, g) match {
          case (Abs(_: Double, com.phasmidsoftware.number.core.Box) | Rel(_: Double, com.phasmidsoftware.number.core.Box),
                Abs(_: Double, com.phasmidsoftware.number.core.Gaussian) | Rel(_: Double, com.phasmidsoftware.number.core.Gaussian)) => -1
          case (Abs(_: Double, com.phasmidsoftware.number.core.Gaussian) | Rel(_: Double, com.phasmidsoftware.number.core.Gaussian),
                Abs(_: Double, com.phasmidsoftware.number.core.Box) | Rel(_: Double, com.phasmidsoftware.number.core.Box)) => 1
          case _ => 0
        }
        if (cShape != 0) cShape.toDouble
        else {
          // magnitude compare via raw bits to avoid +0.0/-0.0 collapse and keep stability
          def magBits(z: Fuzziness[Double]): Option[Long] = z match {
            case Abs(m: Double, _) => Some(java.lang.Double.doubleToRawLongBits(m))
            case Rel(m: Double, _) => Some(java.lang.Double.doubleToRawLongBits(m))
            case _ => None
          }
          (magBits(f), magBits(g)) match {
            case (Some(a), Some(b)) => java.lang.Long.compare(a, b).toDouble
            case (Some(_), None)    => 1.0
            case (None, Some(_))    => -1.0
            case _                  => 0.0
          }
        }
      }
    }
  }

  /*
   * WARNING: Avoid providing inputs that "look like zero but are not exactly zero".
   * Examples: 0.00(...), 0.00[...], 0* — these are fuzzy renderings whose nominal values may be non-zero.
   * This Eq[Number] deems two numbers equal only if their numeric values are equal within a tolerance AND their factors match.
   * Near-zero-but-non-zero inputs can therefore compare unequal to exact zero and may violate users’ expectations in equality/ordering checks.
   *
   * Guidance for callers:
   * - When zero is intended, pass an exact zero (e.g., ExactNumber(Rational(0,1)) or Number.zeroR), not a fuzzy/near-zero value.
   * - Upstream, normalize or filter values with |x| <= 1e-10 to exact zero if semantic zero is desired.
   * - Avoid comparing numbers across different factors unless you first normalize them into the same factor.
   */

  // A strict Eq for Number: structural or zero-difference equality, excluding NaN
  implicit val numberEq: Eq[Number] = Eq.instance {
    case (Number.NaN, Number.NaN) => true
    case (a: GeneralNumber, b: GeneralNumber) =>
      val ax = a.simplify.specialize
      val bx = b.simplify.specialize

      def asDouble(v: Value): Option[Double] =
        Value.maybeRational(v).map(r => (BigDecimal(r.n) / BigDecimal(r.d)).toDouble)
          .orElse(Value.maybeInt(v).map(_.toDouble))
          .orElse(Value.maybeDouble(v))
      
      def isNearZero(d: Double, eps: Double) = d == 0.0 || math.abs(d) < eps

      def numericEqual(v1: Value, v2: Value, eps: Double = 1e-9): Boolean =
        (asDouble(v1), asDouble(v2)) match {
          case (Some(x), Some(y)) => (isNearZero(x, eps) && isNearZero(y, eps)) || math.abs(x - y) < eps
          case _ => false
        }

      if (numericEqual(ax.nominalValue, bx.nominalValue) && ax.factor == bx.factor) true
      else false
      
    case _ => false
  }

  implicit val numberShow: Show[Number] = Show.show(_.render)

  // Number: prefer PartialOrder to reflect fuzzy/NaN comparability limits
  implicit val numberPartialOrder: PartialOrder[Number] = new PartialOrder[Number] {


    private def compareBigInt(a: BigInt, b: BigInt): Int =
      if (a == b) 0 else if (a < b) -1 else 1


    private def numericCompareStrict(a: Value, b: Value): Option[Int] = {
      def asRational(v: Value): Option[Rational] =
        Value.maybeRational(v).orElse(Value.maybeInt(v).map(Rational(_)))

      (asRational(a), asRational(b)) match {
        case (Some(ra), Some(rb)) =>
          val num = compareBigInt(ra.n, rb.n)
          Some(if (num != 0) num else compareBigInt(ra.d, rb.d))
        case _ =>
          (Value.maybeDouble(a), Value.maybeDouble(b)) match {
            case (Some(x), Some(y)) => Some(java.lang.Double.compare(x, y))
            case _ => None
          }
      }
    }

    private def normalizeToPure(n: GeneralNumber): Option[GeneralNumber] =
    n.normalize match {
      case Real(m: GeneralNumber) => Some(m.simplify.specialize.asInstanceOf[GeneralNumber])
      case _                      => None
    }

    // Strict partial order: returns nonzero only if the values ​​are comparable; otherwise returns NaN
    def partialCompare(x: Number, y: Number): Double = {
      if (numberEq.eqv(x, y)) 0.0
      else if (x == Number.NaN || y == Number.NaN) Double.NaN
      else (x, y) match {
        case (a: GeneralNumber, b: GeneralNumber) =>
          (normalizeToPure(a), normalizeToPure(b)) match {
            case (Some(pa), Some(pb)) =>
              numericCompareStrict(pa.nominalValue, pb.nominalValue) match {
                case Some(c) if c != 0 => c.toDouble
                case _ => Double.NaN
              }
            case _ => Double.NaN
          }
        case _ => Double.NaN
      }
    }
    
  }

  // Real: delegate to underlying Number semantics
  implicit val realEq: Eq[Real] = Eq.instance((a, b) => a.x === b.x)
  implicit val realPartialOrder: PartialOrder[Real] = new PartialOrder[Real] {
    def partialCompare(a: Real, b: Real): Double =
      if (realEq.eqv(a, b)) 0.0 else numberPartialOrder.partialCompare(a.x, b.x)
    override def eqv(a: Real, b: Real): Boolean = realEq.eqv(a, b)
  }

  implicit val realShow: Show[Real] = Show.show(_.render)

  // Field: conservative, law-abiding semantics
  implicit val fieldShow: Show[Field] = Show.show(_.render)

  implicit val complexEq: Eq[Complex] = Eq.instance { (a, b) =>
    // Normalize both sides to Cartesian form, then compare components structurally (real/imag)
    val aC: com.phasmidsoftware.number.core.Complex = a match {
      case c: ComplexCartesian => c
      case p: ComplexPolar     => com.phasmidsoftware.number.core.Complex.convertToCartesian(p)
    }
    val bC: com.phasmidsoftware.number.core.Complex = b match {
      case c: ComplexCartesian => c
      case p: ComplexPolar     => com.phasmidsoftware.number.core.Complex.convertToCartesian(p)
    }
    val (ax, ay) = aC match {
      case com.phasmidsoftware.number.core.BaseComplex(x, y) => (x, y)
    }
    val (bx, by) = bC match {
      case com.phasmidsoftware.number.core.BaseComplex(x, y) => (x, y)
    }
    ax === bx && ay === by
  }

  implicit val complexShow: Show[Complex] = Show.show(_.render)

  // Re-introduce Eq[Field] with strict structural semantics (+ Algebraic bridges)
  implicit val fieldEq: Eq[Field] = Eq.instance {
    case (Real(x), Real(y)) => x === y
    case (a: Complex, b: Complex) => a === b
    // Algebraic bridges: compare by resolved value type
    case (a: Algebraic, b: Algebraic) =>
      val (va, vb) = (a.value, b.value)
      (va.asReal, vb.asReal) match {
        case (Some(ra), Some(rb)) => ra === rb
        case _ => va.asComplex === vb.asComplex
      }
    case (a: Algebraic, b: Real) => a.value.asReal.exists(ra => ra === b)
    case (a: Real, b: Algebraic) => b.value.asReal.exists(rb => a === rb)
    case (a: Algebraic, b: Complex) => a.value.asComplex === b
    case (a: Complex, b: Algebraic) => a === b.value.asComplex
    case (a: Complex, b: Algebraic) => a === b.value.asComplex
    case _ => false
  }

  implicit val fieldPartialOrder: PartialOrder[Field] = new PartialOrder[Field] {
    @tailrec
    def partialCompare(a: Field, b: Field): Double =
      // Cannot use a === b in PartialOrder because of StackOverflowError
      if (fieldEq.eqv(a, b) || a == b) 0.0
      else (a, b) match {
        case (Real(x), Real(y)) => numberPartialOrder.partialCompare(x, y)
        case (_: Complex, _: Complex) => Double.NaN
        case (aa: Algebraic, bb: Algebraic) => algebraicPartialOrder.partialCompare(aa, bb)
        case (aa: Algebraic, other) => partialCompare(aa.value, other)
        case (other, bb: Algebraic) => partialCompare(other, bb.value)
        case _ => Double.NaN
      }
  }

  // Algebraic: equality by comparing resolved Real/Complex explicitly; partial order via Field
  implicit val algebraicEq: Eq[Algebraic] = Eq.instance { (a, b) =>
    (a eq b) || {
      val (va, vb) = (a.value, b.value)
      (va.asReal, vb.asReal) match {
        case (Some(ra), Some(rb)) => ra === rb
        case _ => va.asComplex === vb.asComplex
      }
    }
  }

  implicit val algebraicPartialOrder: PartialOrder[Algebraic] = new PartialOrder[Algebraic] {
    def partialCompare(a: Algebraic, b: Algebraic): Double =
      fieldPartialOrder.partialCompare(a.value, b.value)

    // Cannot use a === b in PartialOrder because of StackOverflowError
    override def eqv(a: Algebraic, b: Algebraic): Boolean = algebraicEq.eqv(a, b)
  }

  implicit val algebraicShow: Show[Algebraic] = Show.show(_.render)

}

object CatsKernel extends CatsKernelInstances


