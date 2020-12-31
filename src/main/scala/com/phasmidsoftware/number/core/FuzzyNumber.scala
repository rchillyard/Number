package com.phasmidsoftware.number.core

/**
  * This class is designed to model a fuzzy Number.
  * See Number for more details on the actual representation.
  *
  * @param value  the value of the Number, expressed as a nested Either type.
  * @param factor the scale factor of the Number: valid scales are: Scalar, Pi, and E.
  * @param fuzz   the fuzziness of this Number.
  */
case class FuzzyNumber(override val value: Value, override val factor: Factor, fuzz: Option[Fuzz[Double]]) extends Number(value, factor) with Fuzzy[Double] {

  /**
    * Auxiliary constructor for an exact number.
    *
    * @param v    the value for the new Number.
    * @param fuzz the fuzz for the new Number.
    */
  def this(v: Value, fuzz: Option[Fuzz[Double]]) = this(v, Scalar, fuzz)

  override def +(x: Number): Number = FuzzyNumber.plus(this, x)

  override def *(x: Number): Number = FuzzyNumber.times(this, x)

  /**
    * Calculate the convolution of two probability density functions (for this and other).
    *
    * @param other    the other Number.
    * @param fuzzOp   the convolution function.
    * @param relative if true, then fuzzOp expects to be given two relative fuzzy values (i.e. tolerances).
    * @return an optional fuzz value.
    */
  private def getConvolutedFuzz(other: Option[Fuzz[Double]])(fuzzOp: (Double, Double) => Double, relative: Boolean): Option[Fuzz[Double]] = fuzz.map(Fuzz.normalizeFuzz).flatMap {
    case r@RelativeFuzz(t1, _) =>
      if (relative) {
        other.map(Fuzz.normalizeFuzz) flatMap {
          case RelativeFuzz(t2, _) => Some(RelativeFuzz(fuzzOp(t1, t2), Gaussian))
          case a@AbsoluteFuzz(_, _) => a.relative(toDouble.getOrElse(0))
        }
      } else makeNumberFuzzy(r.absolute(t1)).getConvolutedFuzz(other)(fuzzOp, relative)
    case a@AbsoluteFuzz(m1, _) =>
      if (relative) makeNumberFuzzy(a.relative(m1)).getConvolutedFuzz(other)(fuzzOp, relative)
      else {
        other.map(Fuzz.normalizeFuzz) flatMap {
          case RelativeFuzz(_, _) => a.relative(toDouble.getOrElse(0))
          case AbsoluteFuzz(m2, _) => Some(RelativeFuzz(fuzzOp(m1, m2), Gaussian))
        }
      }
  }

  /**
    * Apply a mapping to this Number's fuzz.
    *
    * @param fuzzOp   the mapping function.
    * @param absolute if true, then the result will be absolute.
    * @return an optional fuzz value.
    */
  private def mapFuzz(fuzzOp: Double => Double, absolute: Boolean): Option[Fuzz[Double]] = fuzz.map {
    case AbsoluteFuzz(mag, shape) if absolute => AbsoluteFuzz(fuzzOp(mag), shape)
    case RelativeFuzz(tolerance, shape) if absolute =>
      AbsoluteFuzz(fuzzOp(toDouble.getOrElse(0.0) * tolerance), shape)
    case RelativeFuzz(tolerance, shape) =>
      RelativeFuzz(fuzzOp(tolerance), shape)
  }

  /**
    * Make a copy of this Number, but with different fuzziness.
    *
    * @param z the optional Fuzz.
    * @return either a Fuzzy or Exact Number.
    */
  def makeNumberFuzzy(z: Option[Fuzz[Double]]): FuzzyNumber = FuzzyNumber(value, factor, z)

  // CONSIDER use getConvolutedFuzz
  def computeFuzz(t: Double, fo1: Option[Fuzz[Double]], fo2: Option[Fuzz[Double]])(fuzzOp: (Double, Double) => Double, absolute: Boolean): Option[Fuzz[Double]] =
    (fo1, fo2) match {
      case (Some(f1), Some(f2)) => Some(computeFuzz2(t, f1, f2)(fuzzOp, absolute))
      case (Some(_), None) => fo1
      case (None, Some(_)) => fo2
      case (None, None) => None
    }

  // CONSIDER use getConvolutedFuzz
  def computeFuzz2(t: Double, f1: Fuzz[Double], f2: Fuzz[Double])(fuzzOp: (Double, Double) => Double, absolute: Boolean): Fuzz[Double] = {
    val (z1, z2) = (f1.normalizeMagnitude(t, absolute), f2.normalizeMagnitude(t, absolute))
    z1 match {
      case AbsoluteFuzz(m1, sh1) => z2 match {
        case AbsoluteFuzz(m2, sh2) if sh2 == sh1 => AbsoluteFuzz(fuzzOp(m1, m2), sh1)
        case AbsoluteFuzz(_, _) => throw new Exception("shapes don't match")
      }
      case RelativeFuzz(m1, sh1) => z2 match {
        case RelativeFuzz(m2, sh2) if sh2 == sh1 => RelativeFuzz(fuzzOp(m1, m2), sh1)
        case RelativeFuzz(_, _) => throw new Exception("shapes don't match")
      }
    }
  }

  /**
    * Evaluate a dyadic operator on this and other, using either plus, times, ... according to the value of op.
    * NOTE: this and other must have been aligned by type so that they have the same structure.
    *
    * @param other the other operand, a Number.
    * @param f     the factor to apply to the result.
    * @param op    the appropriate DyadicOperation.
    * @return a new Number which is result of applying the appropriate function to the operands this and other.
    */
  def composeDyadicFuzzy(other: Number, f: Factor)(op: DyadicOperation, fuzzOp: (Double, Double) => Double, absolute: Boolean): Option[Number] = {
    val no = composeDyadic(other, f)(op)
    no.map(n => FuzzyNumber(n.value, n.factor, computeFuzz(n.toDouble.get, fuzz, other.fuzz)(fuzzOp, absolute))) // FIXME
  }

  /**
    * Evaluate a monadic operator on this, using either negate or... according to the value of op.
    *
    * @param f  the factor to apply to the result.
    * @param op the appropriate MonadicOperation.
    * @return a new Number which is result of applying the appropriate function to the operand this.
    */
  def composeMonadicFuzzy(f: Factor)(op: MonadicOperation, fuzzOp: Double => Double, absolute: Boolean): Option[Number] =
    composeMonadic(f)(op).map { case n: FuzzyNumber => n.makeNumberFuzzy(mapFuzz(fuzzOp, absolute)) }

  /**
    * Render this FuzzyNumber in String form, including the factor, and the fuzz.
    *
    * @return
    */
  override def toString: String = {
    val sb = new StringBuilder()
    val w = fuzz match {
      case Some(f) => f.toString(toDouble.getOrElse(0.0))
      case None => valueToString
    }
    sb.append(w)
    sb.append(factor.toString)
    sb.toString
  }

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Both the value and the factor will be changed.
    *
    * @param v the value.
    * @param f the factor.
    * @return a FuzzyNumber.
    */
  protected def makeNumber(v: Value, f: Factor): Number = makeNumber(v, f, fuzz)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Both the value and the factor will be changed.
    * CONSIDER: not entirely sure we need this method.
    *
    * @param v the value.
    * @param f the factor.
    * @param z the new fuzziness.
    * @return a FuzzyNumber.
    */
  protected def makeNumber(v: Value, f: Factor, z: Option[Fuzz[Double]]): Number = FuzzyNumber(v, f, z)
}

object FuzzyNumber {
  def apply(): Number = Number.apply()

  private def plus(x: FuzzyNumber, y: Number): Number = {
    val (a, b) = x.alignFactors(y)
    val (p, q) = a.alignTypes(b)
    // CONSIDER matching on (p, q)
    p match {
      case n: FuzzyNumber =>
        n.composeDyadicFuzzy(q, p.factor)(DyadicOperationPlus, _ + _, absolute = true).getOrElse(Number()).specialize
      case _ => q match {
        // NOTE: we assume that the operators commute.
        case n2: FuzzyNumber =>
          // CONSIDER simply invoking plus with parameters reversed
          n2.composeDyadicFuzzy(p, p.factor)(DyadicOperationPlus, _ + _, absolute = true).getOrElse(Number()).specialize
        case _ =>
          p + q
      }
    }


  }

  private def times(x: FuzzyNumber, y: Number): Number = {
    val (p, q) = x.alignTypes(y)
    val factor = p.factor + q.factor

    def doMultiplication(n: FuzzyNumber, q1: Number, factor1: Factor) =
      n.composeDyadicFuzzy(q1, factor1)(DyadicOperationPlus, _ * _, absolute = false).getOrElse(Number()).specialize

    p match {
      case n: FuzzyNumber => doMultiplication(n, q, p.factor)
      case _ => q match {
        // NOTE: we assume that the operators commute.
        case n2: FuzzyNumber => doMultiplication(n2, q, p.factor)
        case _ => p + q
      }
    }
  }

}
