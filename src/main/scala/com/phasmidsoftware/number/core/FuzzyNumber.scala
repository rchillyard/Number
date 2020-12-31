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
    * Calculate the convolution of two probability density functions (for this and that).
    *
    * @param that     the other fuzz.
    * @param t        the magnitude of the resulting Number.
    * @param fuzzOp   the convolution function.
    * @param relative if true, then fuzzOp expects to be given two relative fuzzy values (i.e. tolerances).
    * @return an optional fuzz value.
    */
  private def getConvolutedFuzz(t: Double, that: Option[Fuzz[Double]])(fuzzOp: (Double, Double) => Double, relative: Boolean): Option[Fuzz[Double]] =
    (Fuzz.normalizeFuzz(fuzz, t, relative), Fuzz.normalizeFuzz(that, t, relative)) match {
      case (Some(f1), Some(f2)) => Some(Fuzz.convoluteFuzzies(f1, f2)(t, fuzzOp, relative))
      case (Some(f1), None) => Some(f1)
      case _ => throw FuzzyNumberException(s"logic error: this is not fuzzy")
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

  /**
    * Compute the convoluted fuzziness, given this fuzz and fo (the optional other fuzz), as well as fuzzOp, t and absolute.
    *
    * @param t        the magnitude of the resulting FuzzyNumber.
    * @param fo       the optional other fuzz.
    * @param fuzzOp   the convolution function.
    * @param absolute true if the convolution is based on absolute values.
    * @return the convolution of fuzz and fo.
    */
  def computeFuzz(t: Double, fo: Option[Fuzz[Double]])(fuzzOp: (Double, Double) => Double, absolute: Boolean): Option[Fuzz[Double]] =
    getConvolutedFuzz(t, fo)(fuzzOp, !absolute)

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
    no.map(n => FuzzyNumber(n.value, n.factor, computeFuzz(n.toDouble.get, other.fuzz)(fuzzOp, absolute))) // FIXME
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

case class FuzzyNumberException(str: String) extends Exception(str)
