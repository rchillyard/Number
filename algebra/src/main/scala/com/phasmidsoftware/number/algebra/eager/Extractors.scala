package com.phasmidsoftware.number.algebra.eager

import com.phasmidsoftware.number.algebra.core.{CanPower, Q, Z}
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.numerical
import com.phasmidsoftware.number.core.numerical.Complex.convertToCartesian
import com.phasmidsoftware.number.core.numerical.{ComplexCartesian, ComplexPolar, Fuzziness}

import scala.annotation.tailrec

/**
  * The Extractors object provides utility methods and patterns for extracting
  * or parsing data from complex structures. It is designed to streamline
  * the process of retrieving specific information by encapsulating common
  * extraction logic.
  *
  * This object is typically used when working with pattern matching,
  * enabling concise and readable code for deconstructing data structures.
  */
object Extractors

/**
  * Provides an extractor for identifying whether an `Eager` instance satisfies
  * the `isZero` condition.
  */
object IsZero {
  /**
    * Extractor method for determining if an `Eager` instance satisfies the `isZero` condition.
    *
    * @param eager The `Eager` instance to be evaluated.
    * @return An `Option` containing the `Eager` instance if it satisfies the `isZero` condition; otherwise, `None`.
    */
  def unapply(eager: Eager): Option[Eager] =
    Option.when(eager.isZero)(eager)
}

/**
  * Provides an extractor for identifying whether an `Eager` instance satisfies
  * the `isZero` condition.
  */
object IsFuzzy {
  /**
    * Extractor method for determining if an `Eager` instance satisfies the `isZero` condition.
    *
    * @param eager The `Eager` instance to be evaluated.
    * @return An `Option` containing the `Eager` instance if it satisfies the `isZero` condition; otherwise, `None`.
    */
  def unapply(eager: Eager): Option[(Double, Fuzziness[Double])] = eager match {
    case functional: Functional =>
      functional.maybeFuzz map (f => functional.nominalValue -> f)
    case Real(value, fuzz) =>
      fuzz map (f => value -> f)
    case _ =>
      None
  }
}

/**
  * Companion object containing a custom extractor for identifying instances of the `Eager` class
  * that satisfy a specific condition defined by the `isUnity` method.
  */
object IsUnity {
  /**
    * Extractor method to check if the given `eager` instance satisfies the `isUnity` condition.
    *
    * @param eager An instance of the `Eager` class to be checked.
    * @return An `Option` containing the `eager` instance if the `isUnity` condition is met, otherwise `None`.
    */
  def unapply(eager: Eager): Option[Eager] =
    Option.when(eager.isUnity)(eager)
}

/**
  * Object containing utility methods for working with integers in the context of the Eager type,
  * including a pattern matcher to extract integer values.
  */
object IsInteger {
  /**
    * Extracts an integer value from the given Eager instance if possible.
    *
    * @param eager The Eager instance to be tested and potentially decomposed.
    * @return An Option containing the integer value if the input can be converted
    *         to an integer, or None otherwise.
    */
  def unapply(eager: Eager): Option[Int] = eager match {
    case z: Z =>
      Some(z.toInt)
    case q: Q =>
      q.toRational.maybeInt
    case _ =>
      None
  }
}

/**
  * A utility object for pattern matching operations involving positive square roots.
  * The main purpose of this object is to identify whether a given number or expression
  * can be represented as the square root of another number.
  */
object IsSquareRoot {
  /**
    * Extractor method used to determine if the given `Eager` instance can be
    * decomposed into a positive square root of a number.
    *
    * @param x the `Eager` instance to be analyzed
    * @return an `Option` containing the square root as a `Number` if `x` represents a
    *         positive square root of a number; `None` otherwise
    */
  def unapply(x: Eager): Option[Number] = x match {
    case InversePower(2, n: Q) if n.signum > 0 =>
      n.toRational.sqrt.toOption.map(RationalNumber(_))
    case InversePower(2, r: Real) if r.signum > 0 =>
      r power Rational.half
    case InversePower(2, n: CanPower[Number] @unchecked) if n.signum > 0 =>
      n.pow(RationalNumber.half)
    case _ =>
      None
  }
}

/**
  * An extractor object used for pattern matching to identify and extract `Eager` instances
  * that satisfy specific conditions related to imaginary or irrational numbers.
  */
object IsImaginary {
  /**
    * Extracts the square root of the negative of the underlying Number of an InversePower if the number
    * is negative and the power is 2.
    *
    * TODO: extend to match any purely imaginary Complex(0, x) where x != 0
    * Currently only matches i = (-1)^(1/2)
    * In this case, we would move this into Extractors.
    *
    * @param x the `Eager` instance to be matched and extracted.
    * @return an `Option` containing the extracted `Number` if the input matches the expected pattern, or `None` otherwise.
    */
  def unapply(x: Eager): Option[Eager] = x.normalize match {
    case InversePower(2, q: Q) if q.signum < 0 =>
      q.toRational.negate.sqrt.toOption.map(RationalNumber(_).normalize)
    case InversePower(2, r: Real) if r.signum < 0 =>
      r.negate power Rational.half
    case InversePower(2, n: CanPower[Number] @unchecked) if n.signum < 0 =>
      for {
        z: Real <- n.convert(Real.zero)
        y <- z.negate.power(Rational.half)
      } yield y.normalize
    case x =>
      None
  }
}

/**
  * An object providing a custom extractor to check if a given `Eager` instance represents a finite value.
  *
  * The `IsFinite` object can be used for pattern matching and extracting instances of `Eager`
  * that are determined to be finite values, as determined by the absence of conditions that make it infinite.
  */
object IsFinite {
  /**
    * Returns `Some(x)` if `this` is finite.
    *
    * @param x the `Eager` instance to be matched and extracted.
    * @return an `Option` containing the extracted `Number` if the input matches the expected pattern, or `None` otherwise.
    */
  def unapply(x: Eager): Option[Eager] =
    Option.when(IsInfinite.unapply(x).isEmpty)(x)

}

/**
  * Provides a utility for extracting infinite values from an `Eager` instance.
  */
object IsInfinite {
  /**
    * Returns `Some(x)` if `this` is infinite.
    *
    * @param x the `Eager` instance to be matched and extracted.
    * @return an `Option` containing the extracted `Number` if the input matches the expected pattern, or `None` otherwise.
    */
  def unapply(x: Eager): Option[Eager] = x.normalize match {
    case Eager.infinity | Eager.negInfinity =>
      Some(x)
    case Real(Double.PositiveInfinity, _) | Real(Double.NegativeInfinity, _) =>
      Some(x)
    case RationalNumber(z, _) if z.isInfinite =>
      Some(x)
    case x =>
      None
  }
}

/**
  * The `HasImaginary` object provides utilities for working with complex mathematical objects,
  * particularly for extracting and transforming specific patterns in `Eager` instances.
  */
object HasImaginary {
  /**
    * Extractor method for optionally extracting the imaginary part of an `Eager` instance.
    * If the input is a complex number, it returns the imaginary part as an `Eager` instance.
    * If the input is an InversePower with a negative value, then it returns the square root of the negated value.
    *
    * @param x the input value of type `Eager` to be analyzed and transformed.
    * @return an `Option` containing a transformed `Eager` instance if a match is found,
    *         or `None` if no pattern matches the input.
    */
  @tailrec
  def unapply(x: Eager): Option[Eager] = x match {
    case Complex(ComplexCartesian(_, i)) =>
      Some(Eager(numerical.Real(i)))
    case Complex(cp: ComplexPolar) =>
      unapply(Complex(convertToCartesian(cp)))
    case IsImaginary(z) =>
      Some(z)
    case _ =>
      None
  }
}