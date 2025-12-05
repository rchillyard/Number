/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra

import algebra.ring.AdditiveCommutativeGroup
import cats.Show
import cats.kernel.Eq
import com.phasmidsoftware.number.algebra.Angle.{angleIsCommutativeGroup, r180}
import com.phasmidsoftware.number.algebra.{Radians, Structure}
import com.phasmidsoftware.number.core.NumberException
import com.phasmidsoftware.number.core.inner.{Radian, Rational, Value}
import com.phasmidsoftware.number.misc.FP
import scala.reflect.ClassTag

/**
  * A case class representing an angle in radians. This class implements the `Additive` and `Radians` traits,
  * allowing operations such as addition, subtraction, and various type conversions.
  * Angle represents the "circle group," which is a compact Abelian (commutative) group under angle addition,
  * where the addition wraps around the circle.
  * It is compact in that it is bounded by -𝛑 and 𝛑 (-180° and 180°).
  * It is also commutative in that the order of addition is irrelevant.
  * The circle group is a subset of the real numbers.
  * The `degrees` attribute of an `Angle` is only of interest in the following situations:
  *   - when the `Angle` is parsed from a `String`, the `degrees` attribute is set according to whether the degree symbol is present.
  *   - when the `Angle` is converted to a `String`, the `degrees` attribute is used to determine whether to use degrees or radians.
  *
  * Angle does not support ordering or (general) comparison.
  *
  * @param radians the value of the angle in radians
  * @param degrees whether the angle is in radians or degrees.
  *                In the latter case, the `radians` attribute is unchanged (1 is still half of a full circle).
  */
case class Angle private[algebra](radians: Number, degrees: Boolean = false) extends Circle with Scalable[Angle] with CanAdd[Angle, Angle] with Radians with CanNormalize[Angle] {

  /**
    * Normalizes an angle instance to its equivalent value within the standard range of radians.
    *
    * This method processes the `radians` value of the current `Angle` and adjusts it
    * to lie within the expected range, typically between 0 and 2π or -π to π, depending
    * on the implementation of the `Radian.modulate` logic.
    * Various specific cases are covered based on the type of the input:
    *   - Rational numbers are converted using their rational representation.
    *   - Reals are adjusted using their double value, with fuzz consideration pending implementation.
    *   - Whole numbers are directly handled using their integer representation.
    *
    * An exception is thrown if the input type is not recognized.
    *
    * @return a new instance of `Angle` representing the normalized value
    */
  def normalize: Angle = radians match {
    case RationalNumber(r, false) =>
      Angle(Radian.modulate(Value.fromRational(r)))
    case Real(value, fuzz) =>
      Angle(Radian.modulate(Value.fromDouble(Some(value)))) // TODO take care of fuzz
    case WholeNumber(x) =>
      Angle(Radian.modulate(Value.fromRational(Rational(x.toBigInt))))
    case _ =>
      throw NumberException(s"normalize: unexpected type $radians")
  }

  /**
    * Compares the current `Angle` instance with another `Number` to determine their exact order.
    *
    * If the provided `Number` is an `Angle`, this method compares their underlying radian values.
    * If the provided `Number` is not an `Angle`, the comparison cannot be performed, and `None` is returned.
    *
    * @param that the `Number` instance to compare with the current `Angle` instance
    * @return an `Option[Int]`, where `Some(-1)` indicates that the current `Angle` is less than the provided `Angle`,
    *         `Some(0)` indicates that both angles are equal, `Some(1)` indicates that the current `Angle` is greater,
    *         and `None` is returned if the comparison cannot be made
    */
  def compareExact(that: Scalar): Option[Int] = that match {
    case Angle(r, _) =>
      // TODO normalize before comparison
      Some(radians.compare(r))
    case _ =>
      None
  }

  /**
    * Converts this Angle to a representation of the specified type `T`, if possible.
    *
    * This method attempts to convert the number to a type `T` that has implicit evidence
    * of `Ordering`. If the conversion is successful, it returns an `Option` containing the
    * resulting typed value. If the conversion is not valid or not possible for the given
    * type `T`, it returns `None`.
    *
    * @return an `Option` containing the converted value of type `T` if successful, or `None` if the conversion is not possible.
    */
  def convert[T <: Structure : ClassTag](t: T): Option[T] = t match {
    case _: Real =>
      // TODO normalize before conversion
      radians.approximation(true).map(x => x.scaleByPi).asInstanceOf[Option[T]]
    case _ =>
      None
  }

  /**
    * Determines if the current number is equal to zero.
    *
    * @return true if the number is zero, false otherwise
    */
  def isZero: Boolean = radians.isZero

  /**
    * Represents the zero value of the `Angle` class.
    *
    * This is a predefined constant that corresponds to an `Angle` of zero radians.
    * It is used as the additive identity in operations involving angles.
    */
  val zero: Angle = Angle.zero

  /**
    * Determines the sign of the scalar value represented by this instance.
    * Returns an integer indicating whether the value is positive, negative, or zero.
    *
    * @return 1 if the value is positive, -1 if the value is negative, and 0 if the value is zero
    */
  def signum: Int = compareExact(Angle.zero).get

  /**
    * Method to determine if this Structure object is exact.
    * For instance, `Number.pi` is exact, although if you converted it into a PureNumber, it would no longer be exact.
    *
    * @return true if this Structure object is exact in the context of No factor, else false.
    */
  override def isExact: Boolean = radians.isExact

  /**
    * Converts the current instance to a Double representation.
    *
    * @return the Double value corresponding to the current instance
    */
  def asDouble: Double = scaleFactor * radians.toDouble

  /**
    * Determines if the `radians` value can be interpreted as an instance of `Q`.
    * If `radians` matches the type `Q`, it returns an `Option` containing the value of `Q`.
    * Otherwise, it returns `None`.
    *
    * @return an `Option[Q]` where `Some(Q)` is returned if `radians` is of type `Q`,
    *         otherwise `None`.
    */
  def maybeQ: Option[Q] = radians match {
    case q: Q => Some(q)
    case _ => None
  }

  /**
    * If this `Valuable` is exact, it returns the exact value as a `Double`.
    * Otherwise, it returns `None`.
    * NOTE: do NOT implement this method to return a Double for a fuzzy Real--only for exact numbers.
    *
    * @return Some(x) where x is a Double if this is exact, else None.
    */
  def maybeDouble: Option[Double] =
    FP.whenever(isExact)(convert(Real.zero) map (_.value))

  /**
    * Renders this `Angle` instance as a string representation of radians in terms of π.
    *
    * The method formats the radius equivalent to π, omitting the numeric coefficient if it is 1.
    *
    * @return a string representation of the `Angle` in terms of π
    */
  def render: String = {
    val value = normalize.radians
    if (degrees)
      value.scale(r180).render + "°"
    else {
      val prefix = value.render
      val suffix = "𝛑"
      (if prefix == "1" then "" else prefix) + suffix
    }
  }

  /**
    * Computes the additive inverse of the current `Angle` instance.
    *
    * This method negates the current angle, returning a new `Angle` instance
    * with the opposite value, relative to `Angle.zero`.
    *
    * @return a new `Angle` instance representing the additive inverse of the current angle.
    */
  def unary_- : Angle = angleIsCommutativeGroup.negate(this)

  /**
    * Adds the specified `Angle` to the current `Angle` instance.
    *
    * This method combines the current angle with the provided angle
    * by adding their respective radians, returning a new `Angle` instance
    * representing the sum.
    *
    * @param a the `Angle` to be added to the current `Angle`
    * @return a new `Angle` representing the sum of the current `Angle` and the specified `Angle`
    */
  def +(a: Angle): Angle =
    angleIsCommutativeGroup.plus(this, a)

  /**
    * Adds the specified `Scalar` to the current `Scalar` and returns the result as an `Option[Scalar]`.
    * This method handles addition based on the type of `Scalar` provided. If the input is an `Angle`,
    * it computes the sum of the current `Angle` and the provided `Angle`. If the input is a `Number`,
    * the addition is delegated to the `doPlus` implementation of the `Number`.
    *
    * @param that the `Scalar` to be added to the current instance
    * @return an `Option[Scalar]` containing the result of the addition, or `None` if the operation is not valid
    */
  infix def doPlus(that: Scalar): Option[Scalar] = that match {
    case a: Angle =>
      Some(this + a)
    case x: Scalar =>
      convert(Real.zero) flatMap (r => r doPlus x)
  }

  /**
    * Scales the current instance by the given factor.
    *
    * This method applies a scaling operation on the instance using the provided
    * rational factor and returns the resulting scaled instance.
    *
    * @param factor the rational number representing the scale factor
    * @return the scaled instance of type `T`
    */
  def *(factor: Rational): Angle = radians match {
    case r: RationalNumber => Angle(r * factor)
    case r: Real => Angle(r * factor)
    case w: WholeNumber => Angle(w.scale(factor))
  }

  /**
    * Scales the current scalar instance by the specified rational factor.
    *
    * @param r the `Rational` factor by which to scale the scalar
    * @return a new `Scalar` instance representing the scaled value
    */
  def scale(r: Rational): Scalar = *(r)

  /**
    * Provides an approximation of this number, if applicable.
    *
    * This method attempts to compute an approximate representation of the number
    * in the form of a `Real`, which encapsulates uncertainty or imprecision
    * in its value. If no meaningful approximation is possible for the number, it
    * returns `None`.
    *
    * @return an `Option[Real]` containing the approximate representation
    *         of this `Number`, or `None` if no approximation is available.
    */
  def approximation: Option[Real] = convert(Real.zero)

}

/**
  * The `Angle` companion object contains utility methods, predefined constants, and
  * typeclass instances for working with angles. Angles are represented using
  * rational numbers and comply with the algebraic structure of a commutative group.
  */
object Angle {
  val r180 = Rational(180)

  /**
    * Converts a `Value` into an `Angle` instance based on its structure.
    *
    * @param value the input `Value` which may represent different numeric types;
    *              `Right(x)` is treated as a whole number,
    *              `Left(Right(x))` is treated as a rational number,
    *              `Left(Left(Some(x)))` is treated as a real number,
    *              and `Left(Left(None))` represents an invalid or undefined angle.
    * @return an `Angle` instance corresponding to the structure of the input `Value`;
    *         returns `Angle.nan` if the input represents an undefined value.
    */
  def apply(value: Value): Angle = value match {
    case Right(x) =>
      new Angle(WholeNumber(x))
    case Left(Right(x)) =>
      new Angle(RationalNumber(x))
    case Left(Left(Some(x))) =>
      new Angle(Real(x))
    case Left(Left(None)) =>
      Angle.nan
  }

  /**
    * Converts the given integer value to an `Angle` instance.
    *
    * @param x the input integer value to be converted into an angle
    * @return an `Angle` instance corresponding to the given integer value
    */
  def apply(x: Int): Angle =
    apply(Value.fromInt(x))

  /**
    * Converts the given long value to an `Angle` instance.
    *
    * @param x the input long value to be converted into an angle
    * @return an `Angle` instance corresponding to the given long value
    */
  def apply(x: Long): Angle =
    apply(Rational(x))

  /**
    * Converts the given rational value into an `Angle`.
    *
    * @param rational the `Rational` value representing the input to be converted into an `Angle`
    * @return an `Angle` instance corresponding to the given `Rational` value
    */
  def apply(rational: Rational): Angle =
    apply(Value.fromRational(rational))

  /**
    * Converts the given rational number to an `Angle` instance by performing modulation and necessary computations.
    * CONSIDER whether we really need this method.
    * The `Angle` class already has a `normalize` method that performs modulation and other necessary computations.
    *
    * @param r the input `RationalNumber` representing the rational value to be converted into an angle
    * @return an `Angle` instance corresponding to the given rational value
    */
  def apply(r: RationalNumber): Angle = Angle(r.r)

  /**
    * Converts the given whole number into an `Angle` instance.
    *
    * @param r the input `WholeNumber` to be converted into an angle
    * @return an `Angle` instance corresponding to the given whole number
    */
  def apply(r: WholeNumber): Angle = Angle(r.x.toBigInt)

  /**
    * Converts the given numeric value to an `Angle` instance represented in degrees.
    *
    * @param x the numeric input value to be converted into an angle in degrees
    * @return an `Angle` instance corresponding to the given numeric value in degrees
    */
  def degrees(x: Number): Angle =
    Angle(x.scale(r180.invert).asInstanceOf[Number], true) // TODO tidy this up!

  /**
    * Converts a `Value` into an `Angle` represented as degrees.
    * CONSIDER invoking degrees(Number) instead.
    *
    * @param x the input `Value` to be converted to an angle in degrees; the type of value can represent
    *          various numeric structures such as whole numbers, rational numbers, or real numbers.
    * @return an `Angle` instance representing the given `Value` in degrees.
    */
  def degrees(x: Value): Angle =
    Angle(Value.scaleRational(r180.invert)(x)).copy(degrees = true)

  /**
    * Converts the given integer to an `Angle` instance represented in degrees.
    *
    * @param x the integer value to be converted into an angle in degrees
    * @return an `Angle` instance corresponding to the given integer value in degrees
    */
  def degrees(x: Int): Angle = degrees(Value.fromInt(x))

  /**
    * Creates an `Angle` instance based on the input `Monotone` value.
    *
    * Converts the input `Monotone` into an `Angle`, handling various types of numerical structures.
    * If the input is already an `Angle`, or if the type is unsupported, an exception is thrown.
    *
    * @param s the input `Monotone` to be converted into an `Angle`; can represent a whole number,
    *          rational number, or real number. Passing an existing `Angle` or unsupported type
    *          results in an exception.
    * @return an `Angle` instance corresponding to the input `Monotone` value
    * @throws NumberException if the input is already an `Angle` or is of an unsupported type
    */
  def create(s: Monotone): Angle = s match {
    case number: WholeNumber =>
      Angle(number)
    case radians: RationalNumber =>
      Angle(radians)
    case Real(x, f) =>
      Angle(Real(x, f))
    case Angle(r, _) =>
      throw NumberException(s"Angle.create: $r is already an Angle")
    case _ =>
      throw NumberException(s"Angle.create: not supported for $s")
  }

  /**
    * Represents the additive identity for angles.
    *
    * This value denotes an angle of zero radians, serving as the identity element in
    * the group structure of angles. It is constructed using the `Angle` companion object
    * initialized with the additive identity of `RationalNumber`.
    */
  val zero: Angle = Angle(RationalNumber.zero)

  /**
    * Represents an angle equivalent to mathematical π radians.
    *
    * The value `pi` is an instance of the `Angle` class initialized with the
    * `RationalNumber.one`, which corresponds to the rational representation of π
    * in the specific context of the `Angle` implementation.
    */
  val pi: Angle = Angle(RationalNumber.one)

  /**
    * Alias for the `pi` value, representing an angle equivalent to mathematical π radians.
    *
    * This value is a symbolic representation of π radians, reused from the `Angle.pi` value.
    * It is denoted by the Greek mathematical symbol 𝛑 and can be used interchangeably with `pi`.
    */
  val 𝛑: Angle = pi

  /**
    * Represents an angle equivalent to π/2 radians.
    *
    * The value `piBy2` is a constant instance of the `Angle` class initialized using
    * a `RationalNumber` constructed with a value of 1/2. This corresponds
    * to π/2 radians in mathematical terms.
    */
  val piBy2: Angle = Angle(RationalNumber(Rational.half))
  val piBy3: Angle = Angle(RationalNumber(Rational.third))
  val piBy4: Angle = Angle(RationalNumber(Rational(1, 4)))
  val piBy2Times3: Angle = Angle(RationalNumber(Rational(3, 2)))
  val twoPi: Angle = Angle(RationalNumber(Rational.two))
  val negPi: Angle = Angle(RationalNumber(Rational.negOne))
  // CONSIDER using NoScalar instead
  val nan: Angle = Angle(RationalNumber(Rational.NaN))

  /**
    * Provides an implicit `Show` instance for the `Angle` class, enabling conversion
    * of an `Angle` instance to a string representation using its `render` method.
    *
    * This allows the `Angle` class to integrate seamlessly with libraries or frameworks
    * requiring a `Show` typeclass instance for displaying or logging purposes.
    */
  implicit val showAngle: Show[Angle] = Show.show(_.render)

  /**
    * Provides an implicit equality instance for comparing two `Angle` objects.
    * NOTE carefully.
    * We do not consider the degrees flag here because that is concerned only with cosmetics (rendering and parsing).
    *
    * This implicit `Eq` implementation determines equality by comparing
    * the `radians` values of two `Angle` instances.
    */
  implicit val angleEq: Eq[Angle] = Eq.instance {
    (a1, a2) =>
      // NOTE that we should be using the === operator here, but it hasn't been defined yet for for Number.
      a1.normalize.radians == a2.normalize.radians
  }

  /**
    * Provides an implicit implementation of a commutative group for the `Angle` type, supporting
    * group operations such as identity, combination, and inversion.
    *
    * This allows `Angle` objects to adhere to the algebraic structure of a commutative group, where
    * the `combine` operation is associative and commutative, an identity element exists, and
    * each element has an additive inverse.
    */
  implicit object angleIsCommutativeGroup extends AdditiveCommutativeGroup[Angle] {
    /**
      * Provides the identity element for the `Angle` group, representing an angle of zero radians.
      *
      * @return an `Angle` instance with zero radians, acting as the identity element in the group structure.
      */
    def zero: Angle = Angle.zero

    /**
      * Adds two `Angle` instances together and returns the resulting `Angle` after normalization.
      *
      * This method combines the components of two `Angle` objects using their `plus` operation
      * and ensures that the resulting `Angle` is in a normalized form to comply with the
      * characteristics of the `Angle` algebraic structure.
      *
      * @param x the first `Angle` instance to be added
      * @param y the second `Angle` instance to be added
      * @return a new `Angle` instance representing the sum of the two input angles, normalized
      */
    def plus(x: Angle, y: Angle): Angle = (x, y) match {
      case (Angle(x1: CanAdd[Number, Number] @unchecked, _), Angle(x2: CanAdd[Number, Number] @unchecked, _)) =>
        val maybeMonotone = x1 plus x2
        Angle.create(FP.getOrThrow(maybeMonotone, new UnsupportedOperationException("Angle.combine"))).normalize
      case _ =>
        throw new UnsupportedOperationException(s"Angle.combine: $x + $y")
    }

    /**
      * Computes the additive inverse of the given `Angle`.
      *
      * This method negates the input angle, returning an `Angle` instance
      * that represents its additive inverse, relative to `Angle.zero`.
      *
      * @param a the `Angle` instance to be inverted
      * @return a new `Angle` instance representing the additive inverse of the input
      */
    def negate(a: Angle): Angle = a.radians match {
      case WholeNumber(x) =>
        Angle(-x.toBigInt)
      case RationalNumber(r, _) =>
        Angle(RationalNumber(r.negate))
      case Real(x, f) =>
        Angle(Real(-x, f))
    }
  }
}
