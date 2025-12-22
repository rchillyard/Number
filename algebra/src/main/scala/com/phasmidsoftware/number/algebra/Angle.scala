/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra

import algebra.ring.AdditiveCommutativeGroup
import cats.Show
import cats.implicits.catsSyntaxEq
import cats.kernel.Eq
import com.phasmidsoftware.number.algebra.Angle.{angleIsCommutativeGroup, r180}
import com.phasmidsoftware.number.algebra.Structure
import com.phasmidsoftware.number.algebra.misc.{AlgebraException, DyadicOperator, FP, FuzzyEq}
import com.phasmidsoftware.number.core.inner.{Factor, Radian, Rational, Value}
import com.phasmidsoftware.number.core.numerical.Fuzziness
import scala.reflect.ClassTag
import scala.util.{Success, Try}

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
  * @param number  the value of the angle in radians
  * @param degrees whether the angle is in radians or degrees.
  *                In the latter case, the `number` attribute is unchanged (1 is still half of a full circle).
  */
case class Angle private[algebra](number: Number, degrees: Boolean = false) extends Radians with Circle with Scalable[Angle] with CanAdd[Angle, Angle] with CanNormalize[Angle] {

  /**
    * Normalizes an angle instance to its equivalent value within the standard range of radians.
    *
    * This method processes the `number` value of the current `Angle` and adjusts it
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
  def normalize: Angle = number match {
    case RationalNumber(r, _) =>
      Angle(Radian.modulate(Value.fromRational(r)))
    case Real(value, fuzz) =>
      Angle(Radian.modulate(Value.fromDouble(Some(value)))) // TODO take care of fuzz
    case WholeNumber(x) =>
      Angle(Radian.modulate(Value.fromRational(Rational(x))))
    case _ =>
      throw AlgebraException(s"normalize: unexpected type $number")
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
      Some(number.compare(r))
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
    case x if x.getClass == this.getClass =>
      Some(this.asInstanceOf[T])
    case _: Real =>
      toMaybeReal.asInstanceOf[Option[T]]
    case _ =>
      None
  }

  /**
    * Determines if the current number is equal to zero.
    *
    * @return true if the number is zero, false otherwise
    */
  def isZero: Boolean = number.isZero

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
  override def isExact: Boolean = number.isExact

  /**
    * Retrieves an optional fuzziness value associated with this instance.
    *
    * TODO we should ensure that the fuzz is relative.
    *
    * The fuzziness value, if present, provides information about the level of uncertainty
    * or imprecision, modeled as a `Fuzziness[Double]`.
    *
    * @return an `Option` containing the `Fuzziness[Double]` value if defined, or `None` if no fuzziness is specified.
    */
  def fuzz: Option[Fuzziness[Double]] = number.fuzz

  /**
    * Converts the current instance to a Double representation.
    *
    * @return the Double value corresponding to the current instance
    */
  def asDouble: Double = scaleFactor * number.toDouble

  /**
    * Determines if the `number` value can be interpreted as an instance of `Q`.
    * If `number` matches the type `Q`, it returns an `Option` containing the value of `Q`.
    * Otherwise, it returns `None`.
    *
    * @return an `Option[Q]` where `Some(Q)` is returned if `number` is of type `Q`,
    *         otherwise `None`.
    */
  def maybeQ: Option[Q] = number match {
    case q: Q => Some(q)
    case _ => None
  }

  /**
    * Renders this `Angle` instance as a string representation of radians in terms of π.
    *
    * The method formats the radius equivalent to π, omitting the numeric coefficient if it is 1.
    *
    * @return a string representation of the `Angle` in terms of π
    */
  def render: String = {
    val value = normalize.number
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
    * by adding their respective number, returning a new `Angle` instance
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
    * CONSIDER eliminating this method.
    *
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
  def *(factor: Rational): Angle = number match {
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

  /**
    * Compares two instances of `Eager` for equality.
    *
    * This method is intended to check if the provided instances, `x` and `y`,
    * are equivalent. Currently, this functionality is not implemented
    * and will return a failure with an appropriate exception message.
    *
    * @param that the first `Eager` instance to compare
    * @param y the second `Eager` instance to compare
    * @return a `Try[Boolean]` where:
    *         - `Success(true)` indicates the objects are equivalent
    *         - `Success(false)` indicates the objects are not equivalent
    *         - `Failure` indicates this functionality is not implemented
    */
  override def eqv(that: Eager): Try[Boolean] = (this, that) match {
    case (a1: Angle, a2: Angle) =>
      Success(a1.normalize.number === a2.normalize.number)
    case _ =>
      super.eqv(that)
  }

  /**
    * Converts the current instance to an optional scaled representation as a `Real`.
    * TODO normalize before conversion
    *
    * This method approximates the `radians` value of the instance with forced scaling
    * by π and returns the result wrapped in an `Option`. If the approximation is not
    * possible, the method returns `None`.
    *
    * @return an `Option[Real]` containing the scaled representation of the current instance,
    *         or `None` if the approximation fails.
    */
  private def toMaybeReal: Option[Real] =
    number.approximation(true).map(x => x.scaleByPi)
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
  def apply(r: WholeNumber): Angle = Angle(r.x)

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
    *
    * @return an `Angle` instance corresponding to the input `Monotone` value
    * @throws AlgebraException if the input is already an `Angle` or is of an unsupported type
    */
  def create(s: Monotone): Angle = s match {
    case number: WholeNumber =>
      Angle(number)
    case radians: RationalNumber =>
      Angle(radians)
    case Real(x, f) =>
      Angle(Real(x, f))
    case Angle(r, _) =>
      throw AlgebraException(s"Angle.create: $r is already an Angle")
    case _ =>
      throw AlgebraException(s"Angle.create: not supported for $s")
  }

  /**
    * Provides an implicit conversion instance for transforming an `Angle` type into another `Angle` type.
    *
    * This given instance conforms to the `Convertible` typeclass, offering a no-op conversion logic
    * where the input `Angle` instance is returned as is without any transformation.
    *
    * @group Typeclass Instances
    * @see Convertible
    * @param witness a template `Angle` object, which is unused in this conversion
    * @param u       the source `Angle` object that is to be returned as is
    * @return the source `Angle` object `u`, effectively performing an identity conversion
    */
  given Convertible[Angle, Angle] with
    def convert(witness: Angle, u: Angle): Angle = u

  /**
    * Provides a `Convertible` instance to allow transformations from `Real` to `Angle`.
    *
    * This implementation leverages the `convert` function to create a new `Angle` instance
    * using a `Real` value. The conversion relies on the `asDouble` method of the `Real` type
    * to extract its numeric representation before constructing the `Angle`.
    *
    * @param witness a value of type `Angle` provided as a witness or template, although not
    *                directly used in this implementation for the conversion process
    *
    * @param u       the input `Real` value to be converted into an `Angle` instance
    * @return an `Angle` instance created from the numeric representation of the `Real` value
    */
  given Convertible[Angle, Real] with
    def convert(witness: Angle, u: Real): Angle = Angle(u.asDouble) // FIXME need the inverse operation

  /**
    * Provides a `DyadicOperator` instance for the type `Angle`.
    *
    * This implementation defines a binary operation (`op`) that operates on two angles 
    * and applies a user-defined function `f` to the inputs. The resulting computation 
    * is encapsulated in a `Try` to handle potential failures.
    *
    * @return A `DyadicOperator[Angle]` instance that allows performing binary operations 
    *         on `Angle` values using a function `f`.
    */
  given DyadicOperator[Angle] = new DyadicOperator[Angle] {
    def op[B <: Angle, Z](f: (Angle, B) => Try[Z])(x: Angle, y: B): Try[Z] = f(x, y)
  }

  /**
    * Provides an instance of equality comparison (`Eq`) for the `Angle` type.
    *
    * Compares two `Angle` instances for equivalence based on their internal
    * equality logic. The method delegates to the `eqv` method of one `Angle`
    * instance and processes the result. If the equivalence check returns
    * `None`, the method defaults to `false`.
    *
    * @return an `Eq[Angle]` instance that defines equality behavior for `Angle`
    */
  given Eq[Angle] = Eq.instance {
    (x, y) => x.eqv(y).getOrElse(false)
  }

  /**
    * Provides an instance of `FuzzyEq[Angle]` that defines fuzzy equality for angles.
    *
    * Fuzzy equality is determined by checking if two angles are exactly equal (`===`) 
    * or approximately equal based on a given probability `p`.
    *
    * @return an instance of `FuzzyEq[Angle]` that implements fuzzy equality logic 
    *         for comparing `Angle` values.
    */
  given FuzzyEq[Angle] = FuzzyEq.instance {
    (x, y, p) =>
      x === y || x.fuzzyEqv(p)(y).getOrElse(false)
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
  private val nan: Angle = Angle(RationalNumber(Rational.NaN))

  /**
    * Provides an implicit `Show` instance for the `Angle` class, enabling conversion
    * of an `Angle` instance to a string representation using its `render` method.
    *
    * This allows the `Angle` class to integrate seamlessly with libraries or frameworks
    * requiring a `Show` typeclass instance for displaying or logging purposes.
    */
  implicit val showAngle: Show[Angle] = Show.show(_.render)

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
    def negate(a: Angle): Angle = a.number match {
      case WholeNumber(x) =>
        Angle(-x)
      case RationalNumber(r, _) =>
        Angle(RationalNumber(r.negate))
      case Real(x, f) =>
        Angle(Real(-x, f))
    }
  }
}

/**
  * The `Radians` trait represents a scalar quantity expressed in radians, a unit of angular measure.
  * It extends the `Scalar` trait, inheriting its properties and behaviors for numerical operations
  * and comparison, while specifically associating the scalar with a conversion factor defined by Pi.
  */
trait Radians extends Scalar with Functional {

  /**
    * Retrieves an optional factor associated with the specified context.
    * The factor may represent a transformation or scaling associated with a computation
    * in a particular evaluation context.
    *
    * @param context the evaluation context within which the factor is to be determined.
    * @return an `Option` containing the factor if it exists; `None` otherwise.
    */
  def maybeFactor(context: Context): Option[Factor] =
    Option.when(context.factorQualifies(Radian))(Radian)

  /**
    * Represents the scalar value for converting radians to a pure number, using Pi as the scaling factor.
    */
  val scaleFactor: Double = math.Pi // TODO change this to be an exact number (not a Double)
}

/**
  * The `Radians` object provides utilities and type class instances for the `Radians` type.
  * It defines operators and equality mechanisms to enable operations and comparisons for
  * instances of `Radians` and its related subtypes.
  */
object Radians {

  import org.slf4j.{Logger, LoggerFactory}
  import scala.util.Try

  val logger: Logger = LoggerFactory.getLogger(getClass)

  /**
    * Provides a `DyadicOperator` instance for the `Radians` type, enabling binary operations
    * on operands of type `Radians` or its subtypes.
    *
    * This instance delegates the operation logic to the `DyadicOperator` of the `Angle` type
    * if both operands are of type `Angle`. Otherwise, it applies the provided operation function directly
    * to the operands.
    *
    * @return A `DyadicOperator[Radians]` that supports performing operations on `Radians` instances
    *         and optionally delegates to the `Angle` operator when applicable.
    */
  given DyadicOperator[Radians] = new DyadicOperator[Radians] {
    def op[B <: Radians, Z](f: (Radians, B) => Try[Z])(x: Radians, y: B): Try[Z] = (x, y) match {
      case (a: Angle, b: Angle) =>
        implicitly[DyadicOperator[Angle]].op(f)(a, b)
      case (a, b) =>
        f(a, b)
    }
  }

  /**
    * Provides an instance of the `Eq` type class for `Radians`, enabling equality comparison
    * between two instances of `Radians`. The comparison is implemented using a dyadic operation,
    * which delegates to the `DyadicOperator` for `Radians`. The equality check may involve evaluating
    * the operands in an eager context and comparing their values.
    *
    * @return An instance of the `Eq` type class for `Radians`, where equality is determined
    *         based on the dyadic operation and the result of the comparison.
    */
  given Eq[Radians] = Eq.instance {
    (x, y) =>
      summon[DyadicOperator[Radians]].op((x: Eager, y: Eager) => x.eqv(y))(x, y).getOrElse(false)
  }

  /**
    * Provides a given `FuzzyEq` instance for the `Radians` type.
    *
    * This instance defines fuzzy equality for `Radians` by checking strict equality first,
    * and if that fails, by using the `fuzzyEqv` method with the given threshold `p`.
    * The logic ensures reliable approximate comparisons for `Radians` values.
    *
    * @return A `FuzzyEq[Radians]` instance enabling approximate equality checks for `Radians`.
    */
  given FuzzyEq[Radians] = FuzzyEq.instance {
    (x, y, p) =>
      x === y || x.fuzzyEqv(p)(y).getOrElse(false)
  }
}
