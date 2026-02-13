/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.eager

import algebra.CommutativeMonoid
import cats.Show
import cats.implicits.catsSyntaxEq
import cats.kernel.Eq
import com.phasmidsoftware.number.algebra.*
import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.eager.Exponential.ExponentialIsCommutativeMonoid
import com.phasmidsoftware.number.algebra.util.{AlgebraException, FP, LatexRenderer}
import com.phasmidsoftware.number.core.inner
import com.phasmidsoftware.number.core.inner.{Factor, PureNumber, Rational}
import com.phasmidsoftware.number.core.numerical.{Fuzziness, WithFuzziness}
import org.slf4j.{Logger, LoggerFactory}

import scala.reflect.ClassTag
import scala.util.Try

/**
  * Represents an abstract exponential number.
  *
  * For example, for an `Exponential` object whose effective value is `e` (2.718...),
  * then the member: `number` should be 1, that's to say ln(e).
  *
  * The `Exponential` class defines properties and operations related to values
  * that are represented internally as logarithmic values.
  *
  * There are subtypes: NaturalExponential and BinaryExponential.
  * We probably don't need DecimalExponential because we have BigDecimal as an alternative.
  *
  * @constructor Creates a new instance of `Exponential` with the specified value.
  * @param number the numerical value defined as the logarithm of this.
  */
abstract class Exponential(val number: Number) extends Transformed with CanAdd[Exponential, Exponential] {

  /**
    * Returns the base of the exponentiation, viz. `e` for NaturalExponential and `2` for BinaryExponential
    * In general, this value will not be exact (particularly in the case of NaturalExponential).
    *
    * @return the base of type `Number`
    */
  def base: Number

  /**
    * Computes an Exponential instance based on the input number.
    * CONSIDER promoting this into `Transformed`.
    *
    * @param x the number to transform, of type `Number`
    * @return a new `Exponential` instance representing the transformed value
    */
  def unit(x: Number): Exponential

  /**
    * This yields the scale function for this Functional.
    *
    * @return a function to transform the nominal value into the actual value as it would appear in a PureNumber context.
    */
  val scaleFunction: Double => Double = x => math.pow(base.toDouble, x)
  
  /**
    * Represents the derivative function associated with this `Functional` instance.
    * That's to say `d(f(number))` by `d(number)` where `f` is this `Functional`.
    * For a Monotone, the derivative should be positive, however, it is possible
    * that it is not positive for certain types of `Functional`.
    *
    * The `derivativeFunction` provides a mathematical operation that computes the derivative
    * with respect to a given input value. It is typically used to evaluate rates of change
    * or sensitivity in the context of numerical transformations.
    *
    * @return A function that accepts a `Double` value and returns the computed derivative as a `Double`.
    */
  val derivativeFunction: Double => Double = x => math.log(base.toDouble) * scaleFunction(x)

  /**
    * Retrieves an optional fuzziness value associated with this instance.
    *
    * The fuzziness value, if present, provides information about the level of uncertainty
    * or imprecision, modeled as a `Fuzziness[Double]`.
    *
    * @return an `Option` containing the `Fuzziness[Double]` value if defined, or `None` if no fuzziness is specified.
    */
  def fuzz: Option[Fuzziness[Double]] =
    Eager(Eager.eagerToField(number).exp) match {
      case fuzzy: WithFuzziness =>
        fuzzy.fuzz
      case _ =>
        None // CONSIDER should this throw an exception?
    }

  /**
    * Compares this `Exponential` instance with another `Functional` instance.
    *
    * This method compares the `number` values of the current `Exponential` instance
    * and the specified `Exponential` instance using their natural order.
    *
    * NOTE do not promote this into `Transformed` as this applies only to Transformed elements
    * that do not involve another variable.
    *
    * @param that the `Exponential` instance to compare with the current instance
    * @return an integer value:
    *         - a negative value if this `Exponential` is less than `that`
    *         - zero if this `Exponential` is equal to `that`
    *         - a positive value if this `Exponential` is greater than `that`
    */
  def compare(that: Functional): Int =
    number.compare(that.number) * derivativeFunction(number.toDouble).sign.toInt

  /**
    * Normalizes this `Valuable` to its simplest equivalent form.
    * This may change the type (e.g., RationalNumber → WholeNumber, Complex(5,0) → WholeNumber(5)).
    *
    * For Expression types, this will attempt to simplify and materialize if the result is exact.
    * For Eager types, this will reduce to the simplest type representation.
    *
    * @return the simplest `Valuable` representation of this value
    */
  def normalize: Eager = number.normalize match {
    case WholeNumber(0) =>
      WholeNumber.one
    case x if x == number =>
      this
    case x: Number =>
      unit(x)
  }

  /**
    * Converts the current number to a representation of the specified type `T`, if possible.
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
      transformation
    case _ =>
      None
  }

  /**
    * Determines if the current number is equal to zero.
    *
    * @return true if the number is zero, false otherwise
    */
  def isZero: Boolean = false

  /**
    * Determines whether this object represents unity.
    *
    * @return true if the object represents unity, false otherwise
    */
  def isUnity: Boolean = number.isZero

  /**
    * Determines the sign of the scalar value represented by this instance.
    * Returns an integer indicating whether the value is positive, negative, or zero.
    *
    * @return 1 if the value is positive, -1 if the value is negative, and 0 if the value is zero
    * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the value is not a Number.
    */
  def signum: Int = FP.recover(number.compareExact(WholeNumber.zero))(AlgebraException(s"Exponential.signum: logic error: $this"))

  /**
    * Method to determine if this Structure object is exact.
    * For instance, `Number.pi` is exact, although if you converted it into a PureNumber, it would no longer be exact.
    *
    * @return true if this Structure object is exact in the context of No factor, else false.
    */
  def isExact: Boolean = number.isExact

  /**
    * Computes the additive inverse of the current `Exponential` instance.
    *
    * This method negates the current Exponential, returning a new `Exponential` instance
    * with the opposite value, relative to `Exponential.zero`.
    *
    * @return a new `Exponential` instance representing the additive inverse of the current Exponential.
    */
  def unary_- : Exponential =
    throw new UnsupportedOperationException("Exponential.unary_-")

  /**
    * Adds the specified `Exponential` to the current `Exponential` instance.
    *
    * This method combines the current Exponential with the provided Exponential
    * by adding their respective values, returning a new `Exponential` instance
    * representing the sum.
    *
    * @param a the `Exponential` to be added to the current `Exponential`
    * @return a new `Exponential` representing the sum of the current `Exponential` and the specified `Exponential`
    */
  def +(a: Exponential): Exponential =
    ExponentialIsCommutativeMonoid.combine(this, a)

  /**
    * Renders the base of this logarithm as a string representation for presentation,
    * including the prefix "^" as appropriate.
    *
    * @return the rendered string representation of the base
    */
  def renderNumber: String =
    if (number.normalize == WholeNumber.one)
      ""
    else
      s"^${number.normalize.render}"

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
    * Checks for structural equality between this `Eager` instance and the specified `Eager` instance.
    * The method is overridden to provide specific equivalence logic for `NaturalExponential`.
    * For other cases, it delegates to the superclass implementation.
    *
    * @param that the `Eager` instance to compare with this instance
    * @return a `Try[Boolean]` indicating whether the two instances are equivalent
    */
  override def eqv(that: Eager): Try[Boolean] = (this, that) match {
    case (a: NaturalExponential, b: NaturalExponential) =>
      a.exponent.eqv(b.exponent)
    case _ =>
      super.eqv(that)
  }

  /**
    * Compares the current `Exponential` instance with another `Eager` instance using a fuzzy equivalence strategy.
    * This method considers two values to be "fuzzy equivalent" if their difference falls within a specified precision margin.
    * The method handles cases where the `Eager` instance belongs to different subtypes such as `NaturalExponential` or `Structure`,
    * applying transformations and delegating to subtype-specific implementations as needed.
    *
    * @param p    the precision margin within which the two instances are considered equal, of type `Double`
    * @param that the `Eager` instance to compare with the current instance
    * @return a `Try[Boolean]` indicating whether the two instances are fuzzy equivalent
    */
  override def fuzzyEqv(p: Double)(that: Eager): Try[Boolean] = (this, that) match {
    case (a: NaturalExponential, b: NaturalExponential) =>
      a.exponent.fuzzyEqv(p)(b.exponent)
    case (a: NaturalExponential, b: Structure) =>
      for {
        aReal <- FP.recoverAsTry(a.transformation[Real])(AlgebraException(s"Cannot transform $a to Real"))
        result <- aReal.fuzzyEqv(p)(b)
      } yield result
    case _ =>
      super.fuzzyEqv(p)(that)
  }
}

/**
  * A case class representing a Exponential. This class implements the `Additive` and `Radians` traits,
  * allowing operations such as addition, subtraction, and various type conversions.
  *
  * Exponential does not support ordering or comparison.
  *
  * An example of the use of this class is as in this definition of Euler's number:
  * {{{
  *   val e: Eager = NaturalExponential(WholeNumber.one)
  * }}}
  *
  * @param x the value of the Exponential of this instance.
  */
case class NaturalExponential(exponent: Number)(val maybeName: Option[String] = None) extends Exponential(exponent) {

  /**
    * Returns the base of this exponential, viz. `e`.
    *
    * @return the base of type `Number`
    */
  val base: Number = Real(math.E)

  /**
    * Represents the additive identity element for the type `T`.
    *
    * The additive identity, commonly referred to as "zero," is the element in an
    * additive algebraic structure that, when added to any element of the structure,
    * results in the same element. For any element `value`, `value + zero` and `zero + value` should
    * equal `value`.
    */
  lazy val zero: Exponential = NaturalExponential(WholeNumber.zero)(Some("1"))

  /**
    * Defines a transformation that transforms a `Monotone` instance into a corresponding `Scalar` value.
    *
    * The transformation defines how a `Monotone` is interpreted or converted in the context of `Scalar`.
    *
    * @return a transformation that maps a `Monotone` object to a `Scalar` result
    * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the input is not a Real number.
    */
  def transformation[T: ClassTag]: Option[T] = {
    val c = implicitly[ClassTag[T]]
    if (c.runtimeClass == classOf[Real]) {
      val result: Real =
        exponent match {
          case WholeNumber.one | RationalNumber(Rational.one, _) =>
            Real(math.E)
          case RationalNumber(Rational.infinity, _) =>
            Real.∞
          case Real(value, fuzz) =>
            Real(math.log(value), fuzz) // TODO check this.
          case _ =>
            throw AlgebraException(s"NaturalExponential.transformation: $exponent not supported")
        }
      Some(result.asInstanceOf[T])
    }
    else
      None
  }

  /**
    * Returns a new instance of `Monotone` that is the negation of the current instance.
    * CONSIDER sorting out the use of CanNegate so that we can extend that for Monotone.
    *
    * @return a `Monotone` representing the negation of this instance
    * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the input is not a Real number.
    */
  def negate: Monotone = throw AlgebraException(s"NaturalExponential.negate: not supported")

  /**
    * Compares the current `Exponential` instance with another `Number` to determine their exact order.
    *
    * If the provided `Number` is a `Exponential`, this method compares their underlying radian values.
    * If the provided `Number` is not a `Exponential`, the comparison cannot be performed, and `None` is returned.
    *
    * @param that the `Number` instance to compare with the current `Exponential` instance
    * @return an `Option[Int]`, where `Some(-1)` indicates that the current `Exponential` is less than the provided `Exponential`,
    *         `Some(0)` indicates that both Logarithms are equal, `Some(1)` indicates that the current `Exponential` is greater,
    *         and `None` is returned if the comparison cannot be made
    */
  def compareExact(that: Number): Option[Int] = that match {
    case NaturalExponential(r) =>
      Some(exponent.compare(r))
    case _ =>
      None
  }

  /**
    * Constructs a natural logarithm (`NaturalExponential`) from the given `Number`.
    *
    * This method creates a `Exponential` instance by interpreting
    * the input `Number` as the argument for the natural logarithm representation.
    *
    * @param x the `Number` value to be used in generating the `Exponential`
    * @return a `Exponential` instance representing the natural logarithm of the input `Number`
    */
  def unit(x: Number): Exponential = NaturalExponential(x)

  /**
    * Renders this `Exponential` instance as a string representation of value in terms of π.
    *
    * The method formats the radius equivalent to π, omitting the numeric coefficient if it is 1.
    *
    * @return a string representation of the `Exponential` in terms of π
    */
  def render: String = maybeName getOrElse {
    val numberStr = exponent.render
    "e" + (if (numberStr == "1") "" else s"^$numberStr")
  }

  /**
    * Computes the potential factor associated with this instance.
    *
    * @return an `Option` containing a `Factor` if available, otherwise `None`
    */
  def maybeFactor(context: Context): Option[Factor] =
    Option.when(context.factorQualifies(inner.NatLog))(inner.NatLog)
}

case class BinaryExponential(exponent: Number)(val maybeName: Option[String] = None) extends Exponential(exponent) {

  /**
    * Returns the base of this logarithm, viz. `e`.
    *
    * @return the base of type `Number`
    */
  val base: Number = WholeNumber.two

  /**
    * Represents the additive identity element for the type `T`.
    *
    * The additive identity, commonly referred to as "zero," is the element in an
    * additive algebraic structure that, when added to any element of the structure,
    * results in the same element. For any element `value`, `value + zero` and `zero + value` should
    * equal `value`.
    */
  lazy val zero: Exponential = NaturalExponential(WholeNumber.zero)

  /**
    * Defines a transformation that transforms a `Monotone` instance into a corresponding `Scalar` value.
    *
    * The transformation defines how a `Monotone` is interpreted or converted in the context of `Scalar`.
    *
    * @return a transformation that maps a `Monotone` object to a `Scalar` result
    * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the input is not a Real number.
    */
  def transformation[T: ClassTag]: Option[T] = {
    val c = implicitly[ClassTag[T]]
    if (c.runtimeClass == classOf[Real]) {
      val result: Real =
        exponent match {
          case WholeNumber.one | RationalNumber(Rational.one, _) =>
            FP.recover(base.convert(Real.zero))(AlgebraException(": BinaryExponential.transformation: base.convert(Real.zero) failed"))
          case RationalNumber(Rational.infinity, _) =>
            Real.∞
          case Real(value, fuzz) =>
            Real(math.log(value), fuzz)
          case _ =>
            throw AlgebraException(s"BinaryExponential.transformation: $exponent not supported")
        }
      Some(result.asInstanceOf[T])
    }
    else
      None
  }

  /**
    * Returns a new instance of `Monotone` that is the negation of the current instance.
    * CONSIDER sorting out the use of CanNegate so that we can extend that for Monotone.
    * TODO sort this out.
    *
    * @return a `Monotone` representing the negation of this instance
    */
  def negate: Monotone = throw AlgebraException(s"BinaryExponential.negate: not supported")

  /**
    * Compares the current `Exponential` instance with another `Number` to determine their exact order.
    *
    * If the provided `Number` is a `Exponential`, this method compares their underlying radian values.
    * If the provided `Number` is not a `Exponential`, the comparison cannot be performed, and `None` is returned.
    *
    * @param that the `Number` instance to compare with the current `Exponential` instance
    * @return an `Option[Int]`, where `Some(-1)` indicates that the current `Exponential` is less than the provided `Exponential`,
    *         `Some(0)` indicates that both Logarithms are equal, `Some(1)` indicates that the current `Exponential` is greater,
    *         and `None` is returned if the comparison cannot be made
    */
  def compareExact(that: Number): Option[Int] = that match {
    case BinaryExponential(r) =>
      Some(exponent.compare(r))
    case _ =>
      None
  }

  /**
    * Constructs a natural logarithm (`NaturalExponential`) from the given `Number`.
    *
    * This method creates a `Exponential` instance by interpreting
    * the input `Number` as the argument for the natural logarithm representation.
    *
    * @param x the `Number` value to be used in generating the `Exponential`
    * @return a `Exponential` instance representing the natural logarithm of the input `Number`
    */
  def unit(x: Number): Exponential = BinaryExponential(x)

  /**
    * Renders this `Exponential` instance as a string representation of value in terms of π.
    *
    * The method formats the radius equivalent to π, omitting the numeric coefficient if it is 1.
    *
    * @return a string representation of the `Exponential` in terms of π
    */
  def render: String = maybeName getOrElse {
    val numberStr = exponent.render
    "2" + (if (numberStr == "1") "" else s"^$numberStr")
  }

  /**
    * Computes the potential factor associated with this instance.
    *
    * @return an `Option` containing a `Factor` if available, otherwise `None`
    */
  def maybeFactor(context: Context): Option[Factor] =
    Some(PureNumber)
}

/**
  * Represents a binary exponential number structure, which provides
  * specialized operations and transformations for exponential numbers.
  */
object BinaryExponential {
  /**
    * Creates an instance of Exponential based on the given number input.
    *
    * @param x the input number used to initialize the exponential value
    * @return a new Exponential instance constructed using the provided input
    */
  def apply(x: Number): Exponential = new BinaryExponential(x)()
}
/**
  * The `Exponential` companion object contains utility methods, predefined constants, and
  * typeclass instances for working with Logarithms. Logarithms are represented using
  * rational numbers and comply with the algebraic structure of a commutative group.
  */
object Exponential {

  import org.slf4j.{Logger, LoggerFactory}

  import scala.util.Try

  val logger: Logger = LoggerFactory.getLogger(getClass)

  given DyadicOperator[Exponential] = new DyadicOperator[Exponential] {
    def op[B <: Exponential, Z](f: (Exponential, B) => Try[Z])(x: Exponential, y: B): Try[Z] = (x, y) match {
      case (a: NaturalExponential, b: NaturalExponential) =>
        implicitly[DyadicOperator[NaturalExponential]].op(f)(a, b)
      case (a, b) =>
        f(a, b)
    }
  }

  given Eq[Exponential] = Eq.instance {
    (x, y) =>
      summon[DyadicOperator[Exponential]].op((x: Eager, y: Eager) => x.eqv(y))(x, y).getOrElse(false)
  }

  given FuzzyEq[Exponential] = FuzzyEq.instance {
    (x, y, p) =>
      x === y || x.fuzzyEqv(p)(y).getOrElse(false)
  }

  /**
    * Provides an implicit `Show` instance for the `Exponential` class, enabling conversion
    * of a `Exponential` instance to a string representation using its `render` method.
    *
    * This allows the `Exponential` class to integrate seamlessly with libraries or frameworks
    * requiring a `Show` typeclass instance for displaying or logging purposes.
    */
  implicit val showLogarithm: Show[Exponential] = Show.show(_.render)

  // In Exponential companion object
  implicit val latexRenderer: LatexRenderer[Exponential] = LatexRenderer.instance {
    case log: NaturalExponential => s"\\mathrm{e}${log.renderNumber}"
    case log => s"${log.base.render}${log.renderNumber}"
  }

  /**
    * Provides an implicit implementation of a commutative group for the `Exponential` type, supporting
    * group operations such as identity, combination, and inversion.
    *
    * This allows `Exponential` objects to adhere to the algebraic structure of a commutative group, where
    * the `combine` operation is associative and commutative, an identity element exists, and
    * each element has an additive inverse.
    */
  implicit object ExponentialIsCommutativeMonoid extends CommutativeMonoid[Exponential] {
    /**
      * Provides the identity element for the `Exponential` group, representing a Exponential of zero value.
      *
      * @return a `Exponential` instance with zero value, acting as the identity element in the group structure.
      */
    lazy val empty: Exponential = NaturalExponential(WholeNumber.zero)

    /**
      * Combines two `Exponential` instances by adding their respective value.
      * NOTE that this is the equivalent of multiplying the corresponding (exponential) values.
      * This method is associative and commutative, meaning that the order of the arguments does not matter.
      * The identity element is the zero value, which is the neutral element for the addition operation.
      *
      * @param x the first `Exponential` to combine
      * @param y the second `Exponential` to combine
      * @return a new `Exponential` representing the sum of the value of the two provided `Exponential` instances
      * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the input types are not compatible.
      */
    def combine(x: Exponential, y: Exponential): Exponential = (x, y) match {
      case (a, b) if a.getClass == b.getClass =>
        a + b
      case _ =>
        throw AlgebraException(s"Exponential.combine: $x, $y")
    }
  }
}

/**
  * Companion object for the `NaturalExponential` class, providing predefined constants and utility methods.
  *
  * The object contains common values associated with natural logarithmic operations
  * and other relevant constants that can be used with `Exponential` instances.
  */
object NaturalExponential {

  def apply(x: Number): NaturalExponential = new NaturalExponential(x)()

  val logger: Logger = LoggerFactory.getLogger(getClass)

  given DyadicOperator[NaturalExponential] = new DyadicOperator[NaturalExponential] {
    def op[B <: NaturalExponential, Z](f: (NaturalExponential, B) => Try[Z])(x: NaturalExponential, y: B): Try[Z] = f(x, y)
  }

  given Eq[NaturalExponential] = Eq.instance {
    (x, y) =>
      summon[DyadicOperator[NaturalExponential]].op((x: Eager, y: Eager) => x.eqv(y))(x, y).getOrElse(false)
  }

  given FuzzyEq[NaturalExponential] = FuzzyEq.instance {
    (x, y, p) =>
      x === y || x.fuzzyEqv(p)(y).getOrElse(false)
  }

  given Show[NaturalExponential] = Show.show(_.render)

  /**
    * Represents the zero value of the `Exponential` class.
    *
    * This is a predefined constant that corresponds to a `Exponential` of zero value.
    * It is used as the additive identity in operations involving Logarithms.
    */
  val one: Exponential = NaturalExponential(WholeNumber.zero)(Some("1"))

  /**
    * Represents the natural logarithmic base `e` as a `Exponential` instance.
    *
    * The value corresponds to the natural logarithm of one in terms of base `e`,
    * often used as a mathematical constant in logarithmic and exponential calculations.
    */
  val e: Exponential = NaturalExponential(WholeNumber.one)(Some("e"))
}
