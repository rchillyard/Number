/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra

import algebra.CommutativeMonoid
import cats.Show
import cats.implicits.catsSyntaxEq
import cats.kernel.Eq
import com.phasmidsoftware.number.algebra.Logarithm.LogarithmIsCommutativeMonoid
import com.phasmidsoftware.number.algebra.Structure
import com.phasmidsoftware.number.algebra.core.{AlgebraException, DyadicOperator, FP, FuzzyEq}
import com.phasmidsoftware.number.core.inner
import com.phasmidsoftware.number.core.inner.{Factor, PureNumber, Rational}
import com.phasmidsoftware.number.core.numerical.{Fuzziness, WithFuzziness}
import org.slf4j.{Logger, LoggerFactory}
import scala.reflect.ClassTag
import scala.util.Try

/**
  * Represents an abstract logarithmic structure.
  *
  * The `Logarithm` class defines properties and operations related to logarithmic
  * values. This class serves as a base class for implementing logarithmic computations,
  * adhering to specific mathematical principles such as addition, scaling, and rendering.
  * CONSIDER renaming this as Exp
  *
  * @constructor Creates a new instance of `Logarithm` with the specified value.
  * @param number the numerical value representing the logarithm
  */
abstract class Logarithm(val number: Number) extends Transformed with CanAdd[Logarithm, Logarithm] with Ordered[Logarithm] {

  /**
    * Returns the base of this logarithm.
    *
    * @return the base of type `Number`
    */
  def base: Number

  /**
    * Normalizes this `Valuable` to its simplest equivalent form.
    * This may change the type (e.g., RationalNumber → WholeNumber, Complex(5,0) → WholeNumber(5)).
    *
    * For Expression types, this will attempt to simplify and materialize if the result is exact.
    * For Eager types, this will reduce to the simplest type representation.
    *
    * @return the simplest `Valuable` representation of this value
    */
  def normalize: Valuable = number.normalize match {
    case WholeNumber(0) =>
      WholeNumber.one
    case x if x == number =>
      this
    case x: Number =>
      unit(x)
    case x =>
      this
  }

  def unit(x: Number): Logarithm

  /**
    * Compares this `Logarithm` instance with another `Logarithm` instance.
    *
    * This method compares the `value` values of the current `Logarithm` instance
    * and the specified `Logarithm` instance using their natural order.
    *
    * @param that the `Logarithm` instance to compare with the current instance
    * @return an integer value:
    *         - a negative value if this `Logarithm` is less than `that`
    *         - zero if this `Logarithm` is equal to `that`
    *         - a positive value if this `Logarithm` is greater than `that`
    */
  def compare(that: Logarithm): Int =
    number.compare(that.number)

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
  def isZero: Boolean = number.isZero

  /**
    * Determines the sign of the scalar value represented by this instance.
    * Returns an integer indicating whether the value is positive, negative, or zero.
    *
    * @return 1 if the value is positive, -1 if the value is negative, and 0 if the value is zero
    */
  def signum: Int = FP.recover(number.compareExact(WholeNumber.zero))(AlgebraException(s"Logarithm.signum: logic error: $this"))

  /**
    * Method to determine if this Structure object is exact.
    * For instance, `Number.pi` is exact, although if you converted it into a PureNumber, it would no longer be exact.
    *
    * @return true if this Structure object is exact in the context of No factor, else false.
    */
  override def isExact: Boolean = number.isExact

  /**
    * If this `Valuable` is exact, it returns the exact value as a `Double`.
    * Otherwise, it returns `None`.
    * NOTE: do NOT implement this method to return a Double for a fuzzy Real--only for exact numbers.
    *
    * @return Some(value) where value is a Double if this is exact, else None.
    */
//  def maybeDouble: Option[Double] =
//    FP.whenever(isExact)(convert(Real.zero) map (_.value))

  /**
    * Computes the additive inverse of the current `Logarithm` instance.
    *
    * This method negates the current Logarithm, returning a new `Logarithm` instance
    * with the opposite value, relative to `Logarithm.zero`.
    *
    * @return a new `Logarithm` instance representing the additive inverse of the current Logarithm.
    */
  def unary_- : Logarithm =
    throw new UnsupportedOperationException("Logarithm.unary_-")

  /**
    * Adds the specified `Logarithm` to the current `Logarithm` instance.
    *
    * This method combines the current Logarithm with the provided Logarithm
    * by adding their respective values, returning a new `Logarithm` instance
    * representing the sum.
    *
    * @param a the `Logarithm` to be added to the current `Logarithm`
    * @return a new `Logarithm` representing the sum of the current `Logarithm` and the specified `Logarithm`
    */
  def +(a: Logarithm): Logarithm =
    LogarithmIsCommutativeMonoid.combine(this, a)


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

  override def eqv(that: Eager): Try[Boolean] = (this, that) match {
    case (a: NatLog, b: NatLog) =>
      a.x.eqv(b.x)
    case _ =>
      super.eqv(that)
  }

  // In Logarithm class
  override def fuzzyEqv(p: Double)(that: Eager): Try[Boolean] = (this, that) match {
    case (a: NatLog, b: NatLog) =>
      a.x.fuzzyEqv(p)(b.x)
    case (a: NatLog, b: Structure) =>
      // Transform NatLog to Real and compare
      for {
        aReal <- FP.recoverAsTry(a.transformation[Real])(AlgebraException(s"Cannot transform $a to Real"))
        result <- aReal.fuzzyEqv(p)(b)
      } yield result
    case _ =>
      super.fuzzyEqv(p)(that)
  }
}

/**
  * A case class representing a Logarithm. This class implements the `Additive` and `Radians` traits,
  * allowing operations such as addition, subtraction, and various type conversions.
  *
  * Logarithm does not support ordering or comparison.
  *
  * An example of the use of this class is as in this definition of Euler's number:
  * {{{
  *   val e: Eager = NatLog(WholeNumber.one)
  * }}}
  *
  * @param x the value of the Logarithm of this instance.
  */
case class NatLog(x: Number)(val maybeName: Option[String] = None) extends Logarithm(x) {
//  require(x.signum>0, s"NatLog: $x is not positive")

  /**
    * Assigns a specified name to the `Eager` instance and returns the updated instance.
    *
    * @param name the name to assign to this `Eager` instance
    * @return the updated `Eager` instance with the specified name
    */
  def named(name: String): Eager = copy()(Some(name))

  /**
    * Returns the base of this logarithm, viz. `e`.
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
  lazy val zero: Logarithm = NatLog(WholeNumber.zero)(Some("1"))

  /**
    * Defines a transformation that transforms a `Monotone` instance into a corresponding `Scalar` value.
    *
    * The transformation defines how a `Monotone` is interpreted or converted in the context of `Scalar`.
    *
    * @return a transformation that maps a `Monotone` object to a `Scalar` result
    */
  def transformation[T: ClassTag]: Option[T] = {
    val c = implicitly[ClassTag[T]]
    if (c.runtimeClass == classOf[Real]) {
      val result: Real =
        x match {
          case WholeNumber.one | RationalNumber(Rational.one, _) =>
            Real(math.E)
          case RationalNumber(Rational.infinity, _) =>
            Real.∞
          case Real(value, fuzz) =>
            Real(math.log(value), fuzz)
          case _ =>
            throw AlgebraException(s"NatLog.transformation: $x not supported")
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
    */
  def negate: Monotone = throw AlgebraException(s"NatLog.negate: not supported")

  /**
    * Compares the current `Logarithm` instance with another `Number` to determine their exact order.
    *
    * If the provided `Number` is a `Logarithm`, this method compares their underlying radian values.
    * If the provided `Number` is not a `Logarithm`, the comparison cannot be performed, and `None` is returned.
    *
    * @param that the `Number` instance to compare with the current `Logarithm` instance
    * @return an `Option[Int]`, where `Some(-1)` indicates that the current `Logarithm` is less than the provided `Logarithm`,
    *         `Some(0)` indicates that both Logarithms are equal, `Some(1)` indicates that the current `Logarithm` is greater,
    *         and `None` is returned if the comparison cannot be made
    */
  def compareExact(that: Number): Option[Int] = that match {
    case NatLog(r) =>
      Some(x.compare(r))
    case _ =>
      None
  }

  /**
    * Constructs a natural logarithm (`NatLog`) from the given `Number`.
    *
    * This method creates a `Logarithm` instance by interpreting
    * the input `Number` as the argument for the natural logarithm representation.
    *
    * @param x the `Number` value to be used in generating the `Logarithm`
    * @return a `Logarithm` instance representing the natural logarithm of the input `Number`
    */
  def unit(x: Number): Logarithm = NatLog(x)

  /**
    * Retrieves an optional fuzziness value associated with this instance.
    *
    * The fuzziness value, if present, provides information about the level of uncertainty
    * or imprecision, modeled as a `Fuzziness[Double]`.
    *
    * @return an `Option` containing the `Fuzziness[Double]` value if defined, or `None` if no fuzziness is specified.
    */
  def fuzz: Option[Fuzziness[Double]] =
    Eager(Valuable.valuableToField(x).exp) match {
      case fuzzy: WithFuzziness =>
        fuzzy.fuzz
      case _ =>
        None // CONSIDER should this throw an exception?
    }

  /**
    * Renders this `Logarithm` instance as a string representation of value in terms of π.
    *
    * The method formats the radius equivalent to π, omitting the numeric coefficient if it is 1.
    *
    * @return a string representation of the `Logarithm` in terms of π
    */
  def render: String = maybeName getOrElse {
    val numberStr = x.render
    "e" + (if (numberStr == "1") "" else s"^$numberStr")
  }

  /**
    * Computes the potential factor associated with this instance.
    *
    * @return an `Option` containing a `Factor` if available, otherwise `None`
    */
  def maybeFactor(context: Context): Option[Factor] =
    Option.when(context.factorQualifies(inner.NatLog))(inner.NatLog)

  /**
    * Returns the multiplicative identity element of type `T` in the context
    * of a structure that supports multiplication.
    *
    * @return the instance of type `T` that acts as the identity element for multiplication
    */
  def one: Logarithm = NatLog(WholeNumber.zero)(Some("1"))

  /**
    * Scales the current instance of type `T` by the specified `Double` value.
    *
    * This method applies a scaling factor to the instance, returning an `Option`
    * that contains the scaled instance if the operation is valid. If the scaling
    * operation is not valid or feasible, `None` is returned.
    *
    * @param that the `Double` value to scale the instance by
    * @return an `Option` containing the scaled instance of type `T`, or `None`
    *         if scaling is not possible
    */
  def doScaleDouble(that: Double): Option[Monotone] =
    None // TODO implement me as appropriate

//  override def fuzzyEqv(p: Double)(that: Eager): Try[Boolean] = (this, that) match {
//    case (a: NatLog, b: Structure) =>
//      for {
//        z <- FP.recoverAsTry(a.transformation[Real])(AlgebraException(s"NatLog.fuzzyEqv: logic transformation error $this $that"))
//        q <- FP.recoverAsTry(b.convert(Real.zero))(AlgebraException(s"NatLog.fuzzyEqv: logic conversion error $this $that"))
//        w <- z.fuzzyEqv(p)(z)
//      } yield w
//    case (a: Real, b: Structure) =>
//      for {
//        q <- FP.recoverAsTry(b.convert(Real.zero))(AlgebraException(s"NatLog.fuzzyEqv: logic conversion error $this $that"))
//        w <- a.fuzzyEqv(p)(a)
//      } yield w
//
//    case (a, b) =>
//      Failure(AlgebraException(s"NatLog.fuzzyEqv: logic conversion error $this $that"))
//  }
}

case class BinaryLog(x: Number)(val maybeName: Option[String] = None) extends Logarithm(x) {

//  require(x.signum>0, s"BinaryLog: $x is not positive")

  /**
    * Assigns a specified name to the `Eager` instance and returns the updated instance.
    *
    * @param name the name to assign to this `Eager` instance
    * @return the updated `Eager` instance with the specified name
    */
  def named(name: String): Eager = copy()(Some(name))

  /**
    * Returns the base of this logarithm, viz. `e`.
    *
    * @return the base of type `Number`
    */
  val base: Real = Real(2, None)

  /**
    * Represents the additive identity element for the type `T`.
    *
    * The additive identity, commonly referred to as "zero," is the element in an
    * additive algebraic structure that, when added to any element of the structure,
    * results in the same element. For any element `value`, `value + zero` and `zero + value` should
    * equal `value`.
    */
  lazy val zero: Logarithm = NatLog(WholeNumber.zero)

  /**
    * Defines a transformation that transforms a `Monotone` instance into a corresponding `Scalar` value.
    *
    * The transformation defines how a `Monotone` is interpreted or converted in the context of `Scalar`.
    *
    * @return a transformation that maps a `Monotone` object to a `Scalar` result
    */
  def transformation[T: ClassTag]: Option[T] = {
    val c = implicitly[ClassTag[T]]
    if (c.runtimeClass == classOf[Real]) {
      val result: Real =
        x match {
          case WholeNumber.one | RationalNumber(Rational.one, _) =>
            base
          case RationalNumber(Rational.infinity, _) =>
            Real.∞
          case Real(value, fuzz) =>
            Real(math.log(value), fuzz)
          case _ =>
            throw AlgebraException(s"NatLog.transformation: $x not supported")
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
    */
  def negate: Monotone = throw AlgebraException(s"BinaryLog.negate: not supported")

  /**
    * Compares the current `Logarithm` instance with another `Number` to determine their exact order.
    *
    * If the provided `Number` is a `Logarithm`, this method compares their underlying radian values.
    * If the provided `Number` is not a `Logarithm`, the comparison cannot be performed, and `None` is returned.
    *
    * @param that the `Number` instance to compare with the current `Logarithm` instance
    * @return an `Option[Int]`, where `Some(-1)` indicates that the current `Logarithm` is less than the provided `Logarithm`,
    *         `Some(0)` indicates that both Logarithms are equal, `Some(1)` indicates that the current `Logarithm` is greater,
    *         and `None` is returned if the comparison cannot be made
    */
  def compareExact(that: Number): Option[Int] = that match {
    case BinaryLog(r) =>
      Some(x.compare(r))
    case _ =>
      None
  }

  /**
    * Constructs a natural logarithm (`NatLog`) from the given `Number`.
    *
    * This method creates a `Logarithm` instance by interpreting
    * the input `Number` as the argument for the natural logarithm representation.
    *
    * @param x the `Number` value to be used in generating the `Logarithm`
    * @return a `Logarithm` instance representing the natural logarithm of the input `Number`
    */
  def unit(x: Number): Logarithm = BinaryLog(x)

  /**
    * Retrieves an optional fuzziness value associated with this instance.
    *
    * The fuzziness value, if present, provides information about the level of uncertainty
    * or imprecision, modeled as a `Fuzziness[Double]`.
    *
    * @return an `Option` containing the `Fuzziness[Double]` value if defined, or `None` if no fuzziness is specified.
    */
  def fuzz: Option[Fuzziness[Double]] =
    Eager(Valuable.valuableToField(x).exp) match { // FIXME we need 2^x
      case fuzzy: WithFuzziness =>
        fuzzy.fuzz
      case _ =>
        None // CONSIDER should this throw an exception?
    }

  /**
    * Renders this `Logarithm` instance as a string representation of value in terms of π.
    *
    * The method formats the radius equivalent to π, omitting the numeric coefficient if it is 1.
    *
    * @return a string representation of the `Logarithm` in terms of π
    */
  def render: String = maybeName getOrElse {
    val numberStr = x.render
    "2" + (if (numberStr == "1") "" else s"^$numberStr")
  }

  /**
    * Computes the potential factor associated with this instance.
    *
    * @return an `Option` containing a `Factor` if available, otherwise `None`
    */
  def maybeFactor(context: Context): Option[Factor] =
    Some(PureNumber)

  /**
    * Returns the multiplicative identity element of type `T` in the context
    * of a structure that supports multiplication.
    *
    * @return the instance of type `T` that acts as the identity element for multiplication
    */
  def one: Logarithm = NatLog(WholeNumber.zero)

  /**
    * Scales the current instance of type `T` by the specified `Double` value.
    *
    * This method applies a scaling factor to the instance, returning an `Option`
    * that contains the scaled instance if the operation is valid. If the scaling
    * operation is not valid or feasible, `None` is returned.
    *
    * @param that the `Double` value to scale the instance by
    * @return an `Option` containing the scaled instance of type `T`, or `None`
    *         if scaling is not possible
    */
  def doScaleDouble(that: Double): Option[Monotone] =
    None // TODO implement me as appropriate

  //  override def fuzzyEqv(p: Double)(that: Eager): Try[Boolean] = (this, that) match {
  //    case (a: NatLog, b: Structure) =>
  //      for {
  //        z <- FP.recoverAsTry(a.transformation[Real])(AlgebraException(s"NatLog.fuzzyEqv: logic transformation error $this $that"))
  //        q <- FP.recoverAsTry(b.convert(Real.zero))(AlgebraException(s"NatLog.fuzzyEqv: logic conversion error $this $that"))
  //        w <- z.fuzzyEqv(p)(z)
  //      } yield w
  //    case (a: Real, b: Structure) =>
  //      for {
  //        q <- FP.recoverAsTry(b.convert(Real.zero))(AlgebraException(s"NatLog.fuzzyEqv: logic conversion error $this $that"))
  //        w <- a.fuzzyEqv(p)(a)
  //      } yield w
  //
  //    case (a, b) =>
  //      Failure(AlgebraException(s"NatLog.fuzzyEqv: logic conversion error $this $that"))
  //  }

}

object BinaryLog {
  def apply(x: Number): BinaryLog = new BinaryLog(x)()
}
/**
  * The `Logarithm` companion object contains utility methods, predefined constants, and
  * typeclass instances for working with Logarithms. Logarithms are represented using
  * rational numbers and comply with the algebraic structure of a commutative group.
  */
object Logarithm {

  import org.slf4j.{Logger, LoggerFactory}
  import scala.util.Try

  val logger: Logger = LoggerFactory.getLogger(getClass)

  given DyadicOperator[Logarithm] = new DyadicOperator[Logarithm] {
    def op[B <: Logarithm, Z](f: (Logarithm, B) => Try[Z])(x: Logarithm, y: B): Try[Z] = (x, y) match {
      case (a: NatLog, b: NatLog) =>
        implicitly[DyadicOperator[NatLog]].op(f)(a, b)
      case (a, b) =>
        f(a, b)
    }
  }

  given Eq[Logarithm] = Eq.instance {
    (x, y) =>
      summon[DyadicOperator[Logarithm]].op((x: Eager, y: Eager) => x.eqv(y))(x, y).getOrElse(false)
  }

  given FuzzyEq[Logarithm] = FuzzyEq.instance {
    (x, y, p) =>
      x === y || x.fuzzyEqv(p)(y).getOrElse(false)
  }

  /**
    * Provides an implicit `Show` instance for the `Logarithm` class, enabling conversion
    * of a `Logarithm` instance to a string representation using its `render` method.
    *
    * This allows the `Logarithm` class to integrate seamlessly with libraries or frameworks
    * requiring a `Show` typeclass instance for displaying or logging purposes.
    */
  implicit val showLogarithm: Show[Logarithm] = Show.show(_.render)

  /**
    * Provides an implicit implementation of a commutative group for the `Logarithm` type, supporting
    * group operations such as identity, combination, and inversion.
    *
    * This allows `Logarithm` objects to adhere to the algebraic structure of a commutative group, where
    * the `combine` operation is associative and commutative, an identity element exists, and
    * each element has an additive inverse.
    */
  implicit object LogarithmIsCommutativeMonoid extends CommutativeMonoid[Logarithm] {
    /**
      * Provides the identity element for the `Logarithm` group, representing a Logarithm of zero value.
      *
      * @return a `Logarithm` instance with zero value, acting as the identity element in the group structure.
      */
    lazy val empty: Logarithm = NatLog(WholeNumber.zero)

    /**
      * Combines two `Logarithm` instances by adding their respective value.
      * NOTE that this is the equivalent of multiplying the corresponding (exponential) values.
      * This method is associative and commutative, meaning that the order of the arguments does not matter.
      * The identity element is the zero value, which is the neutral element for the addition operation.
      *
      * @param x the first `Logarithm` to combine
      * @param y the second `Logarithm` to combine
      * @return a new `Logarithm` representing the sum of the value of the two provided `Logarithm` instances
      */
    def combine(x: Logarithm, y: Logarithm): Logarithm = (x, y) match {
      case (a, b) if a.getClass == b.getClass =>
        a + b
      case _ =>
        throw AlgebraException(s"Logarithm.combine: $x, $y")
    }
  }
}

/**
  * Companion object for the `NatLog` class, providing predefined constants and utility methods.
  *
  * The object contains common values associated with natural logarithmic operations
  * and other relevant constants that can be used with `Logarithm` instances.
  */
object NatLog {

  def apply(x: Number): NatLog = new NatLog(x)()

  val logger: Logger = LoggerFactory.getLogger(getClass)

  given DyadicOperator[NatLog] = new DyadicOperator[NatLog] {
    def op[B <: NatLog, Z](f: (NatLog, B) => Try[Z])(x: NatLog, y: B): Try[Z] = f(x, y)
  }

  given Eq[NatLog] = Eq.instance {
    (x, y) =>
      summon[DyadicOperator[NatLog]].op((x: Eager, y: Eager) => x.eqv(y))(x, y).getOrElse(false)
  }

  given FuzzyEq[NatLog] = FuzzyEq.instance {
    (x, y, p) =>
      x === y || x.fuzzyEqv(p)(y).getOrElse(false)
  }

  /**
    * Represents the zero value of the `Logarithm` class.
    *
    * This is a predefined constant that corresponds to a `Logarithm` of zero value.
    * It is used as the additive identity in operations involving Logarithms.
    */
  val one: Logarithm = NatLog(WholeNumber.zero)(Some("1"))

  /**
    * Represents the natural logarithmic base `e` as a `Logarithm` instance.
    *
    * The value corresponds to the natural logarithm of one in terms of base `e`,
    * often used as a mathematical constant in logarithmic and exponential calculations.
    */
  val e: Logarithm = NatLog(WholeNumber.one)(Some("e"))
}
