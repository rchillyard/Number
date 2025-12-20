/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra

import algebra.CommutativeGroup
import cats.Show
import cats.implicits.catsSyntaxEq
import cats.kernel.Eq
import com.phasmidsoftware.number.algebra
import com.phasmidsoftware.number.algebra.Structure
import com.phasmidsoftware.number.algebra.misc.{AlgebraException, DyadicOperator, FP, FuzzyEq}
import com.phasmidsoftware.number.core.inner.*
import com.phasmidsoftware.number.core.numerical
import com.phasmidsoftware.number.core.numerical.{Fuzziness, WithFuzziness}
import org.slf4j.{Logger, LoggerFactory}
import scala.reflect.ClassTag
import scala.util.Try

/**
  * Represents a mathematical Root object, parameterized with an integral root degree and a base number.
  *
  * The `Root` class extends `Monotone`, `MultiplicativeWithPower[Root]`, and `Ordered[Root]`,
  * providing mathematical operations for roots and ensuring ordering functionality.
  *
  * TODO this may be only a temporary class because we can model all roots (and more) as solutions to Algebraic equations.
  *
  * @param n    the degree of the root, specified as an integer
  * @param number the base `Number` value on which the root operation is defined
  */
case class InversePower(n: Int, number: Number) extends Transformed with CanMultiplyAndDivide[Monotone] with Ordered[InversePower] {
  /**
    * Defines a transformation that transforms a `Monotone` instance into a corresponding `Scalar` value.
    *
    * The transformation defines how a `Monotone` is interpreted or converted in the context of `Scalar`.
    *
    * @return a transformation that maps a `Monotone` object to a `Scalar` result
    */
  def transformation[T: ClassTag]: Option[T] = ???

  /**
    * Returns a new instance of `Monotone` that is the negation of the current instance.
    * CONSIDER sorting out the use of CanNegate so that we can extend that for Monotone.
    *
    * @return a `Monotone` representing the negation of this instance
    */
  def negate: Monotone = ???

  def compare(that: InversePower): Int = (this.isExact, that.isExact) match {
    case (true, true) =>
      compareExact(that).getOrElse(0) // TODO fix this
    case _ =>
      this.n.compare(that.n)
  }

  /**
    * Represents the multiplicative identity element of the structure.
    *
    * The `one` value serves as the neutral element for the multiplication operation, meaning
    * that for any instance `t` of type `T`, the equation `one * t = t * one = t` holds true.
    */
  def one: InversePower = InversePower(1, Real.one)

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
  def doScaleDouble(that: Double): Option[Monotone] = doScale(Real(that))

  /**
    * Retrieves an optional fuzziness value associated with this instance.
    *
    * The fuzziness value, if present, provides information about the level of uncertainty
    * or imprecision, modeled as a `Fuzziness[Double]`.
    *
    * @return an `Option` containing the `Fuzziness[Double]` value if defined, or `None` if no fuzziness is specified.
    */
  def fuzz: Option[Fuzziness[Double]] =
    Eager(Valuable.valuableToField(number).power(numerical.Number(Rational(n).invert))) match {
      case fuzzy: WithFuzziness =>
        fuzzy.fuzz
      case _ =>
        None // CONSIDER should this throw an exception?
    }

  /**
    * Compares the current `Root` instance with another `Number` to determine their exact order.
    *
    * If the provided `Number` is a `Root`, this method compares their underlying radian values.
    * If the provided `Number` is not a `Root`, the comparison cannot be performed, and `None` is returned.
    *
    * @param that the `Number` instance to compare with the current `Root` instance
    * @return an `Option[Int]`, where `Some(-1)` indicates that the current `Root` is less than the provided `Root`,
    *         `Some(0)` indicates that both roots are equal, `Some(1)` indicates that the current `Root` is greater,
    *         and `None` is returned if the comparison cannot be made
    */
  def compareExact(that: Monotone): Option[Int] = that match {
    case InversePower(m, x) if m == n => // TODO - there are other situations where the result should be Some(0) (see Factor class).
      Some(number.compare(x))
    case _ =>
      None
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
    case x =>
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
  def signum: Int = compare(InversePower.zero)

  /**
    * Method to determine if this Structure object is exact.
    * For instance, `Number.pi` is exact, although if you converted it into a PureNumber, it would no longer be exact.
    *
    * @return true if this Structure object is exact in the context of No factor, else false.
    */
  override def isExact: Boolean = false // TODO there are some situations where a Root is actually exact.

  /**
    * Converts this `Number` into its corresponding `Rational` representation, if possible.
    *
    * @return an `Option[Rational]` containing the `Rational` representation of this `Number`
    *         if it can be converted, or `None` if the conversion is not possible.
    */
  def toRational: Option[Rational] = number match {
    case z: Z =>
      z.toRational.power(Rational(n).invert).toOption
    case _ =>
      None
  }

  /**
    * If this `Valuable` is exact, it returns the exact value as a `Double`.
    * Otherwise, it returns `None`.
    * NOTE: do NOT implement this method to return a Double for a fuzzy Real--only for exact numbers.
    *
    * @return Some(x) where x is a Double if this is exact, else None.
    */
//  def maybeDouble: Option[Double] =
//    FP.whenever(isExact)(convert(Real.zero) map (_.value))

  /**
    * Renders this `Root` instance as a string representation of base in terms of π.
    *
    * The method formats the radius equivalent to π, omitting the numeric coefficient if it is 1.
    *
    * @return a string representation of the `Root` in terms of π
    */
  def render: String = {
    val suffix = number.render
    n match {
      case 2 => s"√$suffix"
      case 3 => s"³√$suffix"
      case _ => toString
    }
  }

  /**
    * Scales the current instance using the provided `Number`.
    *
    * The method performs a scaling operation by applying the given `Number` to the current instance,
    * producing an optional result of type `T`. If the scaling operation cannot be defined for the given `Number`,
    * it returns `None`.
    *
    * @param that the `Number` used to scale the current instance
    * @return an `Option[T]` containing the result of the scaling operation if successful, or `None` if the operation cannot be performed
    */
  private infix def doScale(that: Number): Option[InversePower] =
    (that, number) match {
      case (x: CanPower[Number] @unchecked, y: Z) =>
        val triedRational = y.toRational.power(Rational(n).invert).toOption
        val value: Option[Number] = triedRational.flatMap(r => x.pow(RationalNumber(r)))
        value.asInstanceOf[Option[InversePower]]
      // TODO need to match on types, not use isInstanceOf, etc.
      case _ =>
        throw AlgebraException(s"InversePower.doScale: cannot scale $this by $that")
    }


  /**
    * Computes the potential factor associated with this instance.
    *
    * @return an `Option` containing a `Factor` if available, otherwise `None`
    */
  def maybeFactor(context: Context): Option[Factor] = n match {
    case 2 if context.factorQualifies(SquareRoot) => Some(SquareRoot)
    case 3 if context.factorQualifies(SquareRoot) => Some(CubeRoot)
    case _ => None
  }

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
  * The `Root` companion object contains utility methods, predefined constants, and
  * typeclass instances for working with roots. Angles are represented using
  * rational numbers and comply with the algebraic structure of a commutative group.
  */
object InversePower {
  val logger: Logger = LoggerFactory.getLogger(getClass)

  given DyadicOperator[InversePower] = new DyadicOperator[InversePower] {
    def op[Z](f: (InversePower, InversePower) => Try[Z])(x: InversePower, y: InversePower): Try[Z] =
      f(x, y)
  }

  given Eq[InversePower] = Eq.instance {
    (x, y) =>
      summon[DyadicOperator[InversePower]].op(x.eqv)(x, y).getOrElse(false)
  }

  given FuzzyEq[InversePower] = FuzzyEq.instance {
    (x, y, p) =>
      x === y || summon[DyadicOperator[InversePower]].op(x.fuzzyEqv(p))(x, y).getOrElse(false)
  }

  private def tryConvertAndCompareTransformed[T <: InversePower, Z](f: (InversePower, InversePower) => Try[Z])(s: Logarithm, e: T): Try[Z] = e match {
    case _ =>
      FP.fail(s"InversePower.tryConvertAndCompareTransformed: unsupported operation: ${s.getClass.getSimpleName} === ${e.getClass.getSimpleName}")
  }

  implicit val inversePowerEq: Eq[InversePower] = Eq.instance {
    case (x: InversePower, y: InversePower) =>
      x.n == y.n && x.number == y.number
  }

  /**
    * Represents the zero value of the `Root` class.
    */
  val zero: InversePower = InversePower(1, Real.zero)

  /**
    * Represents the multiplicative identity for roots.
    *
    * This value denotes a root of zero base, serving as the identity element in
    * the group structure of roots. It is constructed using the `Root` companion object
    * initialized with the additive identity of `RationalNumber`.
    */
  val one: InversePower = InversePower(1, WholeNumber.one)
  val nan: InversePower = InversePower(0, RationalNumber(Rational.NaN))

  /**
    * Provides an implicit `Show` instance for the `Root` class, enabling conversion
    * of a `Root` instance to a string representation using its `render` method.
    *
    * This allows the `Root` class to integrate seamlessly with libraries or frameworks
    * requiring a `Show` typeclass instance for displaying or logging purposes.
    */
  implicit val showInversePower: Show[InversePower] = Show.show(_.render)

  /**
    * Provides an implicit implementation of a commutative group for the `Root` type, supporting
    * group operations such as identity, combination, and inversion.
    *
    * This allows `Root` objects to adhere to the algebraic structure of a commutative group, where
    * the `combine` operation is associative and commutative, an identity element exists, and
    * each element has a multiplicative inverse.
    */
  implicit object rootIsCommutativeGroup extends CommutativeGroup[InversePower] {
    /**
      * Provides the identity element for the `Root` group, representing a root of one.
      *
      * @return a `Root` instance with zero base, acting as the identity element in the group structure.
      */
    def empty: InversePower = InversePower.one

    /**
      * Combines two `Root` instances by adding their respective base.
      *
      * @param x the first `Root` to combine
      * @param y the second `Root` to combine
      * @return a new `Root` representing the sum of the base of the two provided `Root` instances
      */
    def combine(x: InversePower, y: InversePower): InversePower = (x, y) match {
      case (InversePower(n1, x1: Number), InversePower(n2, x2: Number)) =>
        throw AlgebraException(s"InversePower.combine: cannot combine $x and $y")
    }

    /**
      * Computes the additive inverse of the given `Root`.
      *
      * This method inverts the input root, returning a `Root` instance
      * that represents its multiplicative inverse, relative to `Root.one`.
      *
      * @param a the `Root` instance to be inverted
      * @return a new `Root` instance representing the multiplicative inverse of the input
      */
    def inverse(a: InversePower): InversePower =
      throw AlgebraException(s"InversePower.inverse: cannot invert $a")
  }
}
