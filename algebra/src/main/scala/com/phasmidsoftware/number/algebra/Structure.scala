/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra

import cats.implicits.catsSyntaxEq
import cats.kernel.Eq
import com.phasmidsoftware.number.algebra.Eager.*
import com.phasmidsoftware.number.algebra.Real
import com.phasmidsoftware.number.algebra.misc.FuzzyEq.{~=, given}
import com.phasmidsoftware.number.algebra.misc.{AlgebraException, DyadicOperator, FP, FuzzyEq}
import com.phasmidsoftware.number.core.inner.{Factor, PureNumber}
import com.phasmidsoftware.number.core.numerical.*
import com.phasmidsoftware.number.core.{inner, numerical}
import com.phasmidsoftware.number.{algebra, core}
import org.slf4j.{Logger, LoggerFactory}
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

/**
  * Represents an Algebraic Structure.
  * In common parlance, we might call such an object, a "number" or "quantity" or a mathematical thing.
  * A `Structure` supports functionality such as exactness evaluation, numeric conversion,
  * rendering, and set membership analysis.
  * In general, we cannot order `Structure` objects, but we can test them for exactness.
  */
trait Structure extends Eager {

  /**
    * Converts the given `Structure` object to an optional instance of the same type.
    *
    * TODO remove this method as it is no longer needed.
    * 
    * @param t the input object of type `T` which is a subtype of `Structure`.
    * @return an `Option` containing a transformed instance of type `T` if the conversion is successful, or `None` otherwise.
    */
  def convert[T <: Structure : ClassTag](t: T): Option[T]

  /**
    * Converts this `Structure` object into an optional `java.lang.Number` provided that the conversion can be
    * performed without loss of precision.
    *
    * The method determines whether the current `Structure` object can be represented as a `java.lang.Number`
    * by leveraging the `asNumber` method and further evaluating certain conditions:
    * - If the `Structure` object is an `ExactNumber` and its factor is `PureNumber`, the result
    * is converted using `Value.asJavaNumber`.
    * - If the `Structure` object is a `Real` with a `wiggle` value below a specified tolerance,
    * the result is also converted using `Value.asJavaNumber`.
    * - In all other cases, `None` is returned.
    *
    * @return an optional `java.lang.Number` representation of this object. The result is `Some(java.lang.Number)`
    *         if the conversion is successful under the stated conditions; otherwise, `None`.
    */
  def asJavaNumber: Option[java.lang.Number] = this match {
    case algebra.Angle(number, _) => number.convert(Real.zero).flatMap(x => x.asJavaNumber)
    case algebra.Real(value, _) => Some(value)
    case algebra.RationalNumber(r, _) => Some(r.toDouble)
    case _ => throw new UnsupportedOperationException(s"asJavaNumber: $this")
  }

  override def fuzzyEqv(p: Double)(x: Eager, y: Eager): Try[Boolean] = (x, y) match {
    case (a: Structure, b: Structure) =>
      FP.toTry(for {
        p: Real <- a.convert(Real.one)
        q: Real <- b.convert(Real.one)
        r = p ~= q
      } yield r, Failure(AlgebraException("Structure.fuzzyEqv")))
  }
}

object Structure {

  import org.slf4j.{Logger, LoggerFactory}
  import scala.util.Try

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  // CONSIDER do we need this?
  given DyadicOperator[Structure] = new DyadicOperator[Structure] {
    def op[Z](f: (Structure, Structure) => Try[Z])(x: Structure, y: Structure): Try[Z] = (x, y) match {
      case (a: Monotone, b: Monotone) =>
        implicitly[DyadicOperator[Monotone]].op(f)(a, b)
      case (a, b) =>
        f(a, b)
    }
  }

  given Eq[Structure] = Eq.instance {
    (x, y) =>
      summon[DyadicOperator[Structure]].op(x.eqv)(x, y).getOrElse(false)
  }

  given FuzzyEq[Structure] = FuzzyEq.instance {
    (x, y, p) =>
      x == y || summon[DyadicOperator[Structure]].op(x.fuzzyEqv(p))(x, y).getOrElse(false)
  }

//  implicit val dyadicOperatorStructure: DyadicOperator[Structure] = new DyadicOperator[Structure] {
//    def op[Z](f: (Structure, Structure) => Try[Z])(x: Structure, y: Structure): Try[Z] = (x, y) match {
//      case (a: Monotone, b: Monotone) => f(a, b)
//      case _ => Try(f(x, y).get)
//    }
//  }

  /**
    * Attempts to cast the provided `Structure` instance to the specified subtype `T`.
    * Throws an `AlgebraException` if the provided instance cannot be cast to the target type.
    *
    * @param x the input instance of `Structure` to be cast to the desired type `T`.
    * @tparam T the target subtype of `Structure` to which the input instance will be cast.
    * @return the input instance cast to the type `T` if the cast is valid and successful.
    * @throws AlgebraException if the input instance cannot be cast to the type `T`.
    */
  def asT[T <: Structure : ClassTag](x: Structure): T = {
    val clazz = summon[ClassTag[T]].runtimeClass
    if (clazz.isAssignableFrom(x.getClass))
      x.asInstanceOf[T]
    else
      throw AlgebraException(s"Logic error: Can.asT failed to cast ${x.getClass} to $clazz")
  }
}

/**
  * This is a placeholder for a Complex number to demonstrate where it should appear in the type hierarchy (it should extend Structure).
  *
  * @see com.phasmidsoftware.number.core.numerical.Complex
  */
case class Complex(complex: numerical.Complex) extends Eager {

  /**
    * Method to render this `Valuable` for presentation to the user.
    *
    * @return a String
    */
  def render: String = complex.render

  /**
    * Determines whether this `Valuable` is exact, i.e., has no approximation.
    *
    * CONSIDER it may be possible that there are non-approximatable entities that are not exact either.
    *
    * The method returns `true` if there is no approximate representation
    * available (i.e., `approximation` is `None`), indicating that the
    * entity is exact. Otherwise, it returns `false`.
    *
    * @return a `Boolean` indicating whether the entity is exact (`true`)
    *         or has an approximation (`false`).
    */
  def isExact: Boolean = complex.isExact

  /**
    * If this `Valuable` is exact, it returns the exact value as a `Double`.
    * Otherwise, it returns `None`.
    * NOTE: do NOT implement this method to return a Double for a fuzzy Real--only for exact numbers.
    *
    * @return Some(x) where x is a Double if this is exact, else None.
    */
//  def maybeDouble: Option[Double] =
//    complex.toRational.map(_.toDouble) // TESTME

  /**
    * Optionally retrieves a factor associated with this `Valuable` if one exists (this is a Scalar).
    *
    * Factors are components or divisors related to the numerical value represented 
    * by this `Valuable`. If no such factor exists or is applicable, the result will 
    * be `None`.
    *
    * @return an `Option` containing the `Factor` if available, otherwise `None`.
    */
  def maybeFactor(context: Context): Option[Factor] = complex.maybeFactor

  override def eqv(x: Eager, y: Eager): Try[Boolean] = (x, y) match {
    case (Complex(a), Complex(b)) => Success(a == b)
    case _ => Failure(AlgebraException(s"Complex.eqv: unexpected input: $x, $y"))
  }

  override def fuzzyEqv(p: Double)(x: Eager, y: Eager): Try[Boolean] = (x, y) match {
    case (Complex(a), Complex(b)) => Success(a.isSame(b)) // TODO we lose the value of `p` here (it defaults to 0.5)
    case _ => Failure(AlgebraException(s"Complex.fuzzyEqv: unexpected input: $x, $y"))
  }

  /**
    * Attempts to compute an approximate representation of the current value.
    *
    * This method provides an optional approximation of the value represented by
    * the implementing class. The approximation may account for uncertainties or
    * computational limitations. By default, this method does not force computation
    * of the approximation unless explicitly requested.
    *
    * @param force a boolean flag indicating whether to force computation of
    *              the approximation. If `true`, the method will attempt to
    *              generate an approximation even if such computation
    *              is resource-intensive or not strictly necessary.
    *
    * @return      an `Option` containing the approximate value as a `Real` if available,
    *              or `None` if no approximation can be computed.
    */
  def approximation(force: Boolean): Option[Real] =
    FP.whenever(complex.isReal) {
      val modulus: Number = complex.modulus
      Scalar.createScalar(modulus.nominalValue, modulus.factor, modulus.fuzz).approximation(force)
    }
}

object Complex {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  // CONSIDER do we need this?
  given DyadicOperator[Complex] = new DyadicOperator[Complex] {
    def op[Z](f: (Complex, Complex) => Try[Z])(x: Complex, y: Complex): Try[Z] = f(x, y)
  }

  given Eq[Complex] = Eq.instance {
    (x, y) =>
      FP.toOptionWithLog(logger.warn("Eq[Complex]", _))(x.eqv(x, y)).getOrElse(false)
  }

  given FuzzyEq[Complex] = FuzzyEq.instance {
    (x, y, p) =>
      x === y || FP.toOptionWithLog(logger.warn("FuzzyEq[Complex]", _))(x.fuzzyEqv(p)(x, y)).getOrElse(false)
  }

}
