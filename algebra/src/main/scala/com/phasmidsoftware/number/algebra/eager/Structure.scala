/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.eager

import cats.implicits.catsSyntaxEq
import cats.kernel.Eq
import com.phasmidsoftware.number.algebra.core.FuzzyEq.~=
import com.phasmidsoftware.number.algebra.core.{DyadicOperator, FuzzyEq}
import com.phasmidsoftware.number.algebra.eager.Real
import com.phasmidsoftware.number.algebra.util.{AlgebraException, FP}
import com.phasmidsoftware.number.core.inner
import com.phasmidsoftware.number.{algebra, core}
import org.slf4j.{Logger, LoggerFactory}
import scala.reflect.ClassTag
import scala.util.{Failure, Try}

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
    *   is converted using `Value.asJavaNumber`.
    * - If the `Structure` object is a `Real` with a `wiggle` value below a specified tolerance,
    *   the result is also converted using `Value.asJavaNumber`.
    * - In all other cases, `None` is returned.
    *
    * @return an optional `java.lang.Number` representation of this object. The result is `Some(java.lang.Number)`
    *         if the conversion is successful under the stated conditions; otherwise, `None`.
    */
  def asJavaNumber: Option[java.lang.Number] = this match {
    case Angle(number, _) => number.convert(Real.zero).flatMap(x => x.asJavaNumber)
    case Real(value, _) => Some(value)
    case RationalNumber(r, _) => Some(r.toDouble)
    case _ => throw new UnsupportedOperationException(s"asJavaNumber: $this")
  }

  override def fuzzyEqv(p: Double)(that: Eager): Try[Boolean] = (this, that) match {
    case (a: Structure, b: Structure) =>
      FP.toTry(for {
        p: Real <- a.convert(Real.one)
        q: Real <- b.convert(Real.one)
        r = p ~= q
      } yield r, Failure(AlgebraException("Structure.fuzzyEqv")))
  }
}

/**
  * Companion object for the `Structure` trait, providing utility functions and type class instances 
  * to facilitate operations on `Structure` objects.
  */
object Structure {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  // CONSIDER do we need this?
  given DyadicOperator[Structure] = new DyadicOperator[Structure] {
    def op[B <: Structure, Z](f: (Structure, B) => Try[Z])(x: Structure, y: B): Try[Z] = (x, y) match {
      case (a: Monotone, b: Monotone) =>
        implicitly[DyadicOperator[Monotone]].op(f)(a, b)
      case (a, b) =>
        f(a, b)
    }
  }

  given Eq[Structure] = Eq.instance {
    (x, y) =>
      summon[DyadicOperator[Structure]].op((x: Eager, y: Eager) => x.eqv(y))(x, y).getOrElse(false)
  }

  given FuzzyEq[Structure] = FuzzyEq.instance {
    (x, y, p) =>
      x === y || x.fuzzyEqv(p)(y).getOrElse(false)
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
