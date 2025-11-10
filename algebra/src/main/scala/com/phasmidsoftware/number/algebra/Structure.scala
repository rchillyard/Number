/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra

import com.phasmidsoftware.number.algebra.{Real, Valuable}
import com.phasmidsoftware.number.{algebra, core}
import com.phasmidsoftware.number.core.*
import com.phasmidsoftware.number.core.inner.Factor
import scala.reflect.ClassTag

/**
  * Represents an Algebraic Structure.
  * In common parlance, we might call such an object, a "number" or "quantity" or a mathematical thing.
  * A `Structure` supports functionality such as exactness evaluation, numeric conversion,
  * rendering, and set membership analysis.
  * In general, we cannot order `Structure` objects, but we can test them for exactness.
  */
trait Structure extends Valuable {

  /**
    * Converts the given `Structure` object to an optional instance of the same type.
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
//    case algebra.Angle(number) => number.convert(Real.zero).flatMap(x => x.asJavaNumber)
    case algebra.Real(value, _) => Some(value)
    case algebra.RationalNumber(r) => Some(r.toDouble)
    case _ => throw new UnsupportedOperationException(s"asJavaNumber: $this")
  }
//
//  /**
//    * Method to determine the NumberSet, if any, to which this Structure object belongs.
//    * NOTE that we don't yet support H, the quaternions.
//    *
//    * @return Some(numberSet) or None if it doesn't belong to any (for example, it is fuzzy).
//    */
//  def memberOf: Option[NumberSet] =
//    Seq(C, R, Q, Z, N).find(set => set.isMember(this))
//
//  /**
//    * Method to determine if this Structure object is a member of the given set.
//    *
//    * @param set the candidate NumberSet.
//    * @return true if this is exact and belongs to set.
//    */
//  def memberOf(set: NumberSet): Boolean = set.isMember(this)
}

/**
  * This is a placeholder for a Complex number to demonstrate where it should appear in the type hierarchy (it should extend Structure).
  *
  * @see com.phasmidsoftware.number.core.Complex
  */
case class Complex(complex: core.Complex) extends Valuable {
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
  def maybeDouble: Option[Double] =
    complex.toRational.map(_.toDouble) // TESTME 

  /**
    * Optionally retrieves a factor associated with this `Valuable` if one exists (this is a Scalar).
    *
    * Factors are components or divisors related to the numerical value represented 
    * by this `Valuable`. If no such factor exists or is applicable, the result will 
    * be `None`.
    *
    * @return an `Option` containing the `Factor` if available, otherwise `None`.
    */
  def maybeFactor: Option[Factor] = complex.maybeFactor
}

//trait Scalar extends Structure with Ordered[Scalar]