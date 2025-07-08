/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.inner

import scala.Option.when

trait Series[X] {

  def evaluate(epsilon: Double): Option[X]

  def evaluate(maybeN: Option[Int]): Option[X]

  def term(n: Int): Option[X]

  def nTerms: Option[Int]
}

abstract class AbstractSeries[X: Numeric](terms: Seq[X]) extends Series[X] {

  def evaluate(epsilon: Double): Option[X] = ???

  def term(i: Int): Option[X] =
    when(nTerms.forall(n => i >= 0 && i < n))(terms(i))

  def evaluate(maybeN: Option[Int] = None): Option[X] = {
    val xn = implicitly[Numeric[X]]
    maybeN.orElse(nTerms) flatMap {
      n =>
        val initialValue: Option[X] = Some(xn.zero)
        val result: Option[X] = Range(0, n).foldLeft(initialValue) {
          (maybeTotal, i) =>
            (maybeTotal, term(i)) match {
              case (Some(total), Some(x)) =>
                Some(xn.plus(total, x))
              case _ =>
                None
            }
        }
        result
    }
  }
}

case class FiniteSeries[X: Numeric](terms: List[X]) extends AbstractSeries[X](terms) {
  def nTerms: Option[Int] =
    Some(terms.length)
}

case class InfiniteSeries[X: Numeric](terms: LazyList[X]) extends AbstractSeries[X](terms) {
  def nTerms: Option[Int] = None
}
