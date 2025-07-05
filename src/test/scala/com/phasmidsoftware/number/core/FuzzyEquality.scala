package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.expression.Expression
import org.scalactic.Equality
import scala.annotation.tailrec

trait FuzzyEquality {

  implicit object NumberEquality extends Equality[Number] {

    def areEqual(a: Number, b: Any): Boolean = b match {
      case n: Number =>
        a.isSame(n)
      case n: Expression =>
        a.compare(n) == 0
      case _ =>
        false
    }
  }

  implicit object RealEquality extends Equality[Real] {

    @tailrec
    def areEqual(a: Real, b: Any): Boolean = b match {
      case r@Real(_) =>
        a.isSame(r)
      case n: Number =>
        areEqual(a, Real(n))
      case n: Field =>
        areEqual(a, Real(n))
//      case x: Algebraic =>
//        x.solve.asNumber match {
//          case Some(n) => areEqual(a, n)
//          case None => areEqual(a, Field.convertToNumber(x.solve.asField))
//        }
      case x =>
        throw new IllegalArgumentException(s"cannot compare Real with $x")
    }
  }

  implicit object FieldEquality extends Equality[Field] {

    def areEqual(a: Field, b: Any): Boolean = b match {
      case n: Field =>
        a.isSame(n)
      case n: Int =>
        a.isSame(Number(n))
      case n: Rational =>
        a.isSame(Number(n))
      case n: Double =>
        a.isSame(Number(n))
      case _ =>
        false
    }
  }
}
