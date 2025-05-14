package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.expression.Expression
import org.scalactic.Equality
import scala.annotation.tailrec

trait FuzzyEquality {

  implicit object NumberEquality extends Equality[Number] {

    def areEqual(a: Number, b: Any): Boolean = b match {
      case n: Number => a.isSame(n)
      case n: Expression => a.compare(n) == 0
      case _ => false
    }
  }

  implicit object RealEquality extends Equality[Real] {

    @tailrec
    def areEqual(a: Real, b: Any): Boolean = b match {
      case r@Real(_) => a.isSame(r)
      case n: Number => areEqual(a, Real(n))
    }
  }

  implicit object FieldEquality extends Equality[Field] {

    def areEqual(a: Field, b: Any): Boolean = b match {
      case n: Field => a.isSame(n)
      case _ => false
    }
  }
}
