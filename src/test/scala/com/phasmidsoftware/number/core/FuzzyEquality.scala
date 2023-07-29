package com.phasmidsoftware.number.core

import org.scalactic.Equality
import scala.annotation.tailrec

trait FuzzyEquality {

  implicit object NumberEquality extends Equality[Number] {

    import com.phasmidsoftware.number.core.Field.FieldIsFuzzy

    def areEqual(a: Number, b: Any): Boolean = b match {
      case n: Number => implicitly[Fuzzy[Field]].same(0.5)(Real(a), Real(n))
      case n: Expression => a.compare(n) == 0
      case _ => false
    }
  }

  implicit object RealEquality extends Equality[Real] {

    import com.phasmidsoftware.number.core.Field.FieldIsFuzzy

    @tailrec
    def areEqual(a: Real, b: Any): Boolean = b match {
      case r@Real(_) => implicitly[Fuzzy[Field]].same(0.5)(a, r)
      case n: Number => areEqual(a, Real(n))
    }
  }

  implicit object FieldEquality extends Equality[Field] {

    import com.phasmidsoftware.number.core.Field.FieldIsFuzzy

    def areEqual(a: Field, b: Any): Boolean = b match {
      case n: Field => implicitly[Fuzzy[Field]].same(0.5)(a, n)
      case _ => false
    }
  }

}
