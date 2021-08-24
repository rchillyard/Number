package com.phasmidsoftware.number.core

import org.scalactic.Equality

trait FuzzyEquality {

    implicit object NumberEquality extends Equality[Number] {

        import com.phasmidsoftware.number.core.Field.FieldIsFuzzy

        def areEqual(a: Number, b: Any): Boolean = b match {
            case n: Number => implicitly[Fuzzy[Field]].same(0.5)(a, n)
            case n: Expression => a.compare(n) == 0
            case _ => false
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
