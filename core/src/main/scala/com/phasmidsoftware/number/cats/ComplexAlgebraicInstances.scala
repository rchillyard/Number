package com.phasmidsoftware.number.cats

import algebra.ring.CommutativeRing
import com.phasmidsoftware.number.core.numerical.{Complex, ComplexCartesian, Number}


trait ComplexAlgebraicInstances {

    implicit val complexCommutativeRing: CommutativeRing[Complex] = new CommutativeRing[Complex] {
        lazy val zero: Complex = ComplexCartesian(Number.zero, Number.zero)
        lazy val one: Complex = Complex.unit
        def plus(x: Complex, y: Complex): Complex = x.doAdd(y)
        def times(x: Complex, y: Complex): Complex = x.doMultiply(y)
        def negate(x: Complex): Complex = x.numberProduct(Number.negOne)
        override def fromInt(n: Int): Complex = Complex(Number(n))
    }
}


