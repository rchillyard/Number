package com.phasmidsoftware.number.cats

import algebra.ring.{CommutativeRing, TruncatedDivision}
import com.phasmidsoftware.number.core.{Constants, Real}
import com.phasmidsoftware.number.core.Real.createFromRealField


trait RealAlgebraicInstances {

    // Provide a convenient local alias for the algebra nested trait
    type TruncatedDivisionCRing[A] = TruncatedDivision.forCommutativeRing[A]

    implicit val realTruncatedDivisionCRing: TruncatedDivisionCRing[Real] = new TruncatedDivisionCRing[Real] {
        // Delegate the basic operations of the loop to realField
        def zero: Real = Constants.zero
        def one: Real = Constants.one
        def plus(x: Real, y: Real): Real = createFromRealField(x add y)
        def times(x: Real, y: Real): Real = createFromRealField(x multiply y)
        def negate(x: Real): Real = createFromRealField(-x)
        override def fromInt(n: Int): Real = Real(n)

        // Provide the required algebra.Order instance via existing scala Ordering
        def order: algebra.Order[Real] = (x: Real, y: Real) => Real.RealIsOrdering.compare(x, y)

        // Truncated division: round the quotient towards zero
        def tquot(x: Real, y: Real): Real = {
            val qReal: Real = createFromRealField(x divide y)
            val qd: Double = qReal.toDouble
            // Round towards zero: positive numbers floor, negative numbers ceil
            val qt: Double = if (qd >= 0.0) math.floor(qd) else math.ceil(qd)
            Real(qt)
        }

        // Remainder: r = x - y * q, where q = tquot(x, y)
        def tmod(x: Real, y: Real): Real = {
            val q: Real = tquot(x, y)
            plus(x, negate(times(y, q)))
        }

        // Return (q, r)
        override def tquotmod(x: Real, y: Real): (Real, Real) = {
            val q: Real = tquot(x, y)
            val r: Real = plus(x, negate(times(y, q)))
            (q, r)
        }
    }
    
    // Provide CommutativeRing[Real] without depending on Field
    implicit val realCommutativeRing: CommutativeRing[Real] = new CommutativeRing[Real] {
        def zero: Real = Constants.zero
        def one: Real = Constants.one
        def plus(x: Real, y: Real): Real = createFromRealField(x add y)
        def times(x: Real, y: Real): Real = createFromRealField(x multiply y)
        def negate(x: Real): Real = createFromRealField(-x)
        override def fromInt(n: Int): Real = Real(n)
    }

    /**
    // Cannot pass the field law tests.
    //TODO: Uncomment this when we have a way to pass the field law tests.
    
     implicit val realField: AlgebraField[Real] = new AlgebraField[Real] {
         def zero: Real = Constants.zero
         def one: Real = Constants.one
         def plus(x: Real, y: Real): Real = createFromRealField(x add y)
         def times(x: Real, y: Real): Real = createFromRealField(x multiply y)
         def negate(x: Real): Real = createFromRealField(-x)
         override def fromInt(n: Int): Real = Real(n)
         def div(x: Real, y: Real): Real = createFromRealField(x divide y)
         def reciprocal(x: Real, y: Real): Real = createFromRealField(x.invert)
     }
    */

    
}