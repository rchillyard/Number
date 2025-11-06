package com.phasmidsoftware.number.cats

import com.phasmidsoftware.number.core.inner.Rational
import algebra.ring.{TruncatedDivision, CommutativeRing}


trait RationalAlgebraicInstances {

    // Provide a convenient local alias for the algebra nested trait
    type TruncatedDivisionCRing[A] = TruncatedDivision.forCommutativeRing[A]

    implicit val rationalTruncatedDivisionCRing: TruncatedDivisionCRing[Rational] = new TruncatedDivisionCRing[Rational] {
        def zero: Rational = Rational.zero
        def one: Rational = Rational.one
        def plus(x: Rational, y: Rational): Rational = x + y
        def times(x: Rational, y: Rational): Rational = x * y
        def negate(x: Rational): Rational = -x
        override def fromInt(n: Int): Rational = Rational(n)

        // Provide the required algebra.Order instance via existing compare
        def order: algebra.Order[Rational] = new algebra.Order[Rational] {
            def compare(x: Rational, y: Rational): Int = x.compare(y)
        }

        // Truncated division: quotient rounded towards zero
        def tquot(x: Rational, y: Rational): Rational = {
            val q: Rational = x / y
            // BigInt division truncates toward zero
            val qi: BigInt = q.n / q.d
            Rational(qi)
        }

        // Remainder: r = x - y * q, where q = tquot(x, y)
        def tmod(x: Rational, y: Rational): Rational = {
            val q: Rational = tquot(x, y)
            plus(x, negate(times(y, q)))
        }

        // Return (q, r)
        override def tquotmod(x: Rational, y: Rational): (Rational, Rational) = {
            val q: Rational = tquot(x, y)
            val r: Rational = plus(x, negate(times(y, q)))
            (q, r)
        }
    }

    // Provide CommutativeRing[Rational]
    implicit val rationalCommutativeRing: CommutativeRing[Rational] = new CommutativeRing[Rational] {
        def zero: Rational = Rational.zero
        def one: Rational = Rational.one
        def plus(x: Rational, y: Rational): Rational = x + y
        def times(x: Rational, y: Rational): Rational = x * y
        def negate(x: Rational): Rational = -x
        override def fromInt(n: Int): Rational = Rational(n)
    }
}


