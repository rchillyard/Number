package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Real.createFromNumberField

case class Real(x: Number) extends Field {
    /**
      * Method to determine if this Real has infinite magnitude.
      *
      * @return true if the magnitude of this Field is infinite.
      */
    def isInfinite: Boolean = x.isInfinite

    /**
      * Method to determine if this Real has zero magnitude.
      * Zero is the additive identity.
      *
      * @return true if the magnitude of this Field is zero.
      */
    def isZero: Boolean = x.isZero

    /**
      * Method to determine if this Real has unity magnitude.
      * Unity is the multiplicative identity.
      *
      * @return true if the magnitude of this Field is one.
      */
    def isUnity: Boolean = x.isUnity

    /**
      * Add y to this Real and return the result.
      * See Number.plus for more detail.
      *
      * @param y the addend.
      * @return the sum.
      */
    def add(y: Field): Field = createFromNumberField(x.add(y))

    /**
      * Multiply this Real by y and return the result.
      *
      * * @param y the multiplicand.
      * * @return the product.
      */
    def multiply(y: Field): Field = createFromNumberField(x.multiply(y))

    /**
      * Divide this Real by y and return the result.
      *
      * @param y the divisor.
      * @return the quotient.
      */
    def divide(y: Field): Field = createFromNumberField(x.divide(y))

    /**
      * Change the sign of this Real.
      */
    def unary_- : Field = createFromNumberField(-x)

    /**
      * Raise this Real to the power p.
      *
      * If the Number of this Real is exact and if the exponent p is rational, then we convert x to a ComplexPolar first
      * and raise that to power p.
      *
      * @param p a Field.
      * @return this Field raised to power p.
      */
    def power(p: Field): Field =
        (x, p.asNumber.flatMap(n => n.toRational)) match {
            case (ExactNumber(_, _), Some(_)) => asComplex.power(p)
            case _ => createFromNumberField(x.power(p))
        }

    /**
      * Yields the inverse of this Real.
      * This Number is first normalized so that its factor is Scalar, since we cannot directly invert Numbers with other
      * factors.
      */
    def invert: Field = createFromNumberField(x.invert)

    /**
      * Method to "normalize" a field.
      *
      * @return a Real which is in canonical form.
      */
    def normalize: Field = createFromNumberField(x.normalize)

    def compare(that: Field): Int = that match {
        case y: Number => x.compare(y)
        case z: Complex => asComplex.compare(z)
    }

    /**
      * Method to determine if this NumberLike object can be evaluated exactly in the context of factor.
      *
      * @param maybeFactor the (optional) context in which we want to evaluate this Expression.
      *                    if factor is None then, the result will depend solely on whether this is exact.
      * @return true if this NumberLike object is exact in the context of factor, else false.
      */
    def isExact(maybeFactor: Option[Factor]): Boolean = x.isExact(maybeFactor)

    /**
      * Method to return the x of this Real.
      *
      * @return Some(x).
      */
    def asNumber: Option[Number] = Some(x)

    def asComplex: Complex = ComplexPolar(x)

    /**
      * Method to render this Field in a presentable manner.
      *
      * @return a String
      */
    def render: String = x.render
}

object Real {
    def createFromNumberField(x: Field): Real = x match {
        case n: Number => Real(n)
        case _ => throw NumberException(s"Real.createFromNumberField: x is not a Number: $x")
    }
}