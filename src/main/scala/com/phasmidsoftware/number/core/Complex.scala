package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.FP.recover

/**
  * Trait which defines the behavior of a type of Field called a Complex.
  * A Complex is a Field and also supports the various methods defined below.
  *
  */
trait Complex extends Field {

  /**
    * Method to determine if this Complex is real-valued (i.e. the point lies on the real axis).
    *
    * @return
    */
  def isReal: Boolean

  /**
    * Method to determine the modulus of this Complex number.
    *
    * @return the modulus of this Complex.
    */
  def modulus: Number

  /**
    * Method to determine the argument (angle) of this Complex number.
    *
    * @return a Number (in radians).
    */
  def argument: Number

  /**
    * Rotate this Complex number by pi/2 counter-clockwise (i.e. multiply by i).
    *
    * @return the value of this * i.
    */
  def rotate: Complex

  /**
    * Method to add this Complex to another Complex.
    *
    * @param complex the other Complex.
    * @return the sum of the Complexes.
    */
  def doAdd(complex: Complex): Complex

  /**
    * Method to multiply this Complex by another Complex.
    *
    * @param complex the other Complex.
    * @return the product of the Complexes.
    */
  def doMultiply(complex: Complex): Complex

  /**
    * Method to scale this Complex by a Number.
    *
    * @param n the Number.
    * @return a Complex with the same argument as this but a different magnitude.
    */
  def numberProduct(n: Number): Complex

  /**
    * Method to determine the conjugate of this Complex number.
    *
    * @return the conjugate of this Complex.
    */
  def conjugate: Complex
}

/**
  * Companion object to Complex.
  *
  */
object Complex {
  /**
    * i in Cartesian form.
    */
  val i: ComplexCartesian = ComplexCartesian(0, 1)
  /**
    * Unit Complex in Cartesian form
    */
  val unit: ComplexCartesian = ComplexCartesian(1, 0)

  def convertToPolar(c: ComplexCartesian): BaseComplex = {
    // CONSIDER can we improve upon this? Basically, we should only need MonadicOperationAtan.
    val ro: Option[Field] = for (p <- ((Literal(c.x) * c.x) plus (Literal(c.y) * c.y)).materialize.asNumber; z = p.sqrt) yield z
    val z: Field = recover(ro, ComplexException(s"logic error: convertToPolar1: $c"))
    apply(z, c.x atan c.y, ComplexPolar.apply(_, _, 1), ComplexException(s"logic error: convertToPolar2: $c"))
  }

  def convertToCartesian(c: ComplexPolar): BaseComplex =
    apply(c.r multiply c.theta.cos, c.r multiply c.theta.sin, ComplexCartesian.apply, ComplexException(s"logic error: convertToCartesian: $c"))

  def apply(a: Field, b: Field, f: (Number, Number) => BaseComplex, x: => ComplexException): BaseComplex =
    recover(for (a <- a.asNumber; b <- b.asNumber) yield f(a, b), x)

  def apply(x: Number): BaseComplex = ComplexCartesian(x, Number.zero)
}
