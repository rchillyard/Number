package com.phasmidsoftware.number.core

/**
  * Trait which defines the behavior of a type of Field called a Complex.
  */
trait Complex extends Field {

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
    * Method to determine the complement of this Complex number.
    *
    * @return the complement of this Complex.
    */
  def complement: Complex
}

object Complex {
  val i: ComplexCartesian = ComplexCartesian(0, 1)
  val unit: ComplexCartesian = ComplexCartesian(1, 0)

  def convertToPolar(c: ComplexCartesian): BaseComplex = {
    val ro: Option[Field] = for (p <- ((Literal(c.x) * c.x) plus (Literal(c.y) * c.y)).materialize.asNumber; z = p.sqrt) yield z
    val z: Field = Field.recover(ro, ComplexException(s"logic error: convertToPolar1: $c"))
    apply(z, c.x atan c.y, ComplexPolar.apply, ComplexException(s"logic error: convertToPolar2: $c"))
  }

  def convertToCartesian(c: ComplexPolar): BaseComplex =
    apply(c.r multiply c.theta.cos, c.r multiply c.theta.sin, ComplexCartesian.apply, ComplexException(s"logic error: convertToCartesian: $c"))


  def apply(a: Field, b: Field, f: (Number, Number) => BaseComplex, x: ComplexException): BaseComplex =
    Field.recover(for (a <- a.asNumber; b <- b.asNumber) yield f(a, b), x)

  def apply(x: Number): BaseComplex = ComplexCartesian(x, Number.zero)

}
