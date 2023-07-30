package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.FP.recover
import com.phasmidsoftware.number.parse.ComplexParser
import scala.util._

/**
  * Trait which defines the behavior of a type of Field called a Complex.
  * A Complex is a Field and also supports the various methods defined below.
  *
  */
trait Complex extends Field {

  /**
    * Method to determine if this Complex is real-valued (i.e. the point lies on the real axis).
    *
    * @return true if the imaginary part is zero (argument is a multiple of pi).
    */
  def isReal: Boolean

  /**
    * Method to determine if this Complex is imaginary-valued (i.e. the point lies on the imaginary axis).
    *
    * @return true if the real part is zero (argument is a multiple of pi/2).
    */
  def isImaginary: Boolean

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
  def create(ro: Option[Number], so: Option[String], io: Option[Number]): Complex = {
    val real = ro getOrElse Number.zero
    val s = so getOrElse "+"
    val i = io getOrElse Number.zero
    val imag = s match {
      case "-" => i.makeNegative
      case _ => i
    }
    i.factor match {
      case Radian => ComplexPolar(real, imag)
      case _ => ComplexCartesian(real, imag)
    }
  }

  /**
    * i in Cartesian form.
    */
  val i: ComplexCartesian = ComplexCartesian(0, 1)
  /**
    * Unit Complex in Cartesian form
    */
  val unit: ComplexCartesian = ComplexCartesian(1, 0)

  /**
    * Method to convert a ComplexCartesian into a ComplexPolar.
    *
    * TODO: determine what to do about 0,0
    *
    * @param c the cartesian complex.
    * @return a Complex.
    */
  def convertToPolar(c: ComplexCartesian): Complex = {
    // CONSIDER can we improve upon this? Basically, we should only need MonadicOperationAtan.
    val ro: Option[Number] = for (p <- ((Literal(c.x) * Real(c.x)) plus (Literal(c.y) * Real(c.y))).materialize.asNumber; z = p.sqrt) yield z
    val z: Number = recover(ro, ComplexException(s"logic error: convertToPolar1: $c"))
    ComplexPolar(z, c.x atan c.y, 1)
  }

  /**
    * Method to convert a ComplexPolar into a ComplexCartesian.
    *
    * @param c the ComplexPolar.
    * @return the equivalent ComplexCartesian.
    */
  def convertToCartesian(c: ComplexPolar): BaseComplex =
    apply(Real(c.r doMultiply c.theta.cos), Real(c.r doMultiply c.theta.sin), ComplexCartesian.apply, ComplexException(s"logic error: convertToCartesian: $c"))

  /**
    * Method to construct a Complex from two fields, a (Number,Number)=>BaseComplex function, and an exception.
    *
    * @param a the first field.
    * @param b the second field.
    * @param f the function.
    * @param x the exception (call-by-name).
    * @return a new BaseComplex.
    */
  def apply(a: Field, b: Field, f: (Number, Number) => BaseComplex, x: => ComplexException): BaseComplex =
    recover(for (u <- a.asNumber; v <- b.asNumber) yield f(u, v), x)

  /**
    * Method to construct a Complex from a Field, a Number, a (Number,Number)=>BaseComplex function, and an exception.
    *
    * @param a the field.
    * @param b the number.
    * @param f the function.
    * @param x the exception (call-by-name).
    * @return a new BaseComplex.
    */
  def apply(a: Field, b: Number, f: (Number, Number) => BaseComplex, x: => ComplexException): BaseComplex =
    recover(for (u <- a.asNumber) yield f(u, b), x)

  /**
    * Method to construct a Complex from a Field, a Number, a (Number,Number)=>BaseComplex function, and an exception.
    *
    * @param a the number.
    * @param b the field.
    * @param f the function.
    * @param x the exception (call-by-name).
    * @return a new BaseComplex.
    */
  def apply(a: Number, b: Field, f: (Number, Number) => BaseComplex, x: => ComplexException): BaseComplex =
    recover(for (u <- b.asNumber) yield f(a, u), x)

  def apply(x: Number): BaseComplex = ComplexCartesian(x, Number.zero)

  def parse(w: String): Try[Complex] = ComplexParser.parse(w)


  /**
    * Implicit class RationalHelper to allow definition of Rationals by Strings of the form r"n/d".
    *
    * @param sc a StringContext.
    */
  implicit class ComplexHelper(val sc: StringContext) extends AnyVal {
    def C(args: Any*): Complex = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val sb = new StringBuffer()
      while (strings.hasNext) {
        val s = strings.next()
        if (s.isEmpty) {
          if (expressions.hasNext)
            sb.append(expressions.next())
          else
            throw NumberException("C: logic error: missing expression")
        }
        else
          sb.append(s)
      }
      if (expressions.hasNext)
        throw NumberException(s"C: ignored: ${expressions.next()}")
      else
        Complex.parse(sb.toString) match {
          case Success(value) => value
          case Failure(x) => throw x
        }
    }
  }
}
