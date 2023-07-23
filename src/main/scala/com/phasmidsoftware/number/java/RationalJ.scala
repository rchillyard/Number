package com.phasmidsoftware.number.java

import com.phasmidsoftware.number.core.Rational
import java.math.BigInteger

object RationalJ {

  /**
    * Method to convert a Java BigDecimal to a Rational.
    *
    * @param x a java.math.BigDecimal.
    * @return a Rational.
    */
  def bigDecimalToRational(x: java.math.BigDecimal): Rational = Rational(x)

  /**
    * Method to convert a Rational to a Java BigDecimal
    *
    * @param r a Rational.
    * @return a java.math.BigDecimal.
    */
  def rationalToBigDecimal(r: Rational): java.math.BigDecimal =
    r.toBigDecimal.getOrElse(r.forceToBigDecimal).bigDecimal

  /**
    * Method to convert a Java BigInteger to a Rational.
    *
    * @param x a java.math.BigInteger.
    * @return a Rational.
    */
  def bigIntegerToRational(x: BigInteger): Rational = Rational(x)

  /**
    * Method to convert a Rational to a Java BigInteger
    *
    * @param r a Rational.
    * @return a java.math.BigInteger.
    */
  def rationalToBigInteger(r: Rational): BigInteger = r.toBigInt.bigInteger

  /**
    * Method to convert a Java Long to a Rational.
    *
    * @param l a java.lang.Long.
    * @return a Rational.
    */
  def longToRational(l: java.lang.Long): Rational = Rational(l)

  /**
    * Method to convert a Rational to a Java Long.
    *
    * @param r a Rational.
    * @return a java.lan.Long.
    */
  def rationalToLong(r: Rational): java.lang.Long = r.toLong

  /**
    * Method to convert a Java Double to a Rational.
    *
    * @param x a java.lang.Double.
    * @return a Rational.
    */
  def doubleToRational(x: java.lang.Double): Rational = Rational(x)

  /**
    * Method to convert a Rational to a Java Double.
    *
    * @param r a Rational.
    * @return a java.lang.Double.
    */
  def rationalToDouble(r: Rational): java.lang.Double = r.toDouble

  /**
    * Method to convert a String to a Rational.
    * The String may be in rational form (e.g. "22/7") or a decimal number.
    *
    * @param s a String.
    * @return a Rational.
    */
  def stringToRational(s: String): Rational = Rational(s)

}
