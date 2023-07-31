/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.java

import com.phasmidsoftware.number.core.FP.getOrThrow
import com.phasmidsoftware.number.core.{Number, NumberException}
import java.math.BigInteger

object NumberJ {

  /**
    * Method to convert a Java BigDecimal to a Number.
    *
    * @param x a java.math.BigDecimal.
    * @return a Number.
    */
  def bigDecimalToNumber(x: java.math.BigDecimal): Number = Number(x)

  /**
    * Method to convert a Number to a Java BigDecimal
    *
    * @param x a Number.
    * @return a java.math.BigDecimal.
    */
  def numberToBigDecimal(x: Number): java.math.BigDecimal = getOrThrow(x.toBigDecimal.map(_.bigDecimal), NumberException("Cannot convert $x to a BigDecimal"))

  /**
    * Method to convert a Java BigInteger to a Number.
    *
    * @param x a java.math.BigInteger.
    * @return a Number.
    */
  def bigIntegerToNumber(x: BigInteger): Number = Number(x)

  /**
    * Method to convert a Number to a Java BigInteger
    *
    * @param x a Number.
    * @return a java.math.BigInteger.
    */
  def numberToBigInteger(x: Number): BigInteger = getOrThrow(x.toBigInt.map(_.bigInteger), NumberException("Cannot convert $x to a BigInteger"))

  /**
    * Method to convert a Java Long to a Number.
    *
    * @param l a java.lang.Long.
    * @return a Number.
    */
  def longToNumber(l: java.lang.Long): Number = Number(BigInt(l))

  /**
    * Method to convert a Number to a Java Long.
    *
    * @param x a Number.
    * @return a java.lan.Long.
    */
  def numberToLong(x: Number): java.lang.Long = x.toLong match {
    case Some(l) => l
    case None => throw NumberException("Cannot convert $x to a Long")
  }

  /**
    * Method to convert a Java Double to a Number.
    *
    * @param x a java.lang.Double.
    * @return a Number.
    */
  def doubleToNumber(x: java.lang.Double): Number = Number(x)

  /**
    * Method to convert a Number to a Java Double.
    *
    * @param x a Number.
    * @return a java.lang.Double.
    */
  def numberToDouble(x: Number): java.lang.Double = x.toDouble match {
    case Some(d) => d
    case None => throw NumberException("Cannot convert $x to a Double")
  }


  /**
    * Method to convert a String to a Number.
    * The String may be in number form (e.g. "22/7") or a decimal number.
    *
    * @param s a String.
    * @return a Number.
    */
  def stringToNumber(s: String): Number = Number(s)

}
