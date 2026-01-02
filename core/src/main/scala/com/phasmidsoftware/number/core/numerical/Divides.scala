/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.core.numerical

/**
  * Object containing an implicit class which can be used to determine factorization.
  */
object Divides {

  /**
    * Implicit class to determine if a number (x) is a factor of another number (y).
    *
    * @param x an Int.
    */
  //noinspection NoTargetNameAnnotationForOperatorLikeDefinition
  implicit class IntDivides(x: Int) {
    /**
      * Method to test if x divides y.
      *
      * @param y an Int.
      * @return true if x divides y.toLong.
      */
    def |>(y: Int): Boolean = |>(y.toLong)

    /**
      * Method to test if x divides y.
      *
      * @param y a Long.
      * @return true if x divides y.
      */
    def |>(y: Long): Boolean = y % x == 0

    /**
      * Method to test if x divides y.
      *
      * @param y a BigInt.
      * @return true if x divides y.
      */
    def |>(y: BigInt): Boolean = y % x == 0

    /**
      * Method to test if x does not divide y.
      *
      * TESTME and all other not methods.
      *
      * @param y an Int.
      * @return false if x divides y.toLong.
      */
    def !|(y: Int): Boolean = !(x |> y)

    /**
      * Method to test if x does not divide y.
      *
      * @param y a Long.
      * @return false if x divides y.
      */
    def !|(y: Long): Boolean = !(x |> y)

    /**
      * Method to test if x does not divide y.
      *
      * @param y a BigInt.
      * @return false if x divides y.
      */
    def !|(y: BigInt): Boolean = !(x |> y)
  }

  /**
    * Implicit class to determine if a number (x) is a factor of another number (y).
    * NOTE that for potential factors larger than an Int, you should just use % directly.
    *
    * @param x an Int.
    */
  //noinspection NoTargetNameAnnotationForOperatorLikeDefinition
  implicit class BigIntDivides(x: BigInt) {
    /**
      * Method to test if x divides y.
      *
      * @param y an Int.
      * @return true if x divides y.toLong.
      */
    def |>(y: Int): Boolean = |>(y.toLong)

    /**
      * Method to test if x divides y.
      *
      * @param y a Long.
      * @return true if x divides y.
      */
    def |>(y: Long): Boolean = y % x == 0

    /**
      * Method to test if x divides y.
      *
      * @param y a BigInt.
      * @return true if x divides y.
      */
    def |>(y: BigInt): Boolean = y % x == 0

    /**
      * Method to test if x does not divide y.
      *
      * @param y an Int.
      * @return false if x divides y.toLong.
      */
    def !|(y: Int): Boolean = !(x |> y)

    /**
      * Method to test if x does not divide y.
      *
      * @param y a Long.
      * @return false if x divides y.
      */
    def !|(y: Long): Boolean = !(x |> y)

    /**
      * Method to test if x does not divide y.
      *
      * @param y a BigInt.
      * @return false if x divides y.
      */
    def !|(y: BigInt): Boolean = !(x |> y)
  }

}
