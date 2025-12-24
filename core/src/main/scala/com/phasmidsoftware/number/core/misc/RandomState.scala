package com.phasmidsoftware.number.core.misc

import scala.util.Random


/**
  * Monadic trait which defines a random-state.
  *
  * Created by scalaprof on 9/24/16.
  *
  * @tparam T the underlying type of this random state, i.e. the type of the result of calling get
  */
trait RandomState[T] {
  /**
    * @return the next random state in the pseudo-random series.
    */
  def next: RandomState[T]

  /**
    * @return the value of this random state.
    */
  def get: T

  /**
    * Method to map this random state into another random state.
    *
    * @param f the function to map a T value into a U value.
    * @tparam U the underlying type of the resulting random state.
    * @return a new random state.
    */
  def map[U](f: T => U): RandomState[U]

  /**
    * Method to flatMap this random state into another random state.
    *
    * @param f the function to map a T value into a RandomState[U] value.
    * @tparam U the underlying type of the resulting random state.
    * @return a new random state.
    */
  def flatMap[U](f: T => RandomState[U]): RandomState[U] = f(get)

  /**
    * @return a LazyList of T values.
    */
  def lazyList: LazyList[T] = LazyList.cons[T](get, next.lazyList)
}

/**
  * A concrete implementation of RandomState based on the Java random number generator
  *
  * @param n the random Long that characterizes this random state
  * @param g the function which maps a Long value into a T
  * @tparam T the underlying type of this random state, i.e. the type of the result of calling get
  */
case class RandomState_Java[T](n: Long)(g: Long => T) extends RandomState[T] {
  def next: RandomState[T] = RandomState_Java[T](new Random(n).nextLong())(g)

  def get: T = g(n)

  def map[U](f: T => U): RandomState[U] = RandomState_Java[U](n)(g andThen f)
}

object RandomState {
  def applySeeded[T](n: Long)(g: Long => T): RandomState[T] = new RandomState_Java[T](new Random(n).nextLong())(g)

  def apply[T](g: Long => T): RandomState[T] = applySeeded(System.currentTimeMillis())(g)

  def intRandomStateBoundedSeeded(m: Int)(n: Long): RandomState[Int] = applySeeded[Int](n)(x => (x & 0x7FFFFFFF).toInt % m)

  def intRandomStateBounded(m: Int): RandomState[Int] = intRandomStateBoundedSeeded(m)(System.currentTimeMillis())

  def intRandomState(n: Long): RandomState[Int] = applySeeded[Int](n)(x => (x & 0xFFFFFFFF).toInt)

  def intRandomState: RandomState[Int] = intRandomState(System.currentTimeMillis())

  def longRandomState(n: Long): RandomState[Long] = applySeeded[Long](n)(identity)

  def longRandomState: RandomState[Long] = longRandomState(System.currentTimeMillis())

  def bigIntRandomState(n: Long): RandomState[BigInt] = applySeeded[BigInt](n)(x => BigInt(x))

  def bigIntRandomState: RandomState[BigInt] = bigIntRandomState(System.currentTimeMillis())
}
