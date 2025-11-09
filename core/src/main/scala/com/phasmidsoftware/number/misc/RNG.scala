package com.phasmidsoftware.number.misc

import scala.util.Random

/**
  * NOTE: I have no idea why I wrote this module here.
  *
  * @tparam T the underlying type of RNG.
  */
sealed trait RNG[T] {

  def next: RNG[T]

  def get: T

  def unit[U](u: U): RNG[U]

  def map[U](f: T => U): RNG[U] = flatMap(t => unit(f(t)))

  def flatMap[U](f: T => RNG[U]): RNG[U]

}

abstract class BaseRNG[T](val get: T) extends RNG[T] {
  def nextRandom: T

  def next: RNG[T] = unit(nextRandom)

  def flatMap[U](f: T => RNG[U]): RNG[U] = f(get)
}

case class RandomLong(value: Long) extends BaseRNG[Long](value) {
  self =>

  def nextRandom: Long = new Random(value).nextLong()

  def unit[U](u: U): RNG[U] = u match {
    case x: Long => new RandomLong(x).asInstanceOf[RNG[U]]
    case _ => new BaseRNG[U](u) {
      // TODO this is nonsense. I don't think we can really implement unit for RNG.
      def nextRandom: U = u

      def convertToLong(u: U): Long = u match {
        case w: String => w.toLong
        case x: Int => x.toLong
        case x => x.hashCode().toLong
      }

      def unit[V](v: V): RNG[V] = self.unit(convertToLong(u)).map(_ => v) // TODO this is nonsense
    }
  }
}

object RandomLong {
  def apply(seed: Long): RNG[Long] = new RandomLong(0L).unit(seed)
}