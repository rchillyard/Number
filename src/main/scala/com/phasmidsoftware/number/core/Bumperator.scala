package com.phasmidsoftware.number.core

import scala.annotation.tailrec

/**
 * Bumperator is an implementation of an Iterator that selectively filters
 * out elements based on a user-supplied function. Elements are compared
 * using a binary predicate, and if the function returns true for a pair
 * of elements, both elements are skipped in the resulting iteration.
 *
 * The name comes from the rowing races at Oxford (and perhaps Cambridge) whereby
 * when a boat "bumps" the boat in front, both withdraw for the day but return
 * the following day with the order reversed.
 *
 * @tparam T the type of each element in the iterator
 * @param xs an input iterator providing elements to iterate over
 * @param f  a binary predicate function that determines whether two consecutive
 *           elements should be "bumped out" (skipped)
 * @throws IllegalArgumentException if the input iterator is empty
 */
case class Bumperator[T](xs: Iterator[T])(f: (T, T) => Boolean) extends Iterator[T] {
  var maybeCurrent: Option[T] = None
  var maybeFollowing: Option[T] = None

  def hasNext: Boolean = {
    fillThePipeline()
    maybeCurrent.isDefined
  }

  def next(): T = {
    val result = maybeCurrent.get // NOTE we expect to throw an exception here if maybeCurrent is None
    maybeCurrent = maybeFollowing
    maybeFollowing = None
    result
  }

  @tailrec
  private def fillThePipeline(): Unit = {
    if (maybeCurrent.isEmpty && xs.hasNext)
      maybeCurrent = Some(xs.next())
    if (maybeFollowing.isEmpty && xs.hasNext)
      maybeFollowing = Some(xs.next())
    (for (a <- maybeCurrent; b <- maybeFollowing) yield f(a, b)) match {
      case Some(true) =>
        maybeCurrent = None
        maybeFollowing = None
        fillThePipeline()
      case _ =>
      // Do nothing
    }
  }
}

object Bumperator {
  def apply[T](xs: Seq[T])(f: (T, T) => Boolean): Iterator[T] =
  new Bumperator(xs.iterator)(f: (T, T) => Boolean)
}