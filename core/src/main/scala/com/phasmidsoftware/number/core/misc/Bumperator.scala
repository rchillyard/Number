/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.misc

import com.phasmidsoftware.number.core.numerical.CoreException
import scala.annotation.tailrec

/**
  * Bumperator is a concrete `Iterator` that selectively filters
  * out elements based on a user-supplied function. Elements are compared
  * using a binary predicate, and if the function returns true for a pair
  * of elements, both elements are skipped in the resulting iteration.
  *
  * The name comes from the rowing races at Oxford and Cambridge whereby
  * when a boat "bumps" the boat in front, both withdraw for the day but return
  * the following day with their former order reversed.
  * See [[https://en.wikipedia.org/wiki/Bumps_race Bumps race]]
  *
  * TODO move this into `expression` module.
  *
  * @tparam T the type of each element in the iterator
  * @param xs an input iterator providing elements to iterate over
  * @param f  a binary predicate function that determines whether two consecutive
  *           elements should be "bumped out" (skipped)
  *
  * @note Throws java.lang.IllegalArgumentException if the input iterator is empty
  */
case class Bumperator[T](xs: Iterator[T])(f: (T, T) => Boolean) extends Iterator[T] {
  // NOTE: These mutable variables represent the state of this Bumperator,
  // that's to say, the pipeline of elements.
  var maybeCurrent: Option[T] = None
  var maybeFollowing: Option[T] = None

  /**
    * Checks if there are more elements available in the iterator.
    *
    * @return true if there is a next element to iterate over, false otherwise
    */
  def hasNext: Boolean = {
    fillThePipeline()
    maybeCurrent.isDefined
  }

  /**
    * Retrieves the next element in the iterator. This method advances the iterator
    * by returning the current element and setting up the next one. It is expected
    * to throw an exception if no more elements are available in the iteration.
    *
    * @return the next element of type T in the iterator
    * @note Throws java.util.NoSuchElementException if the iterator has no more elements
    */
  def next(): T = {
    // NOTE we expect to throw an exception here if maybeCurrent is None
    val result = FP.recover(maybeCurrent, CoreException("Bumperator.next: logic error: no more elements"))
    maybeCurrent = maybeFollowing
    maybeFollowing = None
    result
  }

  /**
    * Recursively fills the pipeline with the next elements from the iterator.
    * If either/both `maybeCurrent` and `maybeFollowing` are empty, it attempts to populate them with elements
    * from the iterator (`xs`). If a function `f` evaluates to true for the current and following elements,
    * they are cleared, and the method recurses to possibly load subsequent elements.
    *
    * The method stops when no further elements can be fetched or when `f` does not evaluate to true.
    *
    * @return Unit, the method performs its operations recursively without returning a concrete value.
    */
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

/**
  * The `Bumperator` companion object provides a factory method to create an instance of the `Bumperator` iterator.
  * It applies a filtering function to consecutive elements of a sequence, ensuring only those satisfying the
  * provided condition remain in the iteration pipeline.
  */
object Bumperator {
  /**
    * Creates an iterator that filters elements from the given sequence based on
    * a binary predicate function.
    * The function determines whether consecutive elements in the sequence should be excluded.
    *
    * @param xs the input sequence to be processed
    * @param f  a binary predicate function that takes two elements of type `T`
    *           and evaluates whether they should be excluded from the iteration
    * @return a `Bumperator[T]` containing elements of the sequence that have not
    *         been skipped.
    */
  def apply[T](xs: Seq[T])(f: (T, T) => Boolean): Iterator[T] =
    new Bumperator(xs.iterator)(f: (T, T) => Boolean)
}