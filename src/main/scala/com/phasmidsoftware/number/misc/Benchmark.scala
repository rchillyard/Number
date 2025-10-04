package com.phasmidsoftware.number.misc

/**
  * An object providing benchmarking utilities for measuring the execution time of code snippets.
  * It includes an implicit class that extends the functionality of the `Int` type,
  * allowing expressions to be evaluated multiple times with performance metrics.
  */
object Benchmark {

  /**
    * An implicit class that adds additional functionality to the `Int` type,
    * allowing for repeated evaluation of expressions along with performance measurement.
    * This class is designed for benchmarking purposes, enabling the user to
    * measure the mean execution time of an expression over the given number of repetitions.
    *
    * @param n the number of repetitions for the evaluation of the provided expression.
    */
  implicit class Repetitions(n: Int) {

    /**
      * Repeatedly evaluates the provided expression a specified number of times and measures the total time taken for all evaluations.
      * The method produces a tuple containing the result of the final evaluation and the average execution time in milliseconds.
      *
      * @param x the by-name parameter (a block of code) to be executed multiple times.
      * @tparam R the result type of the expression being evaluated.
      * @return a tuple containing:
      *         1. The result of the last evaluation of the expression.
      *            2. The average time in milliseconds for each evaluation.
      */
    def times[R](x: => R): (R, Double) = {
      val start = System.nanoTime()
      for (_ <- 1 to n) x
      val r = x
      val stop = System.nanoTime()
      (r, 1E-6 * (stop - start) / n)
    }
  }

}
