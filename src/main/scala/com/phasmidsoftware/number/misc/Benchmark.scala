package com.phasmidsoftware.number.misc

object Benchmark {

    implicit class Repetitions(n: Int) {

      def times[R](x: => R): (R, Double) = {
        val start = System.nanoTime()
        for (_ <- 1 to n) x
        val r = x
        val stop = System.nanoTime()
        (r, 1E-6 * (stop - start) / n)
      }
    }

}
