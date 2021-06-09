package com.phasmidsoftware.number.problems

import scala.annotation.tailrec
import scala.language.postfixOps

object PotHoles {

  def cost(ps: Seq[Double], cs: Seq[Double]): Double = {
    @tailrec
    def inner(r: Double, w: Seq[(Double, Double)]): Double =
      if (w.isEmpty) r
      else {
        val xs = w map { case (a, b) => math.abs(a - b) }
        val (z, i) = (xs zipWithIndex).minBy { case (x, _) => x }
        inner(r + z, (w take i) ++ (w drop i + 1))
      }

    inner(0, ps.sorted zip cs.sorted)
  }
}