package com.phasmidsoftware.number.core.misc

object XXX extends App {

  /**
    * Periodic Cicadas: https://en.wikipedia.org/wiki/Periodical_cicadas
    *
    * @param next   year of the next emergence
    * @param period years between emergences
    */
  case class Cicada(next: Int, period: Int) extends (Int => Boolean) {
    /**
      * Determine if year x is an emergence year
      *
      * @param x the year given
      * @return true if this Cicada brood will emerge in year x
      */
    def apply(x: Int): Boolean = (x - next) % period == 0
  }

  /**
    * This app prints the next n years in which Broods XIX and XIV will emerge together.
    */
  val xix = Cicada(2024, 13) // Brood XIX (most of the Southern USA)
  val xiv = Cicada(2025, 17) // Brood XIV (most of SE USA as far North as Massachusetts)
  val n = 10

  def coemergence(x: Int): Boolean = xix(x) && xiv(x)

  val emergences = LazyList.from(2021).filter(coemergence).take(n) // TODO

  println(emergences.mkString(","))

  type X = (Int, Double)

}
