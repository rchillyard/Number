package com.phasmidsoftware.number.core


trait Matchers {

  sealed trait MatchResult[R] {
    def success: Boolean
  }

  case class Match[R](r: R) extends MatchResult[R] {
    def success: Boolean = true
  }

  case class Miss[T, R](t: T) extends MatchResult[R] {
    def success: Boolean = false
  }

  def Matcher[T, R](f: T => MatchResult[R]): Matcher[T, R] = (t: T) => f(t)

  def success[T, R](r: R): Matcher[T, R] = Matcher(_ => Match(r))

  def fail[T, R]: Matcher[T, R] = Matcher(e => Miss(e))

  abstract class Matcher[T, R] extends (T => MatchResult[R]) {

    /**
      * Method to combine Matchers in the sense that, if this fails, then we try to match using m.
      *
      * @param m the alternative Matcher.
      * @return a Matcher[T, R] which will match either on this or on m.
      */
    def |(m: Matcher[T, R]): Matcher[T, R] = Matcher(e =>
      this (e) match {
        case v@Match(_) => v
        case Miss(_) => m(e)
      }
    )

    /**
      * Method to combine Matchers in the sense that, when this successfully matches a T, resulting in an R,
      * then m is invoked on the result, such that if it is successful, we return an S..
      *
      * @param m the alternative Matcher.
      * @tparam S the underlying type of the resulting Matcher.
      * @return a Matcher[T, S] which will match in composition on both this and m.
      */
    def &[S](m: Matcher[R, S]): Matcher[T, S] = Matcher(e =>
      this (e) match {
        case Match(x) => m(x)
        case Miss(t) => Miss(t)
      }
    )
  }
}
