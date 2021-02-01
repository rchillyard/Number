package com.phasmidsoftware.number.core

import scala.util.{Failure, Success, Try}

/**
  * This trait defines a set of Matchers which operate in a parallel fashion to the Parsers of the Scala
  * Parser Combinator library.
  *
  * CONSIDER using ^^ in more situations.
  */
trait Matchers {

  sealed trait MatchResult[R] {
    def successful: Boolean

    def isEmpty: Boolean = !successful

    def success[S](s: S): MatchResult[S] = Match(s)

    def get: R

    def map[S](f: R => S): MatchResult[S] = flatMap(r => success(f(r)))

    def flatMap[S](f: R => MatchResult[S]): MatchResult[S]

    def |(m: => Matcher[Any, R]): MatchResult[R]

    def &[S](m: => Matcher[R, S]): MatchResult[S]
  }

  case class Match[R](r: R) extends MatchResult[R] {
    def successful: Boolean = true

    def get: R = r

    def |(m: => Matcher[Any, R]): MatchResult[R] = this

    def &[S](m: => Matcher[R, S]): MatchResult[S] = m(get)

    def flatMap[S](f: R => MatchResult[S]): MatchResult[S] = f(r)
  }

  case class Miss[T, R](t: T) extends MatchResult[R] {
    def successful: Boolean = false

    override def map[S](f: R => S): MatchResult[S] = Miss(t)

    def |(m: => Matcher[Any, R]): MatchResult[R] = m(t)

    def &[S](m: => Matcher[R, S]): MatchResult[S] = Miss(t)

    def get: R = throw MatcherException("cannot call get on Miss")

    def flatMap[S](f: R => MatchResult[S]): MatchResult[S] = throw MatcherException("cannot call flatMap on Miss")
  }

  def Matcher[T, R](f: T => MatchResult[R]): Matcher[T, R] = (t: T) => f(t)

  def success[T, R](r: R): Matcher[T, R] = Matcher(_ => Match(r))

  def fail[T, R]: Matcher[T, R] = Matcher(e => Miss(e))

  abstract class Matcher[T, R] extends (T => MatchResult[R]) {
    private var name: String = ""

    def named(n: String): this.type = {
      name = n; this
    }

    override def toString = s"Matcher ($name)"

    def map[S](f: R => S): Matcher[T, S] = Matcher { in => this (in) map f }

    /**
      * Method to transform a MatchResult.
      *
      * `p ^^ f` succeeds if `p` succeeds; it returns `f` applied to the result of `p`.
      *
      * @param f a function that will be applied to this matcher's result (see `map` in `MatchResult`).
      * @return a parser that has the same behaviour as the current matcher, but whose result is
      *         transformed by `f`.
      */
    def ^^[S](f: R => S): Matcher[T, S] = map(f).named(toString + "^^")

    /** Returns a matcher that optionally matches what this parser parses.
      *
      * @return opt(this)
      */
    def ? : Matcher[T, Option[R]] = opt(this)

    /**
      * Method to combine Matchers in the sense that, if this fails, then we try to match using m.
      *
      * @param m the alternative Matcher.
      * @return a Matcher[T, R] which will match either on this or on m.
      */
    def |(m: Matcher[T, R]): Matcher[T, R] = Matcher(e => match2Any(this, m)((e, e)))

    /**
      * Method to combine Matchers in the sense that, when this successfully matches a T, resulting in an R,
      * then m is invoked on the result, such that if it is successful, we return an S..
      *
      * @param m the alternative Matcher.
      * @tparam S the underlying type of the resulting Matcher.
      * @return a Matcher[T, S] which will match in composition on both this and m.
      */
    def &[S](m: Matcher[R, S]): Matcher[T, S] = Matcher(e => this (e) & m)

    /**
      * Matcher which always succeeds but whose result is based on a Try[R].
      *
      * @return Matcher[T, Option of R]
      */
    def trial: Matcher[T, Try[R]] = Matcher(e =>
      Try(this (e)) match {
        case Success(Match(z)) => Match(Success(z))
        case Success(Miss(_)) => Match(Failure(MatcherException("Miss")))
        case Failure(x) => Match(Failure(x))
      }
    )
  }

  /**
    * Matcher which always succeeds but whose result is based on an Option[R].
    *
    * @return Matcher[T, Option of R]
    */
  def opt[T, R](m: Matcher[T, R]): Matcher[T, Option[R]] = m ^^[Option[R]] (x => Some(x)) | success(None)

  /**
    * Method to match any element of a Tuple2.
    *
    * @param m1 the Matcher corresponding to the first element.
    * @param m2 the Matcher corresponding to the second element.
    * @tparam T1 the input type for the first Matcher.
    * @tparam T2 the input type for the second Matcher.
    * @tparam R  the MatchResult type.
    * @return a Matcher[(T1, T2), R] that matches at least one of the elements of the given tuple.
    */
  def match2Any[T1, T2, R](m1: Matcher[T1, R], m2: => Matcher[T2, R]): Matcher[(T1, T2), R] =
    Matcher { case (t1, t2) => matchProduct2Any(m1, m2)(->.apply)(t1 -> t2) }

  /**
    * Method to match any element of a Product with two elements.
    *
    * @param m1 the Matcher corresponding to the first element.
    * @param m2 the Matcher corresponding to the second element.
    * @param f  a function which takes a (t1, t2) and returns a P.
    * @tparam T1 the input type for the first Matcher.
    * @tparam T2 the input type for the second Matcher.
    * @tparam R  the MatchResult type.
    * @tparam P  the input type.
    * @return a Matcher[P, R] that matches at least one of the elements of the given tuple.
    */
  def matchProduct2Any[T1, T2, R, P <: Product](m1: Matcher[T1, R], m2: => Matcher[T2, R])(f: (T1, T2) => P): Matcher[P, R] = Matcher {
    p =>
      m1(p.productElement(0).asInstanceOf[T1]) match {
        case result@Match(_) => result
        case Miss(_) => m2(p.productElement(1).asInstanceOf[T2])
      }
  }

  //  /**
  //    * Method to match any element of a Product with two elements.
  //    *
  //    * @param m1 the Matcher corresponding to the first element.
  //    * @param m2 the Matcher corresponding to the second element.
  //    * @param m3 the Matcher corresponding to the third element.
  //    * @param f  a function which takes a (t1, t2, t3) and returns a P.
  //    * @tparam T1 the input type for the first Matcher.
  //    * @tparam T2 the input type for the second Matcher.
  //    * @tparam T3 the input type for the third Matcher.
  //    * @tparam R  the MatchResult type.
  //    * @tparam P  the input type.
  //    * @return a Matcher[P, R] that matches at least one of the elements of the given tuple.
  //    */
  //  def matchProduct3Any[T1, T2, T3, R, P <: Product](m1: Matcher[T1, R], m2: => Matcher[T2, R], m3: => Matcher[T3, R])(f: (T1, T2, T3) => P): Matcher[P, R] = Matcher {
  //    p =>
  //      m1(p.productElement(0).asInstanceOf[T1]) match {
  //        case result@Match(_) => result
  //        case Miss(_) => m2(p.productElement(1).asInstanceOf[T2])
  ////          val tuple: (T2, T3) = (p.productElement(1).asInstanceOf[T2], p.productElement(2).asInstanceOf[T3])
  ////          matchProduct2Any[T2, T3, R, (T2, T3)](m2, m3)(g)(tuple)
  //      }
  //
  //    //      p1 <- readCell[T, P1](wo, row, columns)(f1)
  //    //    t <- cellParser1(construct.curried(p1), fs).parse(wo, row, columns)
  //
  //  }

  //  /**
  //    * Method to match any element of a Tuple2.
  //    *
  //    * @param m1 the Matcher corresponding to the first element.
  //    * @param m2 the Matcher corresponding to the second element.
  //    * @tparam T1 the input type for the first Matcher.
  //    * @tparam T2 the input type for the second Matcher.
  //    * @tparam R  the MatchResult type.
  //    * @return a Matcher[(T1, T2), R] that matches at least one of the elements of the given tuple.
  //    */
  //  def match3Any[T1, T2, T3, R](m1: Matcher[T1, R], m2: => Matcher[T2, R]): Matcher[(T1, T2), R] = Matcher {
  //    case (t1, t2) =>
  //      m1(t1) match {
  //        case result@Match(_) => result
  //        case Miss(_) => m2(t2)
  //      }
  //  }

  /**
    * Method to match any element of a Tuple2.
    *
    * @param m1 the Matcher corresponding to the first element.
    * @param m2 the Matcher corresponding to the second element.
    * @tparam T1 the input type for the first Matcher.
    * @tparam T2 the input type for the second Matcher.
    * @tparam R1 the MatchResult type for m1.
    * @tparam R2 the MatchResult type for m2.
    * @return a Matcher[(T1, T2), (R1, R2)] that matches at least one of the elements of the given tuple.
    */
  def match2All[T1, T2, R1, R2](m1: Matcher[T1, R1], m2: => Matcher[T2, R2]): Matcher[(T1, T2), (R1, R2)] = Matcher {
    case (t1, t2) =>
      m1(t1) match {
        case Match(r1) => m2(t2) match {
          case Match(r2) => Match((r1, r2))
          case Miss(_) => Miss((t1, t2))
        }
        case Miss(_) => Miss((t1, t2))
      }
  }
}

case class MatcherException(msg: String) extends Exception(msg)