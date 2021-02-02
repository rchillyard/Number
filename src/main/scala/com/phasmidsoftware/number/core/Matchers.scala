package com.phasmidsoftware.number.core

import scala.util.{Failure, Success, Try}

/**
  * This trait defines a set of Matchers which operate in a parallel fashion to the Parsers of the Scala
  * Parser Combinator library.
  *
  * CONSIDER using ^^ in more situations.
  */
trait Matchers {

  /**
    * Trait to define the behavior of the result of a Match.
    *
    * @tparam R the type of the result.
    */
  sealed trait MatchResult[R] {
    /**
      * @return true if this is a Match
      */
    def successful: Boolean

    /**
      * @return false if this is a Match
      */
    def isEmpty: Boolean = !successful

    /**
      * "unit" method for a successful match.
      *
      * @param s the value of the result.
      * @tparam S the underlying type of the result.
      * @return a Match[S] with value s.
      */
    def success[S](s: S): MatchResult[S] = Match(s)

    /**
      * @return the result of the MatchResult.
      * @throws MatcherException if this is not a Match.
      */
    def get: R

    /**
      * Map method.
      *
      * @param f a function of R => S.
      * @tparam S the underlying type of the returned MatchResult.
      * @return MatchResult[S].
      */
    def map[S](f: R => S): MatchResult[S] = flatMap(r => success(f(r)))

    /**
      * FlatMap method.
      *
      * @param f a function of R => MatchResult[S].
      * @tparam S the underlying type of the returned MatchResult.
      * @return MatchResult[S].
      */
    def flatMap[S](f: R => MatchResult[S]): MatchResult[S]

    /**
      * Alternation method which takes a MatchResult as the alternative.
      *
      * @param r a MatchResult which will be used if this is empty.
      * @return a MatchResult[R].
      */
    def ||(r: => MatchResult[R]): MatchResult[R]

    /**
      * Alternation method which takes a Matcher as the alternative.
      * If this MatchResult is empty then return the value of m applied to the input.
      *
      * @param m a Matcher of Any to R.
      * @return a MatchResult[R].
      */
    def |(m: => Matcher[Any, R]): MatchResult[R]

    /**
      * Composition method.
      * All the results are combined into one result.
      *
      * @param s a MatchResult[S].
      * @tparam S the underlying type of s.
      * @return a MatchResult[(R,S)].
      */
    def &&[S](s: => MatchResult[S]): MatchResult[(R, S)]

    /**
      * Composition method.
      * If this MatchResult is successful then return the value of m applied to the result.
      *
      * @param m a Matcher of R to S.
      * @tparam S the underlying type of the returned value.
      * @return a MatchResult[S].
      */
    def &[S](m: => Matcher[R, S]): MatchResult[S]
  }

  /**
    * Successful match.
    *
    * @param r the result.
    * @tparam R the type of the result.
    */
  case class Match[R](r: R) extends MatchResult[R] {
    def successful: Boolean = true

    def get: R = r

    def ||(r: => MatchResult[R]): MatchResult[R] = this

    def |(m: => Matcher[Any, R]): MatchResult[R] = this

    def &&[S](s: => MatchResult[S]): MatchResult[(R, S)] = s match {
      case Match(s) => Match(r -> s)
      case Miss(x) => Miss(x)
    }

    def &[S](m: => Matcher[R, S]): MatchResult[S] = m(get)

    def flatMap[S](f: R => MatchResult[S]): MatchResult[S] = f(r)
  }

  /**
    * Unsuccessful match.
    *
    * @param t the value that was not matched.
    * @tparam T the underlying type of t.
    * @tparam R the result-type of this.
    */
  case class Miss[T, R](t: T) extends MatchResult[R] {
    def successful: Boolean = false

    override def map[S](f: R => S): MatchResult[S] = Miss(t)

    def ||(r: => MatchResult[R]): MatchResult[R] = r

    def |(m: => Matcher[Any, R]): MatchResult[R] = m(t)

    def &&[S](s: => MatchResult[S]): MatchResult[(R, S)] = Miss(t)

    def &[S](m: => Matcher[R, S]): MatchResult[S] = Miss(t)

    def get: R = throw MatcherException("cannot call get on Miss")

    def flatMap[S](f: R => MatchResult[S]): MatchResult[S] = throw MatcherException("cannot call flatMap on Miss")
  }

  object MatchResult {
    def unroll3[R0, R1, R2](t: ((R0, R1), R2)): (R0, R1, R2) = (t._1._1, t._1._2, t._2)
  }

  /**
    * Method to create a Matcher, based on the given function f.
    *
    * @param f a T => MatchResult[R].
    * @tparam T the input type.
    * @tparam R the result type.
    * @return a Matcher[T, R] based on f.
    */
  def Matcher[T, R](f: T => MatchResult[R]): Matcher[T, R] = (t: T) => f(t)

  /**
    * Matcher which always succeeds and creates a Match with value r.
    *
    * @param r the predetermined result.
    * @tparam T the input type (input is ignored).
    * @tparam R the result type.
    * @return a Matcher[T, R]
    */
  def success[T, R](r: => R): Matcher[T, R] = Matcher(_ => Match(r))

  /**
    * Matcher which always fails and creates a Miss with the value tried.
    *
    * @tparam T the input type.
    * @tparam R the result type.
    * @return a Matcher[T, R]
    */
  def fail[T, R]: Matcher[T, R] = Matcher(t => Miss(t))

  /**
    * Matcher which always succeeds and whose input type and result type are the same.
    *
    * @tparam R both the input type and the result type.
    * @return a Matcher[R, R] which always succeeds.
    */
  def always[R]: Matcher[R, R] = Matcher(x => Match(x))

  /**
    * Matcher which succeeds if the input is equal to the given t0
    *
    * @param t the value which must be matched.
    * @tparam T the type of the input and result for Matcher.
    * @return a Matcher[T, T].
    */
  def matches[T](t: T): Matcher[T, T] = Matcher(x => if (x == t) Match(x) else Miss(x))

  /**
    * Abstract class Matcher.
    *
    * @tparam T the input type.
    * @tparam R the result type.
    */
  abstract class Matcher[T, R] extends (T => MatchResult[R]) {
    private var name: String = ""

    def named(n: String): this.type = {
      name = n
      this
    }

    override def toString = s"Matcher ($name)"

    def map[S](f: R => S): Matcher[T, S] = Matcher { t => this (t) map f }

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

    /**
      * Matcher which reverses the sense of this Matcher.
      *
      * @param r the default result value, only to be used in the even of a Miss.
      * @return a Matcher[T, R] which works in the opposite sense to this.
      */
    def !(r: => R): Matcher[T, R] = not(this, r)

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
    def |(m: Matcher[T, R]): Matcher[T, R] = Matcher(t => match2Any(this, m)((t, t)))

    /**
      * Method to combine Matchers in the sense that, when this successfully matches a T, resulting in an R,
      * then m is invoked on the result, such that if it is successful, we return an S..
      *
      * @param m the alternative Matcher.
      * @tparam S the underlying type of the resulting Matcher.
      * @return a Matcher[T, S] which will match in composition on both this and m.
      */
    def &[S](m: Matcher[R, S]): Matcher[T, S] = Matcher(t => this (t) & m)

    /**
      * Matcher which always succeeds but whose result is based on a Try[R].
      *
      * @return Matcher[T, Option of R]
      */
    def trial: Matcher[T, Try[R]] = Matcher(t =>
      Try(this (t)) match {
        case Success(Match(z)) => Match(Success(z))
        case Success(Miss(_)) => Match(Failure(MatcherException("Miss")))
        case Failure(x) => Match(Failure(x))
      }
    )
  }

  /**
    * Matcher which reverses the sense of this Matcher.
    *
    * @param m a Matcher[T, R]
    * @param r the default result value, only to be used in the even of a Miss.
    * @tparam T the input type of m.
    * @tparam R the result type of m.
    * @return a Matcher[T, R] which works in the opposite sense to this.
    */
  def not[T, R](m: Matcher[T, R], r: => R): Matcher[T, R] = t => m(t) match {
    case Match(_) => Miss(t)
    case Miss(_) => Match(r)
  }

  /**
    * Matcher which always succeeds but whose result is based on an Option[R].
    *
    * @param m a Matcher[T, R]
    * @tparam T the input type of m.
    * @tparam R the result type of m.
    * @return Matcher[T, Option of R]
    */
  def opt[T, R](m: Matcher[T, R]): Matcher[T, Option[R]] = m ^^[Option[R]] (r => Some(r)) | success(None)

  /**
    * Method to match a T, resulting in an R, where the match is indirectly determined by
    * the given lens function.
    *
    * @param m    a Matcher[U, R].
    * @param lens a function T => U.
    * @tparam U the type of a property that is matched by m.
    * @return a Matcher[T, R]
    */
  def having[T, U, R](m: Matcher[U, R])(lens: T => U): Matcher[T, R] = Matcher(t => m(lens(t)))

  /**
    * Method to match a T, resulting in an R, where the match is indirectly determined by
    * the given lens function.
    *
    * @param m    a Matcher[U, R].
    * @param lens a function T => U.
    * @tparam U the type of a property that is matched by m.
    * @return a Matcher[T, R]
    */
  def havingSame[T, U, R](m: Matcher[U, T])(lens: T => U): Matcher[T, R] = Matcher(
    t => {
      val result: MatchResult[T] = m(lens(t))
      result map (_ => t.asInstanceOf[R])
    }
  )

  /**
    * Method to match any element of a Tuple2.
    *
    * @param m0 the Matcher corresponding to the first element.
    * @param m1 the Matcher corresponding to the second element.
    * @tparam T0 the input type for the first Matcher.
    * @tparam T1 the input type for the second Matcher.
    * @tparam R  the MatchResult type.
    * @return a Matcher[(T0, T1), R] that matches at least one of the elements of the given tuple.
    */
  def match2Any[T0, T1, R](m0: Matcher[T0, R], m1: => Matcher[T1, R]): Matcher[(T0, T1), R] =
    Matcher { case (t0, t1) => matchProduct2Any(m0, m1)(->.apply)(t0 -> t1) }


  /**
    * Method to match any element of a Tuple3.
    *
    * @param m0 the Matcher corresponding to the first element.
    * @param m1 the Matcher corresponding to the second element.
    * @param m2 the Matcher corresponding to the third element.
    * @tparam T0 the input type for the first Matcher.
    * @tparam T1 the input type for the second Matcher.
    * @tparam T2 the input type for the second Matcher.
    * @tparam R  the MatchResult type.
    * @return a Matcher[(T0, T1, T2), R] that matches at least one of the elements of the given tuple.
    */
  def match3Any[T0, T1, T2, R](m0: Matcher[T0, R], m1: => Matcher[T1, R], m2: => Matcher[T2, R]): Matcher[(T0, T1, T2), R] =
    Matcher {
      case (t0, t1, t2) =>
        val f: (T0, T1, T2) => (T0, T1, T2) = (t0, t1, t2) => Tuple3(t0, t1, t2)
        matchProduct3Any(m0, m1, m2)(f)(t0, t1, t2)
    }

  /**
    * Method to match any element of a Product with two elements.
    *
    * @param m0 the Matcher corresponding to the first element.
    * @param m1 the Matcher corresponding to the second element.
    * @param f  a function which takes a (t0, t1) and returns a P.
    * @tparam T0 the input type for the first Matcher.
    * @tparam T1 the input type for the second Matcher.
    * @tparam R  the MatchResult type.
    * @tparam P  the input type.
    * @return a Matcher[P, R] that matches at least one of the elements of the given tuple.
    */
  def matchProduct2Any[T0, T1, R, P <: Product](m0: Matcher[T0, R], m1: => Matcher[T1, R])(f: (T0, T1) => P): Matcher[P, R] = Matcher {
    p => m0(p.productElement(0).asInstanceOf[T0]) || m1(p.productElement(1).asInstanceOf[T1])
  }

  /**
    * Method to match any element of a Product with two elements.
    *
    * @param m0 the Matcher corresponding to the first element.
    * @param m1 the Matcher corresponding to the second element.
    * @param m2 the Matcher corresponding to the third element.
    * @param f  a function which takes a (t0, t1, t3) and returns a P.
    * @tparam T0 the input type for the first Matcher.
    * @tparam T1 the input type for the second Matcher.
    * @tparam T2 the input type for the third Matcher.
    * @tparam R  the MatchResult type.
    * @tparam P  the input type.
    * @return a Matcher[P, R] that matches at least one of the elements of the given tuple.
    */
  def matchProduct3Any[T0, T1, T2, R, P <: Product](m0: Matcher[T0, R], m1: => Matcher[T1, R], m2: => Matcher[T2, R])(f: (T0, T1, T2) => P): Matcher[P, R] = Matcher {
    p => m0(p.productElement(0).asInstanceOf[T0]) || m1(p.productElement(1).asInstanceOf[T1]) || m2(p.productElement(2).asInstanceOf[T2])
  }

  /**
    * Method to match any element of a Product with two elements.
    *
    * @param m0 the Matcher corresponding to the first element.
    * @param m1 the Matcher corresponding to the second element.
    * @param f  a function which takes a (t0, t1) and returns a P.
    * @tparam T0 the input type for the first Matcher.
    * @tparam T1 the input type for the second Matcher.
    * @tparam R0 the first MatchResult type.
    * @tparam R1 the second MatchResult type.
    * @tparam P  the input type.
    * @return a Matcher[P, R] that matches at least one of the elements of the given tuple.
    */
  def matchProduct2All[T0, T1, R0, R1, P <: Product](m0: Matcher[T0, R0], m1: => Matcher[T1, R1])(f: (T0, T1) => P): Matcher[P, (R0, R1)] = Matcher {
    p => m0(p.productElement(0).asInstanceOf[T0]) && m1(p.productElement(1).asInstanceOf[T1])
  }

  /**
    * Method to match any element of a Product with two elements.
    *
    * @param m0 the Matcher corresponding to the first element.
    * @param m1 the Matcher corresponding to the second element.
    * @param m2 the Matcher corresponding to the third element.
    * @param f  a function which takes a (t0, t1, t3) and returns a P.
    * @tparam T0 the input type for the first Matcher.
    * @tparam T1 the input type for the second Matcher.
    * @tparam T2 the input type for the third Matcher.
    * @tparam R0 the first MatchResult type.
    * @tparam R1 the second MatchResult type.
    * @tparam R2 the third MatchResult type.
    * @tparam P  the input type.
    * @return a Matcher[P, (R0, R1, R2)] that matches at least one of the elements of the given tuple.
    */
  def matchProduct3All[T0, T1, T2, R0, R1, R2, P <: Product](m0: Matcher[T0, R0], m1: => Matcher[T1, R1], m2: => Matcher[T2, R2])(f: (T0, T1, T2) => P): Matcher[P, (R0, R1, R2)] = Matcher {
    p => m0(p.productElement(0).asInstanceOf[T0]) && m1(p.productElement(1).asInstanceOf[T1]) && m2(p.productElement(2).asInstanceOf[T2]) map MatchResult.unroll3
  }

  /**
    * Method to match all element of a Tuple2.
    *
    * @param m0 the Matcher corresponding to the first element.
    * @param m1 the Matcher corresponding to the second element.
    * @tparam T0 the input type for the first Matcher.
    * @tparam T1 the input type for the second Matcher.
    * @tparam R0 the MatchResult type for m0.
    * @tparam R1 the MatchResult type for m1.
    * @return a Matcher[(T0, T1), (R0, R1)] that matches at least one of the elements of the given tuple.
    */
  def match2All[T0, T1, R0, R1](m0: Matcher[T0, R0], m1: => Matcher[T1, R1]): Matcher[(T0, T1), (R0, R1)] = Matcher {
    case (t0, t1) => m0(t0) && m1(t1)
  }

  /**
    * Method to match all elements of a Tuple3.
    *
    * @param m0 the Matcher corresponding to the first element.
    * @param m1 the Matcher corresponding to the second element.
    * @tparam T0 the input type for the first Matcher.
    * @tparam T1 the input type for the second Matcher.
    * @tparam R0 the MatchResult type for m0.
    * @tparam R1 the MatchResult type for m1.
    * @tparam R2 the MatchResult type for m2.
    * @return a Matcher[(T0, T1), (R0, R1)] that matches at least one of the elements of the given tuple.
    */
  def match3All[T0, T1, T2, R0, R1, R2](m0: Matcher[T0, R0], m1: => Matcher[T1, R1], m2: => Matcher[T2, R2]): Matcher[(T0, T1, T2), (R0, R1, R2)] = Matcher {
    case (t0, t1, t2) => m0(t0) && m1(t1) && m2(t2) map MatchResult.unroll3
  }
}

case class MatcherException(msg: String) extends Exception(msg)