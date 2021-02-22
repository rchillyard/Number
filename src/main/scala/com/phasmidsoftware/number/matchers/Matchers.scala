package com.phasmidsoftware.number.matchers

import scala.util.{Failure, Success, Try}

/**
  * This trait defines a set of Matchers which operate in a parallel fashion to the Parsers of the Scala
  * Parser Combinator library.
  */
trait Matchers {

  /**
    * Matcher based on the function f.
    *
    * @param f a function of T => R
    * @tparam T the input type to both f and the resulting Matcher.
    * @tparam R the result type to both f and the resulting Matcher.
    * @return a Matcher[T, R].
    */
  def lift[T, R](f: T => R): Matcher[T, R] = t => Match(f(t))

  /**
    * Matcher which always fails and creates a Miss with the value tried.
    *
    * @param msg the message to describe this failure.
    * @tparam T the input type.
    * @tparam R the result type.
    * @return a Matcher[T, R]
    */
  def fail[T, R](msg: String): Matcher[T, R] = t => Miss(msg, t)

  /**
    * Matcher which always fails and creates a Miss with the value tried.
    *
    * @param e a Throwable.
    * @tparam R the result type.
    * @return a Matcher[T, R]
    */
  def error[R](e: Throwable): Matcher[Any, R] = _ => Error(e)

  /**
    * Matcher which always succeeds and whose input type and result type are the same.
    *
    * @tparam R both the input type and the result type.
    * @return a Matcher[R, R] which always succeeds.
    */
  def always[R]: Matcher[R, R] = lift(identity)

  /**
    * Matcher which succeeds only if the predicate p evaluates to true.
    *
    * @param p a predicate on type R.
    * @tparam R both the input type and the result type.
    * @return a Matcher[R, R] which succeeds only if p(r) is true.
    */
  def filter[R](p: R => Boolean): Matcher[R, R] = r => if (p(r)) Match(r) else Miss("filter", r)

  /**
    * Matcher which succeeds only if the predicate p evaluates to true.
    *
    * @param b a constant Boolean value.
    * @tparam R both the input type and the result type.
    * @return a Matcher[R, R] which succeeds only if p(r) is true.
    */
  def maybe[R](b: Boolean): Matcher[R, R] = filter(_ => b)

  /**
    * Matcher which succeeds if the input is equal to the given t0
    *
    * @param t the value which must be matched.
    * @tparam T the type of the input and result for Matcher.
    * @return a Matcher[T, T].
    */
  def matches[T](t: T): Matcher[T, T] = filter(_ == t)

  /**
    * Matcher whose success depends on the application of a function f to the input,
    * then the application of a predicate to a control value and the result of f.
    *
    * @param f a T => R.
    * @param p a predicate based on the tuple (q, r) where r is the result of applying f to t.
    * @tparam Q the "control" type.
    * @tparam T the "input" type.
    * @tparam R the result type.
    * @return a Matcher[(Q,R), T].
    */
  def valve[Q, T, R](f: T => R, p: (Q, R) => Boolean): Matcher[(Q, T), R] = LoggingMatcher("valve") {
        // TODO redesign this in terms of other Matchers, not MatchResult
    case (q, t) => MatchResult(f, p)(q, t)
  }

  /**
    * Matcher whose success depends on the application of a function f to the input,
    * then the application of a predicate to a control value and the result of f.
    *
    * @param p a predicate based on the tuple (q, r) where r is the result of applying f to t.
    * @tparam Q the "control" type.
    * @tparam T the "input" type.
    * @return a Matcher[(Q,R), T].
    */
  def valve[Q, T](p: (Q, T) => Boolean): Matcher[(Q, T), T] = {
    case (q, t) => MatchResult.create(p)(q, t, t)
  }

  /**
    * Matcher which reverses the sense of this Matcher.
    * However, an Error remains an Error.
    *
    * @param m a Matcher[T, R]
    * @param r the default result value, only to be used in the even of a Miss.
    * @tparam T the input type of m.
    * @tparam R the result type of m.
    * @return a Matcher[T, R] which works in the opposite sense to this.
    */
  def not[T, R](m: Matcher[T, R], r: => R): Matcher[T, R] = t => m(t) match {
    case Match(_) => Miss("not", t)
    case Miss(_, _) => Match(r)
    case Error(e) => Error(e)
    case x => throw MatcherException(s"not: logic error: $x")
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
    * Matcher which always succeeds and creates a Match with value r.
    *
    * @param r the predetermined result.
    * @tparam T the input type (input is ignored).
    * @tparam R the result type.
    * @return a Matcher[T, R]
    */
  def success[T, R](r: => R): Matcher[T, R] = _ => Match(r)

  /**
    * Method to match a T, resulting in an R, where the match is indirectly determined by
    * the given lens function.
    *
    * @param m    a Matcher[U, R].
    * @param lens a function T => U.
    * @tparam U the type of a property that is matched by m.
    * @return a Matcher[T, R]
    */
  def having[T, U, R](m: Matcher[U, R])(lens: T => U): Matcher[T, R] = t => m(lens(t))

  /**
    * Not sure why we need this but it's here.
    *
    * @param q a control value.
    * @param r a result value.
    * @tparam R the common type.
    * @return true if they are the same.
    */
  def isEqual[R](q: R, r: R): Boolean = q == r

  /**
    * Matcher which tries m on the given (Tuple2) input.
    * If m is unsuccessful, it then tries m on the swapped (inverted) tuple.
    *
    * @param m a Matcher[(T,T), R].
    * @tparam T the input type.
    * @tparam R the result type.
    * @return a Matcher[(T,T), R].
    */
  def *[T, R](m: Matcher[(T, T), R]): Matcher[(T, T), R] = m | (swap & m)

  /**
    * Matcher which tries m on the given (Tuple3) input.
    * If m is unsuccessful, it then tries m on the rotated tuple.
    * If that's unsuccessful, it then tries m on the inverted tuple.
    *
    * @param m a Matcher[(T,T), R].
    * @tparam T the input type.
    * @tparam R the result type.
    * @return a Matcher[(T,T), R].
    */
  def **[T, R](m: Matcher[(T, T, T), R]): Matcher[(T, T, T), R] = m | (rotate3 & m) | (invert3 & m)

  /**
    * Method to create a Matcher, which always succeeds, of a P whose result is a T0, based on the first element of P.
    *
    * @param f method to convert a (T0, T1) into a P (used at compile-time only).
    * @tparam T0 first of the member types.
    * @tparam T1 second of the member types.
    * @tparam P  the product type.
    * @return a Matcher[P,T0]
    */
  def select2_0[T0, T1, P <: Product](f: (T0, T1) => P): Matcher[P, T0] = lift(
    p => p.productElement(0).asInstanceOf[T0]
  )

  /**
    * Method to create a Matcher, which always succeeds, of a P whose result is a T1, based on the second element of P.
    *
    * @param f method to convert a (T0, T1) into a P (used at compile-time only).
    * @tparam T0 first of the member types.
    * @tparam T1 second of the member types.
    * @tparam P  the product type.
    * @return a Matcher[P,T1]
    */
  def select2_1[T0, T1, P <: Product](f: (T0, T1) => P): Matcher[P, T1] = lift(
    p => p.productElement(1).asInstanceOf[T1]
  )

  /**
    * Method to create a Matcher, which always succeeds, of a P whose result is a T0, based on the first element of P.
    *
    * @param f method to convert a (T0, T1, T2) into a P (used at compile-time only).
    * @tparam T0 first of the member types.
    * @tparam T1 second of the member types.
    * @tparam T2 third of the member types.
    * @tparam P  the product type.
    * @return a Matcher[P,T0]
    */
  def select3_0[T0, T1, T2, P <: Product](f: (T0, T1, T2) => P): Matcher[P, T0] = lift(
    p => p.productElement(0).asInstanceOf[T0]
  )

  /**
    * Method to create a Matcher, which always succeeds, of a P whose result is a T1, based on the second element of P.
    *
    * @param f method to convert a (T0, T1, T2) into a P (used at compile-time only).
    * @tparam T0 first of the member types.
    * @tparam T1 second of the member types.
    * @tparam T2 third of the member types.
    * @tparam P  the product type.
    * @return a Matcher[P,T1]
    */
  def select3_1[T0, T1, T2, P <: Product](f: (T0, T1, T2) => P): Matcher[P, T1] = lift(
    p => p.productElement(1).asInstanceOf[T1]
  )

  /**
    * Method to create a Matcher, which always succeeds, of a P whose result is a T2, based on the third element of P.
    *
    * @param f method to convert a (T0, T1, T2) into a P (used at compile-time only).
    * @tparam T0 first of the member types.
    * @tparam T1 second of the member types.
    * @tparam T2 third of the member types.
    * @tparam P  the product type.
    * @return a Matcher[P,T1]
    */
  def select3_2[T0, T1, T2, P <: Product](f: (T0, T1, T2) => P): Matcher[P, T2] = lift(
    p => p.productElement(2).asInstanceOf[T2]
  )

  /**
    * Method to create a Matcher of Tuple2, which succeeds if the first member matches the given Matcher.
    *
    * @tparam T0 first of the member types.
    * @tparam T1 second of the member types.
    * @tparam R0 type of result's first member.
    * @return a Matcher[(T0, T1), (R0, T1)].
    */
  def filter2_0[T0, T1, R0](m: Matcher[T0, R0]): Matcher[(T0, T1), (R0, T1)] = {
    case (t0, t1) => m(t0) && Match(t1)
  }

  /**
    * Method to create a Matcher of Tuple2, which succeeds if the first member matches the given Matcher.
    *
    * @tparam T0 first of the input member types.
    * @tparam T1 second of the input member types.
    * @tparam R1 type of result's second member.
    * @return a Matcher[(T0, T1), (T0, R1)].
    */
  def filter2_1[T0, T1, R1](m: Matcher[T1, R1]): Matcher[(T0, T1), (T0, R1)] = {
    case (t0, t1) => Match(t0) && m(t1)
  }

  /**
    * Method to create a Matcher of Tuple3, which succeeds if the first member matches the given Matcher.
    *
    * @tparam T0 first of the member types.
    * @tparam T1 second of the member types.
    * @tparam T2 third of the member types.
    * @tparam R0 type of result's first member.
    * @return a Matcher[(T0, T1, T2), (R0, T1, T2)].
    */
  def filter3_0[T0, T1, T2, R0](m: Matcher[T0, R0]): Matcher[(T0, T1, T2), (R0, T1, T2)] = {
    case (t0, t1, t2) => m(t0) && Match(t1) && Match(t2) map MatchResult.unroll21
  }

  /**
    * Method to create a Matcher of Tuple3, which succeeds if the second member matches the given Matcher.
    *
    * @tparam T0 first of the member types.
    * @tparam T1 second of the member types.
    * @tparam T2 third of the member types.
    * @tparam R1 type of result's second member.
    * @return a Matcher[(T0, T1, T2), (R0, T1, T2)].
    */
  def filter3_1[T0, T1, T2, R1](m: Matcher[T1, R1]): Matcher[(T0, T1, T2), (T0, R1, T2)] = {
    case (t0, t1, t2) => Match(t0) && m(t1) && Match(t2) map MatchResult.unroll21
  }

  /**
    * Method to create a Matcher of Tuple3, which succeeds if the third member matches the given Matcher.
    *
    * @tparam T0 first of the member types.
    * @tparam T1 second of the member types.
    * @tparam T2 third of the member types.
    * @tparam R2 type of result's third member.
    * @return a Matcher[(T0, T1, T2), (T0, T1, R2)].
    */
  def filter3_2[T0, T1, T2, R2](m: Matcher[T2, R2]): Matcher[(T0, T1, T2), (T0, T1, R2)] = {
    case (t0, t1, t2) => Match(t0) && Match(t1) && m(t2) map MatchResult.unroll21
  }

  /**
    * Method to swap the order of elements in a Tuple2.
    *
    * @tparam T0 the first element type.
    * @tparam T1 the second element type.
    * @return a Matcher from (T0,T1) to (T1,T0).
    */
  def swap[T0, T1]: Matcher[(T0, T1), (T1, T0)] = lift[(T0, T1), (T1, T0)] {
    case (t0, t1) => (t1, t0)
  }

  /**
    * Method to rotate the order of elements in a Tuple3.
    *
    * @tparam T0 the first element type.
    * @tparam T1 the second element type.
    * @tparam T2 the third element type.
    * @return a Matcher from (T0,T1,T2) to (T1,T2,T0).
    */
  def rotate3[T0, T1, T2]: Matcher[(T0, T1, T2), (T1, T2, T0)] = lift {
    case (t0, t1, t2) => (t1, t2, t0)
  }

  /**
    * Method to invert the order of elements in a Tuple3.
    *
    * @tparam T0 the first element type.
    * @tparam T1 the second element type.
    * @tparam T2 the third element type.
    * @return a Matcher from (T0,T1,T2) to (T1,T2,T0).
    */
  def invert3[T0, T1, T2]: Matcher[(T0, T1, T2), (T2, T1, T0)] = lift {
    case (t0, t1, t2) => (t2, t1, t0)
  }

  /**
    * Tee method.
    * This method will return its input, however, a side-effect occurs which is to invoke f(x).
    *
    * CONSIDER setting up a MatchResultOps and making tee of method of that.
    *
    * @param x an X value.
    * @param f a function which takes an X and yields Unit.
    * @tparam X the underlying type of x.
    * @return x unchanged.
    */
  def tee[X](x: MatchResult[X])(f: X => Unit): MatchResult[X] = {
    x foreach f
    x
  }

  /**
    * (Internal) log method.
    * If ll is LogOff, p is returned unchanged, other than that on failure of m, the matcher fail(name) is invoked.
    * If ll is LogInfo, a matcher based on m, which on successful matching, logging with println will occur, is returned.
    * If ll is LogDebug, then the value of log(m)(name) is returned.
    *
    * @param m    a Matcher[T, R].
    * @param name a String to identify this parser.
    * @param ll   (implicit) LogLevel.
    * @tparam T the underlying type of the input to m.
    * @tparam R the underlying type of the result of m.
    * @return a Matcher[T, R].
    */
  def log[T, R](m: => Matcher[T, R])(name: => String)(implicit ll: LogLevel, logger: MatchLogger): Matcher[T, R] = ll match {
    // TODO use flatMap
    case LogDebug => Matcher { t =>
      logger(s"trying $name on $t...")
      val r = m(t)
      logger(s"... $name: $r")
      r
    }

    case LogInfo =>
      Matcher { t =>
        val q: Matcher[T, R] = m | fail(name)
        tee(q(t))(x => logger(s"$name: matched $x"))
      }

    case _ => m | fail(name)
  }

  /**
    * Implicit class MatcherOps which allows us to use the method :| on a Matcher[T,R].
    *
    * @param p a Matcher[T,R].
    * @tparam T the input type of p.
    * @tparam R the result type of p.
    */
  implicit class MatcherOps[T, R](p: Matcher[T, R]) {
    def :|(name: => String)(implicit ll: LogLevel, logger: MatchLogger): Matcher[T, R] = log(p)(name)
  }

  /**
    * Method to create a Matcher, based on the given function f.
    * NOTE: f is a partial function which may throw a MatchError.
    * In such a case, this is simply treated as a Miss.
    *
    * @param f a T => MatchResult[R].
    * @tparam T the input type.
    * @tparam R the result type.
    * @return a Matcher[T, R] based on f.
    */
  def Matcher[T, R](f: T => MatchResult[R]): Matcher[T, R] = (t: T) => try f(t) catch {
    case e: MatchError => Miss(s"matchError: $e", t)
  }

  /**
    * Method to create a Matcher, based on the given function f.
    *
    * @param name the name for the logger to mention.
    * @param f    a T => MatchResult[R].
    * @tparam T the input type.
    * @tparam R the result type.
    * @return a Matcher[T, R] based on f.
    */
  def LoggingMatcher[T, R](name: => String)(f: T => MatchResult[R])(implicit ll: LogLevel, logger: MatchLogger): Matcher[T, R] = Matcher(f) :| name

  /**
    * Method to create a Matcher which operates on a similar, but inverted, tuple as m.
    *
    * CONSIDER maybe we can simply compose m with swap.
    *
    * @param m a Matcher[(T0,T1),R].
    * @tparam T0 one of the input types.
    * @tparam T1 the other input type.
    * @tparam R  the result type.
    * @return a Matcher[(T1,T0),R].
    */
  def flip[T0, T1, R](m: Matcher[(T0, T1), R]): Matcher[(T1, T0), R] = {
    case (t1, t0) => m(t0, t1)
  }

  /**
    * Method to create a Matcher, which always succeeds, of a P whose result is a Tuple2.
    *
    * @param f method to convert a (T0, T1) into a P.
    * @tparam T0 first of the member types.
    * @tparam T1 second of the member types.
    * @tparam P  the product type.
    * @return a Matcher[P,(T0,T1)]
    */
  def tuple2[T0, T1, P <: Product](f: (T0, T1) => P): Matcher[P, (T0, T1)] = lift(
    p => (p.productElement(0).asInstanceOf[T0], p.productElement(1).asInstanceOf[T1])
  )

  /**
    * Method to create a Matcher, which always succeeds, of a P whose result is a Tuple3.
    *
    * @param f method to convert a (T0, T1, T2) into a P.
    * @tparam T0 first of the member types.
    * @tparam T1 second of the member types.
    * @tparam T2 third of the member types.
    * @tparam P  the product type.
    * @return a Matcher[P,(T0,T1,T2)]
    */
  def tuple3[T0, T1, T2, P <: Product](f: (T0, T1, T2) => P): Matcher[P, (T0, T1, T2)] = lift(
    p => (p.productElement(0).asInstanceOf[T0], p.productElement(1).asInstanceOf[T1], p.productElement(2).asInstanceOf[T2])
  )

  /**
    * Method to create a Matcher, which always succeeds, of a Tuple2 whose result is a P.
    * This method is the inverse of tuple2.
    *
    * @param f method to convert a (T0, T1) into a P.
    * @tparam T0 first of the member types.
    * @tparam T1 second of the member types.
    * @tparam P  the product type.
    * @return a Matcher[P,(T0,T1)]
    */
  def product2[T0, T1, P <: Product](f: (T0, T1) => P): Matcher[(T0, T1), P] = lift(
    t => f(t._1, t._2) // CONSIDER using tupled or curried here
  )

  /**
    * Method to create a Matcher, which always succeeds, of a Tuple2 whose result is a P.
    * This method is the inverse of tuple2.
    *
    * @param f method to convert a (T0, T1) into a P.
    * @tparam T0 first of the member types.
    * @tparam T1 second of the member types.
    * @tparam P  the product type.
    * @return a Matcher[P,(T0,T1)]
    */
  def product3[T0, T1, T2, P <: Product](f: (T0, T1, T2) => P): Matcher[(T0, T1, T2), P] = lift(
    t => f(t._1, t._2, t._3)
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
  def match2Any[T0, T1, R](m0: Matcher[T0, R], m1: => Matcher[T1, R]): Matcher[(T0, T1), R] = {
    case (t0, t1) => matchProduct2Any(m0, m1)(->.apply)(t0 -> t1)
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
  def matchProduct2Any[T0, T1, R, P <: Product](m0: Matcher[T0, R], m1: => Matcher[T1, R])(f: (T0, T1) => P): Matcher[P, R] = p =>
    m0(p.productElement(0).asInstanceOf[T0]) || m1(p.productElement(1).asInstanceOf[T1])

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
  def match3Any[T0, T1, T2, R](m0: Matcher[T0, R], m1: => Matcher[T1, R], m2: => Matcher[T2, R]): Matcher[(T0, T1, T2), R] = {
    case (t0, t1, t2) =>
      val f: (T0, T1, T2) => (T0, T1, T2) = (t0, t1, t2) => Tuple3(t0, t1, t2)
      matchProduct3Any(m0, m1, m2)(f)(t0, t1, t2)
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
  def matchProduct3Any[T0, T1, T2, R, P <: Product](m0: Matcher[T0, R], m1: => Matcher[T1, R], m2: => Matcher[T2, R])(f: (T0, T1, T2) => P): Matcher[P, R] = p =>
    m0(p.productElement(0).asInstanceOf[T0]) || m1(p.productElement(1).asInstanceOf[T1]) || m2(p.productElement(2).asInstanceOf[T2])

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
  def matchProduct2All[T0, T1, R0, R1, P <: Product](m0: Matcher[T0, R0], m1: => Matcher[T1, R1])(f: (T0, T1) => P): Matcher[P, (R0, R1)] = p =>
    m0(p.productElement(0).asInstanceOf[T0]) && m1(p.productElement(1).asInstanceOf[T1])

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
  def matchProduct3All[T0, T1, T2, R0, R1, R2, P <: Product](m0: Matcher[T0, R0], m1: => Matcher[T1, R1], m2: => Matcher[T2, R2])(f: (T0, T1, T2) => P): Matcher[P, (R0, R1, R2)] = p =>
    m0(p.productElement(0).asInstanceOf[T0]) && m1(p.productElement(1).asInstanceOf[T1]) && m2(p.productElement(2).asInstanceOf[T2]) map MatchResult.unroll21

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
  def match2All[T0, T1, R0, R1](m0: Matcher[T0, R0], m1: => Matcher[T1, R1]): Matcher[(T0, T1), (R0, R1)] = {
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
  def match3All[T0, T1, T2, R0, R1, R2](m0: Matcher[T0, R0], m1: => Matcher[T1, R1], m2: => Matcher[T2, R2]): Matcher[(T0, T1, T2), (R0, R1, R2)] = {
    case (t0, t1, t2) => m0(t0) && m1(t1) && m2(t2) map MatchResult.unroll21
  }

  /**
    * Method to create a Matcher which operates on an instance of a case class (or other Product) P but which invokes m (which takes a 2-tuple).
    *
    * TESTME
    *
    * @param m a Matcher[(T0, T1), R].
    * @param f a function (T0, T1) => P.
    * @tparam T0 the first element type.
    * @tparam T1 the second element type.
    * @tparam P  the Product type.
    * @tparam R  the result type.
    * @return a Matcher[P, R]
    */
  def from2[T0, T1, P <: Product, R](m: Matcher[(T0, T1), R])(f: (T0, T1) => P): Matcher[P, R] = p =>
    m(p.productElement(0).asInstanceOf[T0], p.productElement(1).asInstanceOf[T1])

  /**
    * Method to create a Matcher which operates on an instance of a case class (or other Product) P but which invokes m (which takes a 2-tuple).
    *
    * TESTME
    *
    * @param m a Matcher[(T0, T1), R].
    * @param f a function P => Option[(T0, T1)] (in other words, an unapply method).
    * @tparam T0 the first element type.
    * @tparam T1 the second element type.
    * @tparam P  the Product type.
    * @tparam R  the result type.
    * @return a Matcher[P, R]
    */
  def from2Alt[T0, T1, P <: Product, R](m: Matcher[(T0, T1), R])(f: P => Option[(T0, T1)]): Matcher[P, R] = p =>
    f(p) match {
      case Some((t0, t1)) => m(t0, t1)
      case _ => Error(MatcherException("logic error"))
    }

  /**
    * Method to create a Matcher which operates on an instance of a case class (or other Product) P
    * but which invokes m (which takes a 3-tuple).
    *
    * TESTME
    *
    * @param m a Matcher[(T0, T1), R].
    * @param f a function (T0, T1) => P.
    * @tparam T0 the first element type.
    * @tparam T1 the second element type.
    * @tparam T2 the third element type.
    * @tparam P  the Product type.
    * @tparam R  the result type.
    * @return a Matcher[P, R]
    */
  def from3[T0, T1, T2, P <: Product, R](m: Matcher[(T0, T1, T2), R])(f: (T0, T1, T2) => P): Matcher[P, R] = p =>
    m(p.productElement(0).asInstanceOf[T0], p.productElement(1).asInstanceOf[T1], p.productElement(2).asInstanceOf[T2])

  /**
    * Trait to define the behavior of the result of a Match.
    *
    * @tparam R the type of the result.
    */
  sealed trait MatchResult[+R] {
    /**
      * @return true if this is a Match
      */
    def successful: Boolean

    /**
      * @return false if this is a Match
      */
    def isEmpty: Boolean = !successful

    /**
      * @return the result of the MatchResult.
      * @throws MatcherException if this is not a Match.
      */
    def get: R

    /**
      * Alternative form of get such that, in the case of a Miss, the default value given by s will be returned.
      *
      * @param s a call-by-name value to be used if this is unsuccessful.
      * @tparam S a super-class of R.
      * @return the result of the MatchResult if it's a Match, otherwise return s if it's a Miss.
      */
    def getOrElse[S >: R](s: => S): S

    /**
      * Map method.
      *
      * @param f a function of R => S.
      * @tparam S the underlying type of the returned MatchResult.
      * @return MatchResult[S].
      */
    def map[S](f: R => S): MatchResult[S] = flatMap(r => success(f(r)))

    /**
      * "unit" method for a successful match.
      *
      * @param s the value of the result.
      * @tparam S the underlying type of the result.
      * @return a Match[S] with value s.
      */
    def success[S](s: S): MatchResult[S] = Match(s)

    /**
      * FlatMap method.
      *
      * @param f a function of R => MatchResult[S].
      * @tparam S the underlying type of the returned MatchResult.
      * @return MatchResult[S].
      */
    def flatMap[S](f: R => MatchResult[S]): MatchResult[S]

    /**
      * Foreach method.
      *
      * @param f a function of R => Unit.
      * @return Unit.
      */
    def foreach(f: R => Unit): Unit

    /**
      * Alternation method which takes a MatchResult as the alternative.
      *
      * @param s a MatchResult which will be used if this is empty.
      * @tparam S the type of the result and a super-type of R.
      * @return a MatchResult[S].
      */
    def ||[S >: R](s: => MatchResult[S]): MatchResult[S]

    /**
      * Alternation method which takes a Matcher as the alternative.
      * If this MatchResult is empty then return the value of m applied to the input.
      *
      * @param m a Matcher of Any to S.
      * @tparam S the type of the result and a super-type of R.
      * @return a MatchResult[S].
      */
    def |[S >: R](m: => Matcher[Any, S]): MatchResult[S]

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
      * @param m a Matcher of S to T.
      * @tparam S the underlying type of the input to m (S is a super-class of R).
      * @tparam T the underlying type of the returned value.
      * @return a MatchResult[T].
      */
    def &[S >: R, T](m: => Matcher[S, T]): MatchResult[T]

    /**
      * TESTME
      *
      * @param m a Matcher[R, S].
      * @tparam S the result type.
      * @return a MatchResult[S].
      */
    def chain[S](m: Matcher[R, S]): MatchResult[S] = this match {
      case Match(x) => m(x)
      case Miss(w, x) => Miss(w, x)
      case Error(e) => Error(e)
      case x => throw MatcherException(s"chain: logic error: $x")
    }
  }

  /**
    * Abstract class Matcher.
    *
    * @tparam T the input type.
    * @tparam R the result type.
    */
  abstract class Matcher[-T, +R] extends (T => MatchResult[R]) {

    private var name: String = ""

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
      * Mutating method which sets the name of this Matcher to n.
      *
      * @param n the new name of this Matcher.
      * @return this.type.
      */
    def named(n: String): this.type = {
      name = n
      this
    }

    override def toString = s"Matcher ($name)"

    /**
      * Map method which transforms this Matcher[T, R] into a Matcher[T, S].
      *
      * @param f a function R => S.
      * @tparam S the result type for the resulting Matcher.
      * @return a Matcher[T, S].
      */
    def map[S](f: R => S): Matcher[T, S] = t => this (t) map f

    /**
      * Matcher which reverses the sense of this Matcher.
      *
      * TESTME
      *
      * @param s the default result value, only to be used in the even of a Miss.
      * @tparam S the type of both s and the result (a super-type of R).
      * @return a Matcher[T, R] which works in the opposite sense to this.
      */
    def ![S >: R](s: => S): Matcher[T, S] = not(this, s)

    /**
      * Returns a matcher that optionally matches what this parser parses.
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
    def |[U <: T, S >: R](m: Matcher[U, S]): Matcher[U, S] = t => match2Any(this, m)((t, t))

    /**
      * Method to combine Matchers in the sense that, when this successfully matches a T, resulting in an R,
      * then m is invoked on the result, such that if it is successful, we return an S..
      *
      * @param m the alternative Matcher.
      * @tparam S the underlying type of the resulting Matcher.
      * @return a Matcher[T, S] which will match in composition on both this and m.
      */
    def &[S](m: Matcher[R, S]): Matcher[T, S] = t => this (t) & m

    /**
      * Method to combine Matchers this and m such that the resulting Matcher takes a tuple and results in a tuple.
      *
      * @param m a Matcher[P, S]
      * @tparam P the input type of m.
      * @tparam S the result type of m.
      * @return a Matcher[(T,P), (R,S)] which is the result of invoking match2All(this, m).
      */
    def ~[P, S](m: Matcher[P, S]): Matcher[(T, P), (R, S)] = match2All(this, m)

    /**
      * Method to combine Matchers this and m such that the resulting Matcher takes a tuple and results in the result from m.
      *
      * @param m a Matcher[P, S]
      * @tparam P the input type of m.
      * @tparam S the result type of m.
      * @return a Matcher[(T,P), S] which is the result of invoking ~ but stripping the first element of the tuple.
      */
    def ~>[P, S](m: Matcher[P, S]): Matcher[(T, P), S] = this ~ m ^^ (_._2)

    /**
      * Method to combine Matchers this and m such that the resulting Matcher takes a tuple and results in the result from this.
      *
      * @param m a Matcher[P, S]
      * @tparam P the input type of m.
      * @tparam S the result type of m.
      * @return a Matcher[(T,P), R] which is the result of invoking ~ but stripping the second element of the tuple.
      */
    def <~[P, S](m: Matcher[P, S]): Matcher[(T, P), R] = this ~ m ^^ (_._1)

    /**
      * Matcher which always succeeds (unless this causes an Error) but whose result is based on a Try[R].
      *
      * @return Matcher[T, Option of R]
      */
    def trial: Matcher[T, Try[R]] = t =>
      Try(this (t)) match {
        case Success(Match(z)) => Match(Success(z))
        case Success(Miss(x, _)) => Match(Failure(MatcherException(x)))
        case Failure(x) => Match(Failure(x))
        case Success(Error(e)) => Error(e)
        case x => throw MatcherException(s"trial: logic error: $x")
      }

    /**
      * CONSIDER maybe doesn't make sense.
      *
      * Matcher which succeeds or not, depending on an additional Q value (the control).
      *
      * @param m a Matcher[(Q, R),U].
      * @tparam Q the type of the control value.
      * @tparam U the result type of m and the returned Matcher.
      * @return a Matcher[(Q,T),U].
      */
    def chain[Q, U](m: Matcher[(Q, R), U]): Matcher[(Q, T), U] = {
      case (q, t) => this (t) flatMap (r => m(q, r))
    }

    /**
      * Matcher which succeeds or not, depending on this and an additional S value.
      *
      * TESTME
      *
      * @param m a Matcher[(R, S), U].
      * @tparam S the type of the additional parameter.
      * @tparam U the result type of m and the returned Matcher.
      * @return a Matcher[T, U].
      */
    def chain[S, U](m: Matcher[(R, S), U], s: S): Matcher[T, U] = t =>
      this (t) flatMap (r => m(r, s))
  }

  /**
    * Successful match.
    *
    * @param r the result.
    * @tparam R the type of the result.
    */
  case class Match[+R](r: R) extends MatchResult[R] {
    /**
      * @return true
      */
    def successful: Boolean = true

    /**
      * If s is a Match, then the result will be a Match of the tuple of r and the result of s.
      *
      * @param s a MatchResult[S].
      * @tparam S the underlying type of s.
      * @return a MatchResult[(R,S)].
      */
    def &&[S](s: => MatchResult[S]): MatchResult[(R, S)] = s.flatMap(z => Match(r -> z))

    /**
      * Returns the result of invoking f on r.
      *
      * @param f a function of R => MatchResult[S].
      * @tparam S the underlying type of the returned MatchResult.
      * @return MatchResult[S].
      */
    def flatMap[S](f: R => MatchResult[S]): MatchResult[S] = f(r)

    /**
      * Apply f to r.
      *
      * @param f a function of R => Unit.
      */
    def foreach(f: R => Unit): Unit = f(r)

    /**
      * Alternation method which takes a MatchResult as the alternative.
      *
      * @param s a MatchResult (ignored)).
      * @return this.
      */
    def ||[S >: R](s: => MatchResult[S]): MatchResult[S] = this

    /**
      * Alternation method which takes a Matcher as the alternative.
      * If this MatchResult is empty then return the value of m applied to the input.
      *
      * TESTME
      *
      * @param m a Matcher of Any to R (ignored).
      * @return this.
      */
    def |[S >: R](m: => Matcher[Any, S]): MatchResult[S] = this

    /**
      * Composition method.
      * If this MatchResult is successful then return the value of m applied to the result.
      *
      * @param m a Matcher of S to T.
      * @tparam S the underlying type of the input to m (S is a super-class of R).
      * @tparam T the underlying type of the returned value.
      * @return m(get).
      */
    def &[S >: R, T](m: => Matcher[S, T]): MatchResult[T] = m(get)

    /**
      * @return r.
      */
    def get: R = r

    /**
      * Alternative form of get such that, in the case of a Miss, the default value given by s will be returned.
      *
      * @param s a call-by-name value to be used if this is unsuccessful.
      * @tparam S a super-class of R.
      * @return r.
      */
    def getOrElse[S >: R](s: => S): S = r

    /**
      * Render this Match as a String.
      *
      * @return s"Match: $r"
      */
    override def toString: String = s"Match: $r"
  }

  /**
    * Unsuccessful match of type dependent on X.
    *
    * @param x either a String or a Throwable.
    * @tparam X the type of x.
    * @tparam R the result-type of this.
    */
  abstract class Unsuccessful[+R, X](x: X) extends MatchResult[R] {
    /**
      *
      * @return false.
      */
    def successful: Boolean = false

    /**
      * TESTME
      *
      * @throws MatcherException cannot call get on Unsuccessful.
      */
    def get: R = x match {
      case x: Throwable => throw MatcherException("Unsuccessful: cannot call get", x)
      case w: String => throw MatcherException(s"Unsuccessful: cannot call get: $w")
      case x => throw MatcherException(s"Unsuccessful: cannot call get: $x")
    }

    /**
      * Do nothing.
      *
      * @param f a function of R => Unit (ignored).
      */
    def foreach(f: R => Unit): Unit = ()
  }

  /**
    * Unsuccessful match (Miss).
    *
    * @param t the value that was not matched.
    * @tparam T the underlying type of t.
    * @tparam R the result-type of this.
    */
  case class Miss[T, +R](msg: String, t: T) extends Unsuccessful[R, String](msg) {
    /**
      * @param f a function of R => S (ignored)
      * @tparam S the underlying type of the returned MatchResult.
      * @return Miss(t).
      */
    override def map[S](f: R => S): MatchResult[S] = Miss(msg, t)

    /**
      * Method to get the value of this MatchResult, otherwise return the parameter s.
      *
      * @param s a call-by-name value to be used if this is unsuccessful.
      * @tparam S a super-class of R.
      * @return s.
      */
    def getOrElse[S >: R](s: => S): S = s

    /**
      * @param s a MatchResult[S] (ignored).
      * @tparam S the underlying type of s.
      * @return Miss(t).
      */
    def &&[S](s: => MatchResult[S]): MatchResult[(R, S)] = Miss(msg, t)

    /**
      * @param f a function of R => MatchResult[S] (ignored).
      * @tparam S the underlying type of the returned MatchResult.
      * @return Miss(t).
      */
    def flatMap[S](f: R => MatchResult[S]): MatchResult[S] = Miss(msg, t)

    /**
      * Alternation method which takes a Matcher as the alternative.
      * If this MatchResult is empty then return the value of m applied to the input.
      *
      * @param m a Matcher of Any to R.
      * @return m(t).
      */
    def |[S >: R](m: => Matcher[Any, S]): MatchResult[S] = m(t)

    /**
      * Alternation method which takes a MatchResult as the alternative.
      *
      * @param s a MatchResult which will be used if this is empty.
      * @return s.
      */
    def ||[S >: R](s: => MatchResult[S]): MatchResult[S] = s

    /**
      * Composition method.
      * If this MatchResult is successful then return the value of m applied to the result.
      *
      * TESTME
      *
      * @param m a Matcher of S to T.
      * @tparam S the underlying type of the input to m (S is a super-class of R).
      * @tparam U the underlying type of the returned value.
      * @return a MatchResult[T].
      */
    def &[S >: R, U](m: => Matcher[S, U]): MatchResult[U] = Miss(msg, t)

    override def toString: String = s"Miss: $msg: $t"
  }

  /**
    * Error when matching.
    *
    * TESTME
    *
    * @param e the exception that was thrown.
    * @tparam R the result-type of this.
    */
  case class Error[+R](e: Throwable) extends Unsuccessful[R, Throwable](e) {
    /**
      * @param f a function of R => S (ignored)
      * @tparam S the underlying type of the returned MatchResult.
      * @return Error(e).
      */
    override def map[S](f: R => S): MatchResult[S] = Error(e)

    /**
      * Method to get the value of this MatchResult, otherwise return the parameter s.
      *
      * @param s a call-by-name value to be used if this is unsuccessful.
      * @tparam S a super-class of R.
      * @throws MatcherException corresponding to the error.
      */
    def getOrElse[S >: R](s: => S): S = get

    /**
      * @param s a MatchResult[S] (ignored).
      * @tparam S the underlying type of s.
      * @return Error(t).
      */
    def &&[S](s: => MatchResult[S]): MatchResult[(R, S)] = Error(e)

    /**
      *
      * @param f a function of R => MatchResult[S].
      * @tparam S the underlying type of the returned MatchResult.
      * @return Error(e).
      */
    def flatMap[S](f: R => MatchResult[S]): MatchResult[S] = Error(e)

    /**
      * Alternation method which takes a MatchResult as the alternative.
      *
      * @param s a MatchResult which will be used if this is empty.
      * @return s.
      */
    def ||[S >: R](s: => MatchResult[S]): MatchResult[S] = Error(e)

    /**
      * Alternation method which takes a Matcher as the alternative.
      * If this MatchResult is empty then return the value of m applied to the input.
      *
      * @param m a Matcher of Any to R.
      * @return m(t).
      */
    def |[S >: R](m: => Matcher[Any, S]): MatchResult[S] = Error(e)

    /**
      * Composition method.
      * If this MatchResult is successful then return the value of m applied to the result.
      *
      * @param m a Matcher of S to T.
      * @tparam S the underlying type of the input to m (S is a super-class of R).
      * @tparam U the underlying type of the returned value.
      * @return a MatchResult[T].
      */
    def &[S >: R, U](m: => Matcher[S, U]): MatchResult[U] = Error(e)

    override def toString: String = s"Error: ${e.getLocalizedMessage}"
  }

  object MatchResult {
    /**
      * Construct a MatchResult[R] based on a Boolean, a T and an R.
      * If b is true, then the result is Match(r); otherwise it is Miss(t).
      *
      * @param b a Boolean.
      * @param t a T.
      * @param r an R.
      * @tparam T input type.
      * @tparam R result type.
      * @return MatchResult[R].
      */
    def apply[T, R](b: Boolean, t: T, r: R): MatchResult[R] = if (b) Match(r) else Miss("false", t)

    /**
      * Construct a MatchResult[R] based on an Either[T, R].
      *
      * @param e either a Left[T] (miss) or a Right[R] (match).
      * @tparam T the Miss type.
      * @tparam R the Match type.
      * @return a MatchResult[R]
      */
    def apply[T, R](e: Either[T, R]): MatchResult[R] = e match {
      case Right(r) => Match(r)
      case Left(t) => Miss("left", t)
    }

    /**
      * Create a MatchResult based on an input value t, a result r, and a predicate p.
      *
      * @param p the predicate which determines whether the result is a Match or a Miss.
      * @param t the input value (ignored for a Match).
      * @param r the result value.
      * @tparam Q the control type.
      * @tparam T the input type.
      * @tparam R the result type.
      * @return a MatchResult[R].
      */
    def create[Q, T, R](p: (Q, R) => Boolean)(q: Q, t: T, r: R): MatchResult[R] = if (p(q, r)) Match(r) else Miss("create", t)

    /**
      * Yield a MatchResult[R] based on an input value t, a function f, and a predicate p.
      *
      * @param f the function T => R.
      * @param p the predicate which is applied to (q, r) to determines whether the result is a Match or a Miss,
      *          where r is the result of applying f to t.
      * @param q the control value.
      * @param t the input value.
      * @tparam Q the control type.
      * @tparam T the input type.
      * @tparam R the result type.
      * @return a MatchResult[R].
      */
    def apply[Q, T, R](f: T => R, p: (Q, R) => Boolean)(q: Q, t: T): MatchResult[R] = MatchResult.create(p)(q, t, f(t))

    // TESTME
    // CONSIDER move these to a tuple-specific class
    // CONSIDER merging with rotate3 and invert3 from Matchers
    def invert3[R0, R1, R2](t: (R0, R1, R2)): (R2, R1, R0) = (t._3, t._2, t._1)

    def unroll12[R0, R1, R2](t: (R0, (R1, R2))): (R0, R1, R2) = (t._1, t._2._1, t._2._2)

    def unroll21[R0, R1, R2](t: ((R0, R1), R2)): (R0, R1, R2) = (t._1._1, t._1._2, t._2)

    def roll3[R0, R1, R2](t: (R0, R1, R2)): ((R0, R1), R2) = t._1 -> t._2 -> t._3
  }

}

case class MatcherException(msg: String, x: Throwable) extends Exception(msg, x)

object MatcherException {
  def apply(msg: String): MatcherException = MatcherException(msg, null)
}

/**
  * Trait which is used to define a logging level for the log method of SignificantSpaceParsers.
  */
trait LogLevel

case object LogDebug extends LogLevel

case object LogInfo extends LogLevel

case object LogOff extends LogLevel

object LogLevel {
  implicit val ll: LogLevel = LogOff
}

trait MatchLogger extends ((String => Unit))

object MatchLogger {
  implicit val defaultMatchLogger: MatchLogger = w => println(w)
}