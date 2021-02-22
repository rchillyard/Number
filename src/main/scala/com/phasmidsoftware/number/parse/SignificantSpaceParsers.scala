package com.phasmidsoftware.number.parse

import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

/**
  * This abstract class extends JavaTokenParsers but where white space is not ignored.
  */
abstract class SignificantSpaceParsers extends JavaTokenParsers {

  self =>

  /**
    * Method to parse a given String (w) such that all of it is consumed,
    * and return, if possible, a Success.
    * If the parsing fails, a Failure will be returned.
    *
    * @param p the parser to employ, of type Parser[X].
    * @param w the String to parse.
    * @tparam X the underlying type of the parser p and also of the result.
    * @return a Try[X].
    */
  def stringParser[X](p: Parser[X], w: String): Try[X] = parseAll(p, w) match {
    case Success(t, _) => scala.util.Success(t)
    case Failure(m, _) => scala.util.Failure(SignificantSpaceParserException(m))
    case Error(m, _) => scala.util.Failure(SignificantSpaceParserException(m))
  }

  /**
    * We do not ignore white space. It's significant.
    *
    * @return false.
    */
  override def skipWhitespace: Boolean = false

  /**
    * A parser which allows for white space to follow what is matched by p.
    *
    * @param p a Parser[T].
    * @tparam T the underlying type of p.
    * @return a Parser[T] that ignores trailing white space.
    */
  def trim[T](p: Parser[T]): Parser[T] = p <~ opt(whiteSpace)

  /**
    * A parser which parses a list of Ts separated by white space.
    *
    * @param p a Parser[T].
    * @tparam T the underlying type of p.
    * @return a Parser of List[T] that expects elements to be separated by white space.
    */
  def repSepSp[T](p: Parser[T]): Parser[List[T]] = repsep(p, whiteSpace)

  /**
    * Implicit class ParserOps which allows us to use the method :| on a Parser[X].
    *
    * @param p a Parser[X].
    * @tparam X the underlying type of p.
    */
  implicit class ParserOps[X](p: Parser[X]) {
    def :|(name: String)(implicit ll: LogLevel): Parser[X] = logit(p)(name)
  }

  /**
    * Implicit class ParserOptionOps which allows us to use the method ?| on a Parser of Option[X].
    *
    * @param p a Parser of Option[X].
    * @tparam X the under-underlying type of p.
    */
  implicit class ParserOptionOps[X](p: Parser[Option[X]]) {
    def ??(q: => Parser[X]): Parser[X] = compose(p, q) :| "compose"

    def ?|[Y](q: => Parser[Y]): Parser[Either[Y, X]] = composeOption(p, q) :| "composeOption"
  }

  /**
    * Implicit class RegexOps which allows us to use the method :| on a Regex.
    *
    * @param r a Regex.
    */
  implicit class RegexOps(r: Regex) {
    def :|(name: String)(implicit ll: LogLevel): Parser[String] = logit(r)(name)
  }

  /**
    * Tee method.
    * This method will return its input, however, a side-effect occurs which is to invoke f(x).
    *
    * @param x an X value.
    * @param f a function which takes an X and yields Unit.
    * @tparam X the underlying type of x.
    * @return x unchanged.
    */
  def tee[X](x: X)(f: X => Unit): X = {
    f(x)
    x
  }

  /**
    * (Internal) log method which expands on the capabilities of Parsers.log.
    * If ll is LogOff, p is returned unchanged, other than that on failure of p, the parser failure(name) is invoked.
    * If ll is LogInfo, a parser based on p, which on successful parsing logging with println will occur, is returned.
    * If ll is LogDebug, then the value of log(p)(name) is returned.
    *
    * @param p    a parser[T].
    * @param name a String to identify this parser.
    * @param ll   (implicit) LogLevel.
    * @tparam T the underlying type of p and the result.
    * @return a Parser[T].
    */
  def logit[T](p: => Parser[T])(name: => String)(implicit ll: LogLevel): Parser[T] = ll match {
    case LogDebug => log(p | failure(name))(name)

    case LogInfo =>
      val q = p | failure(name)
      Parser { in =>
        tee(q(in)) {
          case this.Success(x, _) => println(s"$name: matched $x")
          case _ =>
        }
      }

    case _ => p | failure(name)
  }

  // NOTE enabled and debug are not currently used.
  implicit val enabled: Boolean = false

  // NOTE enabled and debug are not currently used.
  protected def debug[X](x: X, w: => String)(implicit enabled: Boolean): X = {
    if (enabled) println(s"debug: $w: $x")
    x
  }

  /**
    * A parser which will first try to parse, using p, an optional X.
    * If that fails or succeeds resulting in None, we try to parse using q.
    *
    * @param p a Parser of Option[X].
    * @param q a Parser[X].
    * @tparam X the under-underlying type of p.
    * @return a Parser[X]
    */
  private def compose[X](p: Parser[Option[X]], q: Parser[X]): Parser[X] = Parser {
    in =>
      p(in) match {
        case s@this.Success(Some(x), _) => s map (_ => x)
        case _ => q(in) match {
          case s@this.Success(_, _) => s
          case _ => this.Failure("compose: failed", in)
        }
      }
  }

  /**
    * A parser which will first try to parse, using p, an optional X.
    * If that fails or succeeds resulting in None, we try to parse using q to get a Y.
    *
    * @param p a Parser of Option[X].
    * @param q a Parser[Y].
    * @tparam X the under-underlying type of p.
    * @tparam Y the underlying type of q.
    * @return a Parser of Either[Y, X]
    */
  private def composeOption[X, Y](p: Parser[Option[X]], q: Parser[Y]): Parser[Either[Y, X]] = Parser {
    in =>
      p(in) match {
        case s@this.Success(Some(x), _) => s map (_ => Right(x))
        case _ => q(in) match {
          case s@this.Success(_, _) => s map (x => Left(x))
          case _ => this.Failure("composeOption: failed", in)
        }
      }
  }
}

case class SignificantSpaceParserException(m: String) extends Exception(m)

/**
  * Trait which is used to define a logging level for the logit method of SignificantSpaceParsers.
  */
trait LogLevel

case object LogDebug extends LogLevel

case object LogInfo extends LogLevel

case object LogOff extends LogLevel

object LogLevel {
  implicit val ll: LogLevel = LogOff
}
