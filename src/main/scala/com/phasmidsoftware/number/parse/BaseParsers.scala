package com.phasmidsoftware.number.parse

import scala.util.parsing.combinator.JavaTokenParsers

abstract class BaseParsers extends JavaTokenParsers {

  self =>

  override def skipWhitespace: Boolean = false

  def logit[T](p: => Parser[T])(name: String)(implicit ll: LogLevel): Parser[T] = ll match {
    case LogDebug => log(p)(name)

    case LogInfo =>
      Parser { in =>
        val r = p(in)
        r match {
          case this.Success(x, _) => println(s"$name: matched $x")
          case _ =>
        }
        r
      }

    case _ => p
  }

  def trim[T](p: Parser[T]): Parser[T] = p <~ opt(whiteSpace)

  def repSepSp[T](p: Parser[T]): Parser[List[T]] = repsep(p, whiteSpace)

  def compose[X, Y](p: Parser[Option[X]], q: Parser[Y]): Parser[Either[X, Y]] = Parser {
    in =>
      p(in) match {
        case s@this.Success(Some(_), _) => s map (xo => Left(xo.get))
        case _ => q(in) match {
          case s@this.Success(_, _) => s map (x => Right(x))
          case _ => this.Failure("combine: failed", in)
        }
      }
  }

  implicit class ParserOps[X](p: Parser[X]) {
    def :|(name: String)(implicit ll: LogLevel): Parser[X] = logit(p)(name)
  }

  implicit class ParserOptionOps[X](p: Parser[Option[X]]) {
    def ?|[Y](q: => Parser[Y]): Parser[Either[X, Y]] = compose(p, q) :| "compose"
  }

  implicit val enabled: Boolean = false

  protected def debug[X](w: => String, x: X)(implicit enabled: Boolean): X = {
    if (enabled) println(s"debug: $w: $x")
    x
  }
}

trait LogLevel

case object LogDebug extends LogLevel

case object LogInfo extends LogLevel

case object LogOff extends LogLevel

object LogLevel {
  implicit val ll: LogLevel = LogOff
}
