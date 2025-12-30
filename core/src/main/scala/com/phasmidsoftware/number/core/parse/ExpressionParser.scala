/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.parse

import scala.util.Try
import scala.util.parsing.combinator.*

/**
  * @author scalaprof
  */
abstract class ExpressionParser[T] extends JavaTokenParsers with (String => Try[T]) {
  self =>

  def apply(s: String): Try[T]

  def div: (T, T) => T

  def negate: T => T

  def plus: (T, T) => T

  def times: (T, T) => T

  def one: T

  def zero: T

  trait Expression {
    def value: Try[T]

    def show(indent: Int): String
  }

  def lift(t: Try[T])(f: T => T): Try[T] = t map f

  def map2(t1: Try[T], t2: Try[T])(f: (T, T) => T): Try[T] = for {tt1 <- t1; tt2 <- t2} yield f(tt1, tt2)

  abstract class Factor extends Expression

  case class Expr(t: Term, ts: List[String ~ Term]) extends Expression {
    def termVal(t: String ~ Term): Try[T] = t match {
      case "+" ~ x => x.value;
      case "-" ~ x => lift(x.value)(negate);
      case z ~ _ => scala.util.Failure(ParseException(s"Expr: operator $z is not supported"))
    }

    def value: Try[T] = ts.foldLeft(t.value)((a, x) => map2(a, termVal(x))(plus))

    def termShow(t: String ~ Term, i: Int): String = t._1 + new_line(i) + t._2.show(i + 1)

    def show(i: Int): String = {
      val sb = new StringBuilder("Expr: " + new_line(i + 1) + t.show(i + 1))
      if (ts.nonEmpty) {
        sb.append(ts.foldLeft(" {")((a, x) => a + new_line(i + 1) + termShow(x, i + 1)))
        sb.append(new_line(i + 1) + "}")
      }
      sb.toString
    }
  }

  case class Term(f: Factor, fs: List[String ~ Factor]) extends Expression {
    def factorVal(t: String ~ Factor): Try[T] = t match {
      case "*" ~ x => x.value;
      case "/" ~ x => map2(Try(one), x.value)(div);
      case z ~ _ => scala.util.Failure(ParseException(s"Term: operator $z is not supported"))
    }

    def value: Try[T] = fs.foldLeft(f.value)((a, x) => map2(a, factorVal(x))(times))

    def factorShow(t: String ~ Factor, i: Int): String = t._1 + new_line(i) + t._2.show(i + 1)

    def show(i: Int): String = {
      val sb = new StringBuilder("Term: " + new_line(i + 1) + f.show(i + 1))
      if (fs.nonEmpty) {
        sb.append(fs.foldLeft(" {")((a, x) => a + new_line(i + 1) + factorShow(x, i + 1)))
        sb.append(new_line(i + 1) + "}")
      }
      sb.toString
    }
  }

  case class FloatingPoint(x: String) extends Factor {
    def value: Try[T] = self.apply(x)

    def show(i: Int): String = "GeneralNumber: " + x
  }

  case class Parentheses(e: Expr) extends Factor {
    def value: Try[T] = e.value

    def show(i: Int): String = "(" + new_line(i) + e.show(i + 1) + ")"
  }

  case class BadFactor(x: Expression) extends Factor {
    def value: Try[T] = scala.util.Failure(ParseException("\n" + x.show(0) + "\n"))

    def show(i: Int): String = "bad factor: " + new_line(i) + x.show(i + 1)
  }

  def expr: Parser[Expr] = term ~ rep("+" ~ term | "-" ~ term) ^^ {
    case t ~ x => Expr(t, x)
  }

  def term: Parser[Term] = factor ~ rep("*" ~ factor | "/" ~ factor) ^^ {
    case f ~ r => Term(f, r)
  }

  def factor: Parser[Factor] = (number | parentheses | failure("factor")) ^^ {
    case f: Factor => f
    case f => BadFactor(f)
  }

  def number: Parser[Factor] = floatingPointNumber ^^ (x => FloatingPoint(x))

  def parentheses: Parser[Parentheses] = "(" ~> expr <~ ")" ^^ (x => Parentheses(x))

  private def new_line(i: Int) = "\n" + "  ".repeat(i)
}

case class ParseException(s: String) extends Exception(s"Parse exception: $s")