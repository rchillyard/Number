package com.phasmidsoftware.number.misc

import scala.util.parsing.combinator._

/**
  * @author scalaprof
  */
class FuzzyParser extends JavaTokenParsers {
  self =>
  def fuzzy: Parser[Fuzzy] = wholeNumber ~ opt(fraction) ~ opt(fuzz) ~ opt(exponent) ^^ { case i ~ x ~ f ~ e => Fuzzy(i, x, f, e) }

  def fuzzyGaussian: Parser[Fuzzy] = wholeNumber ~ opt(fraction) ~ fuzz ~ opt(exponent) ^^ { case i ~ x ~ f ~ e => Gaussian(i.toDouble, f.toDouble) }

  def fraction: Parser[String] = """\.\d*""".r

  def exponent: Parser[String] = ("E" | "e") ~> """[+-]?\d+""".r

  def fuzz: Parser[String] = "(" ~> """\d+""".r <~ ")"

  def fuzzBox: Parser[String] = "[" ~> """\d+""".r <~ "]"

  // The following are for testing purposes
  def fuzzyRep: Parser[(String, Option[String], Option[String], Option[String])] = wholeNumber ~ opt(fraction) ~ opt(fuzz) ~ opt(exponent) ^^ { case i ~ x ~ f ~ e => (i, x, f, e) }

  def nominal: Parser[(String, Option[String], Option[String])] = wholeNumber ~ opt(fraction) ~ opt(exponent) ^^ { case i ~ x ~ e => (i, x, e) }
}
