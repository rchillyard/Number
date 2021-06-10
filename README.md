[![Codacy Badge](https://api.codacy.com/project/badge/Grade/bb7de1b3ea4e4256997e6b1fac66281b)](https://app.codacy.com/gh/rchillyard/Number?utm_source=github.com&utm_medium=referral&utm_content=rchillyard/Number&utm_campaign=Badge_Grade)
[![CircleCI](https://circleci.com/gh/rchillyard/Number.svg?style=svg)](https://circleci.com/gh/rchillyard/Number)
![GitHub Top Languages](https://img.shields.io/github/languages/top/rchillyard/Number)
![GitHub](https://img.shields.io/github/license/rchillyard/Number)
![GitHub last commit](https://img.shields.io/github/last-commit/rchillyard/Number)
![GitHub issues](https://img.shields.io/github/issues-raw/rchillyard/Number)
![GitHub issues by-label](https://img.shields.io/github/issues/rchillyard/Number/bug)


# Number
This project is about numbers and their mathematics.
The chief features of this library are:
* all numbers are exact wherever it is possible;
* inexact numbers are represented along with their error bounds;
* lazy evaluation to help avoid temporary inexact values which become part of a result;
* there are several domains of _Number_ (expressed with different "factors") to support angles, logarithms. 

There is no such thing as accidental loss of precision (at least, provided that code follows the recommendations).
For example, if you write:

    val x = 1 / 2

your _x_ will be an _Int_ of value 0.

However, if you write the idiomatically correct form:

    val x: Number = 1 / 2

then _x_ will be a _Number_ with value __exactly__ one half.

Introduction
============
The Number project provides mathematical utilities where error bounds are tracked (and not forgotten).
All functions handle the transformation or convolution of error bounds appropriately.
When the error bound is sufficiently large compared to a number, that number is considered to be zero.
This implies that when comparing numbers, any significant overlap of their error bounds will result in them testing
as equal (according to the _compare_ function, but not the _equals_ function).

Numbers are represented internally as either _Int_, _Rational_, or _Double_.
For more detail, see Representation below.

It is of course perfectly possible to use the _Rational_ classes directly, without using the _Number_ (or _Expression_) classes.

Mill
====
The _Mill_ trait allows expressions to be evaluated using RPN (Reverse Polish Notation).
For example:

    val eo: Option[Expression] = Mill.parseMill("42 37 + 2 *").toOption.flatMap(_.evaluate)

yields the optional _Expression_ with a materialized value of 158.
See the code for other methods for defining _Mill_ operations.

The _Mill.parse_ method in turn invokes methods of _MillParser_.

The Mill offers two parsers: one is a pure RPN parser (as described above).
The other is an infix parser which uses [Dijkstra's Shunting Yard algorithm](https://en.wikipedia.org/wiki/Shunting-yard_algorithm)
to build a Mill.

Parsing
=======
A String representing a number with two or fewer decimal places is considered exact--a number with more than two decimal places is
considered fuzzy, unless it ends in two zeroes in which case it is considered exact.
Here are some examples:
* Number("1.00"): exact
* Number("1.0100"): exact
* Number("1.100"): exact
* Number("1.010"): fuzzy

You can always override this behavior by adding "*" or "..." to the end of a number with fewer than two DPs,
or by adding two 0s to the end of a number with more than two decimal places.

The rules are a little different if you define a number using a floating-point literal such as Number(1.23400),
the compiler will treat that as an fuzzy number, even though it ends with two zeroes because the compiler essentially ignores them.
However, Number(1.23) will be considered exact.
It's best always to use a String if you want to override the default behavior.

In general, the form of a number to be parsed from a String is:
  
    number ::= value? factor?
    factor ::= "Pi" | "pi" | "PI" | 𝛑 | 𝜀
    value ::= sign? nominalValue fuzz* exponent*
    nominalValue ::= integerPart ( "." fractionalPart )? | rational
    rational ::= digits "/" digits
    integerPart ::= digits
    fractionalPart ::= digits
    fuzz ::= "..." | "*" | "(" digits ")"
    exponent ::= E sign? digits

Note that the __e__ and __pi__ symbols are, respectively,
(in unicode):   \uD835\uDF00 and \uD835\uDED1 (&#xD835;&#xDF00; and &#xD835;&#xDED1;)  
A number must have at least one of the value or factor components.
If no explicit factor is specified, then the number will be a _Scalar_ (an ordinary number).
If you want to get exact trigonometric values, then it's important to specify the factor as pi (or e).

Number creation
===============
Parsing, described above is really the most precise way of specifying numerical values.
But, of course, it's a lot easier to write code that uses numerical literals.
For _Int_ and _Long_, these give us no problems, of course.
Neither is there any issue with _Rational_, _BigDecimal_, and _BigInt_.
_BigDecimal_ values are represented internally by _Rational_.
There are two ways to specify _Rational_ numbers:

* one is to create a _String_ of the form r"n/d" where n and d represent the numerator and the denominator;
* the other way is simply to write n:/d (again n and d are as above).

Either of these methods will require importing the appropriate implicit classes from _Rational_.
It's probably the simplest just to include:

    import Rational._

_Doubles_ are where the trickiest conversions apply.
Writing something like _Number(3.1415927)_ will result in a _FuzzyNumber_ with error bounds of 5 * 10^-7.
To be consistent with the _String_ representation, _Number(1.25)_ will result in an _ExactNumber_ represented internally
by a _Rational_ of 5/4.
However, if you want to force a number like 3.1415927 to be exact, then you will need to write

    Number("3.141592700")

Rendering
=========
Generally speaking, the output String corresponding to a Number will be the same as the input String,
although at this stage of the software, that is not guaranteed.
Numbers followed by "(xx)" show standard scientific notation where xx represents the standard deviation of the error
with respect to the last two digits (sometimes there is only one x which corresponds to the last digit).
If a Number is followed by "[xx]," this corresponds to a "box" (i.e. truncated uniform) probability density function.

Comparison
==========
Comparison between _Numbers_ is based on, first, equality of value.
If, after any scaling for the factors is taken into account, the two values compare equal, then the Numbers are equal.
For _ExactNumber_, comparison ends there.
However, for _FuzzyNumber_, it is then determined whether there is significant overlap
between the fuzz of the two numbers.
If the overlap is sufficient that there is deemed to be a 50% probability that the numbers are really the same,
then the comparison yields 0 (equal).
Additionally, each of the methods involved has a signature which includes a p value (the confidence probability).
  
Representation
==============
There are two kinds of _Number_: _ExactNumber_ and _FuzzyNumber_.
A _FuzzyNumber_ has a fuzz quantity which is an optional _Fuzz[Double]_.
The "value" of a _Number_ is represented by the following type:

    type Value = Either[Either[Option[Double], Rational], Int]

Thus, an integer _x_ is represented by _Right(x)_.
A _Rational_ _x_ is represented by a _Left(Right(x))_.
A _Double_ _x_ is represented by a _Left(Left(Some(x)))_.
There is also an invalid _Number_ case which is represented by _Left(Left(Left(None)))_.

This _Value_ is always of the rightmost type possible: given the various possible specializations.
Thus, an _Int_ x which is in range will be represented by _Right(x)_.
Thus, a _BigInt_ x outside the _Int_ range will be represented by _Left(Right(Rational(x)))_.
Similarly, a _Rational_ with numerator _x_ and unit denominator, where _x_ is in the range of an _Int_, will be represented by _Right(x)_.
It is also possible that a _Double_ _x_ will be represented by a _Left(Right(Rational(x)))_.
For this to happen, the value in question must have fewer than three decimal places (similar to the parsing scheme).

Factors
=======
There are three "factors:" Scalar (for ordinary dimensionless numbers), __Pi__ (used to represent radians or any multiple of pi),
and __E__ (for powers of the Euler number).
Trigonometrical functions are designed to work with __Pi__.
Such values are limited (modulated) to be in the range 0..2pi.
However, this happens as the result of operations, so it is still possible to define a value of 2pi.
For example, if you want to check that the sine of pi/2 is equal to 1 exactly, then you should write the following:

    val target = (Number.pi/2).sin
    target shouldBe Number.one

Similarly, if you use the _atan_ method on a Scalar number, the result will be a number (possibly exact) whose factor is __Pi__.

The e factor works quite differently.
It is not a simple matter of scaling.
A Number of the form Number(x, e) actually evaluates to e^x rather than e x.

Lazy Evaluation
===============
Version 1.0.3 supports lazy evaluation via a trait called _Expression_.
The advantage of lazy evaluation is not so much performance.
That's going to be neither here nor there.
But it is in avoiding precision loss in some circumstances.

For example, suppose an expression you are working on involves the square root of, say, 7.
However, you don't particularly pay attention to the fact that later on in the calculation, you square everything.
If you don't use lazy evaluation, your final result will have an error bound, even though the true value should be
proportional to exactly 7.

It's important to realize that, to get the benefit of this behavior, you have to use the _Expression_ mechanism.

      it should "give precise result for sqrt(7)^2" in {
        val seven: Expression = Number(7)
        val x = seven.sqrt
        val y = x ^ 2
        y shouldBe Number(7)
        y.materialize should matchPattern { case ExactNumber(_, _) => }
      }
      it should "show ^2 and sqrt for illustrative purposes" in {
        val seven = Number(7)
        val x = seven.sqrt
        val y = x power 2
        y shouldEqual Number(7)
        y shouldBe Number(7)
      }

The second test fails with "7.000000000000001 was not equal to 7," although if we do a fuzzy compare,
using a custom equality test, we can at least make _y shouldEqual 7_ work.

There is an implicit class _ExpressionOps_ which provides methods which allow _Number_ operations to behave as expressions.
So, for example, you can write:

    val x = Number(1) + 2

For this to compile properly, you will need to import the _ExpressionOps_ class.

From version 1.0.7, the simplification mechanism uses a feature called _Matchers_, in particular, _ExpressionMatchers_.
The current set of expression optimizations is somewhat limited, but it catches the most important cases. 

Error Bounds (Fuzziness)
========================
The error bounds are represented by the _Fuzz[Double]_ class.
A _Number_ with _None_ for the _fuzz_ is an _ExactNumber_, otherwise, _FuzzyNumber_.
The are three major attributes of fuzz: shape, style (either relative or absolute), and the value
(called _magnitude_ when absolute, and _tolerance_ when relative).
Shape describes the probability density function (PDF) of values compared to the nominal value.
There are currently only two types of shape:
* _Box_: a truncated uniform probability density function--magnitude/tolerance relate to half the width of the non-zero probability section.
* _Gaussian_: a normal probability density function: the nominal value is at the mean, and _magnitude/tolerance_ is the standard deviation.

It's easy to convert between these four different possibilities.
Generally speaking, when doing addition (or when a _Number_ is first defined), it's convenient for the fuzz to be absolute.
When performing any other operation, it's most convenient for the fuzz to be relative.
It's not possible to combine two _Box_-shaped fuzzes: it would be possible if we allowed for trapezoids as well as rectangles,
but that's far too complicated.
So, whenever we combine fuzz (using convolution), we operate on _Gaussian_ PDFs which can easily be combined.

So, why is relative fuzz usually the best? Well consider scaling--multiplying by a constant.
The relative fuzz doesn't change at all.
In the following, _f_ is a constant factor.
Let's assume that _y = f x._

Differentiating, we get,

    Δy = f Δx
    
Dividing both sides by _f_, yields

    Δy / y = Δx / x
    
Thus, the relative fuzz of _y_ is equal to the relative fuzz of _x_.
    
The same will be true, more or less, when we multiply two fuzzy numbers together.
This time, _z = x y_.
Therefore,

    Δz = y Δx + x Δy
    
Dividing both sides by _z_:

    Δz / z = Δx / x + Δy / y
    
Thus, the relative fuzz of _z_ is equal to the sum of the relative fuzzes of _x_ and _y_.
    
But, when _Δx_ and _Δy_ are taken from a _Gaussian_ probability density function, the convolution of those two PDFs,
is given by slightly different expressions depending on whether the PDFs are independent or correlated.
See the code (_Fuzz_) for details.

Things get only slightly more complex when applying monadic (single operand) functions or applying a function such
as _z = x ^ y._
Again, these formulas can be looked up in the code.

Comparing two fuzzy numbers involves subtracting the two numbers and then determining if the probability
at zero is sufficiently high to consider the difference to be zero.
If the probability is greater than 50% (the default--although there are method signatures that allow for different values),
then we consider that the different is zero (method isZero) or that it has a signum of 0.

Versions
========
The Current version is 1.0.8

Version 1.0.7: added Matchers.

Version 1.0.6: added Mill (RPN evaluator).

Version 1.0.5: reimplement the e factor.

Version 1.0.4 Made improvements to Rational, removed BigInt from Value,
and effected many refactorings.

Version 1.0.3 implements lazy evaluation.

Version 1.0.2 Included fixing the bug mentioned in 1.0.1 (actually a Rational bug), as well as adding the :/ operator
and many other fixes/features.

Version 1.0.1 Fixed many issues with minor inconsistencies.
Most important, perhaps, was the implementation of _compare_, along with _signum_ and _isZero_.
Each of these has, significantly, a signature with a confidence value (the default value is 0.5).

Initial version is 1.0.0

Future Upgrades
===============
