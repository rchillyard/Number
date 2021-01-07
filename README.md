[![CircleCI](https://circleci.com/gh/rchillyard/Number.svg?style=svg)](https://circleci.com/gh/rchillyard/Number)


# Number
This project is about numbers and their mathematics

Introduction
============
The Number project provides mathematical utilities where error bounds are tracked (and not forgotten).
All functions handle the transformation or convolution of error bounds appropriately.
When the error bound is sufficiently large compared to a number, that number is considered to be zero.
This implies that when comparing numbers, any significant overlap of their error bounds will result in them testing
as equal (according to the _compare_ function, but not the _equals_ function).

Parsing
=======
A number with two or fewer decimal places is considered exact--a number with more than two decimal places is
considered fuzzy.
You can always override this by adding "*" or "..." to the end of a number with fewer than two DPs,
or by adding two 0s to the end of a number with more than two decimal places.

In general, the form of a number to be parsed is:
  
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
If a Number is followed by "[xx]," this corresponds to a "box" (i.e. truncated uniform) error distribution.

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
A FuzzyNumber has a fuzz quantity which is an optional _Fuzz[Double]_.
The "value" of a _Number_ is represented by the following type:

    type Value = Either[Either[Either[Option[Double], Rational], BigInt], Int]

This _Value_ is always of the rightmost type possible.
Thus, an integer which is in range will always be represented by _Right(Int)_.

Factors
=======
There are three "factors:" Scalar (for ordinary dimensionless numbers), __Pi__ (used to represent radians or any multiple of pi),
and __E__ (for multiples of the Euler constant).
Trigonometrical functions are designed to work with __Pi__.
For example, if you want to check that the sine of pi/2 is equal to 1 exactly, then you should write the following:

    val target = (Number.pi/2).sin
    target shouldBe Number.one

Similarly, if you use the _atan_ method on a Scalar number, the result will be a number (possibly exact) whose factor is __Pi__.

Versions
========
The Current version is 1.0.3
This version implements lazy evaluation.

Version 1.0.2 Was more of similar changes, including fixing the bug of 1.0.1 and implementing, inter alia,
the :/ operator for Rational.

Version 1.0.1 Fixed many issues with minor inconsistencies.
Most important, perhaps, was the implementation of _compare_, along with _signum_ and _isZero_.
Each of these has, significantly, a signature with a confidence value (the default value is 0.5).

**However**, there remains a particularly serious bug which causes a deadlock situation in the runtime (who knew that was even possible?).

Initial version is 1.0.0

Future Upgrades
===============
We expect to provide lazy Numbers in the near future.