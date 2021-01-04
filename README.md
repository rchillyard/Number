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

Note that the e and pi symbols are, respectively, (in unicode):   \uD835\uDF00 and \uD835\uDED1
A number must have at least one of the value or factor components.
If no explicit factor is specified, then the number will be a _Scalar_ (an ordinary number).
If you want to get exact trigonometric values, then it's important to specify the factor as pi (or e).

Rendering
=========
Generally speaking, the output String corresponding to a Number will be the same as the input String,
although at this stage of the software, that is not guaranteed.
Numbers followed by "(xx)" show standard scientific notation where xx represents the standard deviation of the error
with respect to the last two digits (sometimes there is only one x which corresponds to the last digit).
If a Number is followed by "[xx]," this corresponds to a "box" (i.e. truncated uniform) error distribution.

Representation
==============
There are two kinds of _Number_: _ExactNumber_ and _FuzzyNumber_.
A FuzzyNumber has a fuzz quantity which is an optional _Fuzz[Double]_.
The "value" of a _Number_ is represented by the following type:

    type Value = Either[Either[Either[Option[Double], Rational], BigInt], Int]

This _Value_ is always of the rightmost type possible.
Thus, an integer which is in range will always be represented by _Right(Int)_.


Versions
========
Current version is 1.0.1
Initial version is 1.0.0

Future Upgrades
===============
We expect to provide lazy Numbers in the near future.