[![Codacy Badge](https://api.codacy.com/project/badge/Grade/bb7de1b3ea4e4256997e6b1fac66281b)](https://app.codacy.com/gh/rchillyard/Number?utm_source=github.com&utm_medium=referral&utm_content=rchillyard/Number&utm_campaign=Badge_Grade)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.phasmidsoftware/number_2.13/badge.svg?color=blue)](https://maven-badges.herokuapp.com/maven-central/com.phasmidsoftware_2.13/number/)
[![CircleCI](https://circleci.com/gh/rchillyard/Number.svg?style=svg)](https://circleci.com/gh/rchillyard/Number)
![GitHub Top Languages](https://img.shields.io/github/languages/top/rchillyard/Number)
![GitHub](https://img.shields.io/github/license/rchillyard/Number)
![GitHub last commit](https://img.shields.io/github/last-commit/rchillyard/Number)
![GitHub issues](https://img.shields.io/github/issues-raw/rchillyard/Number)
![GitHub issues by-label](https://img.shields.io/github/issues/rchillyard/Number/bug)

![Example Image](docs/images/%20logo.png)
# Number
This project is about numbers and their mathematics.
The chief features of this library are:
* all numbers are exact _wherever it is possible_, including $e$ (Unicode: xD835DF00) and $\pi$ (Unicode: xD835DED1);
* inexact numbers are represented along with their error bounds;
* lazy evaluation of _expressions_ to help avoid temporary inexact values from becoming part of a result;
* there are several domains of _Number_ (expressed with different "factors") to support angles, logarithms, and roots.

There is no such thing as accidental loss of precision (at least, provided that code follows the recommendations).
For example, if you write:

    val x = 1 / 2

your _x_ will be an _Int_ of value 0, because of the way Java-style operators work (in this case, integer division).

However, if you write the idiomatically correct form:

    import com.phasmidsoftware.number.core.Number.NumberOps
    val x = 1 :/ 2

then _x_ will be a _Number_ with value __exactly__ one half.

You probably want to see some code: so go to the worksheets package and take a look, starting with
NumberWorksheet.sc, Foucault1.sc, Newton.sc, and so on.

Introduction
============
There are three articles on Medium regarding this library.
They are [Number (part 1)](https://medium.com/codex/number-part-1-c98313903714),
[Number (part 2)](https://scala-prof.medium.com/number-part-2-7925400624d5), and 
[Fuzzy, lazy, functional numeric computing in Scala](https://medium.com/codex/fuzzy-lazy-functional-numeric-computing-in-scala-4b47588d310f)

The _Number_ project provides mathematical utilities where error bounds are tracked (and not forgotten).
All functions handle the transformation or convolution of error bounds appropriately.
When the error bound is sufficiently large compared to a number, that number is considered to be zero (see **Comparison**).
This implies that, when comparing numbers, any significant overlap of their error bounds will result in them testing
as equal (according to the _compare_ function, but not the _equals_ function).

The values of Numbers are represented internally as either _Int_, _Rational_, or _Double_.
_Rational_ is simply a case class with _BigInt_ elements for the numerator and denominator.
It is, of course, perfectly possible to use the _Rational_ classes directly,
without using the _Number_ (or _Expression_) classes.

There are four domains of values, each identified by a Factor (see _Factors_ below).
These allow the exact representation of roots, logarithmic numbers, radians, and pure numbers.

Java API
========
In addition to the Scala API, version 1.0.14 introduces a Java API where it is harder to invoke the
Scala classes directly from Java.
These situations involve classes which have similar names (or have no Java equivalent).

Here are the current API specifications:

RationalJ:
----------

    def bigDecimalToRational(x: java.math.BigDecimal): Rational
    def rationalToBigDecimal(r: Rational): java.math.BigDecimal
    def bigIntegerToRational(x: BigInteger): Rational
    def rationalToBigInteger(r: Rational): BigInteger
    def longToRational(l: java.lang.Long): Rational
    def rationalToLong(r: Rational): java.lang.Long
    def doubleToRational(x: java.lang.Double): Rational
    def rationalToDouble(r: Rational): java.lang.Double
    def stringToRational(s: String): Rational

NumberJ
-------

    def bigDecimalToNumber(x: java.math.BigDecimal): Number
    def numberToBigDecimal(x: Number): java.math.BigDecimal
    def bigIntegerToNumber(x: BigInteger): Number
    def numberToBigInteger(x: Number): BigInteger
    def longToNumber(l: java.lang.Long): Number
    def numberToLong(x: Number): java.lang.Long
    def doubleToNumber(x: java.lang.Double): Number
    def stringToNumber(s: String): Number

ExpressionJ
-----------

    def add(x: Expression, y: Expression): Expression
    def multiply(x: Expression, y: Expression): Expression

Parsing
=======
A String representing a number with two or fewer decimal places is considered exact--a number with more than two decimal places is
considered fuzzy, unless it ends in two zeroes, in which case it is considered exact.
Here are some examples:
* Real("1.00"): exact
* Real("1.0100"): exact
* Real("1.100"): exact
* Real("1.010"): fuzzy

You can always override this behavior by adding "*" or "..." to the end of a number with fewer than two DPs,
or by adding two 0s to the end of a number with more than two decimal places.
* Real("1.100*")" fuzzy

See _RealWorksheet.sc_

The rules are a little different if you define a number using a floating-point literal such as _Number(1.23400)_,
the compiler will treat that as a fuzzy number, even though it ends with two zeroes because the compiler essentially ignores them.
However, _Real(1.23)_ will be considered exact while _Real(1.234)_ will not.
It's best always to use a String if you want to override the default behavior.

In general, the form of a number to be parsed from a String is:
  
    number ::= value? factor?
    factor ::= "Pi" | "pi" | "PI" | 𝛑 | 𝜀 | √ | ³√
    value ::= sign? nominalValue fuzz* exponent*
    nominalValue ::= integerPart ( "." fractionalPart )? | rational
    rational ::= digits "/" digits
    integerPart ::= digits
    fractionalPart ::= digits
    fuzz ::= "..." | "*" | "(" fuzz ")" | "[" fuzz "]"
    exponent ::= E sign? digits
    fuzz ::= one or two digits

Note that the __e__ and __pi__ symbols are, respectively,
(in Unicode): \uD835\uDF00 and \uD835\uDED1 (&#xD835;&#xDF00; and &#xD835;&#xDED1;)  
A number must have at least one of either the value or the factor components.
If no explicit factor is specified, then the number will be a _PureNumber_ (an ordinary number).
If you want to get exact trigonometric values, then it's important to specify the factor as pi (or e).

Number creation
===============
Parsing, described above, is really the most precise way of specifying numerical values.
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

For fuzzy numbers in standard scientific notation, there is an operator "~" which, when following a _Double_,
will add the next one or two integer digits as the standard deviation.
For example, the proton-electron mass ratio:

    1836.15267343~11

Rendering
=========
The _render_ method is defined in the trait _NumberLike_ and thus is defined by all subtypes,
including _Field_, _Number_, _Rational_, Complex, etc.
For the prettiest output, you should use _render_ rather than _toString_ (which is basically for debugging).

Generally speaking, the output _String_ corresponding to a _Number_ will be the same as the input _String_,
although that is not guaranteed.
Numeric quantities followed by "(xx)" show standard scientific notation where _xx_ represents the standard deviation of the error
with respect to the last two digits (sometimes there is only one _x_ which corresponds to the last digit).
If a number is followed by "\[x\]" or "\[xx\]" this corresponds to a "box" (i.e., truncated uniform) probability density function.
It's unlikely that you'll need to use this form since box is the default shape when specifying fuzzy numbers with a _String_.

For _Rational_ numbers, it is most likely that the number will be rendered as exactly as possible.
For values which are exactly renderable using decimal notation, that will be the result.
For values which have a repeating sequence in decimal notation, the repeating sequence will be enclosed within &lt; and &gt;.
If the repeating sequence is too long (or too hard to identify), and if the denominator is less than 100,000,
the number will render as a rational, i.e., numerator/denominator.
Otherwise, the number will render as many digits as possible, with "..." added to the end.

Fuzzy
=====
The _Fuzzy[X]_ trait defines a typeclass which adds fuzziness to any object type.
There is exactly one method defined and that is _same_:

    def same(p: Double)(x1: X, x2: X): Boolean

Given a confidence value _p_ (a probability between 0 and 1), this method will determine if any two objects of type _X_
can be considered the same.
If _p_ is 0, then all _Fuzzy_ quantities will be considered the same (i.e. _same_ returns true).
If _p_ is 1, then _Fuzzy_ quantities will only be considered the same if the numbers actually are exactly the same
(in practice, this generally means that passing 1 for _p_ will result in a false return).

The _fuzzyCompare_ method of _FuzzyNumber_ does use the _same_ method.

Note that the _Fuzzy_ trait assumes nothing at all about the representation of _X_, or even if _X_ is numeric.
The spec file shows an example where _X_ represents a color.
In the vast majority of cases, the _X_ of _Fuzzy_ will be _Double_.

Comparison
==========
Comparison between _Numbers_ is based on their values, providing that they belong to the same domain (see Factor, below).
If they are from different domains, one number will be converted to the domain of the other.
If, after any conversion is taken into account, the two values compare equal, then the _Numbers_ are equal.
For _ExactNumber_, comparison ends there.

However, for _FuzzyNumber_, it is then determined whether there is a significant overlap 
between the fuzz of the two numbers.
See _Fuzzy_, above.
The _FuzzyNumber_ object has a method _fuzzyCompare_, which invokes _same_ for two fuzzy numbers, given a confidence value (_p_).
This, in turn, is invoked by _fuzzyCompare_ of _GeneralNumber_, which compares this with another _Number_.

If the overlap is sufficient that there is deemed to be a 50% probability that the numbers are really the same,
then the comparison yields 0 (equal).
Additionally, each of the comparison methods involved has a signature which includes a _p_ value (the confidence probability).
The _compare(Number)_ method of _FuzzyNumber_ (arbitrarily) sets the _p_ value to be 0.5.

Mill
====
The _Mill_ trait allows expressions to be evaluated using RPN (Reverse Polish Notation).
For example:

    val eo: Option[Expression] = Mill.parseMill("42 37 + 2 *").toOption.flatMap(_.evaluate)

yields the optional _Expression_ with a materialized value of 158.
See the code for other methods for defining _Mill_ operations.

The _Mill\.parse_ method in turn invokes methods of _MillParser_.

The _Mill_ offers two parsers: one is a pure RPN parser (as described above).
The other is an infix parser that uses 
[Dijkstra's Shunting Yard algorithm](https://en.wikipedia.org/wiki/Shunting-yard_algorithm)
to build a _Mill_.

Some of the operators of _Mill_ are as follows:

    ^: Power
    +: Add
    -: Subtract
    *: Multiply (also ×)
    /: Divide (also ÷)
    v: Sqrt
    <>: Swap
    : Noop
    (: Open
    ): Close

Additional operators include _clr_, _chs_, _inv_, _ln_, _exp_, _sin_, _cos_.

Field
=====
The most general form of mathematical quantity is represented by a _Field_.
See [Field](https://en.wikipedia.org/wiki/Field_(mathematics)).
A field supports operations such as addition, subtraction, multiplication, and division.
We also support powers because, at least for integer powers, raising to a power is simply iterating over a number of multiplications.

_Field_ extends _Numerical_ which, in turn, extends _NumberLike_ (see definitions below).

The three types of _Field_ supported are _Real_, _Algebraic_, and _Complex_.
_Real_ is a wrapper around a _Number_ (see below) while _Complex_ (see below) is a wrapper around two _Number_s (more or less).

Number
======
_Number_ is a trait that extends _Numerical_ (but not _Field_).

There are two subtypes of _Number_: _ExactNumber_ and _FuzzyNumber_.
Each of these types extends an abstract type called _GeneralNumber_ (although this relationship is expected to change at some future point).
_GeneralNumber_ has three members:
* value (type _Value_): the numerical value of the number;
* factor (type _Factor_): the domain of the value (scalar, radian, log, root, etc.);
* (optional) fuzz (type _Fuzz\[Double]_): the fuzziness of the number (always _None_ for an _ExactNumber_).

Value
=====
The "value" of a _Number_ is represented by the following type (see _com.phasmidsoftware.number.package.scala_):

    type Value = Either[Either[Option[Double], Rational], Int]

Thus, an integer _x_ is represented by _Right(x)_.
A _Rational_ _x_ is represented by a _Left(Right(x))_.
A _Double_ _x_ is represented by a _Left(Left(Some(x)))_.
There is also an invalid _Number_ case (_NaN_) which is represented by _Left(Left(Left(None)))_.

This _Value_ is always of the rightmost type possible: given the various possible specializations.
Thus, an _Int_ _x_ which is in range will be represented by _Right(x)_.
Thus, a _Rational_ with numerator _x_ and unit denominator, where _x_ is in the range of an _Int_, will be represented by _Right(x)_.
It is also possible that a _Double_ _x_ will be represented by a _Left(Right(Rational(x)))_.
For this to happen, the value in question must have fewer than three decimal places (similar to the parsing scheme).

Real
====
_Real_ is a wrapper around a _Number_ and implements _Field_.
Most of the things you can do with a _Number_, you can also do with a _Real_.
In general, a Real will be part of the domain $\mathbb{R}$ but specific instances might belong to $\mathbb{N}$, $\mathbb{Z}$, or $\mathbb{Q}$.
It's even possible to have a _Real_ which belongs to $\mathbb{C}$, in the case of a root of a negative quantity.

In addition to the properties of _Field_, the following methods are defined:

    def sqrt: Field
    def sin: Field
    def cos: Field
    def tan: Field
    def atan(y: Real): Field
    def log: Field
    def exp: Field
    def toDouble: Double

For examples of usage, especially constructing _Real_ objects, please see _RealWorksheet.sc_.

Complex
=======
There are two types of _Complex_: _ComplexCartesian_ and _ComplexPolar_.
Complex numbers support all the _Field_ operations, as well as _modulus_, _argument_, _rotate_, and _conjugate_.
It is easy to convert between the two types of _Complex_.

The _ComplexPolar_ object has an additional member (as well as the real and imaginary parts):
the number of branches.
For example, the square root of 2 should have two branches, yielding: $±√2$.

There are two ways to parse a _String_ as a _Complex_ (in each case, the parsing of the _String_ is identical):
* _Complex.parse(String)_: will return a _Try\[Complex]_;
* **C**"...": will return a Complex (or throw an exception).

For example (see also _Complex.sc_).

    C"1i0" : ComplexCartesian(1,0)
    C"1-i1" : ComplexCartesian(1,-1)
    C"1ipi" : ComplexPolar(1,pi)

Additionally (see below), it is possible to define imaginary values on their own using the following syntax:

    val x = Number.i
    import SquareRoot.IntToImaginary
    val y = 2.i // to give 2i

Algebraic
=======
An _Algebraic_ is a root of some polynomial function.
It has an equation attribute and a branch attribute (where the number of branches is the degree of the polynomial).
In order to realize an _Algebraic_ as an actual numerical value (or _String_), you must solve it and thus create s
_Solution_.
A _Solution_ typically has two attributes: the base value (a _PureNumber_) and an offset which depends on the branch.
The offset will typically have a different factor such as _SquareRoot_.
An example of an _Algebraic_ is _phi_, the Golden Ratio.
An _Algebraic_ is considered an exact value, although rendering it in decimal form may result in an approximation.


An _Algebraic_ extends _Field_ so can be operated on as any other field.
Additionally, there are other operations, unique to _Algebraic_, that allow exact transformations.
Examples include scale, negate, add, multiply, etc.

Factor
======
There are three types of "factor":
* _Scalar_, in particular, _PureNumber_ (for ordinary dimensionless numbers), and _Radian_ (used to represent radians or any multiple of $\pi$);
* _Logarithmic_, in particular, _NatLog_, _Log2_, and _Log10_;
* _Root(n)_, in particular: _SquareRoot_ (for square roots) and _CubeRoot_ (for cube roots).

As of V 1.0.2, _Root_ is a subclass of _InversePower_.
The inverse power (which root) is a _Rational_ in the case of _InversePower_ but an _Int_ in the case of _Root_.

These allow certain quantities to be expressed exactly, for example, $sin(\frac{\pi}{3})$ is the square root of $\frac{3}{4}$.
The true (_Scalar_) values of the logarithmic numbers are
$e^x$, $2^x$, and $10^x$ respectively, where $x$ is the "value" of the _Number_.

Trigonometrical functions are designed to work with _Radian_ quantities.
Such values are limited (modulated) to be in the range $-\pi...\pi$.
However, this modulation typically happens inside operations or as part of _render_, so it is still possible to define a value of $2\pi$.
For example, if you want to check that the sine of $\frac{\pi}{2}$ is equal to 1 exactly, then you should write the following:

    val target = (Number.pi/2).sin
    target shouldBe Number.one

Similarly, if you use the _atan_ method on a _Scalar_ number, the result will be a number (possibly exact) whose factor is __Radian__.

The 𝜀 factor works quite differently.
It is not a simple matter of scaling.
A _Number_ of the form _Number(x, NatLog)_ actually evaluates to $e^x$ rather than $e x$.

It would be possible to implement $\pi$ values similarly to 𝜀 values (as evaluations of $e^{ix}$).
However, this is not currently done (future enhancement?).
See Complex numbers.

Negative values associated with _SquareRoot_ are imaginary numbers.

Constants
=========
Constant values of fields are defined in the _Constants_ object.
Many of the values are dependent on constants in the _Number_ class which defines values for _pi_,
$\pi$, _e_, _one_, _zero_, _i_, etc.

The _Constants_ object also contains a number of fundamental (physical and mathematical) constant definitions, in addition to those defined by _Number_.
For example, _c_ (speed of light), _alpha_ (fine structure constant), etc.

NumberLike
==========
_NumberLike_ is a trait that defines behavior which is of the most general number-like nature.
The specific methods defined are:

    def isExact(maybeFactor: Option[Factor]): Boolean // determines if this object is exact in the domain of the (optional) factor
    def isExact: Boolean = isExact(None)
    def asNumber: Option[Number]
    def render: String

Additionally, there are two methods relating to the Set of which this _NumberLike_ object is a member, such as
the integers ($\mathbb{Z}$)

    def memberOf: Option[NumberSet]
    def memberOf(set: NumberSet): Boolean

Numerical
=========
_Numerical_ extends _NumberLike_.
Additional methods include:

    def isSame(x: Numerical): Boolean // determines if this and x are equivalent, numerically.
    def isInfinite: Boolean
    def isZero: Boolean
    def isUnity: Boolean
    def signum: Int
    def unary_- : Field
    def invert: Field
    def normalize: Field
    def asComplex: Complex
    def asReal: Option[Real]

NumberSet
=========
_NumberSet_ is a trait that recognizes the following sets:
* N: $\mathbb{N}$ (the counting numbers);
* Z: $\mathbb{Z}$ (the integers);
* Q: $\mathbb{Q}$ (the rationals);
* R: $\mathbb{R}$ (the reals);
* C: $\mathbb{C}$ (the complex numbers);

The most important method is:

    def isMember(x: NumberLike): Boolean

which will yield the most exclusive set that x belongs to.

Lazy Evaluation
===============
Version 1.0.3 supports lazy evaluation via a trait called _Expression_.
The advantage of lazy evaluation is not so much performance.
That's going to be neither here nor there.
But it is in avoiding precision loss in some circumstances.
The simplification mechanism which is invoked when materializing an expression goes to great lengths to cancel out any loss of precision.

An example of this is the expression _(√3 + 1)(√3 - 1)_.
It is easy to see that this should have a value of exactly 2.
However, it is not trivial to do the appropriate matching to achieve this simplification.
This is why _Number_ uses the **Matchers** package (https://github.com/rchillyard/Matchers).

The simplification mechanism uses its own _ExpressionMatchers_, which is an extension of _Matchers_.
The current set of expression optimizations is somewhat limited, but it catches the most important cases.

For example, suppose an expression you are working on involves the square root of, say, 7.
However, you don't particularly pay attention to the fact that later on in the calculation, you square everything.
If you don't use lazy evaluation, your final result will have an error bound, even though the true value should be
proportional to exactly 7.

It's important to realize that, to get the benefit of this behavior, you must use the _Expression_ mechanism (not a pure _Number_).

      it should "give precise result for sqrt(7)^2" in {
        val x: Expression = Number(7)
        val y = x.sqrt
        val z = y ^ 2
        z.materialize shouldBe Number(7)
      }
      it should "show ^2 and sqrt for illustrative purposes" in {
        val seven = Number(7)
        val x = seven.sqrt
        val y = x power 2
        y shouldEqual Number(7)
        y shouldBe Number(7)
      }

The second test fails with "7.000000000000001 was not equal to 7," although if we do a fuzzy comparison,
using a custom equality test, we can at least make _y shouldEqual 7_ work.

NOTE: from V1.0.12 onwards, there are more special cases implemented in the _Number_ code, and so many of these issues
which required the use of _Expressions_ will now work just using _Numbers_.
This is particularly true of the example above involving the square root of 7.

There is an implicit class _ExpressionOps_ which provides methods which allow _Number_ operations to behave as expressions.
So, for example, you can write:

    val x = Number(1) + 2

For this to compile properly, you will need to import the _ExpressionOps_ class.

The one drawback of the _Expression_ mechanism is that, when you want to convert back to a _Number_, it is a little awkward.
You can use the _asNumber_ method (which returns an _Option\[Number\]_) or you can use an implicit converter
(in which case, you will need to ensure that you have Number._ imported).
If you use the latter mechanism, keep in mind that it's possible that an exception will be thrown.

Error Bounds (Fuzziness)
========================
The error bounds are represented by the _Fuzz[Double]_ class.
A _Number_ with _None_ for the _fuzz_ is an _ExactNumber_, otherwise, _FuzzyNumber_.
There are three major attributes of fuzz: shape, style (either relative or absolute), and the value
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
So, whenever we combine fuzz (using convolution), we operate on _Gaussian_ PDFs, which can easily be combined.

So, why is relative fuzz usually the best? Well, consider scaling--multiplying by a constant.
The relative fuzz doesn't change at all.
In the following, $f$ is a constant factor.
Let's assume that $y=fx$.

Differentiating, we get,

$$Δy=fΔx$$
    
Dividing both sides by _y_, yields

$$\frac{Δy}{y}=\frac{Δx}{x}$$
    
Thus, the relative fuzz of _y_ is equal to the relative fuzz of _x_.

When we multiply two fuzzy numbers together, we add the relative fuzzes together:

$$z+Δz=(x+Δx)(y+Δy)$$

Therefore, (ignoring the term which is $ΔxΔy$),

$$Δz=yΔx+xΔy$$
    
Dividing both sides by $z$:

$$\frac{Δz}{z}=\frac{Δx}{x}+\frac{Δy}{y}$$
 
Thus, the relative fuzz of _z_ is equal to the sum of the relative fuzzes of _x_ and _y_.
    
But, when _Δx_ and _Δy_ are taken from a _Gaussian_ probability density function, the convolution of those two PDFs,
is given by slightly different expressions depending on whether the PDFs are independent or correlated.
See the code (_Fuzz_) for details.

Things get only slightly more complex when applying monadic (single operand) functions or applying a function such
as $z=x^y$:

In general, when we apply a monadic operator $y=f(x)$ (such as constant factor, as above, or power, or one of the trigonometric operators),
the formula for the relative fuzz of the result $\frac{Δy}{y}$ based on the relative fuzz of the input $\frac{Δx}{x}$ is:

$$\frac{Δy}{y}=\frac{x \frac{dy}{dx}(x)}{f(x)}\frac{Δx}{x}$$

Constants cancel, powers survive as is, and so on.

For example, if $y=e^x$ then 

$$\frac{Δy}{y}=x\frac{Δx}{x}$$

Again, these formulas can be looked up in the code.

Comparing two fuzzy numbers involves subtracting the two numbers and then determining if the probability
at zero is sufficiently high to consider the difference to be zero.
If the probability is greater than 50% (the default--although there are method signatures that allow for different values),
then we consider that the different is zero (method _isZero_) or that it has a signum of 0.

Approximation
=============
The _Approximation_ object provides a method _solve_ which will implement the Newton-Raphson method of approximation
and also Halley's method (if you need it).
See Newton.sc for examples.

Continued Fractions
===================

This library includes a facility to create continued fractions which can be used to define (or approximate)
constant values.
See the worksheet _ContinuedFractions.sc_.

For example, the golden ratio ($\phi$) can be evaluated using an infinite continued fraction where
the coefficients are all 1.
Continued fractions can be used to generate "convergents" which are rational numbers and which
approximate a value.
For example, the convergents for $\pi$ include with the familiar 22/7, 355/113, etc. 

Versions
========
* Version 1.2.2: Changed the name of RQR into Quadratic and introduced Algebraic.
* Version 1.2.1: Improved RQR classes.
* Version 1.2.0: Another massive refactoring.
  - In particular:
  - ExpressionMatchers has undergone a complete re-write.
  - Solution has been added as a Field type (with RQR as a subtype of Solution).
  - Root classes have been refactored (now based on InversePower).
  -
* Version 1.1.1: Massive refactoring: fixed many issues. Most significantly, expressions are evaluated in the context of a Factor.
* Version 1.1.0: Significant refactoring:
    - Number is no longer a subtype of Field. Code should use the wrapper Real(number) to form a Field.
    - Some of the worksheets were broken and have been fixed.
* Version 1.0.17: Minor changes.
* Version 1.0.16: Added C-interpolator for Complex objects; various other fixes, including radian values now range from -pi to pi.
* Version 1.0.15: Significant improvements to the rendering of rational numbers.
* Version 1.0.14: ComplexPolar now keeps track of branches; introduced Real type. Java API.
* Version 1.0.13: Mostly cleanup together with some fixes related to Root factors and rendering of fuzziness.
* Version 1.0.12: Mostly cleanup together with some fixes related to the new factors.
* Version 1.0.11: Changes to the factors: renamed Pi as Radian, E as NatLog, and added Log2, Log10, Root2 and Root3.
* Version 1.0.10: Many improvements and fixes:
    - added Constants,
    - implicit converter from Expression to Number,
    - refactored structure of classes, 
    - totally reworked the expression matchers.
* Version 1.0.9: Added complex numbers; improved simplifications somewhat; use version 1.0.4 of Matchers (now in main).
* Version 1.0.8: This includes better simplification and, in particular, evaluates (√3 + 1)(√3 - 1) as exactly 2. 
* Version 1.0.7: added Matchers.
* Version 1.0.6: added Mill (RPN evaluator).
* Version 1.0.5: reimplement the e factor.
* Version 1.0.4 Made improvements to Rational, removed BigInt from Value,
and effected many refactorings.
* Version 1.0.3 implements lazy evaluation.
* Version 1.0.2 Included fixing the bug mentioned in 1.0.1 (actually a Rational bug), as well as adding the :/ operator
and many other fixes/features.
* Version 1.0.1 Fixed many issues with minor inconsistencies.  Most important, perhaps, was the implementation of _compare_, along with _signum_ and _isZero_. Each of these has, significantly, a signature with a confidence value (the default value is 0.5).
* Initial version is 1.0.0

Future Upgrades
===============
