![Sonatype Central](https://maven-badges.sml.io/sonatype-central/com.phasmidsoftware/number_2.13/badge.svg?color=blue)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/bb7de1b3ea4e4256997e6b1fac66281b)](https://app.codacy.com/gh/rchillyard/Number?utm_source=github.com&utm_medium=referral&utm_content=rchillyard/Number&utm_campaign=Badge_Grade)
[![CircleCI](https://dl.circleci.com/status-badge/img/gh/rchillyard/Number/tree/main.svg?style=shield)](https://dl.circleci.com/status-badge/redirect/gh/rchillyard/Number/tree/main)
![GitHub Top Languages](https://img.shields.io/github/languages/top/rchillyard/Number)
![GitHub](https://img.shields.io/github/license/rchillyard/Number)
![GitHub last commit](https://img.shields.io/github/last-commit/rchillyard/Number)
![GitHub issues](https://img.shields.io/github/issues-raw/rchillyard/Number)
![GitHub issues by-label](https://img.shields.io/github/issues/rchillyard/Number/bug)
![Logo](docs/images/%20logo.png)
# Number

This project provides exact and fuzzy numeric computation with lazy evaluation in Scala.

## Key Features

* **Exact arithmetic wherever possible** - including π, e, and √2
* **Tracked error bounds** - inexact numbers carry their uncertainty
* **Lazy evaluation** - expressions optimize away precision loss
* **Multiple numeric domains** - angles, logarithms, roots, complex, and more
* **Quantities** which are based on (eager or lazy) numeric values together with their respective units.
* **Cats integration** - leverages typelevel algebra for abstract algebra

## Design Philosophy

For a comprehensive explanation of how Number maintains exact arithmetic and when to use approximate values, see:

📖 **[Exact vs Approximate Arithmetic](docs/EXACT_VS_APPROXIMATE.md)** - Understanding materialization, symbolic
computation, and the `fuzzy` method

## Project Structure

Number is organized into multiple modules:

* **`algebra`** - Algebraic structures based on Cats typeclasses
* **`dimensions`** - Typesafe dimensional quantities and units
* **`parse`** - Parsing facilities for (lazy) expressions and (eager) algebraic structures
* **`expression`** - Lazy expression evaluation (being migrated to algebra)
* **`core`** - Legacy numeric types (Number, Field, Rational, Complex, Factor, Fuzz, ****etc.)
* **`top`** - Top level example code

#### Current Version
The current version is 1.5.6.

**Migration Note**: The `algebra` module is replacing `core.Number` and `core.Field` with a cleaner type hierarchy based on algebraic structures.
**Migration Note**: For version history and more detail regarding migration, see the [HISTORY](docs/HISTORY.md).

## Quick Start

### Using the Top Module (Recommended)
```scala

import com.phasmidsoftware.number.algebra.eager.Eager
import com.phasmidsoftware.number.algebra.util.LatexRenderer.LatexRendererOps

@main def exampleMainProgram(): Unit =
import expr.*

// Method 1: Start with identity operator
val expr0 = ∅ + 1 + 2 * 3
val expr1 = ∅ * 1 * 2 * 3

// Method 2: String interpolators
val expr2 = math"1 + 2 * 3" // resulting type is Eager
val expr3 = lazymath"$expr1∧2 + 3 * $expr1 - 5" // resulting type is a simplified Expression
val expr4 = puremath"1 + 2 * 3" // resulting type is Expression

// Method 3: Predefined constants
val expr5 = one + 2 * 3
val expr6 = π / 2

// Method 4: Explicit type annotation
val expr7: Expression = 1 + 2

```
The key import here is `import expr.*` such that as many `Expression` methods and constants are available as possible,
including the various interpolators.
For more examples, see the `GettingStarted.sc` worksheet in the `top` module.

#### Sources
Wikipedia has been my constant reference for basic mathematical relationships.
I'm also indebted to Claude (by Anthropic) for her excellent advice regarding the restructuring in versions 1.3.2 and beyond.
**Note**: While "Claude" is typically a male name in the English-speaking world, in France it is also common as a female
name. I'm particularly honoring poor Claude-Emma Debussy ("Chouchou") here.

However, many of the specific ideas and much of the theory behind this project derives from the following book:

- Abramowitz and Stegun, (1970). *Handbook of Mathematical Functions with Formulas, Graphs and Mathematical Tables, 9th printing*. Dover Publications.

You can also find the 7th printing free online:

- <cite>[Abramowitz and Stegun][1]</cite>

[1]: https://archive.org/details/handbookofmathem00abra

### Overall type hierarchy

All objects that have a "value" in `Number` are instances of `Valuable`, which in turn extends `Renderable`, `Numeric`,
, `Exactitude`, `Normalizable`, and `TypeSafe`.

Valuable has two subtypes: `Eager` and `Lazy`.
`Eager` values are evaluated and can be rendered as `String`s or converted to `Double` (or even Java Numbers).
`Lazy` values are not evaluated (they're lazy) and represent numerical expressions.

The purpose of the lazy values is that often, composing a value with another value might not be renderabl
exactly.
In such a case, eager arithmetic would be forced to evaluate the expression as a fuzzy value.

Yet, sometimes, that loss of precision is premature. For example, in the expression
$$(\sqrt{3} + 1)(\sqrt{3}-1)$$
the value should be exactly 2.

## Algebra Module

The `algebra` module provides a type hierarchy rooted at `Eager` and based on mathematical structures, with full integration of Cats typeclasses.
`Eager` is extended by `Solution`, `Nat`, and `Structure`.

The `Nat` type represents the natural numbers (non-negative integers), based on Peano arithmetic.
The `Solution` type represents the set of solutions to an equation.
`Solution` is extended by `Algebraic` (for non-complex solutions) and `Complex` (for complex solutions).

### Structure Hierarchy

All algebraic types extend `Structure`, which provides:
- Type-safe conversions: `convert[T <: Structure](t: T): Option[T]`
- Java interop: `asJavaNumber: Option[java.lang.Number]`


## Mermaid Diagrams

See [docs/DIAGRAMS.md](docs/DIAGRAMS.md) for all project diagrams.

### Monotone vs Non-Monotone Types

**`Monotone`** types have a meaningful total ordering where the order increases monotonically with the underlying value (though not always linearly):

- **`Scalar`** - Linear relationship (e.g., pure numbers)
- **`InversePower`** - Non-linear monotone (e.g., x^(-n))
- **`Transformed`** - Non-linear monotone (e.g., logarithms)

**Non-Monotone** types lack total ordering:

- **`Complex`** - No natural ordering for complex numbers
- **`Angle`** - Circular structure makes ordering meaningless

### Core Types

Some types from the legacy code (`core` module) but are still used, in particular, `Rational`, `Factor`, `Fuzziness`, and `Complex`.
The `Number` class in the legacy code (not to be confused with the class of the same name in the `algebra` module) represents a number by
specifying its value (as a `Value`), its factor (as a `Factor`), and optional fuzziness (as a `Fuzziness`).
For much more detail on these types, see below.

#### Number (algebra module)

The main numeric hierarchy supports exact and fuzzy arithmetic:
```scala
// WholeNumber - integers/naturals
val n = WholeNumber(42)

// RationalNumber - exact rationals
val third = RationalNumber(r"1/3")
val percent = RationalNumber(r"1/2", isPercentage = true)
percent.render  // "50%"

// Real - fuzzy/uncertain numbers  
val piApprox = Real("3.14159*")
```
For details of the parsing of fuzzy numbers like for `piApprox` (above), please see **Core Module Parsing** below.

#### Angle

Angles support both radians and degrees as display preferences:
```scala
val a1 = Angle(Number(1.5), radians = true)
val a2 = Angle(Number(1.5), radians = false)

a1.render  // displays in radians
a2.render  // displays in degrees

// But they compare equal (normalized values are the same)
a1 === a2  // true
```

Angles normalize to the range [-1, 1) where a full circle = 2.

**Important**: `Angle` has no `Order` instance because circular ordering is meaningless. It only supports `Eq` for equality testing.

#### Exponential

Transformed values, including `Exponential`, store a value which is then rendered as a pure number via a transforming function.
However, this makes the terminology slightly confusing.
For example, the value stored in a `Exponential` _is_ the logarithm.
Thus,

```scala
val natLog = NatLog(Number(2))  // represents e^2
```
...because 2 is the natural logarithm of e^2.

### Cats Typeclasses

The algebra module integrates with Cats to provide standard algebraic structures:

| Type             | Cats Typeclass(es)                | Meaning                                  |
|------------------|-----------------------------------|------------------------------------------|
| `RationalNumber` | `Field[RationalNumber]`           | All operations with inverses (strongest) |
| `Real`           | `Ring[Real]`                      | Addition group + multiplication monoid   |
| `WholeNumber`    | `CommutativeRing[WholeNumber]`    | No additive inverses (can't subtract)    |
| `Angle`          | `AdditiveCommutativeGroup[Angle]` | Circle group structure                   |

#### Why These Typeclasses?

- **`RationalNumber`** gets `Field` because rationals support all operations: +, -, ×, ÷
- **`Real`** gets `Ring` (not `Field`) because fuzzy numbers don't have proper multiplicative inverses
- **`WholeNumber`** gets `CommutativeRing` because you can't subtract and stay in whole numbers
- **`Angle`** gets `AdditiveCommutativeGroup` for the circle group, but multiplication doesn't make geometric sense

### Equality and Testing

The algebra module provides both Cats `Eq` instances and ScalaTest `Equality` instances:
```scala
import cats.syntax.eq._
import com.phasmidsoftware.number.algebra.StructuralEquality._

// Using Cats Eq in production code
angle1 === angle2

// Using Scalactic Equality in tests
class MySpec extends AnyFlatSpec with Matchers with StructuralEquality {
  angle1 should === (angle2)
}
```

For `Angle` and `RationalNumber`, equality ignores display flags (radians/degrees, percentage) and compares normalized mathematical values.

Additionally, the algebra module provides both `FuzzyEq` instances for `Eager` types:
```scala
import com.phasmidsoftware.number.algebra.core.FuzzyEq.~=

val x: Eager = Real(scala.math.Pi)
val y: Eager = Angle(Real(1.0))
x == y // false
x ~= y // true
```
## Dimensions Module

Number includes support for dimensional quantities and units.

Although there are many pre-defined `Dimension` and `Units` available, it is extremely easy
to build new ones:

```scala
val Fortnight: Unit[Time] = Day.scaled(14, "ftn")
val Furlong: Unit[Length] = Chain.scaled(10, "fur")
val Firkin: Unit[Mass] = Pound.scaled(90, "fir")
val FurlongPerFortnight: Unit[Velocity] = Furlong / Fortnight

Quantity(1, C).in(FurlongPerFortnight) match {
  case Some(q@Quantity[Velocity] (RationalNumber (x, false), units) ) =>
    println (q.renderLaTeX) // the value cannot be rendered exactly using decimal notation.
    println (x.toDouble) // should be approximately 1.8026E12 according to Wikipedia
  case _ =>
    println("No speed of light found.")
}

```

### Comparison with JSR-385

JSR-385 ("Units of Measurement API") is Java's standardized approach to quantities and units. While both systems share
similar goals, this library takes a fundamentally different approach:

#### Type Safety

**JSR-385**: Uses runtime dimension checking with parameterized types

```java
Quantity<Length> distance = Quantities.getQuantity(5, Units.METRE);
Quantity<Speed> speed = distance.divide(time).asType(Speed.class); // runtime cast
```

**This library**: Uses compile-time type-level dimension arithmetic

```scala
val distance: PhysicalUnit[Length] = Meter * 5
val speed: PhysicalUnit[Velocity] = distance / time // type computed at compile time
```

### Dimension Tracking

**JSR-385**: Dimensions checked at runtime through the `Dimension` interface

**This library**: Dimensions represented as type-level rationals (`BaseDim[M, L, T, I, Θ, N, J]`) where each exponent is
computed at compile time using match types

### Compositional Design

**JSR-385**: Predefined units must be explicitly registered

**This library**: Units can be composed on-the-fly

```scala
val pressure = Newton / Meter.squared // creates Pressure dimension automatically
val myCustomUnit = Kilogram * Meter.cubed / Second.squared // any combination works
```

#### Mathematical Precision

**JSR-385**: Typically uses floating-point arithmetic

**This library**: Built on exact arithmetic (`RationalNumber`, `AlgebraicNumber`) with optional fuzzy arithmetic for
uncertainty propagation

#### Advantages of This Library

- **Stronger type safety**: Dimension errors caught at compile time, not runtime
- **Mathematical rigor**: Exact arithmetic eliminates floating-point errors
- **Composability**: Define complex units through simple operations
- **Pedagogical value**: Type-level dimension arithmetic makes the mathematics explicit
- **Scala 3 features**: Leverages match types, singleton types, and type-level computation

#### Advantages of JSR-385

- **Standardization**: Industry-standard Java API
- **Ecosystem**: Large library of predefined units and quantity types
- **Adoption**: Supported across the Java ecosystem
- **Maturity**: Well-tested in production environments

#### When to Use Each

Use **JSR-385** if you need Java interoperability or want a mature, standardized library with extensive unit
definitions.

Use **this library** if you want compile-time type safety, exact arithmetic, and are working in a Scala 3 environment
where mathematical precision and type-level guarantees are priorities.

## Parse Module

Number supports flexible parsing of numeric values from strings, with automatic detection of exact vs. fuzzy numbers.

### Algebra Module Parsing

The algebra module provides limited parsing currently:
```scala
import com.phasmidsoftware.number.algebra._

val maybeTheAnswer: Option[RationalNumber] = RationalNumber.parse("42")
val maybeHalf: Option[RationalNumber] = RationalNumber.parse("1/2")
val maybeSevenPercent: Option[RationalNumber] = RationalNumber.parse("7%")
```

For more complex expression parsing with LaTeX-style syntax, see the **Expression Module** section below, which provides the `math`, `lazymath`, `puremath`, and `mathOpt` interpolators.

### Core Module Parsing

A String representing a number with 14 or fewer decimal places is considered exact--a number with more than 14 decimal
places is
considered fuzzy, unless it ends in two zeroes, in which case it is considered exact.
Here are some examples:
* Real("1.00"): exact
* Real("1.0100"): exact
* Real("1.100"): exact
* Real("1.010"): exact

You can always override this behavior by adding "*" or "..." to the end of a number with fewer than 14 DPs,
or by adding two 0s to the end of a number with more than 14 decimal places.
* Real("1.100*")" fuzzy

See _RealWorksheet.sc_

The rules are a little different if you define a number using a floating-point literal such as _Number(1.23400)_,
the compiler will treat that as a fuzzy number, even though it ends with two zeroes because the compiler essentially ignores them.
However, _Real(1.23)_ will be considered exact while _Real(1.234)_ will not.
It's best always to use a String if you want to be explicit about the fuzziness.

In general, the form of a number to be parsed from a String is:

```ebnf
// Grammar for parsing Number strings
number ::= value? factor?
factor ::= "Pi" | "pi" | "PI" | π | ε | √ | ³√
value ::= sign? nominalValue fuzz* exponent*
nominalValue ::= integerPart ( "." fractionalPart )? | rational
rational ::= digits "/" digits
integerPart ::= digits
fractionalPart ::= digits
fuzz ::= "..." | "*" | "(" fuzz ")" | "[" fuzz "]"
exponent ::= E sign? digits
fuzz ::= one or two digits
```

Note that the __e__ and __pi__ symbols are, respectively,
(in Unicode): \uD835\uDF00 and \uD835\uDED1 (&#xD835;&#xDF00; and &#xD835;&#xDED1;)  
A number must have at least one of either the value or the factor components.
If no explicit factor is specified, then the number will be a _PureNumber_ (an ordinary number).
If you want to get exact trigonometric values, then it's important to specify the factor as $\pi$ (or e).

### Number creation
Parsing, described above, is really the most precise way of specifying numerical values.
But, of course, it's a lot easier to write code that uses numerical literals.
For _Int_ and _Long_, these give us no problems, of course.
Neither is there any issue with _Rational_, _BigDecimal_, and _BigInt_.
_BigDecimal_ values are represented internally by _Rational_.
There are two ways to specify _Rational_ numbers:

* one is to create a _String_ of the form _r"n/d"_ where _n_ and _d_ represent the numerator and the denominator;
* the other way is simply to write _n:/d_ (again _n_ and _d_ are as above).

Either of these methods will require importing the appropriate implicit classes from _Rational_.
It's probably the simplest just to include:

```scala
import Rational._
```

_Doubles_ are where the trickiest conversions apply.
Writing something like _Number(3.1415927)_ will result in a _FuzzyNumber_ with error bounds of 5 * 10∧-7.
To be consistent with the _String_ representation, _Number(1.25)_ will result in an _ExactNumber_ represented internally
by a _Rational_ of 5/4.
However, if you want to force a number like 3.1415927 to be exact, then you will need to write

    Number("3.141592700")

For fuzzy numbers in standard scientific notation, there is an operator "~" which, when following a _Double_,
will add the next one or two integer digits as the standard deviation.
For example, the proton-electron mass ratio:

    1836.15267343~11

### Rendering
There are three types of rendering available: _render_, _toLatex_ and _toString_.
The latter method is primarily for debugging purposes and so tends to mirror the actual structure of an object.
The _render_ method is defined in the trait _Renderable_.
Its purpose is to render a _Valuable_ object in as natural and appropriate a form as possible.
The latex renderer is defined in the trait _LatexRenderable_.
Its purpose is to render a _Valuable_ object in a form suitable for use in a LaTeX document,
that's to say as a mathematical expression.
For this to work, you will need to import com.phasmidsoftware.number.algebra.util.LatexRenderer._

The rest of this section pertains to the `core` module.
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

### Fuzzy
The _Fuzzy[X]_ trait defines a typeclass which adds fuzziness to any object type.
There is exactly one method defined and that is _same_:

```scala
def same(p: Double)(x1: X, x2: X): Boolean
```

Given a confidence value _p_ (a probability between 0 and 1), this method will determine if any two objects of type _X_
can be considered the same.
If _p_ is 0, then all _Fuzzy_ quantities will be considered the same (i.e. _same_ returns true).
If _p_ is 1, then _Fuzzy_ quantities will only be considered the same if the numbers actually are exactly the same
(in practice, this generally means that passing 1 for _p_ will result in a false return).

The _fuzzyCompare_ method of _FuzzyNumber_ does use the _same_ method.

Note that the _Fuzzy_ trait assumes nothing at all about the representation of _X_, or even if _X_ is numeric.
The spec file shows an example where _X_ represents a color.
In the vast majority of cases, the _X_ of _Fuzzy_ will be _Double_.

### Comparison
Comparison between _Numbers_ is based on their values, providing that they belong to the same domain (see _Factor_, below).
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

### Mill
The _Mill_ trait allows expressions to be evaluated using RPN (Reverse Polish Notation).
For example:

```scala
val eo: Option[Expression] = Mill.parseMill("42 37 + 2 *").toOption.flatMap(_.evaluate)
```

yields the optional _Expression_ with a materialized value of 158.
See the code for other methods for defining _Mill_ operations.

The _Mill\.parse_ method in turn invokes methods of _MillParser_.

The _Mill_ offers two parsers: one is a pure RPN parser (as described above).
The other is an infix parser that uses
[Dijkstra's Shunting Yard algorithm](https://en.wikipedia.org/wiki/Shunting-yard_algorithm)
to build a _Mill_.

Some of the operators of _Mill_ are as follows:

    ∧: Power (also ^)
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

### (core) Field
The most general form of mathematical quantity is represented by a _Field_.
See [Field](https://en.wikipedia.org/wiki/Field_(mathematics)).
A field supports operations such as addition, subtraction, multiplication, and division.
We also support powers because, at least for integer powers, raising to a power is simply iterating over a number of multiplications.

_Field_ extends _Numerical_ which, in turn, extends _NumberLike_ (see definitions below).

The three types of _Field_ supported are _Real_, _Algebraic_, and _Complex_.
_Real_ is a wrapper around a _Number_ (see below) while _Complex_ (see below) is a wrapper around two _Number_s (more or less).

### (core) Number
_Number_ is a trait that extends _Numerical_ (but not _Field_).

There are two subtypes of _Number_: _ExactNumber_ and _FuzzyNumber_.
Each of these types extends an abstract type called _GeneralNumber_ (although this relationship is expected to change at some future point).
_GeneralNumber_ has three members:
* value (type _Value_): the numerical value of the number;
* factor (type _Factor_): the domain of the value (scalar, radian, log, root, etc.);
* (optional) fuzz (type _Fuzz\[Double]_): the fuzziness of the number (always _None_ for an _ExactNumber_).

### Value
The "value" of a _Number_ is represented by the following type (see _com.phasmidsoftware.number.package.scala_):

```scala
type Value = Either[Either[Option[Double], Rational], Int]
```

Thus, an integer _x_ is represented by _Right(x)_.
A _Rational_ _x_ is represented by a _Left(Right(x))_.
A _Double_ _x_ is represented by a _Left(Left(Some(x)))_.
There is also an invalid _Number_ case (_NaN_) which is represented by _Left(Left(Left(None)))_.

This _Value_ is always of the rightmost type possible: given the various possible specializations.
Thus, an _Int_ _x_ which is in range will be represented by _Right(x)_.
Thus, a _Rational_ with numerator _x_ and unit denominator, where _x_ is in the range of an _Int_, will be represented by _Right(x)_.
It is also possible that a _Double_ _x_ will be represented by a _Left(Right(Rational(x)))_.
For this to happen, the value in question must have fewer than three decimal places (similar to the parsing scheme).

### Real
_Real_ is a wrapper around a _Number_ and implements _Field_.
Most of the things you can do with a _Number_, you can also do with a _Real_.
In general, a Real will be part of the domain $\mathbb{R}$ but specific instances might belong to $\mathbb{N}$, $\mathbb{Z}$, or $\mathbb{Q}$.
It's even possible to have a _Real_ which belongs to $\mathbb{C}$, in the case of a root of a negative quantity.

In addition to the properties of _Field_, the following methods are defined:

```scala
def sqrt: Field
def sin: Field
def cos: Field
def tan: Field
def atan(y: Real): Field
def log(b: Real): Field
def ln: Field
def exp: Field
def toDouble: Double
```

For examples of usage, especially constructing _Real_ objects, please see _RealWorksheet.sc_.

### Complex
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

```scala
val x = Number.i
import SquareRoot.IntToImaginary
val y = 2.i // to give 2i
```

### Algebraic
An _Algebraic_ is a particular root of some polynomial function.
It has an equation attribute (of type _Equation_) and a branch attribute (where the number of branches is the degree of the polynomial).
In order to realize an _Algebraic_ as an actual numerical value (or _String_), you must solve it and thus create s
_Solution_.
A _Solution_ typically has two parts: the base value (a _PureNumber_) and the offset (which depends on the branch).
The offset will typically have a different factor such as _SquareRoot_.
An example of an _Algebraic_ is _phi_, the Golden Ratio.
An _Algebraic_ is considered an exact value, although rendering it in decimal form may result in an approximation.

An _Algebraic_ extends _Field_ so can be operated on as any other field.
Additionally, there are other operations, unique to _Algebraic_, that allow exact transformations.
Examples include scale, negate, add, multiply, etc.

_Algebraic_ is a parallel concept to _Complex_ (and _Real_).

The hierarchy of _Algebraic_ is (currently) as follows:
* _Algebraic_
    * _Algebraic_Linear_ (case class representing a Rational number defined as the root (solution) of a monic linear equation)
    * _Algebraic_Quadratic_ (case class representing a quantity defined as the root (solution) of a monic quadratic equation)

### Factor
Factor represents the domain in which a numerical quantity exists.
We are most familiar with the pure-number domain, including all the counting numbers,
the decimal numbers, and the so-called "real" numbers.

Slightly less familiar are numbers like $2\pi$ (we call the domain of such numbers "radians"), $\log_e 2$, $e∧2$, and $√2$.
All these numbers can be represented exactly by judicious use of the following classes.

The hierarchy of _Factor_ is as follows:
* _Factor_ (trait: the domain of factors)
    * _Scalar_ (trait: the domain of ordinary numbers)
        * _PureNumber_ (object: the domain of pure numbers)
        * _Radian_ (object: the domain of radians)
  * _Exponential_ (trait: the domain of exponential quantities where the corresponding value is a logarithm)
      * _NaturalExponential_
      * _BinaryExponential_
    * _InversePower_ (trait: all the roots)
        * _NthRoot_ (abstract class)
            * _SquareRoot_ (object: the domain of square roots)
            * _CubeRoot_ (object: the domain of cube roots)
        * _AnyRoot_ (case class: a generalized root based on a _Rational_)

As of V 1.0.2, _NthRoot_ is a subclass of _InversePower_.
The inverse power (which root) is a _Rational_ in the case of _InversePower_ but an _Int_ in the case of _NthRoot_.

These allow certain quantities to be expressed exactly, for example, $sin(\frac{\pi}{3})$ is the square root of $\frac{3}{4}$.
The true (_Scalar_) values of the logarithmic numbers are
$e∧x$, $2∧x$, and $10∧x$ respectively, where $x$ is the "value" of the _Number_.

Trigonometrical functions are designed to work with _Radian_ quantities.
Such values are limited (modulated) to be in the range $-\pi...\pi$.
However, this modulation typically happens inside operations or as part of _render_, so it is still possible to define a value of $2\pi$.
For example, if you want to check that the sine of $\frac{\pi}{2}$ is equal to 1 exactly, then you should write the following:

```scala
val target = (Number.pi/2).sin
target shouldBe Number.one
```
Similarly, if you use the _atan_ method on a _Scalar_ number, the result will be a number (possibly exact) whose factor is __Radian__.

The 𝜀 factor works quite differently.
It is not a simple matter of scaling.
A _Number_ of the form _Number(x, NatLog)_ actually evaluates to $e∧x$ rather than $e x$.

It would be possible to implement $\pi$ values similarly to 𝜀 values (as evaluations of $e∧{ix}$).
However, this is not currently done (future enhancement?).
See Complex numbers.

Negative values associated with _SquareRoot_ are imaginary numbers.

### Constants
Constant values of fields are defined in the _Constants_ object.
Many of the values are dependent on constants in the _Number_ class which defines values for _pi_,
$\pi$, _e_, _one_, _zero_, _i_, etc.

The _Constants_ object also contains a number of fundamental (physical and mathematical) constant definitions, in addition to those defined by _Number_.
For example, _c_ (speed of light), _alpha_ (fine structure constant), etc.

### NumberLike (obsolete)
_NumberLike_ is a trait that defines behavior which is of the most general number-like nature.
The specific methods defined are:

```scala
def isExact(maybeFactor: Option[Factor]): Boolean // determines if this object is exact in the domain of the (optional) factor
def isExact: Boolean = isExact(None)
def asNumber: Option[Number]
def render: String
```

Additionally, there are two methods relating to the Set of which this _NumberLike_ object is a member, such as
the integers ($\mathbb{Z}$)

```scala
def memberOf: Option[NumberSet]
def memberOf(set: NumberSet): Boolean
```

### Numerical
_Numerical_ extends _NumberLike_.
Additional methods include:

```scala
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
```

### NumberSet (obsolete)
_NumberSet_ is a trait that recognizes the following sets:
* N: $\mathbb{N}$ (the counting numbers);
* Z: $\mathbb{Z}$ (the integers);
* Q: $\mathbb{Q}$ (the rationals);
* R: $\mathbb{R}$ (the reals);
* C: $\mathbb{C}$ (the complex numbers);

The most important method is:

```scala
def isMember(x: NumberLike): Boolean
```

which will yield the most exclusive set that x belongs to.

### Lazy Evaluation
`Number` lazy evaluation via a trait called _Expression_.
The advantage of lazy evaluation is not so much performance.
That's going to be neither here nor there.
But it is in avoiding precision loss in some circumstances.
The simplification mechanism which is invoked when materializing an expression goes to great lengths to cancel out any loss of precision.

An example of this is the expression _(√3 + 1)(√3 - 1)_.
It is clear that this should have a value of exactly 2.
However, it is not trivial to do the appropriate matching to achieve this simplification.
This is why _Number_ uses the **Matchers** package (https://github.com/rchillyard/Matchers).

The simplification mechanism uses its own _ExpressionMatchers_, which is an extension of _Matchers_.
The current set of expression optimizations is somewhat limited, but it catches the most important cases.

For example, suppose an expression you are working on involves the square root of, say, 7.
However, you don't particularly pay attention to the fact that later on in the calculation, you square everything.
If you don't use lazy evaluation, your final result will have an error bound, even though the true value should be
proportional to exactly 7.

It's important to realize that, to get the benefit of this behavior, you must use the _Expression_ mechanism (not a pure _Number_).

      it should "give precise result for sqrt(7)∧2" in {
        val x: Expression = Number(7)
        val y = x.sqrt
        val z = y ∧ 2
        z.materialize shouldBe Number(7)
      }
      it should "show ∧2 and sqrt for illustrative purposes" in {
        val seven = Number(7)
        val x = seven.sqrt
        val y = x ∧ 2
        y shouldEqual Number(7)
        y shouldBe Number(7)
      }

The second test fails with "7.000000000000001 was not equal to 7," although if we do a fuzzy comparison,
using a custom equality test, we can at least make _y shouldEqual 7_ work.

NOTE: from V 1.0.12 onwards, there are more special cases implemented in the _Number_ code, and so many of these issues
which required the use of _Expressions_ will now work just using _Numbers_.
This is particularly true of the example above involving the square root of 7.

There is an implicit class _ExpressionOps_ which provides methods which allow _Number_ operations to behave as expressions.
So, for example, you can write:

```scala
val x = Number(1) + 2
```

For this to compile properly, you will need to import the _ExpressionOps_ class.

The one drawback of the _Expression_ mechanism is that, when you want to convert back to a _Number_, it is a little awkward.
You can use the _asNumber_ method (which returns an _Option\[Number\]_), or you can use an implicit converter
(in which case, you will need to ensure that you have _Number.__ imported).
If you use the latter mechanism, keep in mind that it's possible that an exception will be thrown.

See below for the different types of _Expression_.

### Context
When evaluating an _Expression_, we need to know what are the acceptable contexts for the evaluation.
For instance, if we are going to try to print a number as a decimal representation (of a binary representation),
we will need to evaluate the number in the _RestrictedContext(PureNumber)_ context.
Often, this will require an approximation (i.e., the generation of a _FuzzyNumber_).

The _hierarchy_ of Context is as follows:
* _Context_ (trait: closely related to Factor: it is used to determine which domains are acceptable in a particular context)
    * _RestrictedContext_ (case class accepts only a specific _Factor_)
    * _AnyContext_ (object: accepts any _Factor_)
    * _ImpossibleContext_ (object: accepts no _Factor_)

### Transcendental Numbers
Transcendental numbers are declared as subtypes of _Transcendental_, although $\pi$ and e are, additionally, declared as _Number_, _Real_, and _Expression_.
A transcendental is declared as a (named) _Expression_, where the expression might simply be a constant.
For example, L2 is defined as the natural log of 2 and has a name: "ln(2)".
Another example is $\gamma$, the Euler-Mascheroni constant.
See the _Introduction.sc_ worksheet for examples of usage.
The current list of transcendental numbers includes: $\pi$, e, $\gamma$, _ln(2)_, _lg(e)_, where _lg_ represents log to the base 2.

### Error Bounds (Fuzziness)
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
as $z=x∧y$:

In general, when we apply a monadic operator $y=f(x)$ (such as constant factor, as above, or power, or one of the trigonometric operators),
the formula for the relative fuzz of the result $\frac{Δy}{y}$ based on the relative fuzz of the input $\frac{Δx}{x}$ is:

$$\frac{Δy}{y}=\frac{x \frac{dy}{dx}(x)}{f(x)}\frac{Δx}{x}$$

Constants cancel, powers survive as is and so on.

For example, if $y=e∧x$ then

$$\frac{Δy}{y}=x\frac{Δx}{x}$$

Again, these formulas can be looked up in the code.

Comparing two fuzzy numbers involves subtracting the two numbers and then determining if the probability
at zero is sufficiently high to consider the difference to be zero.
If the probability is greater than 50% (the default--although there are method signatures that allow for different values),
then we consider that the different is zero (method _isZero_) or that it has a signum of 0.

### Numeric Operations
Numeric operations (i.e., eager operations) are performed using a set of subtypes of _Operation_.
The common feature of these _Operation_ types is that they provide a set of functions
each of which can be applied to a different type of _Value_ (viz., _Int_, _Rational_, _Double_).

The hierarchy of Operation is as follows:
* _Operation_ (trait)
    * _MonadicOperation_ (trait)
        * _MonadicOperationAtan_
        * _MonadicOperationNegate_
        * _MonadicOperationLog_
        * _MonadicOperationModulate_
        * _MonadicOperationScale_
        * _MonadicOperationSin_
        * _MonadicOperationFunc_
        * _MonadicOperationSqrt_
        * _MonadicOperationInvert_
        * _MonadicOperationExp_
    * _DyadicOperation_ (trait)
        * _DyadicOperationPlus_
        * _DyadicOperationTimes_
        * _DyadicOperationPower_

### Approximation
The _Approximation_ object provides a method _solve_ which will implement the Newton-Raphson method of approximation
and also Halley's method (if you need it).
See Newton.sc for examples.

### Continued Fractions
This library includes a facility to create continued fractions which can be used to define (or approximate)
constant values.
See the worksheet _ContinuedFractions.sc_.

For example, the golden ratio ($\phi$) can be evaluated using an infinite continued fraction where
the coefficients are all 1.
Continued fractions can be used to generate "convergents" which are rational numbers and which
approximate a value.
For example, the convergents for $\pi$ include with the familiar 22/7, 355/113, etc.

### Expressions
The lazy mechanism (see above) is based on _Expressions_.
Expressions are simplified (essentially, normalization) before being materialized.
The simplification mechanism is based on the _ExpressionMatchers_ package and is described
in the following document:

- **[Expression Simplification Pipeline](docs/testing/SimplificationPipeline.md)** - Detailed guide to the multi-phase simplification system

In the following, by "exact," we mean a quantity that is exact (like $\pi$ or $√2$),
even though it might not be possible to represent it exactly using
base-10 (or base-2) notation.
Obviously, we could represent $\pi$ exactly if we wrote it in base-$\pi$ notation.

The hierarchy of _Expression_ (i.e., lazy) types is as follows (as of version V 1.2.8):
* _Expression_ (trait: all lazy _NumberLike_ objects)
    * _AtomicExpression_ (trait: a single exact number)
        * _FieldExpression_ (abstract class)
            * _Literal_ (case class: defined as a _Field_ and an optional name)
            * _NamedConstant_ (abstract class)
                * _ScalarConstant_ (abstract class)
                    * _Zero_
                    * _One_
                    * _Two_
                    * _Half_
                    * _MinusOne_
                    * _ConstPi_
                * _Infinity_
                * _ConstE_
                * _ConstI_
        * _Transcendental_ (trait) defining exact numbers with no other definition
        * _Root_
            * _phi_ (object: $\phi$, the Golden Ratio)
            * _psi_ (object: $\psi$, the conjugate of $\phi$)
        * _Noop_ (object: not an expression)
    * _CompositeExpression_ (trait for any Expression that is defined by a tree of functions)
        * _BiFunction_ (case class: two expressions combined by a dyadic function--see below)
        * _Function_ (case class: one expression modified by a function--see below)
        * _Aggregate_ (case class: similar to _BiFunction_ but with multiple expressions all combined by the same dyadic function)
* _ExpressionFunction_ (trait)
    * _ExpressionBiFunction_ (trait used in _BiFunction_ (above))
        * _Atan_
        * _Log_
        * _Sum_
        * _Product_
        * _Power_
    * _ExpressionMonoFunction_ (trait used in _Function_ above)
        * _Cosine_
        * _Sine_
        * _Exp_
        * _Ln_
        * _Negate_
        * _Reciprocal_

### Other Types
Other types (for reference):
* _Factor_ (see above)
* _Context_ (see above)
* _Multivalued_
    * _Equation_ (trait defining an equation to be used in an _Algebraic_ quantity)
        * _Quadratic_ (case class defining a monic quadratic equation)
        * _LinearEquation_ (case class defining a monic linear equation)
* _Fuzz_
    * _GeneralNumber_
    * _Number_
    * _FuzzyNumber_
* _Fuzziness_
    * _RelativeFuzz_ (case class to represent relative values of fuzziness)
    * _AbsoluteFuzz_ (case class to represent absolute values of fuzziness)
* _Shape_
    * _Box_ (a probability density function for errors which is in the shape of a box)
    * _Gaussian_ (a probability density function for errors which is in the shape of a "normal" (Gaussian) distribution)
* _Valuable_ (type-class trait used in fuzzy arithmetic and which extends _Fractional_ from the Scala library)
    * _ValuableDouble_
* _NumberSet_
    * _N_ (the natural, i.e., counting numbers)
    * _Z_ (the integers, i.e., whole numbers)
    * _Q_ (the rational numbers)
    * _R_ (the real numbers)
    * _C_ (the complex numbers)
* _Approximation_ (object with methods for solving functions using the Newton-Raphson method or, more generally, Householder's method)
* _Approximatable_ (supertype of _Field_ and _Expression_)
* _Numerical_ (super-trait of _Field_ and _Number_)
* _Operation_ (see above)
* _Prime_ (case class)
* _ContinuedFraction_ (case class)
* _Evaluatable_ (trait)
    * _ConFrac_ (class)

## Expression Module

Expressions are lazily evaluated and so can be used to avoid any unnecessary computation and,
especially, any approximation of what should be an exact value.

**Note** The expression module has been cloned and restructured from the `core` module. Although the expression package
still exists in core, it should not be used directly.

The Expression trait supports the following operations:

* **`materialize`** - Simplifies and evaluates the expression (as an `Eager`) by applying rules of arithmetic.
* **`simplify`** - Simplifies the expression by applying rules of arithmetic, returning a new `Expression`.
* **`approximation`** - Approximates the expression as an `Option[Real]` value, but only if the expression is not exact.
  An exact expression can be approximated by passing the parameter `force=true` into this method.
* **`evaluateAsIs`** - Evaluates the expression to an `Option[Eager]` value, which will be defined providing that the
  expression is exact (i.e., it can be evaluated in the natural context of the expression).
* **`evaluate(Context)`** - Evaluates the expression to an `Option[Eager]` value, in the given context.

There is additionally, an implicit conversion from `Expression` to `Eager`, provided that you have used the following
import, for example:

```scala
import Expression._

val expression = ∅ + 6 :* Literal(RationalNumber(2, 3)) :+ One
val eager: Eager = expression // yields 5
```

The best way to define expressions (or the eager values) is to use the LaTeX-like syntax which you can invoke
using one of the following interpolators (defined in `com.phasmidsoftware.number.parse.ExpressionParser`):

* **`puremath`** - Parses an infix expression resulting in an `Expression` but without any simplification.
* **`lazymath`** - Equivalent to `puremath` but with simplification.
* **`mathOpt`** - Parses, simplifies, and evaluates an expression, returning an `Option[Eager]` rather than an
  `Expression`.
* **`math`** - Parses, simplifies, and evaluates an expression (same as `mathOpt`), but returns an `Eager` rather than
  an `Option[Eager]`. Note that an exception may be thrown if the expression given cannot be parsed.

The following examples illustrate the use of the `math` interpolator:

```scala
val theAnswer: Eager = math"6*(3+4)"
val seven: Eager = math"""\frac{42}{6}"""
val rootTwo: Eager = math"""\sqrt{2}"""
val rootTwoAlt: Eager = math"√2"
val pi: Eager = math"""\pi"""
val twoPi: Eager = math"""2\pi"""
```

You just need to import the interpolators as follows:

```scala
import com.phasmidsoftware.number.parse.ExpressionParser.*
```

Another way to define expressions is to use the empty expression symbol `∅`.
For example:

```scala
val theAnswer0: Expression = ∅ + 42 // Defines an expression which will evaluate to 42
val theAnswer1: Expression = ∅ * 42 // Also defines an expression which will evaluate to 42
val theAnswer2: Expression = ∅ + 𝛑 + 42 - 𝛑 // Also defines an expression which will evaluate to 42 (exactly)
```

The empty expression ∅ evaluates to the identity for either additive or multiplicative operations.

## Core Module

**Note**: This section describes the legacy `core` module (as of version 1.2.11). New code should use the `algebra` module (see above). The `core` module is being gradually superseded as we migrate to a cleaner type hierarchy based on mathematical structures.

The core module provides the original implementation of fuzzy, lazy numbers with the following features:
* Exact arithmetic using Value types (Int, Rational, or Double)
* Factor-based domains for different numeric types
* Expression-based lazy evaluation
* Comprehensive fuzzy number support with error tracking

There is no such thing as accidental loss of precision (at least, provided that code follows the recommendations).
For example, if you write:

```scala
val x = 1 / 2
```

your _x_ will be an _Int_ of value 0, because of the way Java-style operators work (in this case, integer division).

However, if you write the idiomatically correct form:

```scala
import com.phasmidsoftware.number.core.Number.NumberOps
val x = 1 :/ 2
```

then _x_ will be a _Number_ with value __exactly__ one half.

Even better is to use the lazy expression mechanism:

```scala
val half: Expression = One / 2
half.materialize
```

You probably want to see some code: so go to the _worksheets_ package and take a look, starting with
NumberWorksheet.sc, Foucault1.sc, Newton.sc, and so on.

### Introduction
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

There are four domains of values, each identified by a domain or factor (see _Factors_ below).
These allow the exact representation of roots, logarithmic numbers, radians, and pure numbers.

#### Operators

The most important operators are those defined in _Expression.ExpressionOps_.
That's because you should normally be using the (lazy) expressions mechanism for arithmetic expressions.
These are the usual operators, except that the power operator is ∧ (not ^ or **).

### Java API
In addition to the Scala API, version 1.0.14 introduces a Java API where it is harder to invoke the
Scala classes directly from Java.
These situations involve classes which have similar names (or have no Java equivalent).

Here are the current API specifications:

#### RationalJ:

    def bigDecimalToRational(x: java.math.BigDecimal): Rational
    def rationalToBigDecimal(r: Rational): java.math.BigDecimal
    def bigIntegerToRational(x: BigInteger): Rational
    def rationalToBigInteger(r: Rational): BigInteger
    def longToRational(l: java.lang.Long): Rational
    def rationalToLong(r: Rational): java.lang.Long
    def doubleToRational(x: java.lang.Double): Rational
    def rationalToDouble(r: Rational): java.lang.Double
    def stringToRational(s: String): Rational

#### NumberJ

    def bigDecimalToNumber(x: java.math.BigDecimal): Number
    def numberToBigDecimal(x: Number): java.math.BigDecimal
    def bigIntegerToNumber(x: BigInteger): Number
    def numberToBigInteger(x: Number): BigInteger
    def longToNumber(l: java.lang.Long): Number
    def numberToLong(x: Number): java.lang.Long
    def doubleToNumber(x: java.lang.Double): Number
    def stringToNumber(s: String): Number

#### ExpressionJ

    def add(x: Expression, y: Expression): Expression
    def multiply(x: Expression, y: Expression): Expression

## Top Module

The `top` module is where `Quantity` is defined for representing a `Valuable`/`PhysicalUnit` pair.
Additionally, various physical constants are defined for convenience.

It also contains high-level example code and practical demonstrations of the Number library.
Perhaps most importantly, it houses the worksheets (listed below) and also Specification (unit tests) for high-level constructs.

### Contents

This module includes:

- **Worksheets** - Interactive Scala worksheets demonstrating library features:
    - `Introduction.sc` - Introduction to the library.
    - `ExpressionWorksheet.sc` - Working with expressions
    - `NumberWorksheet.sc` - Basic number operations and type conversions
    - `RationalWorksheet.sc` - Basics of rational numbers
    - `Foucault1.sc` and `Foucault2.sc` - Physics calculations (Foucault pendulum) [I'm not sure why we have two]
    - `Newton.sc` - Numerical approximation methods
    - `RealWorksheet.sc` - Working with fuzzy/uncertain numbers
    - `ContinuedFractions.sc` - Continued fraction demonstrations
    - `Complex.sc` - Complex number examples
    - `Algebraic.sc` - Algebraic number examples (from the `core` package)
    - and others (to be added here).

- **Examples** - Practical usage patterns showing how the modules work together:
    - `Foucault.scala` - Foucault pendulum example
    - `Newton.scala` - Newton-Raphson approximation method example
    - `Flog template.sc` - Template for how to use functional logging

See the worksheets for hands-on examples of the library in action.

For version history and migration notes, see the [HISTORY](docs/HISTORY.md).

## Testing
See [Testing Conventions](docs/testing/TESTING_CONVENTIONS.md) for comprehensive testing guidelines.

Quick reference: [Testing Quick Reference](docs/testing/TESTING_QUICK_REFERENCE.md)