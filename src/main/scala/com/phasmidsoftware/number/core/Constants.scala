/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.core

/**
  * Definitions of constant fields.
  */
object Constants {
  /**
    * Represents the constant value of one as a `Real` number.
    * This is equivalent to the numerical value 1 in mathematical terms.
    */
  val one: Real = Real(Number.one)
  /**
    * Represents the negative of the `one` field element.
    *
    * This value is computed by applying the unary `-` operation on the `one` element,
    * resulting in the additive inverse of the `one` field.
    *
    * Provides a convenient symbolic representation for the value `-1` within the context
    * of the `Field` trait.
    */
  val minusOne: Field = -one
  /**
    * A constant representing the value 2 as a `Real`.
    *
    * The `Real` instance is created using the `Number.two` value. This
    * is part of the mathematical constants defined in `Constants`.
    */
  val two: Real = Real(Number.two)
  /**
    * Represents the constant value of one-half (0.5) as a `Real`.
    *
    * This value is derived from `Number.half`.
    * It is a predefined value within the `Constants` class and can be used
    * in mathematical computations requiring the value of 1/2.
    */
  val half: Real = Real(Number.half)
  /**
    * Represents the constant zero as a `Real` type.
    * Zero is the additive identity in the arithmetic context.
    */
  val zero: Real = Real(Number.zero)
  /**
    * Represents positive infinity in the domain of real numbers.
    *
    * This value is of type `Real` and is constructed using a `Number` encapsulating a `Rational.infinity`.
    * It is a constant member of the `Constants` class and indicates the concept of unbounded growth in mathematics.
    */
  val infinity: Real = Real(Number(Rational.infinity))
  /**
    * A predefined constant representing negative infinity as a real number.
    * This is constructed using a `Number` wrapping the rational representation of negative infinity.
    */
  val negInfinity: Real = Real(Number(Rational.negInfinity))
  /**
    * Represents the mathematical constant π (pi) as a `Real` object.
    * π is the ratio of the circumference of a circle to its diameter,
    * approximately equal to 3.14159.
    */
  val pi: Real = Real(Number.pi)
  /**
    * A constant representing the mathematical value 2π (two times pi).
    * This value is useful in various calculations, especially those
    * involving circular or periodic phenomena, such as trigonometry
    * and geometry.
    *
    * The value is stored as a `Real` number wrapping the predefined
    * constant `Number.twoPi`.
    */
  val twoPi: Real = Real(Number.twoPi)
  /**
    * Represents the mathematical constant π/2 as a Real number.
    * π/2, also known as pi over two, is half the value of π, commonly used in trigonometric calculations.
    */
  val piBy2: Real = Real(Number.piBy2)
  /**
    * Represents the mathematical expression equivalent to \( \pi/2 + \pi \).
    * Combines the predefined mathematical fields `piBy2` and `pi` using the addition operation defined in the `Field` trait.
    *
    * This value is a constant and belongs to the `Constants` class.
    */
  val piBy2Times3: Field = Number.piBy2 add pi
  /**
    * Represents the value of π/4 (quarter of π) as a `Real` object.
    *
    * This value is derived using the constant `piBy4` from the `Number` class
    * and wrapped as an instance of `Real`, which is a specific subclass of `Field`.
    *
    * It can be used in mathematical computations involving π/4.
    */
  val piBy4: Real = Real(Number.piBy4)
  /**
    * Represents the mathematical constant π/3 as a `Real` type.
    *
    * This constant is defined for calculations involving the value of π (pi) divided by 3,
    * which corresponds to an angle of 60 degrees in radians.
    */
  val piBy3: Real = Real(Number.piBy3)
  /**
    * Represents the mathematical constant π (pi) as a `Real` value.
    *///noinspection NonAsciiCharacters
  val `𝛑`: Real = Real(Number.`𝛑`)
  /**
    * Represents the mathematical constant e (Euler's number, approximately 2.718),
    * encapsulated as a `Real` value.
    */
  val e: Real = Real(Number.e)
  /**
    * A pre-defined constant for the imaginary unit `i`, which represents the
    * square root of -1. This is a standard component of complex numbers
    * used in fields involving imaginary and real number computations.
    */
  val i: Real = Real(Number.i)
  /**
    * Exact value of iPi.
    */
  val iPi: Complex = ComplexCartesian(0, Number.pi)

  /**
    * Represents the square root of 2 as a field constant.
    *
    * This value is particularly useful in mathematical computations where the
    * irrational constant √2 is required, expressed within the `Field` type system.
    */// CONSIDER making the following Complex
  val root2: Field = Real(Number.root2)
  /**
    * Represents the constant value √3 encapsulated as a `Field` in the mathematical context.
    *
    * Utilizes the `Real` implementation of `Field` to create the square root of 3 (`√3`).
    *
    * This constant can be used in various mathematical computations that involve fields.
    */
  val root3: Field = Real(Number.root3)
  /**
    * Represents the square root of 5 as a constant value, conforming to the `Field` type.
    * This value is defined as a member of the `Constants` class and provides numeric and algebraic operations
    * as supported by the `Field` trait.
    */
  val root5: Field = Real(Number.root5)
  /**
    * Exact value of the Complex Number ±√2
    */
  val root2s: Field = ComplexPolar(Number.root2, Number.zeroR, 2)

  import com.phasmidsoftware.number.core.Number.FuzzOps

  /**
    * [[https://en.wikipedia.org/wiki/Golden_ratio]]
    */
  lazy val phi: Real = Real(sPhi)

  /**
    * [[https://en.wikipedia.org/wiki/Euler–Mascheroni_constant]].
    */
  lazy val gamma: Real = Real(sGamma)

  /**
    * [[https://en.wikipedia.org/wiki/Gravitational_constant]]
    */
  lazy val G: Real = Real(sG)

  /**
    * [[https://en.wikipedia.org/wiki/Fine-structure_constant]]
    */
  lazy val alpha: Real = Real(0.0072973525693 ~ 11) // (dimensionless)

  /**
    * [[https://en.wikipedia.org/wiki/Avogadro_constant]] (exact)
    */
  lazy val avagadro: Real = Real("6.0221407600E23")

  /**
    * [[https://en.wikipedia.org/wiki/Boltzmann_constant]] (exact).
    */
  lazy val boltzmann: Real = Real(sBoltzmann)

  /**
    * [[https://en.wikipedia.org/wiki/Planck_constant]] (exact).
    */
  lazy val planck: Real = Real("6.6260701500E-34") // J Hz ^ -1

  /**
    * [[https://en.wikipedia.org/wiki/Speed_of_light]] (exact).
    */
  lazy val c: Real = Real("299792458") // m sec ^ -1

  /**
    * [[https://en.wikipedia.org/wiki/Proton-to-electron_mass_ratio]]
    */
  lazy val mu: Real = Real(1836.15267343 ~ 11) // (dimensionless)

  /**
   * A string representation of the golden ratio (φ) with high precision.
   * The value is truncated to 104 decimal places.
   * From [[https://oeis.org/A001622]]
   */
  val sPhi = "1.61803398874989484820458683436563811772030917980576286213544862270526046281890244970720720418939113748475"
  val sGamma = "0.57721566490153286060651209008240243104215933593992*"
  val sG = "6.67430(15)E-11" // m ^ 3 kg ^ -1 s ^ -2
  val sBoltzmann = "1380649.E-29" // J K ^ -1
}
