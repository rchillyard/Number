package com.phasmidsoftware.number.core

import scala.language.implicitConversions

object Constants {
  val sPhi = "1.618033988749894"
  val sGamma = "0.57721566490153286060651209008240243104215933593992*"
  val sG = "6.67430(15)E-11" // m ^ 3 kg ^ -1 s ^ -2
  val sBoltzmann = "1380649.E-29" // J K ^ -1

  val one: Real = Real(Number.one)
  val minusOne: Field = -one
  val two: Real = Real(Number.two)
  val half: Real = Real(Number.half)
  val zero: Real = Real(Number.zero)
  val pi: Real = Real(Number.pi)
  val piBy2: Real = Real(Number.piBy2)
  //noinspection NonAsciiCharacters
  val `𝛑`: Real = Real(Number.`𝛑`)
  val e: Real = Real(Number.e)
  val i: Complex = Complex.i
  /**
    * Exact value of iPi.
    */
  val iPi: Complex = ComplexCartesian(0, Number.pi)

  // CONSIDER making the following Complex
  val root2: Field = Real(Number.root2)
  val root3: Field = Real(Number.root3)
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
  lazy val avagadro: Real = Real(6.0221407600E23)

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
  lazy val c: Real = Real(Number("299792458")) // m sec ^ -1

  /**
    * [[https://en.wikipedia.org/wiki/Proton-to-electron_mass_ratio]]
    */
  lazy val mu: Real = Real(1836.15267343 ~ 11) // (dimensionless)
}
