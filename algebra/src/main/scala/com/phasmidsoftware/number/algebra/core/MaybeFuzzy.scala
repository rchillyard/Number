package com.phasmidsoftware.number.algebra.core

import com.phasmidsoftware.number.core.numerical.Fuzziness

/**
  * A trait representing an optional association with a `Fuzziness[Double]`.
  *
  * This can be used to indicate whether an entity has an associated
  * fuzzy or uncertain aspect to its representation or behavior. The presence of
  * fuzziness is represented as an `Option[Fuzziness[Double]]`, where `Some`
  * indicates the presence of fuzziness and `None` indicates its absence.
  */
trait MaybeFuzzy extends Renderable {
  /**
    * Represents an optional fuzziness value associated with the implementing entity.
    *
    * This method provides a way to retrieve the fuzziness information if it exists.
    * The fuzziness is represented as an `Option[Fuzziness[Double]]`:
    * - `Some(Fuzziness[Double])`: Indicates the presence of fuzziness.
    * - `None`: Indicates the absence of fuzziness.
    *
    * @return An `Option` containing a `Fuzziness[Double]` if fuzziness is present,
    *         or `None` if it is not.
    */
  def maybeFuzz: Option[Fuzziness[Double]]

  /**
    * Retrieves the nominal (non-fuzzy) value associated with the entity.
    *
    * This value represents the precise or primary measurement or parameter of the entity,
    * without considering any associated fuzziness or uncertainty.
    *
    * @return The nominal value as a `Double`.
    */
  def nominalValue: Double
}

/**
  * Provides extension methods for the `MaybeFuzzy` trait to render its properties
  * in absolute (scientific) form.
  */
object MaybeFuzzy:
  /**
    * Extension methods for the `MaybeFuzzy` trait.
    */
  extension (m: MaybeFuzzy)
    /**
      * Converts the value of the current instance into its absolute `String` representation,
      * always using scientific notation (e.g. "1.00(1)E+02").
      *
      * If fuzziness is present, it normalizes it to absolute form and formats it in scientific
      * notation with embedded uncertainty digits.
      * If no fuzziness is present, the nominal value is returned as rendered by `m.render`.
      *
      * @return A string in scientific notation with absolute uncertainty,
      *         or the nominal value if no fuzziness is associated.
      */
    def asAbsolute: String =
      m.maybeFuzz match
        case None => m.render
        case Some(fuzz) =>
          fuzz.normalize(m.nominalValue, relative = false) match
            case Some(absFuzz) =>
              val (embedded, str) = absFuzz.getQualifiedString(m.nominalValue, forceScientific = true)
              if embedded then str
              else s"${m.nominalValue} ± $str"
            case None => m.render