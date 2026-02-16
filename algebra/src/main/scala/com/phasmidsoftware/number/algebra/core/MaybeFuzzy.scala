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
  * in various formats, including absolute, relative, and percentage representations.
  */
object MaybeFuzzy:
  /**
    * Extension methods for the `MaybeFuzzy` trait to render its properties in various formats.
    */
  extension (m: MaybeFuzzy)
    /**
      * Converts the value of the current instance into its (absolute) `String` representation.
      *
      * This method takes into account the presence of fuzziness associated with the value.
      * If fuzziness is present, it normalizes it to an absolute representation and formats
      * it accordingly. If no fuzziness is present, the nominal value is returned as rendered by `m.render`.
      *
      * @return A string representing the absolute value and its associated fuzziness (if any),
      *         or the nominal value if no fuzziness is associated.
      */
    def asAbsolute: String =
      m.maybeFuzz match
        case None => m.render
        case Some(fuzz) =>
          fuzz.normalize(m.nominalValue, relative = false) match
            case Some(absFuzz) =>
              val (embedded, str) = absFuzz.getQualifiedString(m.nominalValue)
              if embedded then str // AbsoluteFuzz embeds the value
              else s"${m.nominalValue} ± $str" // Just in case
            case None => m.render

    /**
      * Converts the value of the current instance into its (relative) `String` representation.
      *
      * This method takes into account the presence of fuzziness associated with the value. If fuzziness is present,
      * it normalizes it to a relative representation and formats it accordingly. When the fuzziness is embedded
      * in the result, the method returns the embedded representation as a string. Otherwise, it combines the nominal
      * value and the fuzziness as a formatted string. If no fuzziness is present, the nominal value is returned as
      * rendered by `m.render`.
      *
      * @return A string representing the relative value and its associated fuzziness (if any), or the nominal value
      *         if no fuzziness is associated.
      */
    def asRelative: String =
      m.maybeFuzz match
        case None => m.render
        case Some(fuzz) =>
          fuzz.normalize(m.nominalValue, relative = true) match
            case Some(relFuzz) =>
              val (embedded, str) = relFuzz.getQualifiedString(m.nominalValue)
              if embedded then str
              else s"${m.nominalValue}±$str" // Show as decimal
            case None => m.render

    /**
      * Converts the value of the current instance into a percentage-based `String` representation.
      *
      * This method evaluates the optional fuzziness associated with the instance.
      * If fuzziness is present, it normalizes it to a relative representation and retrieves the qualified string.
      * When the fuzziness is embedded in the resulting string, the method directly returns the embedded representation.
      * Otherwise, it combines the nominal value and the relative fuzziness percentage into a formatted string.
      * If no fuzziness exists, the nominal value is returned as rendered by `m.render`.
      *
      * @return A string representing the nominal value along with its relative fuzziness as a percentage (if present),
      *         or the nominal value alone if no fuzziness is associated.
      */
    def asPercentage: String =
      m.maybeFuzz match
        case None => m.render
        case Some(fuzz) =>
          fuzz.normalize(m.nominalValue, relative = true) match
            case Some(relFuzz) =>
              val (embedded, str) = relFuzz.getQualifiedString(m.nominalValue)
              if embedded then str
              else s"${m.nominalValue}±${relFuzz.asPercentage}"
            case None => m.render
