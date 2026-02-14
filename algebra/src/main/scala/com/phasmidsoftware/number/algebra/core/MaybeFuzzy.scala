package com.phasmidsoftware.number.algebra.core

import com.phasmidsoftware.number.algebra.core.MaybeFuzzy.asAbsolute
import com.phasmidsoftware.number.core.numerical.{AbsoluteFuzz, Fuzziness, RelativeFuzz}

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
