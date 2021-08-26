/**
  * This code can be used as a template for using Flog, the functional logger.
  */

import com.phasmidsoftware.flog.{Flog, Loggable, Loggables}
import com.phasmidsoftware.number.core.{Field, GeneralNumber, Number, Value}

val flog: Flog = Flog[GeneralNumber]

import flog._

import scala.util.Try

implicit object LoggableValue extends Loggable[Value] {
    def toLog(t: Value): String = Value.valueToString(t)
}

implicit object LoggableNumber extends Loggable[Number] {
    def toLog(t: Number): String = t.render
}

implicit object LoggableField extends Loggable[Field] {
    def toLog(t: Field): String = t.render
}

implicit val tryIntLoggable: Loggable[Try[Int]] = new Loggables {}.tryLoggable
implicit val optNumberLoggable: Loggable[Option[Number]] = new Loggables {}.optionLoggable

val number = Number.one
"example of using Flog for 1" !! number

