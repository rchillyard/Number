/**
  * This code can be used as a template for using Flog, the functional logger.
  */

import com.phasmidsoftware.flog.{Flog, Loggable, Loggables}
import com.phasmidsoftware.number.core.{GeneralNumber, Number, Value}

val flog: Flog = Flog[GeneralNumber]

import flog._
import scala.util.Try

implicit object LoggableValue extends Loggable[Value] {
    def toLog(t: Value): String = Value.valueToString(t)
}

implicit object LoggableNumber extends Loggable[Number] {
    def toLog(t: Number): String = t.render
}

implicit val tryLoggable: Loggable[Try[Int]] = new Loggables {}.tryLoggable
implicit val optLoggable: Loggable[Option[Int]] = new Loggables {}.optionLoggable

val number = Number.one
"example of using Flog for 1" !! number

