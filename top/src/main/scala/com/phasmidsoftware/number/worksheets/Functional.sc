/**
  * This worksheet illustrates the use of Functional Numbers in the algebra module.
  */

import cats.implicits.toShow
import com.phasmidsoftware.number.algebra.eager.{Angle, Eager, NatLog, WholeNumber}
import com.phasmidsoftware.number.expression.expr.{E, Pi, Root}
import com.phasmidsoftware.number.parse.ExpressionParser.puremath

// Euler's constant (as an expression)
E

// Other ways to express e
NatLog(1)
val e1 = puremath"\e"
e1.toDouble

// What about e^2?
NatLog(2)
val e2 = puremath"\e^2"
e2.toDouble

// Pi as an expression
Pi

// Other ways to express pi
val pi1 = Angle(1)
pi1.show
Pi.materialize

val deg180 = Angle.degrees(WholeNumber(180))
deg180.show

// √2 as an expression
Root.rootTwo
Root.rootTwo.materialize

// Other ways to express √2
val root2 = puremath"\sqrt{2}"
root2.show
root2.materialize
root2.toDouble





