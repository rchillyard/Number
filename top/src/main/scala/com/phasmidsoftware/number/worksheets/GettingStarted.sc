// GettingStarted with Number


// Usage:
import com.phasmidsoftware.number.top.expr._
//import com.phasmidsoftware.number.expression.expr.ExpressionPrelude.*
import com.phasmidsoftware.number.algebra.eager.{Angle, WholeNumber}
import com.phasmidsoftware.number.expression.expr.Expression

// Now this works beautifully:
val x = one                           // Expression
val y = 42                            // converts to Expression automatically
val z = x + y                         // Expression (Sum)
val w = x * 2 + y * 3                // Expression (complex tree)
val result = ((x: Expression) + 1: Expression) / (y - 2)       // Expression

// Variables
//val a = Variable("a")
//val b = Variable("b")
//val quadratic = a * "x" * "x" + b * "x" + 5  // if String => Variable

// Evaluation when needed
val concrete = result.evaluateAsIs

// LaTeX rendering
println(w.toLatex)  // "2x + 3y" or similar

// Pattern matching still works
w match
  case Sum(Product(Literal(WholeNumber(2)), _), _) => println("Starts with 2*something")
  case _ => println("Other form")

import com.phasmidsoftware.number.expression.expr.Root.{phi, psi}
import com.phasmidsoftware.number.expression.expr.Expression.convertInt

1 + 2

phi + 1

phi * phi

phi + psi

