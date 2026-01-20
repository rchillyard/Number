// GettingStarted with Number

import com.phasmidsoftware.number.algebra.eager.{Angle, WholeNumber}
import com.phasmidsoftware.number.algebra.util.LatexRenderer.LatexRendererOps
import com.phasmidsoftware.number.expression.expr.*
import com.phasmidsoftware.number.top.expr.*

// Create some expressions:
val x = one                           // Expression
val y: Expression = 42                // converts to Expression automatically
val z = x + y                         // Expression (Sum)
val w = x * 2 + y * 3                 // Expression (complex tree)
val result = (x + 1) / (y - 2)        // Expression

// Evaluation when possible
val maybeEager = result.evaluateAsIs // Some(RationalNumber(Rational(1, 20), false))

// LaTeX rendering
println(w.toLatex) // 128

// Pattern matching still works
w match
  case BiFunction(BiFunction(Literal(WholeNumber(x), _), y, Product), z, Sum) => println(s"($x*$y)+$z")
  case _ => println("Other form")

import com.phasmidsoftware.number.expression.expr.Root.{phi, psi}

1 + 2

phi + 1

(phi * phi).toLatex // should appear as 𝛗 + 1

(phi + psi).toLatex // should be 1

