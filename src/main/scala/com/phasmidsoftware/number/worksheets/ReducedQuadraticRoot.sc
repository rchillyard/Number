import com.phasmidsoftware.number.expression.{Expression, Phi, Psi, ReducedQuadraticRoot}
// this worksheet demonstrates ReducedQuadraticRoot
// TODO replace ReducedQuadraticRoot with Root

val phi = new ReducedQuadraticRoot("\uD835\uDED7", -1, -1, true)
phi.toString
phi.asAlgebraic.render
phi.evaluateAsIs
val psi = new ReducedQuadraticRoot("\uD835\uDED9", -1, -1, false)
psi.toString
psi.asAlgebraic.render
psi.evaluateAsIs

val x1: Expression = phi plus psi
x1.render
val x2: Expression = phi * psi
x2.render


// Now we use the built-in classes
val phi2 = Phi
phi2.toString
phi2.asAlgebraic.render
phi2.evaluateAsIs

val psi2 = Psi
psi2.toString
psi2.asAlgebraic.render
psi2.evaluateAsIs

(phi2 ^ 2).simplify
