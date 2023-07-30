// Worksheet for Complex numbers

import com.phasmidsoftware.number.core.Complex.ComplexHelper
import com.phasmidsoftware.number.core.Number.√

C"1" // should be ComplexCartesian(1,0) CONSIDER why doesn't it render as 1?
C"1i0" // should be ComplexCartesian(1,0)
C"1+i0" // should be ComplexCartesian(1,0)
C"1-i1" // should be ComplexCartesian(1,-1)
C"1i0pi" // should be ComplexPolar(1,pi) which renders at "1"
C"1i0.5pi" // should render as 1e^i0.5𝛑
C"1i0.5𝛑" // should render as 1e^i0.5𝛑

//C"1+i0" + C"1-i1"

// The following will be represented as "±√5", that's to say +- square root(5).
√(5).asComplex.render // should render as +- root(5)