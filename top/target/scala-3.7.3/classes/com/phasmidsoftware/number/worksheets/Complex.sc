// Worksheet for Complex numbers

import com.phasmidsoftware.number.core.numerical.Complex.ComplexHelper
import com.phasmidsoftware.number.core.numerical.Number.√

C"1".render // should be "1"
C"1i0".render // should be "1"
C"1+i0".render // should be "1"
C"1-i1".render // should be "(1-i1)"
C"1i0pi".render // should be "1"
C"1i0.5pi".render // should render as "1e∧i½𝛑"
C"1i0.5𝛑".render // should render as "1e∧i½𝛑"

(C"1+i0" + C"1-i1").render

// The following will be represented as "±√5", that's to say +- square root(5).
√(5).asComplex.render // should render as +- root(5)