## Deferred Topics

### as of V1.10.2

1. **Option B for Series** — accumulate series terms as `Double` rather than `Rational`
   from the start (only Option A — the safety net in `FuzzyNumber.apply` — was implemented).

2. **`AbstractInfiniteSeries.evaluateToTolerance`** — duplicated code fragment to be merged.

3. **Trapezoid refactor** — make `Trapezoid` conform to the stateless pattern of `Box` and
   `Gaussian` by storing only the eccentricity `a/b` (as a `Rational`, where infinity is legal)
   rather than both `a` and `b` separately.

4. **Issue #196** — Box/Gaussian fuzz combination in subtraction.

5. **Issue #197** — asymmetric `Complex + Real` addition.

6. **Issue #199** — `fuzzyCompare` asymmetry when one operand is exact
   (`zero.fuzzyCompare(result, 0.1)`) — currently marked pending.

7. **`tan`/`tanh` as first-class functions** — currently represented as `Sin/Cos`
   division; secant identity rule deferred.

8. **Symbolic hyperbolic identity for concrete numeric `x`** — architecturally blocked.

9. **One `LaTeXParserSpec` test** — currently pending.

10. **Typelevel submission prerequisites** — planning document exists but not yet acted on.

11. **`asComparedWith` and `probabilityOfZero` merger** — noted as a future consideration.

12. **Context-sensitive simplification** — mentioned in WI13 design doc as future work.