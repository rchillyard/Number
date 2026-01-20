# Migration and Version History

## Versions

* Version 1.5.6: Cleanup and enhancement of the fuzzy utilities in the core module. 
* Version 1.5.5: Cleanup and bug fixes.
* Version 1.5.4: Composition of Quantities
* Version 1.5.3: Parsing of units, quantities, together with physical constants.
* Version 1.5.2: Introduced Quantity.
* Version 1.5.1: Introduced Dimension and Units (in a new module called dimensions). There is as yet no integration with Numbers.
* Version 1.5.0: Working on resolving TODO issues.
* Version 1.4.5/6: Mostly warnings and documentation issues.
* Version 1.4.4: Resolved some publishing issues.
* Version 1.4.0: This represents a prototype of the forthcoming 2.0.0 release.
* Version 1.3.7: Issue resolution: better integration between Root/Algebraic/Complex
* Version 1.3.6: Solutions to quadratic equations can now be Complex; add LaTeX rendering for Eager values.
* Version 1.3.5: Here's a list of all the new features since version 1.2:
  * Multi-module architecture (algebra, parse, expressions, core, top)
  * Clean Scala 2 → Scala 3 migration (not yet complete as the core module is still in Scala 2)
  * Sophisticated type hierarchy with Solution, Complex, and Algebraic
  * Comprehensive normalization system
  * LaTeX parsing and string interpolators
  * Convertible typeclass for compile-time safe conversions
  * Lazy vs Eager evaluation semantics
* Version 1.3.4: Completed equality and fuzzy equality tests throughout Eager classes.
* Version 1.3.3: 
* Version 1.3.2: Complete the migration to 5-module project.
* Version 1.3.1: 
  * Restructured the project into two modules: core and algebra;
  * Introduced the algebra package which will replace all of the Number classes (see Future Upgrades below).
* Version 1.2.12: No changes.
* Version 1.2.11: Mostly the introduction of classes based on cats.
* Version 1.2.10: Another housekeeping release.
* Version 1.2.9: Mostly minor details that missed the previous version.
* Version 1.2.8: Major restructuring: renamed old _Root_ as _NthRoot_ and introduced new _Root_ which effectively replaced _ReducedQuadraticRoot_.
* Version 1.2.7: Introduced dyadic _Log_ functions and, in general, renamed (natural) _log_ method as _ln_, allowing for new dyadic _log_ method.
  * Also, fixed various bugs and restructured the Expression classes.
* Version 1.2.6: Added Transcendental Numbers.
* Version 1.2.5: Fixed badges in this README file; also added social card.
* Version 1.2.4: Restored functioning of CircleCI as well as some very minor changes to Rational (and fewer ignored tests).
* Version 1.2.3: Introduced Series, PowerSeries, and TaylorSeries.
* Version 1.2.2: Changed the name of RQR into Quadratic and introduced Algebraic.
* Version 1.2.1: Improved RQR classes.
* Version 1.2.0: Another massive refactoring.
  - In particular:
  - ExpressionMatchers has undergone a complete re-write.
  - Solution has been added as a Field type (with RQR as a subtype of Solution).
  - NthRoot classes have been refactored (now based on InversePower).
  -
* Version 1.1.1: Massive refactoring: fixed many issues. Most significantly, expressions are evaluated in the context of a Factor.
* Version 1.1.0: Significant refactoring:
  - Number is no longer a subtype of Field. Code should use the wrapper Real(number) to form a Field.
  - Some of the worksheets were broken and have been fixed.
* Version 1.0.17: Minor changes.
* Version 1.0.16: Added C-interpolator for Complex objects; various other fixes, including radian values now range from -$\pi$ to $\pi$.
* Version 1.0.15: Significant improvements to the rendering of rational numbers.
* Version 1.0.14: ComplexPolar now keeps track of branches; introduced Real type. Java API.
* Version 1.0.13: Mostly cleanup together with some fixes related to NthRoot factors and rendering of fuzziness.
* Version 1.0.12: Mostly cleanup together with some fixes related to the new factors.
* Version 1.0.11: Changes to the factors: renamed Pi as Radian, E as NatLog, and added Log2, Log10, Root2, and Root3.
* Version 1.0.10: Many improvements and fixes:
  - added Constants,
  - implicit converter from Expression to Number,
  - refactored structure of classes,
  - totally reworked the expression matchers.
* Version 1.0.9: Added complex numbers; improved simplifications somewhat; use version 1.0.4 of Matchers (now in main).
* Version 1.0.8: This includes better simplification and, in particular, evaluates (√3 + 1)(√3 - 1) as exactly 2.
* Version 1.0.7: added Matchers.
* Version 1.0.6: added Mill (RPN evaluator).
* Version 1.0.5: reimplement the e factor.
* Version 1.0.4 Made improvements to Rational, removed BigInt from Value,
  and effected many refactorings.
* Version 1.0.3 implements lazy evaluation.
* Version 1.0.2 Included fixing the bug mentioned in 1.0.1 (actually a Rational bug), as well as adding the :/ operator
  and many other fixes/features.
* Version 1.0.1 Fixed many issues with minor inconsistencies.  Most important, perhaps, was the implementation of _compare_, along with _signum_ and _isZero_. Each of these has, significantly, a signature with a confidence value (the default value is 0.5).
* Initial version is 1.0.0

### Future Upgrades

#### Active Migrations

The Number project is undergoing a significant restructuring:

**Completed (v1.3.x)**:
- ✅ Multi-module architecture (`algebra`, `parse`, `expression`, `core`, `top`)
- ✅ Algebra module with Cats typeclass integration
- ✅ Structure hierarchy for algebraic types

**In Progress**:
- 🔄 Migrating Expression types to the `expression` module
- 🔄 Replacing core.Number and core.Field with algebra types

**Planned**:
- Additional algebraic structures (Matrix, Vector, Polynomial)
- Complete migration guide
- Enhanced pattern matching and simplification
- Extended Cats typeclass coverage

We intend to restructure the hierarchy of numeric types entirely.
The traits and classes should strictly follow the mathematical concepts of field, ring, etc.
To begin, the hierarchy should look like this:

* _Expression_ (trait that is the root of all lazy values with the key method being _evaluate(Context)_: _Option\[Field]_
  * _AtomicExpression_ (as now but with _ReducedQuadraticRoot_ being completely replaced by _Algebraic_)
    * _Algebraic_ (quadratic, linear, equations, etc.) (as now, but with _solve_ changed to _evaluate_)
  * _BiFunction_ (as now)
  * _Function_ (renamed as MonoFunction, perhaps?) (as now)
  * _Aggregate_ (more or less as now but with _PowerSeries_ included--such that the length of the components may or may not be known)
    * _PowerSeries_ (as now, but extending _Aggregate_, and with an additional method _apply(X)_)
  * _Transcendental_ (as now but extends _Expression_)
* _Field_ (as now, but with _Solution_ and _Series_ included)
  * _Real_ (as now a single _Number_)
  * _Multivariate_ (similar to _Multivalued_ in that there are multiple solutions or branches--these are the solutions to _Algebraic_s)
    * _Complex_ (as now with two _Number_ fields--real and imaginary--conceivably, we might merge _Complex_ and _Solution_ and insist that the imaginary aspect of a _Number_ is represented in the _Number_ itself)
    * _Solution_ (with real roots)
  * _Series_

Former type hierarchy:

### Type Hierarchy

**Note** that the type hierarchy is very likely to change in version 1.3
* _NumberLike_ (trait)
    * _Numerical_ (trait: most numeric quantities)
        * _Field_ (trait: something like the mathematical concept of a field)
            * _Real_ (case class: a "real" number based on one _Number_)
            * _Multivariate_ (trait: perhaps should be called "Algebraic")
                * _Complex_ (trait: a complex number)
                    * _BaseComplex_ (abstract class)
                        * _ComplexCartesian_ (case class: Cartesian form of complex number)
                        * _ComplexPolar_ (case class: polar form of complex number)
                * _Algebraic_ (trait: an algebraic number)
        * _Number_ (trait: a quantity representing a number)
            * _GeneralNumber_ (abstract class)
                * _ExactNumber_ (case class: an exact number defined by a _Value_ and a _Factor_)
                * _FuzzyNumber_ (case class: an exact number defined by a _Value_, a _Factor_, and an optional _Fuzziness_)
    * _Rational_ (case class: rational numbers)
    * _Solution_ (trait: a solution to an _Algebraic_ quantity--think of this is defining a named tuple that represents the components of the solution)
        * _LinearSolution_ (case class: a linear solution)
        * _QuadraticSolution_ (case class: a quadratic solution)
    * _Expression_ (trait: lazy numeric quantities: see below)
* _Series_ (trait)
    * _AbstractSeries_
        * _FiniteSeries_
    * _AbstractInfiniteSeries_
        * _InfiniteSeries_
* _PowerSeries_ (trait)
    * _LazyPowerSeries_
    * _FinitePowerSeries_
    * _TaylorSeries_
