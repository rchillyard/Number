# Contributing to `Number`

Thank you for your interest in contributing to Number! This guide will help you get started with development and
understand the project's architecture and conventions.

## Table of Contents

- [Getting Started](#getting-started)
- [Project Architecture](#project-architecture)
- [Coding Conventions](#coding-conventions)
- [Testing](#testing)
- [Debugging](#debugging)
- [Contribution Workflow](#contribution-workflow)

## Getting Started

### Prerequisites

- **Scala 3.7.3** (or later 3.x version)
- **sbt** (Scala Build Tool)
- **JDK 17 or later**
- **Git**

### Setting Up Your Development Environment

1. **Clone the repository:**
   ```bash
   git clone https://github.com/rchillyard/Number.git
   cd Number
   ```

2. **Build the project:**
   ```bash
   sbt compile
   ```

3. **Run all tests:**
   ```bash
   sbt test
   ```

4. **Generate documentation:**
   ```bash
   sbt unidoc
   ```
   The unified documentation will be generated in `target/scala-3.7.3/unidoc/`

### IDE Setup

**IntelliJ IDEA** (recommended):

- Install the Scala plugin
- Import the project as an sbt project
- The IDE will automatically detect the module structure

**VS Code**:

- Install the Metals extension
- Open the project folder
- Metals will prompt you to import the build

## Project Architecture

Number is organized into multiple modules, each with a specific purpose:

### Module Structure

```
Number (root)
├── core        - Legacy numeric types (Rational, Factor, Fuzz, etc.)
├── algebra     - Modern algebraic structures based on Cats typeclasses
├── expression  - Lazy expression evaluation
├── parse       - Parsing facilities for expressions and algebraic structures
├── dimensions  - Typesafe dimensional quantities and units
└── top         - High-level API and examples
```

**Dependency flow**: `core` → `algebra` → `expression` → `parse` → `top`

The `dimensions` module depends on `algebra` and `core`.

### Key Design Principles

1. **Exact arithmetic by default**: Number maintains exactness wherever possible. Loss of precision requires explicit
   action (e.g., calling `.fuzzy` or `.materialize`)

2. **Eager vs Lazy evaluation**:
    - **Eager** (`Eager`, `Solution`, `Complex`, etc.): Values are evaluated immediately
    - **Lazy** (`Expression`): Symbolic expressions that defer evaluation

3. **Symbolic computation**: Mathematical constants like π, e, √2 are kept symbolic until materialization is requested

4. **Factor systems**: Different numeric domains (roots, logarithms, angles) are represented through factors

## Coding Conventions

### The + Operator Problem

⚠️ **CRITICAL**: Scala's default `+` operator can accidentally concatenate strings instead of performing arithmetic.
Number has strict compiler settings to catch this.

**DON'T:**

```scala
val x = someNumber + "accidentally a string" // Compiles to string concatenation!
```

**DO:**

```scala
import com.phasmidsoftware.number.core.Number.NumberOps

val x = 1 :/ 2 // Exact division using custom operators
val y = a :+ b // Safe addition
val z = a :* b // Safe multiplication
```

### Working with Expressions

**When to use `Expression` vs `Eager`:**

- Use `Expression` when you want to:
    - Defer evaluation
    - Maintain symbolic form (e.g., `π/2` instead of `1.5707...`)
    - Allow for algebraic simplification

- Use `Eager` when you:
    - Need an immediate result
    - Are doing simple arithmetic
    - Want type-safe numeric operations

**Example:**
```scala
// Lazy - maintains exactness through simplification
val expr: Expression = (√3 + 1) * (√3 - 1)  // Simplifies to exactly 2

// Eager - evaluates immediately
val eager: Eager = math"2 * 3"  // Evaluates to 6
```

### Normalize vs Materialize

- **`normalize()`**: Converts to canonical symbolic form (stays exact)
    - Example: `Expression` → simplified `Expression`

- **`materialize()`**: Converts to approximate numeric value (may lose precision)
    - Example: `Expression` → `Double` or `Fuzzy`

### Code Style

- Follow standard Scala 3 conventions
- Use meaningful variable names
- Add ScalaDoc comments for public APIs
- Keep functions focused and testable
- Prefer immutability

### TODO Comment Conventions

The project uses a standardized set of TODO-style comments to track technical debt and ideas. Configure your IDE to
recognize these:

* **FIXME**: Will be annotated by an Issue #. Top priority to be fixed.
* **TESTME**: Indicates that there are no unit tests for this code.
    * Not used in private methods, or `unapply` methods. Not used for cases designed to catch all possibilities.
* **TODO**: Technical debt to be paid back.
* **CONSIDER**: An idea for an alternative approach.
* **NOTE**: A note to readers that might perhaps be a surprise.
* **XXX**: Just a general comment.

When adding code, use these tags appropriately to communicate intent and priority to other contributors.

## Testing

Number has comprehensive testing conventions. Please see:

- **[Testing Conventions](docs/testing/TESTING_CONVENTIONS.md)** - Comprehensive guidelines
- **[Testing Quick Reference](docs/testing/TESTING_QUICK_REFERENCE.md)** - Quick lookup

### Running Tests

```bash
# Run all tests
sbt test

# Run tests for a specific module
sbt algebra/test

# Run a specific test class
sbt "testOnly com.phasmidsoftware.number.algebra.ComplexSpec"

# Run tests matching a pattern
sbt "testOnly *Complex*"
```

### Test Requirements

When contributing code:

1. **Write tests for new functionality**
    - Unit tests for individual methods
    - Property-based tests for algebraic laws (when applicable)
    - Edge case coverage

2. **Maintain existing test coverage**
    - All existing tests must pass
    - Don't reduce coverage without good reason

3. **Follow naming conventions**
    - Test classes end with `Spec`
    - Test methods describe behavior: `"should return exact result for rational division"`

4. **Use appropriate matchers**
    - For exact equality: `shouldBe`, `===`
    - For fuzzy equality: `~==` (with appropriate tolerance)

## Debugging

### Expression Matchers

It can be useful to debug the behavior of the expression matchers by setting
the implicit logger to use either `LogInfo` or `LogDebug`.
The former will only log the results of successful matches, while the latter will
also log all the start of each match attempt.

To do this you can add:

```scala
implicit val logger: MatchLogger = MatchLogger(LogDebug, classOf[Expression])
```

in the `Expression` companion object.

You will also need to make sure that logging is configured for "DEBUG" ("INFO" is sufficient if you specified
`LogInfo`).
In `test/resources/logback.xml`, make sure that the root logger is set accordingly.

### Debugging Simplification

If expressions aren't simplifying as expected:

1. Check the simplification pipeline phases:
    - Components
    - Structural
    - Identities
    - Exact
    - Constant

2. Use `.toString` or `.render` to see the current form

3. Check if the expression has been normalized:
   ```scala
   val normalized = expr.normalize()
   println(normalized.render)
   ```

### Debugging Factor Systems

When working with factors (roots, logarithms, etc.):

```scala
val num = Number(2, Root(2)) // √2
println(num.factor) // See the factor
println(num.value) // See the underlying value
```

## Contribution Workflow

### Before You Start

1. **Check existing issues** - Look for related issues or discussions
2. **Open an issue** (for major changes) - Discuss your approach before investing time
3. **Fork the repository** - Create your own fork to work in

### Making Changes

1. **Create a feature branch:**
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. **Make your changes:**
    - Follow coding conventions
    - Write tests
    - Update documentation if needed

3. **Run tests locally:**
   ```bash
   sbt test
   ```

4. **Commit with clear messages:**
   ```bash
   git commit -m "Add support for hyperbolic functions"
   ```

### Submitting a Pull Request

1. **Push to your fork:**
   ```bash
   git push origin feature/your-feature-name
   ```

2. **Open a Pull Request** on GitHub:
    - Describe what changed and why
    - Reference any related issues
    - Note any breaking changes

3. **Respond to feedback:**
    - Address review comments
    - Update tests if requested
    - Keep the PR focused and manageable

### PR Review Process

- PRs are reviewed by project maintainers
- Tests must pass (CircleCI will run automatically)
- Code quality is checked by Codacy
- At least one approval is required
- Maintainers may request changes or suggest improvements

## Documentation

When adding new features:

1. **Add ScalaDoc comments** for public APIs
2. **Update relevant markdown docs** in the `docs/` folder
3. **Add examples** if introducing new concepts
4. **Update README.md** if changing core functionality

## Questions?

- **GitHub Issues** - For bugs or feature requests
- **GitHub Discussions** - For questions and general discussion
- **README.md** - For project overview and quick start

## License

By contributing to Number, you agree that your contributions will be licensed
under the same license as the project (MIT License).

---

Thank you for contributing to Number! Your efforts help make exact arithmetic in Scala more accessible and reliable for
everyone.