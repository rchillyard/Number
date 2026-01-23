# Coding Guidelines

## Debugging

### Expression Matchers
It can be useful to debug all of the expression matchers by setting
```scala
  implicit val logger: MatchLogger = MatchLogger(LogDebug, classOf[Expression])
```
in the `Expression` companion object.
You will also need to make sure that logging is configured for DEBUG.
In test/resources/logback.xml, make sure that the root logger is set to DEBUG.
