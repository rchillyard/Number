# Coding Guidelines

## Debugging

### Expression Matchers
It can be useful to debug the behavior of the expression matchers by setting
the implicit logger to use either `LogInfo` or `LogDebug`.
The former will only log the results of successful matches, while the latter will
also log all the start of each match attempt.
To do this you can add
```scala
  implicit val logger: MatchLogger = MatchLogger(LogDebug, classOf[Expression])
```
in the `Expression` companion object.
You will also need to make sure that logging is configured for "DEBUG" ("INFO" is sufficient if you specified `LogInfo`).
In test/resources/logback.xml, make sure that the root logger is set accordingly.
