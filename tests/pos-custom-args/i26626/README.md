This test tests backwards compatibility of inline methods after some stdlib changes,
where the dependency was compiled using Scala 3.8.4 (and the issue was introduced in Scala 3.9)

To regenerate the dependency jar use:
```scala
scala-cli package --power tests/pos-custom-args/i26626/Dependency.scala -S 3.8.4 --library -o tests/pos-custom-args/i26626/Dependency.jar -f
```