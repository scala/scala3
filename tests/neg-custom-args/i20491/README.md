# Test case for https://github.com/scala/scala3/issues/20491

The `cp/ox/Ox.class` is compiled from the following source using JDK 21:

```scala
// Ox.scala
//> using jvm 21
package ox
import java.util.concurrent.StructuredTaskScope
trait Ox:
  def scope: StructuredTaskScope[Any]
```

`$ scala-cli compile Ox.scala -d ./cp`

This test must be run on JDK < 21 to trigger the missing type error for `java.util.concurrent.StructuredTaskScope`.
