---
layout: doc-page
title: "TASTy Inspection"
---

```scala
libraryDependencies += "ch.epfl.lamp" %% "dotty-tasty-inspector" % scalaVersion.value
```

TASTy files contain the full typed tree of a class including source positions
and documentation. This is ideal for tools that analyze or extract semantic
information of the code. To avoid the hassle of working directly with the TASTy
file we provide the `TastyInspector` which loads the contents and exposes it
through the TASTy reflect API.


## Inspecting TASTy files

To inspect the TASTy Reflect trees of a TASTy file a consumer can be defined in
the following way.

```scala
import scala.tasty.Reflection
import scala.tasty.file._

class Consumer extends TastyInspector {
  final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect._
    // Do something with the tree
  }
}
```

Then the consumer can be instantiated with the following code to get the tree of
the class `foo.Bar` for a foo in the classpath.

```scala
object Test {
  def main(args: Array[String]): Unit = {
    InspectTasty("", List("foo.Bar"), new Consumer)
  }
}
```

Note that if we need to run the main (in an object called `Test`) after
compilation we need make available the compiler to the runtime:

```shell
dotc -with-compiler -d out Test.scala
dotr -with-compiler -classpath out Test
```
