---
layout: doc-page
title: "TASTy Inspection"
---

TASTy files contain the full typed tree of a class including source positions
and documentation. This is ideal for tools that analyze or extract semantic
information of the code. To avoid the hassle of working directly with the TASTy
file we provide the `TastyConsumer` which loads the contents and exposes it
through the TASTy reflect API.


## Inspecting TASTy files

To inspect the TASTy Reflect trees of a TASTy file a consumer can be defined in
the following way.

```scala
class Consumer extends TastyConsumer {
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
    ConsumeTasty("", List("foo.Bar"), new Consumer)
  }
}
```