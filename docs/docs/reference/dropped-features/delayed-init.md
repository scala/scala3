---
layout: doc-page
title: "Dropped: DelayedInit"
movedTo: https://docs.scala-lang.org/scala3/reference/dropped-features/delayed-init.html
---

The special handling of the [`DelayedInit`](https://scala-lang.org/api/3.x/scala/DelayedInit.html)
trait is no longer supported.

One consequence is that the [`App`](https://scala-lang.org/api/3.x/scala/App.html) class,
which used [`DelayedInit`](https://scala-lang.org/api/3.x/scala/DelayedInit.html) is
now partially broken. You can still use `App` as a simple way to set up a main program. Example:

```scala
object HelloWorld extends App {
  println("Hello, world!")
}
```

However, the code is now run in the initializer of the object, which on
some JVM's means that it will only be interpreted. So, better not use it
for benchmarking! Also, if you want to access the command line arguments,
you need to use an explicit `main` method for that.

```scala
object Hello:
  def main(args: Array[String]) =
    println(s"Hello, ${args(0)}")
```

On the other hand, Scala 3 offers a convenient alternative to such "program" objects
with [`@main` methods](../changed-features/main-functions.md).
