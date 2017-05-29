---
layout: doc-page
title: "Using Dotty with cbt"
---

cbt comes with built-in Dotty support. Follow the
[cbt tutorial](https://github.com/cvogt/cbt/), then simply extend `Dotty` in the Build class.

```scala
// build/build.scala
import cbt._
class Build(val context: Context) extends Dotty {
  ...
}
```

Also see the [example project](https://github.com/cvogt/cbt/tree/master/examples/dotty-example).
