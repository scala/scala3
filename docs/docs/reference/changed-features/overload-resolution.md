---
layout: doc-page
title: "Changes in Overload Resolution"
---

Overload resolution in Dotty takes all argument blocks into account instead of
just the first argument block.

For example, the following code compiles in Dotty, while it results in an
ambiguous overload error in Scala2:

```Scala
def f(x: Int)(y: String): Int = 0
def f(x: Int)(y: Int): Int = 0

f(3)("")     // ok
```

The following code compiles as well:

```Scala
def g(x: Int)(y: Int)(z: Int): Int = 0
def g(x: Int)(y: Int)(z: String): Int = 0

g(2)(3)(4)     // ok
g(2)(3)("")    // ok
```

This change is motivated by the new language feature [extension
methods](../contextual/extension-methods.html), where emerges the need to do
overload resolution based on additional argument blocks.
