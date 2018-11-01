---
layout: doc-page
title: "Automatic Eta Expansion"
---

The conversion of _methods_ into _functions_ has been improved and happens automatically for methods with one or more parameters.

```scala
def m(x: Boolean, y: String)(z: Int): List[Int]
val f1 = m
val f2 = m(true, "abc")
```

This creates two function values:
```scala
f1: (Boolean, String) => Int => List[Int]
f2: Int => List[Int]
```

[More details](eta-expansion-spec.html)

