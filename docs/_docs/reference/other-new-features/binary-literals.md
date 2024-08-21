---
layout: doc-page
title: "Binary Integer Literals"
nightlyOf: https://docs.scala-lang.org/scala3/reference/changed-features/binary-integer-literals.html
---

A new syntax for integer literals has been added, it is now possible to do the following:
```scala
val bitmask = 0b0010_0000 // equivalent to 32, 0x20
```

Binary integer literals behave similarly to hex integer literals (`0x...`), for example:
* Both `0b...` and `0B...` are allowed
* `0b`/`0B` on its own is disallowed, possible alternatives: `0`, `0b0`, `0B0`
* Only `0` and `1` are allowed after the b (`b`/`B`)
* Underscores `_` are allowed anywhere between digits, and are ignored: `0b__1 == 0b1`


Note: This change has been backported to Scala 2.13.13, it is therefore not technically a changed feature
