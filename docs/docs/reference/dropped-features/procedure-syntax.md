---
title: "Dropped: Procedure Syntax"
type: section
num: 76
previous-page: /scala3/reference/dropped-features/do-while
next-page: /scala3/reference/dropped-features/package-objects
---

Procedure syntax
```scala
def f() { ... }
```
has been dropped. You need to write one of the following instead:
```scala
def f() = { ... }
def f(): Unit = { ... }
```
Scala 3 accepts the old syntax under the `-source:3.0-migration` option.
If the `-migration` option is set, it can even rewrite old syntax to new.
The [Scalafix](https://scalacenter.github.io/scalafix/) tool also
can rewrite procedure syntax to make it Scala 3 compatible.
