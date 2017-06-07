---
layout: doc-page
title: "Literal Singleton Types"
---

Literal Singleton Types allows primitive literals to be used as types. For example:

```scala
val t: 42 = 42
val t: "Jedi" = "Jedi"
```

It is also possible to define functions returning or taking singleton types such as

```scala
def f(t: Double): t.type = t
val a: 1.2 = f(1.2)
```

For more details and the motivation behind the need for literal singleton types, check out [SIP-23](http://docs.scala-lang.org/sips/pending/42.type.html)
