---
layout: doc-page
title: Changed: Lazy Vals
---

Lazy field initialization no longer guarantees safe publishing. This change was done
to avoid the performance overhead of thread synchonization because most lazy vals are only used in
a single-threaded context.

## Compatibility considerations

To get back safe publishing (Scala 2's default) you need to annotate a lazy val with `@volatile`. In

```scala
@volatile lazy val x = expr
```

it is guaranteed that readers of a lazy val will see the value of `x` once it is assigned.

Local lazy vals (i.e. defined inside methods) are left unchanged.

The [ScalaFix](https://scalacenter.github.io/scalafix/) rewrite tool
as well as Dotty's own `-language:Scala2 -rewrite` option will rewrite all normal
lazy vals to volatile ones in order to keep their semantics compatible
with current Scala's.

[More details](./lazy-vals-spec.html)
