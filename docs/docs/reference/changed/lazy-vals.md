---
layout: doc-page
title: Changed: Lazy Vals and @volatile
---

Lazy val initialization no longer guarantees safe publishing. This change was done
to avoid the performance overhead of thread synchonization because many lazy vals are only used in a single-threaded context.

To get back safe publishing you need to annotate a lazy val with `@volatile`. In

    @volatile lazy val x = expr

it is guaranteed that readers of a lazy val will see the value of `x`
once it is assigned.

The [ScalaFix](https://scalacenter.github.io/scalafix/) rewrite tool
as well as Dotty's own `-language:Scala2 -rewrite` option will rewrite all normal
lazy vals to volatile ones in order to keep their semantics compatible
with current Scala's.
