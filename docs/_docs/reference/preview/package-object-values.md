---
layout: doc-page
title: "Reference-able Package Objects"
redirectFrom: /experimental/package-object-values.html
nightlyOf: https://docs.scala-lang.org/scala3/reference/preview/package-object-values.html
---

This feature is available under `-preview` since Scala 3.10.0. It is specified in [SIP-68](https://docs.scala-lang.org/sips/68.html).

One limitation with `package object`s is that we cannot currently assign them to values: `a.b` fails to compile when `b` is a `package object`, even though it succeeds when `b` is a normal `object`. The workaround is to call
```scala
  a.b.`package`
```
But this is ugly and non-obvious. Or one could use a normal `object`, which is not always possible.

This preview feature drops that limitation. It is enabled by the `-preview` compiler flag.

The feature turns the following into valid code:

```scala
package a
package object b

val z = a.b
```

Without the feature, the workaround is to use a `.package` suffix:

```scala
val z = a.b.`package`
```

With the feature, a reference such as `a.b` where `b` is a `package` containing a `package object`, expands to `a.b.package` automatically.

## Limitations

* `a.b` only expands to `a.b.package` when used "standalone", i.e. not when part of a larger select chain `a.b.c` or equivalent postfix expression `a.b c`, prefix expression `!a.b`, or infix expression `a.b c d`.

* `a.b` expands to `a.b.package` of the type `a.b.package.type`, and only contains the contents of the `package object`. It does not contain other things in the `package` `a.b` that are outside of the `package object`

Both these requirements are necessary for backwards compatibility, and anyway do not impact the main goal of removing the irregularity between `package object`s and normal `object`s.
