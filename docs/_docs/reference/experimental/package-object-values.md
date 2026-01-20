---
layout: doc-page
title: "Reference-able Package Objects"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/package-object-values.html
---

One limitation with `package object`s is that we cannot currently assign them to values: `a.b` fails to compile when `b` is a `package object`, even though it succeeds when `b` is a normal `object`. The workaround is to call
```scala
  a.b.`package`
```
But this is ugly and non-obvious. Or one could use a normal `object`, which is not always possible.

The `packageObjectValues` language extension drops this limitation. The extension is enabled by the language import `import scala.language.experimental.packageObjectValues` or by setting the command line option `-language:experimental.packageObjectValues`.

The extension, turns the following into valid code:

```scala
package a
package object b

val z = a.b // Currently fails with "package is not a value"
```

Currently the workaround is to use a `.package` suffix:

```scala
val z = a.b.`package`
```

With the extension, a reference such as `a.b` where `b` is a `package` containing a `package object`, expands to `a.b.package` automatically

## Limitations

* `a.b` only expands to `a.b.package` when used "standalone", i.e. not when part of a larger select chain `a.b.c` or equivalent postfix expression `a.b c`, prefix expression `!a.b`, or infix expression `a.b c d`.

* `a.b` expands to `a.b.package` of the type `a.b.package.type`, and only contains the contents of the `package object`. It does not contain other things in the `package` `a.b` that are outside of the `package object`

Both these requirements are necessary for backwards compatibility, and anyway do not impact the main goal of removing the irregularity between `package object`s and normal `object`s.

