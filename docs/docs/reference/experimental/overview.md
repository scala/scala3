---
layout: doc-page
title: "Experimental language features"
movedTo: https://docs.scala-lang.org/scala3/reference/experimental/overview.html
---

### Experimental language features

All experimental language features can be found under the `scala.language.experimental` package.
They are enabled by importing the feature or using the `-language` compiler flag.

* [`erasedDefinitions`](./erased-defs.md): Enable support for `erased` modifier.
* `fewerBraces`: Enable support for using indentation for arguments.
* [`genericNumberLiterals`](./numeric-literals.md): Enable support for generic number literals.
* [`namedTypeArguments`](./named-typeargs.md): Enable support for named type arguments
* [`saferExceptions`](./canthrow.md): Enable support for checked exceptions.

### Experimental language imports

In general, experimental language features can be imported in an experimental scope (see [experimental definitions](../other-new-features/experimental-defs.md).
They can be imported at the top-level if all top-level definitions are @experimental.

### Experimental language features supported by special compiler options

Some experimental language features that are still in research and development can be enabled with special compiler options. These include

* `-Yexplicit-nulls` Enable support for tracking null references in the type system.
* [`-Ycc`](./cc.md) Enable capture checking.

