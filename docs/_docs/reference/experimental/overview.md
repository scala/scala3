---
layout: doc-page
title: "Experimental"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/overview.html
redirectFrom: overview.html
---

## Experimental language features

All experimental language features can be found under the `scala.language.experimental` package.
They are enabled by importing the feature or using the `-language` compiler flag.

* [`erasedDefinitions`](./erased-defs.md): Enable support for `erased` modifier.
* `fewerBraces`: Enable support for using indentation for arguments.
* [`genericNumberLiterals`](./numeric-literals.md): Enable support for generic number literals.
* [`namedTypeArguments`](./named-typeargs.md): Enable support for named type arguments
* [`saferExceptions`](./canthrow.md): Enable support for checked exceptions.

## Experimental language imports

In general, experimental language features can be imported in an experimental scope (see [experimental definitions](../other-new-features/experimental-defs.md)).
They can be imported at the top-level if all top-level definitions are `@experimental`.

### `-experimental` compiler flag

This flag enables the use of any experimental language feature in the project.
It does this by adding an `@experimental` annotation to all top-level definitions.
Hence, dependent projects also have to be experimental.

## Experimental language features supported by special compiler options

Some experimental language features that are still in research and development can be enabled with special compiler options. These include

* [`-Yexplicit-nulls`](./explicit-nulls.md). Enable support for tracking null references in the type system.
* [`-Ycc`](./capture-checking/cc.md). Enable support for capture checking.
