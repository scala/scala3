---
layout: doc-page
title: "Preview"
nightlyOf: https://docs.scala-lang.org/scala3/reference/preview/overview.html
redirectFrom: overview.html
---

## Preview language features

New Scala language features or standard library APIs are initially introduced as experimental, but once they become fully implemented and accepted by the [SIP](https://docs.scala-lang.org/sips/) these can become a preview features.

Preview language features and APIs are guaranteed to be standardized in some next Scala minor release, but allow the compiler team to introduce small, possibly binary incompatible, changes based on the community feedback.
These can be used by early adopters who can accept the possibility of binary compatibility breakage. For instance, preview features could be used in some internal tool or application. On the other hand, preview features are discouraged in publicly available libraries.

More information about preview featues can be found in [preview defintions guide](../other-new-features/preview-defs.md)

### `-preview` compiler flag

This flag enables the use of all preview language feature in the project.


## List of available preview features

* [The `into` Type and Modifier](into.md)
