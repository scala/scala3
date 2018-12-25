---
layout: doc-page
title: Dropped: Procedure Syntax
---

Procedure syntax

    def f() { ... }

has been dropped. You need to write one of the following instead:

    def f() = { ... }
    def f(): Unit = { ... }

Dotty will accept the old syntax under the `-language:Scala2` option.
If the `-migration` option is set, it can even rewrite old syntax to new.
The [ScalaFix](https://scalacenter.github.io/scalafix/) tool also
can rewrite procedure syntax to make it Dotty-compatible.

