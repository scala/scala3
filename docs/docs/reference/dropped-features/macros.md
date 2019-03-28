---
layout: doc-page
title: Dropped: Scala 2 Macros
---

The previous, experimental macro system has been dropped. Instead,
there is a cleaner, more restricted system based on two complementary
concepts: `inline` and `'{ ... }`/`${ ... }` code generation.

* `inline` has been [implemented](../other-new-features/inline.md) in Dotty.
* Quotes `'{ ... }` and splices `${ ... }` has been [implemented](../other-new-features/principled-meta-programming.md) in Dotty.
  * [TASTy reflect](../other-new-features/tasty-reflect.md) provides more complex tree based APIs to inspect or create quoted code.
