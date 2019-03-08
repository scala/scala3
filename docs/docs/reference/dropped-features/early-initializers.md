---
layout: doc-page
title: Dropped: Early Initializers
---

Early initializers of the form

    class C extends { ... } with SuperClass ...

have been dropped. They were rarely used, and mostly to compensate for the lack of
[trait parameters](../other-new-features/trait-parameters.md), which are now directly supported in Dotty.
