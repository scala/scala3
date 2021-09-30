---
title: "Dropped: private[this] and protected[this]"
type: section
num: 85
previous-page: /scala3/reference/dropped-features/weak-conformance
next-page: /scala3/reference/dropped-features/wildcard-init
---

The `private[this]` and `protected[this]` access modifiers are deprecated and will be phased out.

Previously, these modifiers were needed for

 - avoiding the generation of getters and setters
 - excluding code under a `private[this]` from variance checks. (Scala 2 also excludes `protected[this]` but this was found to be unsound and was therefore removed).

The compiler now infers for `private` members the fact that they are only accessed via `this`. Such members are treated as if they had been declared `private[this]`. `protected[this]` is dropped without a replacement.

