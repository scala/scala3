---
layout: doc-page
title: "Dropped: General Type Projection"
nightlyOf: https://docs.scala-lang.org/scala3/reference/dropped-features/type-projection.html
---

Scala so far allowed general type projection `T#A` where `T` is an arbitrary type
and `A` names a type member of `T`.

Scala 3 disallows this if `T` is an abstract type (class types and type aliases
are fine). This change was made because unrestricted type projection
is [unsound](https://github.com/lampepfl/dotty/issues/1050).

This restriction rules out the [type-level encoding of a combinator
calculus](https://michid.wordpress.com/2010/01/29/scala-type-level-encoding-of-the-ski-calculus/).

To rewrite code using type projections on abstract types, consider using
path-dependent types or implicit parameters.
