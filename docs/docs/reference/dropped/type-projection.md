---
layout: doc-page
title: Dropped: General Type Projection
---

Scala so far allowed general type projection `T#A` where `T` is an arbitrary type
and `A` names a type member of `T`.

Dotty allows this only if `T` is a class type. The change was made because
unrestricted type projection is [unsound](https://github.com/lampepfl/dotty/issues/1050).

The restriction rule out the [type-level encoding of compbinator
 calculus](https://michid.wordpress.com/2010/01/29/scala-type-level-encoding-of-the-ski-calculus/). It also rules out the previous encodings of type
lambdas using structural types with projection as application.  Type
 lambdas are now [directly supported](../type-lambdas.md) in Dotty.