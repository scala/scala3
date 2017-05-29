---
layout: doc-page
title: Dropped: Existential Types
---

Existential types using `forSome` have been dropped. The reasons for dropping them were:

 - Existential types violate a type soundness principle on which DOT
   and Dotty are constructed. That principle says that every the
   prefix (`p`, respectvely `S`) of a type selection `p.T` or `S#T`
   must either come from a value constructed at runtime or refer to a
   type that is known to have only good bounds.

 - Existential types create many difficult feature interactions
   with other Scala constructs.

 - Existential types have large overlap with path-dependent types,
   so the gain of having them is relatively minor.

Existential types that can be expressed using only wildcards (but not
`forSome`) are still supported, but are treated as refined types.
For instance, the type

    Map[_ <: AnyRef, Int]

is treated as the type `Map`, where the first type parameter
is upper-bounded by `AnyRef` and the second type parameter is an alias
of `Int`.

When reading classfiles compiled with _scalac_, Dotty will do a best
effort to approximate existential types with its own types. It will
issue a warning is a precise emulation is not possible.

