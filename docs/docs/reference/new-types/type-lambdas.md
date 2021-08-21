---
title: "Type Lambdas"
type: section
num: 5
previous-page: /scala3/reference/new-types/union-types
next-page: /scala3/reference/new-types/match-types
---

A _type lambda_ lets one express a higher-kinded type directly, without
a type definition.

```scala
[X, Y] =>> Map[Y, X]
```

For instance, the type above defines a binary type constructor, which maps arguments `X` and `Y` to `Map[Y, X]`.
Type parameters of type lambdas can have bounds, but they cannot carry `+` or `-` variance annotations.

[More details](./type-lambdas-spec.html)
