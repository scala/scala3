---
layout: doc-page
title: "Type Lambdas"
---

A _type lambda_ lets one express a higher-kinded type directly, without
a type definition.

    [+X, Y] =>> Map[Y, X]

For instance, the type above defines a binary type constructor, with a
covariant parameter `X` and a non-variant parameter `Y`. The
constructor maps arguments `X` and `Y` to `Map[Y, X]`. Type parameters
of type lambdas can have variances and bounds. A parameterized type
definition or declaration such as

    type T[X] = (X, X)

is a shorthand for a plain type definition with a type-lambda as its
right-hand side:

    type T = [X] =>> (X, X)

[More details](./type-lambdas-spec.html)
