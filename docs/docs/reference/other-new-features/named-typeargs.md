---
layout: doc-page
title: "Named Type Arguments"
---

**Note:** This feature is implemented in Dotty, but is not expected to be part of Scala 3.0.

Type arguments of methods can now be named, as well as by position. Example:

``` scala
def construct[Elem, Coll[_]](xs: Elem*): Coll[Elem] = ???

val xs1 = construct[Coll = List, Elem = Int](1, 2, 3)
val xs2 = construct[Coll = List](1, 2, 3)
```

Similar to a named value argument `(x = e)`, a named type argument
`[X = T]` instantiates the type parameter `X` to the type `T`. Type
arguments must be all named or un-named, mixtures of named and
positional type arguments are not supported.

## Motivation

The main benefit of named type arguments is that unlike positional arguments,
you are allowed to omit passing arguments for some parameters, like in the
definition of `xs2` above. A missing type argument is inferred as usual by
local type inference. This is particularly useful in situations where some type
arguments can be easily inferred from others.

[More details](./named-typeargs-spec.html)
