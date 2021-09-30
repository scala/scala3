---
layout: singlepage-overview
scala3: true
title: "Named Type Arguments - More Details"
---

## Syntax

The addition to the grammar is:

```
SimpleExpr1       ::=  ...
                    |  SimpleExpr (TypeArgs | NamedTypeArgs)
NamedTypeArgs     ::=  ‘[’ NamedTypeArg {‘,’ NamedTypeArg} ‘]’
NamedTypeArg      ::=  id ‘=’ Type
```

Note in particular that named arguments cannot be passed to type constructors:

``` scala
class C[T]

val x: C[T = Int] = // error
  new C[T = Int] // error

class E extends C[T = Int] // error
```

## Compatibility considerations

Named type arguments do not have an impact on binary compatibility, but they
have an impact on source compatibility: if the name of a method type parameter
is changed, any existing named reference to this parameter will break. This
means that the names of method type parameters are now part of the public API
of a library.

(Unimplemented proposal: to mitigate this,
[`scala.deprecatedName`](https://www.scala-lang.org/api/current/scala/deprecatedName.html)
could be extended to also be applicable on method type parameters.)
