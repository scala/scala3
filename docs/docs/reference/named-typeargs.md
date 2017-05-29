---
layout: doc-page
title: "Named Type Arguments"
---

Type arguments of methods can now be named as well as by position. Example:


    def construct[Elem, Coll[_]](xs: Elem*): Coll[Elem] = ???

    val xs2 = construct[Coll = List, Elem = Int](1, 2, 3)
    val xs3 = construct[Coll = List](1, 2, 3)

Similar to a named value argument `(x = e)`, a named type argument
`[X = T]` instantiates the type parameter `X` to the type `T`. Type
arguments must be all named or un-named, mixtures of named and
positional type arguments are not supported.

The main benefit of named type arguments is that they allow some
arguments to be omitted. Indeed, if type arguments are named, some
arguments may be left out.  An example is the definition of `xs3`
above. A missing type argument is inferred as usual by local type
inference. The same is not true for positional arguments, which have
to be given always for all type parameters.

