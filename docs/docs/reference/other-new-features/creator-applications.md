---
layout: doc-page
title: "Creator Applications"
---

Creator applications allow to use simple function call syntax to create instances
of a class, even if there is no apply method implemented. Example:
```scala
class StringBuilder(s: String) {
   def this() = this("")
}

StringBuilder("abc")  // same as new StringBuilder("abc")
StringBuilder()       // same as new StringBuilder()
```
Creator applications generalize a functionality provided so far only for case classes, but the mechanism how this is achieved is different. Instead generating an apply method, the compiler adds a new possible interpretation to a function call `f(args)`. The previous rules are:

Given a function call `f(args)`,

 - if `f` is a method applicable to `args`, typecheck `f(args)` unchanged,
 - otherwise, if `f` has an `apply` method applicable to `args` as a member, continue with `f.apply(args)`,
 - otherwise, if `f` is of the form `p.m` and there is an implicit conversion `c` applicable to `p` so that `c(p).m` is applicable to `args`, continue with  `c(p).m(args)`

There's now a fourth rule following these rules:

 - otherwise, if `f` is syntactically a stable identifier, and `new f` where `f` is interpreted as a type identifier is applicable to `args`, continue with `new f(args)`.

 Analogously, the possible interpretations of a function call with type arguments `f[targs]` are augmented with the following interpretation as a final fallback:

 - if `f` is syntactically a stable identifier, and `new f[targs]` where `f` is interpreted as a type identifier is well-typed, continue with `new f[targs]`.

### Motivation

Leaving out `new` hides an implementation detail and makes code more pleasant to read. Even though it requires a new rule, it will likely increase the perceived regularity of the language, since case classes already provide function call creation syntax (and are often defined for this reason alone).

### Discussion

An alternative design would auto-generate `apply` methods for normal classes, in the same way it is done now for case classes. This design was tried but abandoned since it
caused numerous problems, including

 - overloading ambiguities
 - overriding errors
 - shadowing of user-defined `apply` methods by more specific auto-generated ones.