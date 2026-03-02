---
layout: doc-page
title: "Union Types"
nightlyOf: https://docs.scala-lang.org/scala3/reference/new-types/union-types.html
---

A union type `A | B` has as values all values of type `A` and also all values of type `B`.


```scala
case class UserName(name: String)
case class Password(hash: Hash)

def help(id: UserName | Password) =
  val user = id match
    case UserName(name) => lookupName(name)
    case Password(hash) => lookupPassword(hash)
  ...
```

Union types are duals of intersection types. `|` is _commutative_:
`A | B` is the same type as `B | A`.

The compiler will assign a union type to an expression only if such a
type is explicitly given. This can be seen in the following [REPL](https://docs.scala-lang.org/overviews/repl/overview.html) transcript:

```scala
scala> val password = Password(123)
val password: Password = Password(123)

scala> val name = UserName("Eve")
val name: UserName = UserName(Eve)

scala> if true then name else password
val res2: Object = UserName(Eve)

scala> val either: Password | UserName = if true then name else password
val either: Password | UserName = UserName(Eve)
```

The type of `res2` is `Object & Product`, which is a supertype of
`UserName` and `Password`, but not the least supertype `Password |
UserName`.  If we want the least supertype, we have to give it
explicitly, as is done for the type of `either`.

## Type inference

When inferring the result type of a definition (`val`, `var`, or `def`) and the
type we are about to infer is a union type, then we replace it by its join.
Similarly, when instantiating a type argument, if the corresponding type
parameter is not upper-bounded by a union type and the type we are about to
instantiate is a union type, we replace it by its join. This mirrors the
treatment of singleton types which are also widened to their underlying type
unless explicitly specified. The motivation is the same: inferring types
which are "too precise" can lead to unintuitive typechecking issues later on.

**Note:** Since this behavior limits the usability of union types, it might
be changed in the future. For example by not widening unions that have been
explicitly written down by the user and not inferred, or by not widening a type
argument when the corresponding type parameter is covariant.

See [PR #2330](https://github.com/scala/scala3/pull/2330) and
[Issue #4867](https://github.com/scala/scala3/issues/4867) for further discussions.

### Example

```scala
import scala.collection.mutable.ListBuffer
val x = ListBuffer(Right("foo"), Left(0))
val y: ListBuffer[Either[Int, String]] = x
```

This code typechecks because the inferred type argument to `ListBuffer` in the
right-hand side of `x` was `Left[Int, Nothing] | Right[Nothing, String]` which
was widened to `Either[Int, String]`. If the compiler hadn't done this widening,
the last line wouldn't typecheck because `ListBuffer` is invariant in its
argument.
