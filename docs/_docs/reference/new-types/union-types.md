---
layout: doc-page
title: "Union Types"
nightlyOf: https://docs.scala-lang.org/scala3/reference/new-types/union-types.html
---

A union type `A | B` includes all values of both types.


```scala
trait ID
case class UserName(name: String) extends ID
case class Password(hash: Hash) extends ID

def help(id: UserName | Password) =
  val user = id match
    case UserName(name) => lookupName(name)
    case Password(hash) => lookupPassword(hash)
  ...
```

Union types are duals of intersection types. `|` is _commutative_:
`A | B` is the same type as `B | A`.

The compiler will assign a union type to an expression only if such a
type is explicitly given or if the common supertype of all alternatives is [transparent](../other-new-features/transparent-traits.md).


This can be seen in the following [REPL](https://docs.scala-lang.org/overviews/repl/overview.html) transcript:

```scala
scala> val password = Password(123)
val password: Password = Password(123)

scala> val name = UserName("Eve")
val name: UserName = UserName(Eve)

scala> if true then name else password
val res1: ID = UserName(Eve)

scala> val either: Password | UserName = if true then name else password
val either: UserName | Password = UserName(Eve)
```
The type of `res1` is `ID`, which is a supertype of
`UserName` and `Password`, but not the least supertype `UserName | Password`.
If we want the least supertype, we have to give it
explicitly, as is done for the type of `either`.

The inference behavior changes if the common supertrait `ID` is declared `transparent`:
```scala
transparent trait ID
```
In that case the union type is not widened.
```scala
scala> if true then name else password
val res2: UserName | Password = UserName(Eve)
```
The more precise union type is also inferred if `UserName` and `Password` are declared without an explicit
parent, since in that case their implied superclass is `Object`, which is among the classes that are
assumed to be transparent. See [Transparent Traits and Classes](../other-new-features/transparent-traits.md)
for a list of such classes.
```scala
case class UserName(name: String)
case class Password(hash: Hash)

scala> if true then UserName("Eve") else Password(123)
val res3: UserName | Password = UserName(Eve)
```


[More details](./union-types-spec.md)
