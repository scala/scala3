
### Existential Types

Existential types using `forSome` (as in [SLS §3.2.12](https://www.scala-lang.org/files/archive/spec/2.13/03-types.html#existential-types)) are not available in Scala 3.
Therefore when reading an existential type from Scala 2, the following happens:

Existential types that can be expressed using only wildcards (but not
`forSome`) are treated as refined types.
For instance, the type
```scala
Map[_ <: AnyRef, Int]
```
is treated as the type `Map`, where the first type parameter
is upper-bounded by `AnyRef` and the second type parameter is an alias
of `Int`.

When reading class files compiled with Scala 2, Scala 3 will do a best
effort to approximate existential types with its own types. It will
issue a warning that a precise emulation is not possible.