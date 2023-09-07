
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

### Procedure Syntax

Procedure syntax
```scala
def f() { ... }
```
has been dropped. You need to write one of the following instead:
```scala
def f() = { ... }
def f(): Unit = { ... }
```
Scala 3 accepts the old syntax under the `-source:3.0-migration` option.
If the `-migration` option is set, it can even rewrite old syntax to new.
The [Scalafix](https://scalacenter.github.io/scalafix/) tool also
can rewrite procedure syntax to make it Scala 3 compatible.

## Compound Types (`with`)

Intersection types `A & B` replace compound types `A with B` in Scala 2.
For the moment, the syntax `A with B` is still allowed and interpreted as `A & B`, but its usage as a type (as opposed to in a `new` or `extends` clause) will be deprecated and removed in the future.
