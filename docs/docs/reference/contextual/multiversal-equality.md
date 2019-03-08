---
layout: doc-page
title: "Multiversal Equality"
---

Previously, Scala had universal equality: Two values of any types
could be compared with each other with `==` and `!=`. This came from
the fact that `==` and `!=` are implemented in terms of Java's
`equals` method, which can also compare values of any two reference
types.

Universal equality is convenient. But it is also dangerous since it
undermines type safety. For instance, let's assume one is left after some refactoring
with an erroneous program where a value `y` has type `S` instead of the correct type `T`.

```scala
val x = ... // of type T
val y = ... // of type S, but should be T
x == y      // typechecks, will always yield false
```

If all the program does with `y` is compare it to other values of type `T`, the program will still typecheck, since values of all types can be compared with each other.
But it will probably give unexpected results and fail at runtime.

Multiversal equality is an opt-in way to make universal equality
safer. It uses a binary typeclass `Eql` to indicate that values of
two given types can be compared with each other.
The example above would not typecheck if `S` or `T` was a class
that derives `Eql`, e.g.
```scala
class T derives Eql
```
Alternatively, one can also provide the derived implied instance directly, like this:
```scala
implied for Eql[T, T] = Eql.derived
```
This definition effectively says that values of type `T` can (only) be
compared to other values of type `T` when using `==` or `!=`. The definition
affects type checking but it has no significance for runtime
behavior, since `==` always maps to `equals` and `!=` always maps to
the negation of `equals`. The right hand side `Eql.derived` of the definition
is a value that has any `Eql` instance as its type. Here is the definition of class
`Eql` and its companion object:
```scala
package scala
import annotation.implicitNotFound

@implicitNotFound("Values of types ${L} and ${R} cannot be compared with == or !=")
sealed trait Eql[-L, -R]

object Eql {
  object derived extends Eql[Any, Any]
}
```

One can have several `Eql` instances for a type. For example, the four
definitions below make values of type `A` and type `B` comparable with
each other, but not comparable to anything else:

```scala
implied for Eql[A, A] = Eql.derived
implied for Eql[B, B] = Eql.derived
implied for Eql[A, B] = Eql.derived
implied for Eql[B, A] = Eql.derived
```
The `scala.Eql` object defines a number of `Eql` instances that together
define a rule book for what standard types can be compared (more details below).

There's also a "fallback" instance named `eqlAny` that allows comparisons
over all types that do not themselves have an `Eql` instance.  `eqlAny` is
defined as follows:

```scala
def eqlAny[L, R]: Eql[L, R] = Eql.derived
```

Even though `eqlAny` is not declared `implied`, the compiler will still
construct an `eqlAny` instance as answer to an implicit search for the
type `Eql[L, R]`, unless `L` or `R` have `Eql` instances
defined on them, or the language feature `strictEquality` is enabled

The primary motivation for having `eqlAny` is backwards compatibility,
if this is of no concern one can disable `eqlAny` by enabling the language
feature `strictEquality`. As for all language features this can be either
done with an import

```scala
import scala.language.strictEquality
```
or with a command line option `-language:strictEquality`.

## Deriving Eql Instances

Instead of defining `Eql` instances directly, it is often more convenient to derive them. Example:
```scala
class Box[T](x: T) derives Eql
```
By the usual rules if [typeclass derivation](./derivation.html),
this generates the following `Eql` instance in the companion object of `Box`:
```scala
implied [T, U] given Eql[T, U] for Eql[Box[T], Box[U]] = Eql.derived
```
That is, two boxes are comparable with `==` or `!=` if their elements are. Examples:
```scala
new Box(1) == new Box(1L)   // ok since `Eql[Int, Long]` is an implied instance
new Box(1) == new Box("a")  // error: can't compare
new Box(1) == 1             // error: can't compare
```

## Precise Rules for Equality Checking

The precise rules for equality checking are as follows.

If the `strictEquality` feature is enabled then
a comparison using `x == y` or `x != y` between values `x: T` and `y: U`
is legal if

 1. there is an implied instance of type `Eql[T, U]`, or
 2. one of `T`, `U` is `Null`.

In the default case where the `strictEquality` feature is not enabled the comparison is
also legal if

 1. `T` and `U` the same, or
 2. one of `T` and `U`is a subtype of the _lifted_ version of the other type, or
 3. neither `T` nor `U` have a _reflexive `Eql` instance_.

Explanations:

 - _lifting_ a type `S` means replacing all references to  abstract types
   in covariant positions of `S` by their upper bound, and to replacing
   all refinement types in covariant positions of `S` by their parent.
 - a type `T` has a _reflexive `Eql` instance_ if the implicit search for `Eql[T, T]`
   succeeds.

## Predefined Eql Instances

The `Eql` object defines implied instances for
 - the primitive types `Byte`, `Short`, `Char`, `Int`, `Long`, `Float`, `Double`, `Boolean`,  and `Unit`,
 - `java.lang.Number`, `java.lang.Boolean`, and `java.lang.Character`,
 - `scala.collection.Seq`, and `scala.collection.Set`.

Implied instances are defined so that everyone of these types is has a reflexive `Eql` instance, and the following holds:

 - Primitive numeric types can be compared with each other.
 - Primitive numeric types can be compared with subtypes of `java.lang.Number` (and _vice versa_).
 - `Boolean` can be compared with `java.lang.Boolean` (and _vice versa_).
 - `Char` can be compared with `java.lang.Character` (and _vice versa_).
 - Two sequences (of arbitrary subtypes of `scala.collection.Seq`) can be compared
   with each other if their element types can be compared. The two sequence types
   need not be the same.
 - Two sets (of arbitrary subtypes of `scala.collection.Set`) can be compared
   with each other if their element types can be compared. The two set types
   need not be the same.
 - Any subtype of `AnyRef` can be compared with `Null` (and _vice versa_).

More on multiversal equality is found in a [blog post](http://www.scala-lang.org/blog/2016/05/06/multiversal-equality.html)
and a [Github issue](https://github.com/lampepfl/dotty/issues/1247).
