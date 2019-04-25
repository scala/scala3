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
Alternatively, one can also provide the derived evidence directly, like this:
```scala
evidence for Eql[T, T] = Eql.derived
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
evidence for Eql[A, A] = Eql.derived
evidence for Eql[B, B] = Eql.derived
evidence for Eql[A, B] = Eql.derived
evidence for Eql[B, A] = Eql.derived
```
The `scala.Eql` object defines a number of `Eql` instances that together
define a rule book for what standard types can be compared (more details below).

There's also a "fallback" instance named `eqlAny` that allows comparisons
over all types that do not themselves have an `Eql` instance.  `eqlAny` is
defined as follows:

```scala
def eqlAny[L, R]: Eql[L, R] = Eql.derived
```

Even though `eqlAny` is not declared as `evidence`, the compiler will still
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
evidence [T, U] given Eql[T, U] for Eql[Box[T], Box[U]] = Eql.derived
```
That is, two boxes are comparable with `==` or `!=` if their elements are. Examples:
```scala
new Box(1) == new Box(1L)   // ok since there is evidence for `Eql[Int, Long]`
new Box(1) == new Box("a")  // error: can't compare
new Box(1) == 1             // error: can't compare
```

## Precise Rules for Equality Checking

The precise rules for equality checking are as follows.

If the `strictEquality` feature is enabled then
a comparison using `x == y` or `x != y` between values `x: T` and `y: U`
is legal if

 1. there is an evidence for `Eql[T, U]`, or
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

The `Eql` object defines evidence for comparing
 - the primitive types `Byte`, `Short`, `Char`, `Int`, `Long`, `Float`, `Double`, `Boolean`,  and `Unit`,
 - `java.lang.Number`, `java.lang.Boolean`, and `java.lang.Character`,
 - `scala.collection.Seq`, and `scala.collection.Set`.

Evidence is defined so that everyone of these types is has a reflexive `Eql` evidence, and the following holds:

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

## Why Two Type Parameters?

One particular feature of the `Eql` type is that it takes _two_ type parameters, representing the types of the two items to be compared. By contrast, conventional
implementations of an equality type class take only a single type parameter which represents the common type of _both_ operands. One type parameter is simpler than two, so why go through the additional complication? The reason has to do with the fact that, rather than coming up with a type class where no operation existed before,
we are dealing with a refinement of pre-existing, universal equality. It's best illustrated through an example.

Say you want to come up with a safe version of the `contains` method on `List[T]`. The original definition of `contains` in the standard library was:
```scala
class List[+T] {
  ...
  def contains(x: Any): Boolean
}
```
That uses universal equality in an unsafe way since it permits arguments of any type to be compared with the list's elements. The "obvious" alternative definition
```scala
  def contains(x: T): Boolean
```
does not work, since it refers to the covariant parameter `T` in a nonvariant context. The only variance-correct way to use the type parameter `T` in `contains` is as a lower bound:
```scala
  def contains[U >: T](x: U): Boolean
```
This generic version of `contains` is the one used in the current (Scala 2.12) version of `List`.
It looks different but it admits exactly the same applications as the `contains(x: Any)` definition we started with.
However, we can make it more useful (i.e. restrictive) by adding an `Eql` parameter:
```scala
  def contains[U >: T](x: U) given Eql[T, U]: Boolean // (1)
```
This version of `contains` is equality-safe! More precisely, given
`x: T`, `xs: List[T]` and `y: U`, then `xs.contains(y)` is type-correct if and only if
`x == y` is type-correct.

Unfortunately, the crucial ability to "lift" equality type checking from simple equality and pattern matching to arbitrary user-defined operations gets lost if we restrict ourselves to an equality class with a single type parameter. Consider the following signature of `contains` with a hypothetical `Eql1[T]` type class:
```scala
  def contains[U >: T](x: U) given Eql1[U]: Boolean   // (2)
```
This version could be applied just as widely as the original `contains(x: Any)` method,
since the `Eql1[Any]` fallback is always available! So we have gained nothing. What got lost in the transition to a single parameter type class was the original rule that `Eql[A, B]` is available only if neither `A` nor `B` have a reflexive `Eql` instance. That rule simply cannot be expressed if there is a single type parameter for `Eql`.

The situation is different under `-language:strictEquality`. In that case,
the `Eql[Any, Any]` or `Eql1[Any]` instances would never be available, and the
single and two-parameter versions would indeed coincide for most practical purposes.

But assuming `-language:strictEquality` immediately and everywhere poses migration problems which might well be unsurmountable. Consider again `contains`, which is in the standard library. Parameterizing it with the `Eql` type class as in (1) is an immediate win since it rules out non-sensical applications while still allowing all sensible ones.
So it can be done almost at any time, modulo binary compatibility concerns.
On the other hand, parameterizing `contains` with `Eql1` as in (2) would make `contains`
unusable for all types that have not yet declared an `Eql1` instance, including all
types coming from Java. This is clearly unacceptable. It would lead to a situation where,
rather than migrating existing libraries to use safe equality, the only upgrade path is to have parallel libraries, with the new version only catering to types deriving `Eql1` and the old version dealing with everything else. Such a split of the ecosystem would be very problematic, which means the cure is likely to be worse than the disease.

For these reasons, it looks like a two-parameter type class is the only way forward because it can take the existing ecosystem where it is and migrate it towards a future where more and more code uses safe equality.

In applications where `-language:strictEquality` is the default one could also introduce a one-parameter type alias such as
```scala
type Eq[-T] = Eql[T, T]
```
Operations needing safe equality could then use this alias instead of the two-parameter `Eql` class. But it would only
work under `-language:strictEquality`, since otherwise the universal `Eq[Any]` instance would be available everywhere.


More on multiversal equality is found in a [blog post](http://www.scala-lang.org/blog/2016/05/06/multiversal-equality.html)
and a [Github issue](https://github.com/lampepfl/dotty/issues/1247).
