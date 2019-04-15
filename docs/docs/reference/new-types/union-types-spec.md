---
layout: doc-page
title: "Union Types - More Details"
---

## Syntax

Syntactically, unions follow the same rules as intersections, but have a lower precedence, see
[Intersection Types - More Details](http://lampepfl.github.io/dotty/docs/reference/new-types/intersection-types-spec.html).

### Interaction with pattern matching syntax
`|` is also used in pattern matching to separate pattern alternatives and has
lower precedence than `:` as used in typed patterns, this means that:

``` scala
case _: A | B => ...
```
is still equivalent to:
``` scala
case (_: A) | B => ...
```
and not to:
``` scala
case _: (A | B) => ...
```

## Subtyping Rules

- `A` is always a subtype of `A | B` for all `A`, `B`.
- If `A <: C` and `B <: C` then `A | B <: C`
- Like `&`, `|` is commutative and associative:
  ```scala
  A | B =:= B | A
  A | (B | C) =:= (A | B) | C
  ```
- `&` is distributive over `|`:
  ```scala
  A & (B | C) =:= A & B | A & C
  ```

From these rules it follows that the _least upper bound_ (lub) of a set of type
is the union of these types. This replaces the
[definition of least upper bound in the Scala 2 specification](https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#least-upper-bounds-and-greatest-lower-bounds).

## Motivation

The primary reason for introducing union types in Scala is that they allow us to
guarantee that for every set of type, we can always form a finite lub. This is
both useful in practice (infinite lubs in Scala 2 were approximated in an ad-hoc
way, resulting in imprecise and sometimes incredibly long types) and in theory
(the type system of Scala 3 is based on the
[DOT calculus](https://infoscience.epfl.ch/record/227176/files/soundness_oopsla16.pdf),
which has union types).

Additionally, union types are a useful construct when trying to give types to existing
dynamically typed APIs, this is why they're [an integral part of TypeScript](https://www.typescriptlang.org/docs/handbook/advanced-types.html#union-types)
and have even been [partially implemented in Scala.js](https://github.com/scala-js/scala-js/blob/master/library/src/main/scala/scala/scalajs/js/Union.scala).

## Join of a union type

In some situation described below, a union type might need to be widened to
a non-union type, for this purpose we define the _join_ of a union type `T1 |
... | Tn` as the smallest intersection type of base class instances of
`T1`,...,`Tn`. Note that union types might still appear as type arguments in the
resulting type, this guarantees that the join is always finite.

### Example

Given

```scala
trait C[+T]
trait D
class A extends C[A] with D
class B extends C[B] with D with E
```

The join of `A | B` is `C[A | B] with D`

## Type inference

When inferring the result type of a definition (`val`, `var`, or `def`), if the
type we are about to infer is a union type, we replace it by its join.
Similarly, when instantiating a type argument, if the corresponding type
parameter is not upper-bounded by a union type and the type we are about to
instantiate is a union type, we replace it by its join. This mirrors the
treatment of singleton types which are also widened to their underlying type
unless explicitly specified. and the motivation is the same: inferring types
which are "too precise" can lead to unintuitive typechecking issues later on.

Note: Since this behavior limits the usability of union types, it might
be changed in the future. For example by not widening unions that have been
explicitly written down by the user and not inferred, or by not widening a type
argument when the corresponding type parameter is covariant. See
[#2330](https://github.com/lampepfl/dotty/pull/2330) and
[#4867](https://github.com/lampepfl/dotty/issues/4867) for further discussions.

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


## Members

The members of a union type are the members of its join.

### Example

The following code does not typecheck, because `hello` is not a member of
`AnyRef` which is the join of `A | B`.

```scala
trait A { def hello: String }
trait B { def hello: String }

def test(x: A | B) = x.hello // error: value `hello` is not a member of A | B
```

## Exhaustivity checking

If the selector of a pattern match is a union type, the match is considered
exhaustive if all parts of the union are covered.

## Erasure

The erased type for `A | B` is the _erased least upper bound_ of the erased
types of `A` and `B`. Quoting from the documentation of `TypeErasure#erasedLub`,
the erased lub is computed as follows:
- if both argument are arrays of objects, an array of the erased lub of the element types
- if both arguments are arrays of same primitives, an array of this primitive
- if one argument is array of primitives and the other is array of objects, Object
- if one argument is an array, Object
- otherwise a common superclass or trait S of the argument classes, with the
  following two properties:
  * S is minimal: no other common superclass or trait derives from S
  * S is last   : in the linearization of the first argument type `|A|`
                  there are no minimal common superclasses or traits that
                  come after S.
  The reason to pick last is that we prefer classes over traits that way,
  which leads to more predictable bytecode and (?) faster dynamic dispatch.

