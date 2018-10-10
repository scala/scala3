---
layout: doc-page
title: "Intersection Types - More Details"
---

## Syntax

Syntactically, an intersection type `S & T` is similar to an infix type,
where the infix operator is `&`.

```
Type              ::=  ...| InfixType
InfixType         ::=  RefinedType {id [nl] RefinedType}
RefinedType       ::=  WithType {[nl] Refinement}
WithType          ::=  AnnotType {‘with’ AnnotType}
```

## Type Checking

The type `S & T` represents values that are of the type `S` and `T` at the same time.

```scala
trait Resettable {
  def reset(): this.type
}
trait Growable[T] {
  def add(x: T): this.type
}
def f(x: Resettable & Growable[String]) = {
  x.reset()
  x.add("first")
}
```

The value `x` is required to be _both_ a `Resettable` and a
`Growable[String]`.

The members of an intersection type `A & B` are all the members of `A` and all
the members of `B`.  For instance `Resettable & Growable[String]`
has member methods `reset` and `add`.

If a member appears in both `A` and `B`, its type in `A & B` is the intersection
of its type in `A` and its type in `B`. For instance, assume the definitions:

```scala
trait A {
  def children: List[A]
}
trait B {
  def children: List[B]
}
val x: A & B = new C
val ys: List[A & B] = x.children
```

The type of `children` in `A & B` is the intersection of `children`'s
type in `A` and its type in `B`, which is `List[A] & List[B]`. This
can be further simplified to `List[A & B]` because `List` is
covariant.

An intersection type `A & B` may not be inhabited, e.g. `Int & String` is not inhabited.
`A & B` is just a type that represents a set of requirements for
values of the type. At the point where a value is _constructed_, one
must make sure that all inherited members are correctly defined.
So if one defines a class `C` that inherits `A` and `B`, one needs
to give at that point a definition of a `children` method with the required type.

```scala
class C extends A with B {
  def children: List[A & B] = ???
}
```

## Subtyping Rules

```
            T <: A    T <: B
            ----------------
              T <: A & B

                A <: T
            ----------------
               A & B <: T

                B <: T
            ----------------
               A & B <: T
```

From the rules above, we can show that `&` is _commutative_: `A & B <: B & A` for any type `A` and `B`.
In another word, `A & B` is the same type as `B & A`, in that sense that the two types
have the same values and are subtypes of each other.

## Erasure

The erased type for `S & T` is the erased _glb_ (greatest lower bound) of the
erased type of `S` and `T`. The rules for erasure of intersection types are given
below in pseudocode:

```
|S & T| = glb(|S|, |T|)

glb(JArray(A), JArray(B)) = JArray(glb(A, B))
glb(JArray(T), _)         = JArray(T)
glb(_, JArray(T))         = JArray(T)
glb(A, B)                 = A             if A extends B
glb(A, B)                 = B             if B extends A
glb(A, _)                 = A             if A is not a trait
glb(_, B)                 = B             if B is not a trait
glb(A, _)                 = A
```

## Relationship with Compound Type (`with`)

Intersection types `A & B` replace compound types `A with B` in Scala 2. For the
moment, the syntax `A with B` is still allowed and interpreted as `A & B`, but
its usage as a type (as opposed to in a `new` or `extends` clause) will be
deprecated and removed in the future.
