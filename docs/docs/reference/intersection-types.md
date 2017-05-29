---
layout: doc-page
title: "Intersection Types"
---

Used on types, the `&` operator creates an intersection type.

```scala
trait Resetable {
  def reset(): this.type
}
trait Growable[T] {
  def add(x: T): this.type
}
def f(x: Resetable & Growable[String]) = {
  x.reset()
  x.add("first")
}
```

The value `x` is required to be _both_ a `Resetable` and a
`Growable[String]`.  Intersection types `A & B` replace compound types
`A with B` in Scala 2 (for the moment, `A with B` is still allowed, but
it will be deprecated and removed in the future).

Unlike `with` types, `&` is _commutative_: `A & B` is the same type as
`B & A`.

The members of an intersection type `A & B` are all the members of `A`
and all the members of `B`. For instance `Resetable & Growable[String]`
has member methods `reset` and `add`.

If a member appears in both `A` and `B`, its type in `A & B` is the
intersection of its type in `A` and its type in `B`. For instance, assume the definitions:

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

One might wonder how the compiler could come up with a definition for
`children` of type `List[A & B]` since all its is given are `children`
definitions of type `List[A]` and `List[B]`. The answer is it does not
need to. `A & B` is just a type that represents a set of requirements for
values of the type. At the point where a value is _constructed_, one
must make sure that all inherited members are correctly defined.
So if one defines a class `C` that inherits `A` and `B`, one needs
to give at that point a definition of a `children` method with the required type.

```scala
class C extends A with B {
  def children: List[A & B] = ???
}
```
