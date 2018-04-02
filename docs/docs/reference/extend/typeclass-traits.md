---
layout: doc-page
title: "Typeclass Traits"
---

Typeclass traits offer two new ways to express the type structure of implementing classes. First, they allow to refer to the implementation type using `This`. Second, they can abstract not only over _instance values_ but also over values that exist only once for each implementing _type_, using "common" definitions.

A typeclass trait is a trait that extends directly or indirectly the marker trait `scala.TypeClass`. Here are two typeclass traits:
```scala
trait SemiGroup extends TypeClass {
  def add(that: This): This
}
trait Monoid extends SemiGroup {
  common def unit: This
}
```
The `SemiGroup` trait refers to the implementation type via `This`. The implementation type can be
an extending class or it can be defined by an instance declaration. Here is an example of the latter:

```scala
extension IntMonoid for Int : Monoid {
  def add(that: Int) = this + that
  common def unit = 0
}
```
In this extension, the `This` type of `SemiGroup` and `Monoid` is bound to `Int`.

Here is an example a class extending `Monoid` directly:

```scala
enum Nat extends Monoid {
  case Z
  case S(n: Nat)

  def add(that: Nat): Nat = this match {
    case Z => that
    case S(n) => S(n.add(that))
  }

  common def unit = Z
}
```

In this enum, the `This` type of `SemiGroup` and `Monoid` is bound to `Nat`.

### The `This` type.

The `This` type can be seen as an analogue of the `this` value. Where the `this` value refers to the "current object" on which some operation is performed, the `This` type refers to the type of that object. Both versions work for implementing classes and for instance declarations.

Note that the first class extending a typeclass trait fixes the meaning of `This`. For instance, the following is legal:

```scala
class A extends SemiGroup {
  def add(that: A) = ???
}
class B extends A with Monoid {
  common def unit: B = ???  // OK since B <: A = This
}
```
But we could not have re-implemented `add` in `B` with
```scala
  def add(that: B): B = ...
```
since `This` is already fixed to be `A`.

`This` can be thought of as am`common` abstract type member of a typeclass trait. It is as if `This` was defined explicitly as
```scala
common type This
```
While it is possible to constrain or bind `This` explicitly, this is not encouraged because it might conflict with the compiler-generated bindings for `This`.
