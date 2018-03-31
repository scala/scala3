---
layout: doc-page
title: "Context Bounds"
---

Context bounds present a new, general way to abstract over extensions, encompassing both class inheritance and instance declarations. Previously, a context
bound `[T : B]` required a parameterized class `B` as the bound of the type parameter `T`.
It specified an implicit evidence parameter of type `B[T]`. For instance,
```scala
  def f[T : math.Ordering]
```
was a shorthand form for
```scala
  def f[T](implicit $ev: math.Ordering[T])
```

This usage pattern is still supported. But there is now a second usage of context bounds which takes effect if the kind of the type parameter `T` and the bound `B` is the same.

### Typeclass Bounds

If `B` is a typeclass trait, a context bound `[T : B]` means "the implementation of typeclass `B` by type `T`". It translates to an implicit evidence parameter of type
`B.Impl[T]`. Example:

```scala
def sum[T: Monoid](xs: Iterable[T]): T =
  xs.foldLeft(Monoid.impl[T].unit)(_ `add` _)
```

This definition of `sum` can be used for all `Monoid` implementations, no matter whether they are defined by extending classes or extension clauses:

```scala
assert(           sum(List(1, 2, 3)) == 6                )
assert( sum(List(Set(1, 2), Set(3))) == Set(1, 2, 3)     )
assert(     sum(List(S(Z), S(S(Z)))) == List(S(S(S(Z)))) )
```

Here is an alternative formulation of `sum` which makes the implicit evidence parameter explicit:

```scala
def sum[T](xs: Iterable[T])(implicit ev: Monoid.Impl[T]): T =
  xs.foldLeft(ev.unit)(_ `add` _)
```

Of course, we could also have defined `sum` as an extension method:

```scala
extension MonoidSum[T : Monoid] for Iterable[T] {
  def sum = foldLeft(Monoid.impl[T].unit)(_ `add` _)
}
List(1, 2, 3).sum
```

### The `Impl` Type.

The companion object of every typeclass trait `TC` defines a type constructor
```scala
object TC {
  type Impl[T] = ... // (implementation dependent)
}
```
The type `TC.Impl[T]` refers to the implementation object that implements `TC` for `T`. For instance, `Monoid.Impl[Int]` refers to the the `IntMonoid` object, whereas
`Monoid.Impl[Nat]` refers to the companion object of class `Nat`. In general, `TC.Impl[T]` is one of the following:

 - if the implementation of `C : TC` is via an extension clause, the object representing the extension
 - if the implementation of `C : TC` is a class `C` extending `TC`, the companion object of `C`.

Besides the `Impl` type, the companion object of a typeclass trait also defines a utility
method `impl`, specified as follows:
```scala
  def impl[T](implicit ev: Impl[T]): Impl[T] = ev
```
This method is used in the the first implementation of `sum` above to retrieve
the monoid's `unit` value:
```scala
def sum[T: Monoid](xs: Iterable[T]): T =
  xs.foldLeft(Monoid.impl[T].unit)(_ `add` _)
```

### Injectors

This explains how `sum` can access `unit`, but what about `add`? The receiver type of `add`
is `Monoid`. How do we get an instance of this type from the implementation type `T`?

In fact every `TC.Impl[T]` implicit also gives rise to an implicit conversion from `T` to
`TC`. This conversion can be inserted on the left-hand side argument of `add` (which is of type `T`) to produce a `Monoid` instance. (Implementations are free to optimize this, for instance by calling a binary version of `add` directly, which avoids the boxing receiver conversion).

The details of the conversion are as follows:

 1. The `scala` package defines an `Injector` trait as follows:

    ```scala
      trait Injector {
        /** The implementing type */
        type This

        /** The implemented trait */
        type $Instance

        /** The implementation via type `T` for this trait */
        implicit def inject(x: This): $Instance
      }
    ```

 The `This` type of this injector is the same as the `This` type of an implementing typeclass trait; it representing the implementing type. The `$Instance` type is name-mangled and therefore should not be referred to in user programs. It refers to the implemented typeclass instance. An `inject` method converts from the first to the second.

 2. Every implementation object `TC.Impl[T]` inherits an `Injector` instance where `This = T` and `$Instance <: TC`. Hence, it defines an `inject` conversion from `T` to `TC`.

 3. There is a globally visible implicit conversion defined as follows:

 ```scala
   implicit def applyInjector[From, U](x: From)(implicit ev: Injector { type This = From }): ev.$Instance =
    ev.inject(x)
 ```
Thus, the fully spelled-out body of the `sum` method is
```scala
  def sum[T](xs: Iterable[T])(implicit ev: Monoid.Impl[T]): T =
    xs.foldLeft(ev.unit)((x, y) => applyInjector(x)(ev).add(y))
```

### Other Context Bounds

Context bounds `[T : B]` can also be written if the bound `B` is not a typeclass trait. Still assuming that `T` and `B` have the same kind, the context bound then amounts to
specifying an implicit evidence of type `Injectable[T, B]`, where `scala.Injectable` is defined as follows:
```scala
  type Injectable[T, +U] = Injector { type This = T; type $Instance <: U }
```
Instance declarations of a non-typeclass trait `B` with an implementation type `I` generate implementation objects that inherit from `Injectable[I, B]`. The type bound `[I: B]` is then resolved as the implementation object, exactly in the same way as for typeclass traits.

If class `C` extends `B` directly, the context bound `[C : B]` is resolved to `selfInject[C, B]`, referring to the following global definition of an injector that works for all pairs of types that are in a subtype relation:
```scala
  def selfInject[U, T <: U]: Injectable[T, U] = new Injector {
    type This = T
    type $Instance = U
    def inject(x: T) = x
  }
```
In other words, context bounds [T: B] for non-typeclass traits `B` work similar to the previous (now deprecated) view bounds [T <% B] in that they specify an implicit conversion between `T` and `B`.

### Conditional Instance Declarations

Context bounds enable conditional instance declarations. For instance, here is a type class
trait expressing an ordering:

```scala
trait Ord extends TypeClass {
  def compareTo(that: This): Int
  def <  (that: This) = compareTo(that) < 0
  def >  (that: This) = compareTo(that) > 0
  def <= (that: This) = compareTo(that) <= 0
  def >= (that: This) = compareTo(that) >= 0
}
```
Here is an instance declaration of `Ord` for `Int`:

```scala
extension IntOrd for Int : Ord {
  def compareTo(that: Int) =
    if (this < that) -1 else if (this > that) +1 else 0
}
```

And here is an instance declaration specifying that `List`s are ordered if their elements are:

```scala
extension ListOrd[T : Ord] for List[T] : Ord {
  def compareTo(that: List[T]): Int = (this, that) match {
    case (Nil, Nil) => 0
    case (Nil, _  ) => -1
    case (_  , Nil) => +1
    case (x :: xs, y :: ys) =>
      val fst = x.compareTo(y)
      if (fst != 0) fst else xs.compareTo(ys)
  }
}
```
