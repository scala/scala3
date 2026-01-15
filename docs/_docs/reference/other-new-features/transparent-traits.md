---
layout: doc-page
title: "Transparent Traits and Classes"
nightlyOf: https://docs.scala-lang.org/scala3/reference/other-new-features/transparent-traits.html
---

Traits are used in two roles:

 1. As mixins for other classes and traits
 2. As types of vals, defs, or parameters

Some traits are used primarily in the first role, and we usually do not want to see them in inferred types. An example is the [`Product`](https://scala-lang.org/api/3.x/scala/Product.html) trait that the compiler adds as a mixin trait to every case class or case object. In Scala 2, this parent trait sometimes makes inferred types more complicated than they should be. Example:

```scala
trait Kind
case object Var extends Kind
case object Val extends Kind
val x = Set(if condition then Val else Var)
```

Here, the inferred type of `x` is `Set[Kind & Product & Serializable]` whereas one would have hoped it to be `Set[Kind]`. The reasoning for this particular type to be inferred is as follows:

- The type of the conditional above is the [union type](../new-types/union-types.md) `Val | Var`. This union type is treated as "soft", which means it was not explicitly written in the source program, but came from forming an upper bound of the types of
some alternatives.
- A soft union type is widened in type inference to the least product of class or trait types that is a supertype of the union type.
  In the example, this type is `Kind & Product & Serializable` since all three traits are super-traits of both `Val` and `Var`.
  So that type becomes the inferred element type of the set.

Scala 3 allows one to mark a trait or class as `transparent`, which means that it can be suppressed in type inference. Here's an example that follows the lines of the code above, but now with a new transparent trait `S` instead of `Product`:

```scala
transparent trait S
trait Kind
object Var extends Kind, S
object Val extends Kind, S
val x = Set(if condition then Val else Var)
```

Now `x` has inferred type `Set[Kind]`. The common transparent trait `S` does not
appear in the inferred type.

In the previous example, one could also declare `Kind` as `transparent`:
```scala
transparent trait Kind
```
The widened union type of `if condition then Val else Var` would then
_only_ contain the transparent traits `Kind` and `S`. In this case,
the widening is not performed at all, so `x` would have type `Set[Val | Var]`.

The root classes and traits `Any`, `AnyVal`, `Object`, and `Matchable` are
considered to be transparent. This means that an expression such
as
```scala
if condition then 1 else "hello"
```
will have type `Int | String` instead of the widened type `Any`.



## Which Traits and Classes Are Transparent?

Traits and classes are declared transparent by adding the modifier `transparent`. Scala 2 traits and classes can also be declared transparent by adding a [`@transparentTrait`](https://scala-lang.org/api/3.x/scala/annotation/transparentTrait.html) annotation. This annotation is defined in [`scala.annotation`](https://scala-lang.org/api/3.x/scala/annotation.html). It has been deprecated since 3.8.0 and it has no effect on the type inference.

The following classes and traits are automatically treated as transparent:
```scala
    scala.Any
    scala.AnyVal
    scala.Matchable
    scala.Product
    java.lang.Object
    java.lang.Comparable
    java.io.Serializable
```

Typically, transparent types other than the root classes are traits
that influence the implementation of inheriting classes and traits that are not usually used as types by themselves. Two examples from the standard collection library are:

- [`IterableOps`](https://scala-lang.org/api/3.x/scala/collection/IterableOps.html), which provides method implementations for an [`Iterable`](https://scala-lang.org/api/3.x/scala/collection/Iterable.html).
- [`StrictOptimizedSeqOps`](https://scala-lang.org/api/3.x/scala/collection/StrictOptimizedSeqOps.html), which optimises some of these implementations for sequences with efficient indexing.

Generally, any trait that is extended recursively is a good candidate to be
declared transparent.

## Rules for Inference

Transparent traits and classes can be given as explicit types as usual. But they are often elided when types are inferred. Roughly, the rules for type inference imply the following.

 - Transparent traits are dropped from intersections where possible.
 - Union types are not widened if widening would result in only transparent supertypes.

The precise rules are as follows:

- When inferring a type of a type variable, or the type of a val, or the return type of a def,
- where that type is not higher-kinded,
- and where `B` is its known upper bound or `Any` if none exists:
- If the type inferred so far is of the form `T1 & ... & Tn` where
  `n >= 1`, replace the maximal number of transparent traits `Ti`s  by `Any`, while ensuring that
  the resulting type is still a subtype of the bound `B`.
- However, do not perform this widening if all types `Ti` can get replaced in that way. This clause ensures that a single transparent trait instance such as [`Product`](https://scala-lang.org/api/3.x/scala/Product.html) is not widened to [`Any`](https://scala-lang.org/api/3.x/scala/Any.html). Transparent trait instances are only dropped when they appear in conjunction with some other type.

- If the original type was a is union type that got widened in a previous step to a product consisting only of transparent traits and classes, keep the original union type instead of its widened form.