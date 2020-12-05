---
layout: doc-page
title: "Polymorphic Function Types"
---

A polymorphic function type is a function type which accepts type parameters.
For example:
```scala
// A polymorphic method:
def foo[A](xs: List[A]): List[A] = xs.reverse

// A polymorphic function value:
val bar: [A] => List[A] => List[A]
//       ^^^^^^^^^^^^^^^^^^^^^^^^^
//       a polymorphic function type
       = [A] => (xs: List[A]) => foo[A](xs)
```
Scala already has _polymorphic methods_, i.e. methods which accepts type parameters.
Method `foo` above is an example, accepting a type parameter `A`.
So far, it
was not possible to turn such methods into polymorphic function values like `bar` above,
which can be passed as parameters to other functions, or returned as results.

In Dotty this is now possible. The type of the `bar` value above is

```scala
[A] => List[A] => List[A]
```

This type describes function values which take a type `A` as a parameter,
then take a list of type `List[A]`, and return a list of the same type `List[A]`.

[More details](https://github.com/lampepfl/dotty/pull/4672)

### Relationship With Type Lambdas

Polymorphic function types are not to be confused with
[_type lambdas_](new-types/type-lambdas.md).
While the former describes the _type_ of a polymorphic _value_,
the latter is an actual function value _at the type level_.

A good way of understanding the difference is to notice that
**_type lambdas are applied in types,
whereas polymorphic functions are applied in terms_**:
One would call the function `bar` above
by passing it a type argument `bar[Int]` _within a method body_.
On the other hand, given a type lambda such as `type F = [A] =>> List[A]`,
one would call `F` _withing a type expression_, as in `type Bar = F[Int]`.
