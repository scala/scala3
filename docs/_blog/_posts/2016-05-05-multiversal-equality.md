---
layout: blog-page
title: Multiversal Equality for Scala
author: Martin Odersky
authorImg: images/martin.jpg
date: 2016-05-05
---

I have been working recently on making equality tests using `==` and `!=` safer in Scala. This has led to a [Language Enhancement Proposal](https://github.com/lampepfl/dotty/issues/1247) which I summarize in this blog.

## Why Change Equality?

Scala prides itself of its strong static type system. Its type discipline is particularly useful when it comes to refactoring. Indeed, it's possible to write programs in such a way that refactoring problems show up with very high probability as type errors. This is essential for being able to refactor with the confidence that nothing will break. And the ability to do such refactorings is in turn very important for keeping code bases from rotting.

Of course, getting such a robust code base requires the cooperation of the developers. They should avoid type `Any`, casts, [stringly typed](http://c2.com/cgi/wiki?StringlyTyped) logic, and more generally any operation over loose types that do not capture the important properties of a value. Unfortunately, there is one area in Scala where such loose types are very hard to avoid: That's equality. Comparisons with `==` and `!=` are _universal_. They compare any two values, no matter what their types are. This causes real problems for writing code and more problems for refactoring it.

For instance, one might want to introduce a proxy for some data structure so that instead of accessing the data structure directly one goes through the proxy. The proxy and the underlying data would have different types. Normally this should be an easy refactoring. If one passes by accident a proxy for the underlying type or _vice versa_ the type checker will flag the error. However, if one accidentally compares a proxy with the underlying type using `==` or a pattern match, the program is still valid, but will just always say `false`.  This is a real worry in practice. I recently abandoned a desirable extensive refactoring because I feared that it would be too hard to track down such errors.

## Where Are We Today?

The problems of universal equality in Scala are of course well-known. Some libraries have tried to fix it by adding another equality operator with more restricted typing. Most often this safer equality is written `===`. While `===` is certainly useful, I am not a fan of adding another equality operator to the language and core libraries. It would be much better if we could fix `==` instead. This would be both simpler and would catch all potential equality problems including those related to pattern matching.

How can `==` be fixed? It looks much harder to do this than adding an alternate equality operator. First, we have to keep backwards compatibility. The ability to compare everything to everything is by now baked into lots of code and libraries.  Second, with just one equality operator we need to make this operator work in all cases where it makes sense. An alternative `===` operator can choose to refuse some comparisons that should be valid because there's always `==` to fall back to. With a unique `==` operator we do not have this luxury.

The current status in Scala is that the compiler will give warnings for _some_ comparisons that are always `false`. But the coverage is weak. For instance this will give a warning:

```scala
scala> 1 == "abc"
<console>:12: warning: comparing values of types Int and String using `==' will always yield false
```

But this will not:

```scala
scala> "abc" == 1
res2: Boolean = false
```

There are also cases where a warning is given for a valid equality test that actually makes sense because the result could be `true`. In summary, the current checking catches some obvious bugs, which is nice. But it is far too weak and fickle to be an effective refactoring aid.


## What's Proposed?

I believe to do better, we need to enlist the cooperation of developers. Ultimately it's the developer who provides implementations of equality methods and who is therefore best placed to characterize which equalities make sense. Sometimes this characterization can be involved. For instance, an `Int` can be compared to other primitive numeric values or to instances of type `java.lang.Number` but any other comparison will always yield `false`. Or, it makes sense to compare two `Option` values if and only if it makes sense to compare the optional element values.

The best known way to characterize such relationships is with type classes. Implicit values of a trait `Eq[T, U]` can capture the property that values of type `T` can be compared to values of type `U`. Here's the definition of `Eq`

```scala
package scala

trait Eq[-T, -U]
```

That is, `Eq` is a pure marker trait with two type parameters and without any members.  Developers can define equality classes by giving implicit `Eq` instances. Here is a simple one:

```scala
implicit def eqString: Eq[String, String] = Eq
```

This states that strings can be only compared to strings, not to values of other types.  Here's a more complicated `Eq` instance:

```scala
implicit def eqOption[T, U](implicit _eq: Eq[T, U]): Eq[Option[T], Option[U]] = Eq
```

This states that `Option` values can be compared if their elements can be compared.

It's foreseen that such `Eq` instances can be generated automatically. If we add an annotation `@equalityClass` to `Option` like this

```scala
@equalityClass class Option[+T] { ... }
```

then the `eqOption` definition above would be generated automatically in `Option`'s companion object.

Given a set of `Eq` instances, the idea is that the Scala compiler will check every time it encounters a _potentially problematic_ comparison between values of types `T` and `U` that there is an implicit instance of `Eq[T, U]`. A comparison is _potentially problematic_ if it is between incompatible types. As long as `T <: U` or `U <: T` the equality could make sense because both sides can potentially be the same value.

So this means we still keep universal equality as it is in Scala now - we don't have a choice here anyway, because of backwards compatibility. But we render it safe by checking that for each comparison the corresponding `Eq` instance exists.

What about types for which no `Eq` instance exists? To maintain backwards compatibility, we allow comparisons of such types as well, by means of a fall-back `eqAny` instance. But we do not allow comparisons between types that have an `Eq` instance and types that have none.  Details are explained in the [proposal](https://github.com/lampepfl/dotty/issues/1247).

## Properties

Here are some nice properties of the proposal

1. It is _opt-in_. To get safe checking, developers have to annotate with `@equalityClass` classes that should allow comparisons only between their instances, or they have to define implicit `Eq` instances by hand.  2. It is backwards compatible. Without developer-provided `Eq` instances, equality works as before.
3. It carries no run-time cost compared to universal equality. Indeed the run-time behavior of equality is not affected at all.
4. It has no problems with parametricity, variance, or bottom types.  5. Depending on the actual `Eq` instances given, it can be very precise. That is, no comparisons that might yield `true` need to be rejected, and most comparisons that will always yield `false` are in fact rejected.

The scheme effectively leads to a partition of the former universe of types into sets of types. Values with types in the same partition can be compared among themselves but values with types in different partitions cannot.  An `@equalityClass` annotation on a type creates a new partition. All types that do not have any `Eq` instances (except `eqAny`, that is) form together another partition.  So instead of a single _universe_ of values that can be compared to each other we get a _multiverse_ of partitions. Hence the name of the proposal: **Multiversal Equality**.
