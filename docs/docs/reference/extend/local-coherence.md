---
layout: doc-page
title: "Local Coherence"
---

In the context of instance declarations, global coherence means that for every type `I` and typeclass trait `TC` there is never more than one way in which `I` implements `TC`.
Scala does not have a global coherence requirement for its implicits. Instead it reports an ambiguity error if an implicit search yields several different results. The advantage of Scala's approach is its flexibility, in particular its support scoped implicits. But sometimes the ambiguity rules get in the way. Consider for instance the typeclass trait hierarchy presented in the last section, which involves `Functor`, `Applicative`, `Monad`, and `Traverse`. Now add a function like `transform` below that requires its parameter type
to be both a `Monad` and a `Traverse`.
```scala
def transform[F[_] : Monad : Traverse](x: F[A], f: A => A) = { ... x.map(f) ... }
```
The call to `map` in the body of that function would be in error, because it is ambiguous - we don't know whether we should call the `map` method defined in `Monad` or
the one defined in `Traverse`. To see this in more detail, expand the context bounds in `transform` and add a type ascription `x`:

```scala
def transform[F[_], A](x: F[A], f: A => A)(
  implicit
    impl1: Monad.Impl[F],
    impl2: Traversable.Impl[F])
  = { ... (x: Functor[A]).map(f) ... }
```
There are two ways `x` can be converted to a `Functor[A]`, one through `impl1.inject`
the other through `impl2.inject`. Hence, an ambiguity error is reported.
This problem was brought up in issue #2029 and was presented
in a [paper at the Scala Symposium 2017](https://adelbertc.github.io/publications/typeclasses-scala17.pdf).

Note that if we assume `List` as the actual implementation, this ambiguity is a false negative, since `List`'s implementation of `map` is the same for `Monad` and `Traverse`,
being defined in the common extension `Applicative`. But in general, a type might well have two different implementations for `map` (or `Functor` methods in general) when seen as
a `Monad` or as a `Traverse`.

The (as yes tentative) idea to solve the problem is to allow a way to constrain implicit values to have common implementations for some of their super-traits.

This can be done by introducing a predefined type constructor `Super[_]` as a member of all implementation objects of typeclass traits. If `impl` is an implementation object for type `I` and typeclass trait `T`, and `U` is a typeclass trait extended by `T`, then `impl.Super[U]` would give the type of the implementation object from which `impl` obtains all
implementations of `U`. For instance, assuming the factored instance declarations in the last section,
```scala
     ListMonad.Super[Functor]  =  ListApplicative.type
  ListTraverse.Super[Functor]  =  ListApplicative.type
```
Indeed, all `Functor` methods defined by `List` are defined in `ListApplicative`, and inherited (i.e. implemented implicitly as forwarders) in both `ListMonad` and `ListTraverse`.
On the other hand, if we had not used factorization for `ListMonad`,
`ListMonad.Super[Functor]` would be `ListMonad.type`, since it would be `ListMonad` that
defines some of the methods in `Functor`.

Once we have the `Super` type defined like this, we can add typeclass constraints to enforce equalities. For instance:

```scala
def transform[F[_], A](x: F[A], f: A => A)(
  implicit
    impl1: Monad.Impl[F],
    impl2: Traversable.Impl[F],
    impl1.Super[Functor] =:= impl2.Super[Functor])
  = { ... (x: Functor[A]).map(f) ... }
```
Note that this construct involves parameter dependencies as implemented in #2079 - the third implicit parameter contains types that depend on the first two.

The final piece of the puzzle is to make ambiguity checking take such constraints into account. More precisely, if searching for a injection from a type `T` to a typeclass
`U`, if there are two injectors `i1` and `i2` from `T` to `U`, but `i1.Super[U] =:= i2.Super[U]`, make an arbitrary choice instead of signaling an ambiguity. With this rule, the
`x.map(f)` call in the `transform` method typechecks.

In effect we have replaced global coherence by local coherence - the ability to detect and require that two implementations implement a typeclass trait in the same way.