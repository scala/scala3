---
layout: doc-page
title: "Explicit Nulls"
---

The explicit nulls feature (enabled via a flag) changes the Scala type hierarchy
so that reference types (e.g. `String`) are non-nullable. We can still express nullability
with union types: e.g. `val x: String|Null = null`.

The implementation of the feature in dotty can be conceptually divided in several parts:
  1. changes to the type hierarchy so that `Null` is only a subtype of `Any`
  2. a "translation layer" for Java interop that exposes the nullability in Java APIs
  3. a "magic" `JavaNull` type (an alias for `Null`) that is recognized by the compiler and
     allows unsound member selections (trading soundness for usability)

## Feature Flag

Explicit nulls are disabled by default. They can be enabled via `-Yexplicit-nulls` defined in
`ScalaSettings.scala`. All of the explicit-nulls-related changes should be gated behind the flag.

## Type Hierarchy

We change the type hierarchy so that `Null` is only a subtype of `Any` by:
  - modifying the notion of what is a nullable class (`isNullableClass`) in `SymDenotations`
    to include _only_ `Null` and `Any`
  - changing the parent of `Null` in `Definitions` to point to `Any` and not `AnyRef`
  - changing `isBottomType` and `isBottomClass` in `Definitions`

## Java Interop

TODO(abeln): add support for recognizing nullability annotations a la
https://kotlinlang.org/docs/reference/java-interop.html#nullability-annotations

The problem we're trying to solve here is: if we see a Java method `String foo(String)`,
what should that method look like to Scala?
  - since we should be able to pass `null` into Java methods, the argument type should be `String|JavaNull`
  - since Java methods might return `null`, the return type should be `String|JavaNull`

`JavaNull` here is a type alias for `Null` with "magic" properties (see below).

At a high-level:
  - we track the loading of Java fields and methods as they're loaded by the compiler
  - we do this in two places: `Namer` (for Java sources) and `ClassFileParser` (for bytecode)
  - whenever we load a Java member, we "nullify" its argument and return types

The nullification logic lives in `JavaNullInterop.scala`, a new file.

The entry point is the function `def nullifyMember(sym: Symbol, tp: Type)(implicit ctx: Context): Type`
which, given a symbol and its "regular" type, produces what the type of the symbol should be in the
explicit nulls world.

In order to nullify a member, we first pass it through a "whitelist" of symbols that need
special handling (e.g. `constructors`, which never return `null`). If none of the "policies" in the
whitelist apply, we then process the symbol with a `TypeMap` that implements the following nullification
function `n`:
  1. n(T)              = T|JavaNull              if T is a reference type
  2. n(T)              = T                       if T is a value type
  3. n(T)              = T|JavaNull              if T is a type parameter
  4. n(C[T])           = C[T]|JavaNull           if C is Java-defined
  5. n(C[T])           = C[n(T)]|JavaNull        if C is Scala-defined
  6. n(A|B)            = n(A)|n(B)|JavaNull
  7. n(A&B)            = (n(A)&n(B))|JavaNull
  8. n((A1, ..., Am)R) = (n(A1), ..., n(Am))n(R) for a method with arguments (A1, ..., Am) and return type R
  9. n(T)              = T                       otherwise

## JavaNull

`JavaNull` is just an alias for `Null`, but with magic power. `JavaNull`'s magic (anti-)power is that
it's unsound.

```scala
val s: String|JavaNull = "hello"
s.length // allowed, but might throw NPE
```

`JavaNull` is defined as `JavaNullAlias` in `Definitions`.
The logic to allow member selections is defined in `findMember` in `Types.scala`:
  - if we're finding a member in a type union
  - and the union contains `JavaNull` on the r.h.s. after normalization (see below)
  - then we can continue with `findMember` on the l.h.s of the union (as opposed to failing)

## Working with Nullable Unions

Within `Types.scala`, we defined a few utility methods to work with nullable unions. All of these
are methods of the `Type` class, so call them with `this` as a receiver:
  - `isNullableUnion` determines whether `this` is a nullable union. Here, what constitutes
     a nullable union is determined purely syntactically:
       1. first we "normalize" `this` (see below)
       2. if the result is of the form `T | Null`, then the type is considered a nullable union.
          Otherwise, it isn't.
  - `isJavaNullableUnion` determines whether `this` is syntactically a union of the form `T|JavaNull`
  - `normNullableUnion` normalizes `this` as follows:
      1. if `this` is not a nullable union, it's returned unchanged.
      2. if `this` is a union, then it's re-arranged so that all the `Null`s are to the right of all
         the non-`Null`s.
  - `stripNull` syntactically strips nullability from `this`: e.g. `String|Null => String`. Notice this
     works only at the "top level": e.g. if we have an `Array[String|Null]|Null` and we call `stripNull`
     we'll get `Array[String|Null]` (only the outermost nullable union was removed).
  - `stripAllJavaNull` is like `stripNull` but removes _all_ nullable unions in the type (and only works
     for `JavaNull`). This is needed when we want to "revert" the Java nullification function.

## Flow Typing

TODO
