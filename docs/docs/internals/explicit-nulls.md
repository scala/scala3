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
  3. a "magic" `UncheckedNull` type (an alias for `Null`) that is recognized by the compiler and
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

The problem we're trying to solve here is: if we see a Java method `String foo(String)`,
what should that method look like to Scala?
  - since we should be able to pass `null` into Java methods, the argument type should be `String|UncheckedNull`
  - since Java methods might return `null`, the return type should be `String|UncheckedNull`

`UncheckedNull` here is a type alias for `Null` with "magic" properties (see below).

At a high-level:
  - we track the loading of Java fields and methods as they're loaded by the compiler
  - we do this in two places: `Namer` (for Java sources) and `ClassFileParser` (for bytecode)
  - whenever we load a Java member, we "nullify" its argument and return types

The nullification logic lives in `compiler/src/dotty/tools/dotc/core/JavaNullInterop.scala`.

The entry point is the function
`def nullifyMember(sym: Symbol, tp: Type, isEnumValueDef: Boolean)(implicit ctx: Context): Type`
which, given a symbol, its "regular" type, and a boolean whether it is a Enum value definition,
produces what the type of the symbol should be in the explicit nulls world.

1. If the symbol is a Enum value definition or a `TYPE_` field, we don't nullify the type
2. If it is `toString()` method or the constructor, or it has a `@NotNull` annotation,
  we nullify the type, without a `UncheckedNull` at the outmost level.
3. Otherwise, we nullify the type in regular way.

See `JavaNullMap` in `JavaNullInterop.scala` for more details about how we nullify different types.

## UncheckedNull

`UncheckedNull` is just an alias for `Null`, but with magic power. `UncheckedNull`'s magic (anti-)power is that
it's unsound.

```scala
val s: String|UncheckedNull = "hello"
s.length // allowed, but might throw NPE
```

`UncheckedNull` is defined as `UncheckedNullAlias` in `Definitions.scala`.
The logic to allow member selections is defined in `findMember` in `Types.scala`:
  - if we're finding a member in a type union
  - and the union contains `UncheckedNull` on the r.h.s. after normalization (see below)
  - then we can continue with `findMember` on the l.h.s of the union (as opposed to failing)

## Working with Nullable Unions

Within `Types.scala`, we defined some extractors to work with nullable unions:
`OrNull` and `OrUncheckedNull`.

```scala
(tp: Type) match {
  case OrNull(tp1) => // if tp is a nullable union: tp1 | Null
  case _ => // otherwise
}
```

These extractor will call utility methods in `NullOpsDecorator.scala`. All of these
are methods of the `Type` class, so call them with `this` as a receiver:

- `stripNull` syntactically strips all `Null` types in the union:
  e.g. `String|Null => String`.
- `stripUncheckedNull` is like `stripNull` but only removes `UncheckedNull` from the union.
  This is needed when we want to "revert" the Java nullification function.
- `stripAllUncheckedNull` collapses all `UncheckedNull` unions within this type, and not just the outermost
  ones (as `stripUncheckedNull` does).
- `isNullableUnion` determines whether `this` is a nullable union.
- `isUncheckedNullableUnion` determines whether `this` is syntactically a union of the form
  `T|UncheckedNull`.

## Flow Typing

As typing happens, we accumulate a set of `NotNullInfo`s in the `Context` (see
`Contexts.scala`). A `NotNullInfo` contains the set of `TermRef`s that are known to
be non-null at the current program point.  See `Nullables.scala` for how `NotNullInfo`s
are computed.

During type-checking, when we type an identity or a select tree (in `typedIdent` and
`typedSelect`), we will call `toNotNullTermRef` on the tree before return the typed tree.
If the tree `x` has nullable type `T|Null` and it is known to be not null according to
the `NotNullInfo` and it is not on the lhs of assignment, then we cast it to `x.type & T`
using `defn.Any_typeCast`.

The reason for casting to `x.type & T`, as opposed to just `T`, is that it allows us to
support flow typing for paths of length greater than one.

```scala
abstract class Node {
  val x: String
  val next: Node | Null
}

def f = {
  val l: Node|Null = ???
  if (l != null && l.next != null) {
    val third: l.next.next.type = l.next.next
  }
}
```

After typing, `f` becomes:

```scala
def f = {
  val l: Node|Null = ???
  if (l != null && l.$asInstanceOf$[l.type & Node].next != null) {
    val third:
      l.$asInstanceOf$[l.type & Node].next.$asInstanceOf$[(l.type & Node).next.type & Node].next.type =
      l.$asInstanceOf$[l.type & Node].next.$asInstanceOf$[(l.type & Node).next.type & Node].next
  }
}
```
Notice that in the example above `(l.type & Node).next.type & Node` is still a stable path, so
we can use it in the type and track it for flow typing.
