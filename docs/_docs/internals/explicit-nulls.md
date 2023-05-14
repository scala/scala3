---
layout: doc-page
title: "Explicit Nulls"
---

The explicit nulls feature (enabled via a flag) changes the Scala type hierarchy
so that reference types (e.g. `String`) are non-nullable. We can still express nullability
with union types: e.g. `val x: String | Null = null`.

The implementation of the feature in Scala 3 can be conceptually divided in several parts:

1. changes to the type hierarchy so that `Null` is only a subtype of `Any`
2. a "translation layer" for Java interoperability that exposes the nullability in Java APIs
3. a `unsafeNulls` language feature which enables implicit unsafe conversion between `T` and `T | Null`

## Explicit-Nulls Flag

The explicit-nulls flag is currently disabled by default. It can be enabled via `-Yexplicit-nulls` defined in
`ScalaSettings.scala`. All of the explicit-nulls-related changes should be gated behind the flag.

## Type Hierarchy

We change the type hierarchy so that `Null` is only a subtype of `Any` by:

- modifying the notion of what is a nullable class (`isNullableClass`) in `SymDenotations`
  to include _only_ `Null` and `Any`, which is used by `TypeComparer`
- changing the parent of `Null` in `Definitions` to point to `Any` and not `AnyRef`
- changing `isBottomType` and `isBottomClass` in `Definitions`

## Working with Nullable Unions

There are some utility functions for nullable types in `NullOpsDecorator.scala`.
They are extension methods for `Type`; hence we can use them in this way: `tp.f(...)`.

- `stripNull` syntactically strips all `Null` types in the union:
  e.g. `T | Null => T`. This should only be used if we can guarantee `T` is a reference type.
- `isNullableUnion` determines whether `this` is a nullable union.
- `isNullableAfterErasure` determines whether `this` type can have `null` value after erasure.

Within `Types.scala`, we also defined an extractor `OrNull` to extract the non-nullable part of a nullable unions .

```scala
(tp: Type) match
  case OrNull(tp1) => // if tp is a nullable union: tp1 | Null
  case _ => // otherwise
```

## Java Interoperability

The problem we're trying to solve here is: if we see a Java method `String foo(String)`,
what should that method look like to Scala?

- since we should be able to pass `null` into Java methods, the argument type should be `String | Null`
- since Java methods might return `null`, the return type should be `String | Null`

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
  we nullify the type, without a `Null` at the outmost level.
3. Otherwise, we nullify the type in regular way.

The `@NotNull` annotations are defined in `Definitions.scala`.

See `JavaNullMap` in `JavaNullInterop.scala` for more details about how we nullify different types.

## Relaxed Overriding Check

If the explicit nulls flag is enabled, the overriding check between Scala classes and Java classes is relaxed.

The `matches` function in `Types.scala` is used to select condidated for overriding check.

The `compatibleTypes` in `RefCheck.scala` determines whether the overriding types are compatible.

## Nullified Upper Bound

Suppose we have a type bound `class C[T >: Null <: String]`, it becomes unapplicable in explicit nulls, since
we don't have a type that is a supertype of `Null` and a subtype of `String`.

Hence, when we read a type bound from Scala 2 Tasty or Scala 3 Tasty, the upper bound is nullified if the lower
bound is exactly `Null`. The example above would become `class C[T >: Null <: String | Null]`.

## Unsafe Nulls Feature and SafeNulls Mode

The `unsafeNulls` language feature is currently disabled by default. It can be enabled by importing `scala.language.unsafeNulls` or using `-language:unsafeNulls`. The feature object is defined in `library/src/scalaShadowing/language.scala`. We can use `config.Feature.enabled(nme.unsafeNulls)` to check if this feature is enabled.

We use the `SafeNulls` mode to track `unsafeNulls`. If explicit nulls is enabled without `unsafeNulls`, there is a `SafeNulls` mode in the context; when `unsafeNulls` is enabled, `SafeNulls` mode will be removed from the context.

Since we want to allow selecting member on nullable values, when searching a member of a type, the `| Null` part should be ignored. See `goOr` in `Types.scala`.

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
abstract class Node:
   val x: String
   val next: Node | Null

def f =
   val l: Node | Null = ???
   if l != null && l.next != null then
      val third: l.next.next.type = l.next.next
```

After typing, `f` becomes:

```scala
def f =
   val l: Node | Null = ???
   if l != null && l.$asInstanceOf$[l.type & Node].next != null then
      val third:
         l.$asInstanceOf$[l.type & Node].next.$asInstanceOf$[(l.type & Node).next.type & Node].next.type =
         l.$asInstanceOf$[l.type & Node].next.$asInstanceOf$[(l.type & Node).next.type & Node].next
```

Notice that in the example above `(l.type & Node).next.type & Node` is still a stable path, so
we can use it in the type and track it for flow typing.
