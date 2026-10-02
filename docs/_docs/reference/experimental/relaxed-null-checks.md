---
layout: doc-page
title: "Relaxed Null Checks under Strict Equality"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/relaxed-null-checks.html
---

Relaxed null checks are an experimental extension of [strict equality](../contextual/multiversal-equality.md)
proposed in [SIP-79](https://github.com/scala/improvement-proposals/pull/133). They are enabled with

```scala
import scala.language.experimental.relaxedNullChecks
```

or with the command line option `-language:experimental.relaxedNullChecks`.

## Motivation

With [explicit nulls](./explicit-nulls.md), nullable types are written as unions with `Null`, and a comparison
with `null` narrows the type of a stable reference. Under `strictEquality`, such comparisons are rejected for
nullable value types and type parameters, because there is no `CanEqual` instance for them:

```scala
//> using options -Yexplicit-nulls -language:strictEquality
def f(x: Int | Null): Int =
  if x != null then x // error: Values of types Int | Null and Null cannot be compared with == or !=
  else 0

def g(x: Int | Null): Int =
  x match
    case null => 0 // error: Values of types Null and Int | Null cannot be compared with == or !=
    case y => y
```

Using `eq` instead is not an option either, since it is not available on value types.

This is especially problematic because explicit nulls enable flow typing: when a not-null check for a
variable of type `X | Null` has been performed, it can be used in places where type `X` is required.
But when strict equality is enabled, it is not currently possible to perform this not-null check for
lack of a `CanEqual` instance.

## Rules

When `relaxedNullChecks` is enabled together with `strictEquality`, no `CanEqual` instance is required

 - for `a == b` and `a != b` if one of the operands is the literal `null` and the type of the other operand is a supertype of `Null`,
 - for a `case null` pattern if the type of the scrutinee is a supertype of `Null`.

With this, the examples above compile, and the type of `x` is narrowed to `Int` in the non-null branches.

The relaxation only applies to the literal `null`. Other expressions of type `Null` still require a `CanEqual` instance:

```scala
def h(x: Int | Null, n: Null) =
  x == null // ok
  x == n    // error: Values of types Int | Null and Null cannot be compared with == or !=
```

Comparisons with `null` remain rejected if the other operand's type is not a supertype of `Null`:

```scala
def k[A](i: Int, a: A) =
  i == null // error
  a == null // error
```
