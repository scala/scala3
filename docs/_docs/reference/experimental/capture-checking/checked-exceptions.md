---
layout: doc-page
title: "Checked Exceptions"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/capture-checking/checked-exceptions.html
---

## Introduction

Scala enables checked exceptions through a language import. Here is an example,
taken from the [safer exceptions page](../canthrow.md), and also described in a
[paper](https://infoscience.epfl.ch/record/290885) presented at the
 2021 Scala Symposium.
```scala sc:nocompile
import language.experimental.saferExceptions

class LimitExceeded extends Exception

val limit = 10e+10
def f(x: Double): Double throws LimitExceeded =
  if x < limit then x * x else throw LimitExceeded()
```
The new `throws` clause expands into an implicit parameter that provides
a `CanThrow` capability. Hence, function `f` could equivalently be written
like this:
```scala sc:nocompile
def f(x: Double)(using CanThrow[LimitExceeded]): Double = ...
```
If the implicit parameter is missing, an error is reported. For instance, the  function definition
```scala sc:nocompile
def g(x: Double): Double =
  if x < limit then x * x else throw LimitExceeded()
```
is rejected with this error message:
```
  |  if x < limit then x * x else throw LimitExceeded()
  |                               ^^^^^^^^^^^^^^^^^^^^^
  |The capability to throw exception LimitExceeded is missing.
  |The capability can be provided by one of the following:
  | - Adding a using clause `(using CanThrow[LimitExceeded])` to the definition of the enclosing method
  | - Adding `throws LimitExceeded` clause after the result type of the enclosing method
  | - Wrapping this piece of code with a `try` block that catches LimitExceeded
```
`CanThrow` capabilities are required by `throw` expressions and are created
by `try` expressions. For instance, the expression
```scala sc:nocompile
try xs.map(f).sum
catch case ex: LimitExceeded => -1
```
would be expanded by the compiler to something like the following:
```scala sc:nocompile
try
  erased given ctl: CanThrow[LimitExceeded] = compiletime.erasedValue
  xs.map(f).sum
catch case ex: LimitExceeded => -1
```
(The `ctl` capability is only used for type checking but need not show up in the generated code, so it can be declared as
erased.)

As with other capability based schemes, one needs to guard against capabilities
that are captured in results. For instance, here is a problematic use case:
```scala sc:nocompile
def escaped(xs: Double*): (() => Double) throws LimitExceeded =
  try () => xs.map(f).sum
  catch case ex: LimitExceeded => () => -1
val crasher = escaped(1, 2, 10e+11)
crasher()
```
This code needs to be rejected since otherwise the call to `crasher()` would cause
an unhandled `LimitExceeded` exception to be thrown.

Under the language import `language.experimental.captureChecking`, the code is indeed rejected

<!--
```
14 |  try () => xs.map(f).sum
   |  ^
   |The expression's type () => Double is not allowed to capture the root capability `any`.
   |This usually means that a capability persists longer than its allowed lifetime.
15 |  catch case ex: LimitExceeded => () => -1
```

-->

To integrate exception and capture checking, only two changes are needed:

 - `CanThrow` is declared as a class extending `Control`, so all references to `CanThrow` instances are tracked.
 - Escape checking is extended to `try` expressions. The result type of a `try` is not allowed to
   capture capabilities defined in the body of the `try`.
