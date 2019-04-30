---
layout: doc-page
title: "Context Bounds"
---

## Context Bounds

A context bound is a shorthand for expressing a common pattern of an implicit parameter that depends on a type parameter. Using a context bound, the `maximum` function of the last section can be written like this:
```scala
def maximum[T: Ord](xs: List[T]): T = xs.reduceLeft(max)
```
A bound like `: Ord` on a type parameter `T` of a method or class is equivalent to a given clause `given Ord[T]`. The implicit parameter(s) generated from context bounds come last in the definition of the containing method or class. E.g.,
```scala
def f[T: C1 : C2, U: C3](x: T) given (y: U, z: V): R
```
would expand to
```scala
def f[T, U](x: T) given (y: U, z: V) given C1[T], C2[T], C3[U]: R
```
Context bounds can be combined with subtype bounds. If both are present, subtype bounds come first, e.g.
```scala
def g[T <: B : C](x: T): R = ...
```

## Syntax

```
TypeParamBounds   ::=  [SubtypeBounds] {ContextBound}
ContextBound      ::=  ‘:’ Type
```
