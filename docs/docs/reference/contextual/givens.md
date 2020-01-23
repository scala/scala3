---
layout: doc-page
title: "Given Instances"
---

Given instances (or, simply, "givens") define "canonical" values of certain types
that serve for synthesizing arguments to [context parameters](./context-parameters.md). Example:

```scala
trait Ord[T] {
  def compare(x: T, y: T): Int
  def (x: T) < (y: T) = compare(x, y) < 0
  def (x: T) > (y: T) = compare(x, y) > 0
}

given intOrd as Ord[Int] {
  def compare(x: Int, y: Int) =
    if (x < y) -1 else if (x > y) +1 else 0
}

given listOrd[T] with (ord: Ord[T]) as Ord[List[T]] {

  def compare(xs: List[T], ys: List[T]): Int = (xs, ys) match
    case (Nil, Nil) => 0
    case (Nil, _) => -1
    case (_, Nil) => +1
    case (x :: xs1, y :: ys1) =>
      val fst = ord.compare(x, y)
      if (fst != 0) fst else compare(xs1, ys1)
}
```
This code defines a trait `Ord` with two given declarations. `intOrd` defines
a given for the type `Ord[Int]` whereas `listOrd[T]` defines givens
for `Ord[List[T]]` for all types `T` that come with a given for `Ord[T]`
themselves. The `with` clause in `listOrd` defines a condition: There must be a
given of type `Ord[T]` for a given of type `List[Ord[T]]` to exist.
Such conditions are expanded by the compiler to context
parameters, which are explained in the [next section](./context-parameters.md).

## Anonymous Givens

The name of a given can be left out. So the definitions
of the last section can also be expressed like this:
```scala
given Ord[Int] { ... }
given [T] with Ord[T] as Ord[List[T]] { ... }
```
If the name of a given is missing, the compiler will synthesize a name from
the implemented type(s).

## Alias Givens

An alias can be used to define a given that is equal to some expression. E.g.:
```scala
given global as ExecutionContext = new ForkJoinPool()
```
This creates a given `global` of type `ExecutionContext` that resolves to the right
hand side `new ForkJoinPool()`.
The first time `global` is accessed, a new `ForkJoinPool` is created, which is then
returned for this and all subsequent accesses to `global`.

Alias givens can be anonymous, e.g.
```scala
given as Position = enclosingTree.position
given with (outer: Context) as Context = outer.withOwner(currentOwner)
```
An alias given can have type parameters and implicit parameters just like any other given,
but it can only implement a single type.

## Given Whitebox Macro Instances

An `inline` alias given can be marked as a whitebox macro by writing
`_ <:` in front of the implemented type. Example:
```scala
inline given mkAnnotations[A, T] as _ <: Annotations[A, T] = ${
  // code producing a value of a subtype of Annotations
}
```
The type of an application of `mkAnnotations` is the type of its right hand side,
which can be a proper subtype of the declared result type `Annotations[A, T]`.

## Given Instance Initialization

A given without type or context parameters is initialized on-demand, the first
time it is accessed. If a given has type or context parameters, a fresh instance
is created for each reference.

## Syntax

Here is the new syntax for givens, seen as a delta from the [standard context free syntax of Scala 3](../../internals/syntax.md).

```
TmplDef           ::=  ...
                   |   ‘given’ GivenDef
GivenDef          ::=  [GivenSig] [‘_’ ‘<:’] Type ‘=’ Expr
                   |   [GivenSig] ConstrApp {‘,’ ConstrApp } [TemplateBody]
GivenSig          ::=  [id] [DefTypeParamClause] {WithParamsOrTypes} ‘as’
WithParamsOrTypes ::=  WithParamClause | AnnotTypes
```
