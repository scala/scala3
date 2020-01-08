---
layout: doc-page
title: "Witnesses"
---

Witnesses define "canonical" values of certain types
that serve for synthesizing arguments to [implicit parameters](./given-clauses.md). Example:

```scala
trait Ord[T] {
  def compare(x: T, y: T): Int
  def (x: T) < (y: T) = compare(x, y) < 0
  def (x: T) > (y: T) = compare(x, y) > 0
}

witness intOrd of Ord[Int] with
  def compare(x: Int, y: Int) =
    if (x < y) -1 else if (x > y) +1 else 0

witness listOrd[T](given ord: Ord[T]) of Ord[List[T]] with

  def compare(xs: List[T], ys: List[T]): Int = (xs, ys) match
    case (Nil, Nil) => 0
    case (Nil, _) => -1
    case (_, Nil) => +1
    case (x :: xs1, y :: ys1) =>
      val fst = ord.compare(x, y)
      if (fst != 0) fst else compare(xs1, ys1)
```
This code defines a trait `Ord` with two witnesses. `intOrd` defines
a witness of the type `Ord[Int]` whereas `listOrd[T]` defines witnesses
of `Ord[List[T]]` for all types `T` that come with a witness of `Ord[T]`
themselves. The `(given ord: Ord[T])` clause in `listOrd` defines a condition: There must be a
witness of type `Ord[T]` so that a witness of type `List[Ord[T]]` can
be synthesized. Such conditions are expanded by the compiler to implicit
parameters, which are explained in the [next section](./given-clauses.md).

## Anonymous Witnesses

The name of a witness can be left out. So the definitions
of the last section can also be expressed like this:
```scala
witness of Ord[Int] { ... }
witness [T](given Ord[T]) of Ord[List[T]] { ... }
```
If the name of a witness is missing, the compiler will synthesize a name from
the implemented type(s).

## Alias Witnesses

An alias can be used to define a witness that is equal to some expression. E.g.:
```scala
witness global of ExecutionContext = new ForkJoinPool()
```
This creates a witness `global` of type `ExecutionContext` that resolves to the right
hand side `new ForkJoinPool()`.
The first time `global` is accessed, a new `ForkJoinPool` is created, which is then
returned for this and all subsequent accesses to `global`.

Alias witnesses can be anonymous, e.g.
```scala
witness of Position = enclosingTree.position
witness (given outer: Context) of Context = outer.withOwner(currentOwner)
```
An alias witness can have type parameters and implicit parameters just like any other witness,
but it can only implement a single type.

## Witness Initialization

A witness without type or implicit parameters is initialized on-demand, the first
time it is accessed. If a witness has type or implicit parameters, a fresh instance
is created for each reference.

## Syntax

Here is the new syntax for witnesses, seen as a delta from the [standard context free syntax of Scala 3](../../internals/syntax.md).
```
TmplDef          ::=  ...
                  |   ‘witness’ WitnessDef
WitnessDef       ::=  WitnessSig ‘of’ [‘_’ ‘<:’] Type ‘=’ Expr
                  |   WitnessSig ‘of’ [ConstrApp {‘,’ ConstrApp }] [TemplateBody]
WitnessSig       ::=  [id] [DefTypeParamClause] {GivenParamClause}
GivenParamClause ::=  ‘(’ ‘given’ (DefParams | GivenTypes) ‘)’
GivenTypes       ::=  Type {‘,’ Type}
```
The identifier `id` can be omitted only if some types are implemented or the template body defines at least one extension method.
