---
layout: doc-page
title: "Given Instances"
---

Given instances (or, simply, "givens") define "canonical" values of certain types
that serve for synthesizing arguments to [given clauses](./given-clauses.md). Example:

```scala
trait Ord[T] {
  def compare(x: T, y: T): Int
  def (x: T) < (y: T) = compare(x, y) < 0
  def (x: T) > (y: T) = compare(x, y) > 0
}

given intOrd: Ord[Int] {
  def compare(x: Int, y: Int) =
    if (x < y) -1 else if (x > y) +1 else 0
}

given listOrd[T](given ord: Ord[T]): Ord[List[T]] {

  def compare(xs: List[T], ys: List[T]): Int = (xs, ys) match {
    case (Nil, Nil) => 0
    case (Nil, _) => -1
    case (_, Nil) => +1
    case (x :: xs1, y :: ys1) =>
      val fst = ord.compare(x, y)
      if (fst != 0) fst else compare(xs1, ys1)
  }
}
```
This code defines a trait `Ord` with two given instances. `intOrd` defines
a given for the type `Ord[Int]` whereas `listOrd[T]` defines givens
for `Ord[List[T]]` for all types `T` that come with a given instance for `Ord[T]` themselves.
The `(given ord: Ord[T])` clause in `listOrd` defines an implicit parameter.
Given clauses are further explained in the [next section](./given-clauses.md).

## Anonymous Given Instances

The name of a given instance can be left out. So the definitions
of the last section can also be expressed like this:
```scala
given Ord[Int] { ... }
given [T](given Ord[T]): Ord[List[T]] { ... }
```
If the name of a given is missing, the compiler will synthesize a name from
the implemented type(s).

## Alias Givens

An alias can be used to define a given instance that is equal to some expression. E.g.:
```scala
given global: ExecutionContext = new ForkJoinPool()
```
This creates a given `global` of type `ExecutionContext` that resolves to the right
hand side `new ForkJoinPool()`.
The first time `global` is accessed, a new `ForkJoinPool` is created, which is then
returned for this and all subsequent accesses to `global`.

Alias givens can be anonymous, e.g.
```scala
given Position = enclosingTree.position
given (given outer: Context): Context = outer.withOwner(currentOwner)
```
An alias given can have type parameters and given clauses just like any other given instance, but it can only implement a single type.

## Given Instance Initialization

A given instance without type parameters or given clause is initialized on-demand, the first
time it is accessed. It is not required to ensure safe publication, which means that
different threads might create different instances for the same `given` definition.
If a `given` definition has type parameters or a given clause, a fresh instance is created for each reference.

## Syntax

Here is the new syntax of given instances, seen as a delta from the [standard context free syntax of Scala 3](../../internals/syntax.md).
```
TmplDef          ::=  ...
                  |   ‘given’ GivenDef
GivenDef         ::=  [GivenSig (‘:’ | <:)] Type ‘=’ Expr
                  |   [GivenSig ‘:’] [ConstrApp {‘,’ ConstrApp }] [TemplateBody]
GivenSig         ::=  [id] [DefTypeParamClause] {GivenParamClause}
GivenParamClause ::=  ‘(’ ‘given’ (DefParams | GivenTypes) ‘)’
GivenTypes       ::=  Type {‘,’ Type}
```
The identifier `id` can be omitted only if some types are implemented or the template body defines at least one extension method.
