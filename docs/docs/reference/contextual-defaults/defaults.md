---
layout: doc-page
title: "Defaults"
---

Defaults define "canonical" values of certain types
that serve for synthesizing arguments to [implicit parameters](./given-clauses.md). Example:

```scala
trait Ord[T] {
  def compare(x: T, y: T): Int
  def (x: T) < (y: T) = compare(x, y) < 0
  def (x: T) > (y: T) = compare(x, y) > 0
}

default intOrd for Ord[Int] {
  def compare(x: Int, y: Int) =
    if (x < y) -1 else if (x > y) +1 else 0
}

default listOrd[T](given ord: Ord[T]) for Ord[List[T]] {

  def compare(xs: List[T], ys: List[T]): Int = (xs, ys) match
    case (Nil, Nil) => 0
    case (Nil, _) => -1
    case (_, Nil) => +1
    case (x :: xs1, y :: ys1) =>
      val fst = ord.compare(x, y)
      if (fst != 0) fst else compare(xs1, ys1)
}
```
This code defines a trait `Ord` with two defaults. `intOrd` defines
a default for `Ord[Int]` whereas `listOrd[T]` defines defaults
for `Ord[List[T]]` for all types `T` that come with a default for `Ord[T]`
themselves. The `(given ord: Ord[T])` clause in `listOrd` defines a condition: There must be a
default of type `Ord[T]` so that a defaults of type `List[Ord[T]]` can
be synthesized. Such conditions are expanded by the compiler to implicit
parameters, which are explained in the [next section](./given-clauses.md).

## Anonymous Defaults

The name of a default can be left out. So the definitions
of the last section can also be expressed like this:
```scala
default for Ord[Int] { ... }
default [T](given Ord[T]) for Ord[List[T]] { ... }
```
If the name of a default is missing, the compiler will synthesize a name from
the implemented type(s).

## Alias Defaults

An alias can be used to define a default that is equal to some expression. E.g.:
```scala
default global for ExecutionContext = new ForkJoinPool()
```
This creates a default `global` of type `ExecutionContext` that resolves to the right
hand side `new ForkJoinPool()`.
The first time `global` is accessed, a new `ForkJoinPool` is created, which is then
returned for this and all subsequent accesses to `global`.

Alias defaults can be anonymous, e.g.
```scala
default for Position = enclosingTree.position
default (given outer: Context) for Context = outer.withOwner(currentOwner)
```
An alias default can have type parameters and implicit parameters just like any other default,
but it can only implement a single type.

## Default Initialization

A default without type or implicit parameters is initialized on-demand, the first
time it is accessed. If a default has type or implicit parameters, a fresh instance
is created for each reference.

## Syntax

Here is the new syntax for defaults, seen as a delta from the [standard context free syntax of Scala 3](../../internals/syntax.md).
```
TmplDef          ::=  ...
                  |   ‘default’ DefaultDef
DefaultDef       ::=  DefaultSig ‘for’ [‘_’ ‘<:’] Type ‘=’ Expr
                  |   DefaultSig ‘for’ [ConstrApp {‘,’ ConstrApp }] [TemplateBody]
DefaultSig       ::=  [id] [DefTypeParamClause] {GivenParamClause}
GivenParamClause ::=  ‘(’ ‘given’ (DefParams | GivenTypes) ‘)’
GivenTypes       ::=  Type {‘,’ Type}
```
The identifier `id` can be omitted only if some types are implemented or the template body defines at least one extension method.
