---
layout: doc-page
title: "Implied Instances"
---

Implied instances define "canonical" values of given types
that can be synthesized by the compiler as arguments for
[given clauses](./inferable-params.html). Example:
```scala
trait Ord[T] {
  def compare(x: T, y: T): Int
  def (x: T) < (y: T) = compare(x, y) < 0
  def (x: T) > (y: T) = compare(x, y) > 0
}

implied IntOrd for Ord[Int] {
  def compare(x: Int, y: Int) =
    if (x < y) -1 else if (x > y) +1 else 0
}

implied ListOrd[T] for Ord[List[T]] given (ord: Ord[T]) {
  def compare(xs: List[T], ys: List[T]): Int = (xs, ys) match {
    case (Nil, Nil) => 0
    case (Nil, _) => -1
    case (_, Nil) => +1
    case (x :: xs1, y :: ys1) =>
      val fst = ord.compare(x, y)
      if (fst != 0) fst else xs1.compareTo(ys1)
  }
}
```
This code defines a trait `Ord` and two implied instance clauses. `IntOrd` defines
an implied instance for the type `Ord[Int]` whereas `ListOrd[T]` defines implied
instances of `Ord[List[T]]` for all types `T` that come with an implied `Ord[T]` instance themselves.
The `given` clause in `ListOrd` defines an _context parameter_.
Given clauses are further explained in the [next section](./inferable-params.html).

## Anonymous Implied Instances

The name of an implied instance can be left out. So the implied instance definitions
of the last section can also be expressed like this:
```scala
implied for Ord[Int] { ... }
implied [T] for Ord[List[T]] given (ord: Ord[T]) { ... }
```
If the name of an instance is missing, the compiler will synthesize a name from
the type(s) in the `for` clause.

## Implied Alias Instances

An implied alias instance defines an implied instance that is equal to some expression. E.g.:
```scala
implied global for ExecutionContext = new ForkJoinPool()
```
This creates an implied instance `global` of type `ExecutionContext` that resolves to the right hand side `new ForkJoinPool()`.
The first time `global` is accessed, a new `ForkJoinPool` is created, which is then
returned for this and all subsequent accesses to `global`.

Alias instances may be anonymous, e.g.
```scala
implied for Position = enclosingTree.position
```
An implied alias instance can have type parameters and given clauses just like any other implied instance, but it can only implement a single type.

## Creating Implied Instances

An implied instance without type parameters or given clause is created on-demand, the first time it is accessed. It is not required to ensure safe publication, which means that different threads might create different representatives for the same `implied` clause. If an implied instance has type parameters or a given clause, its definition is evaluated each time it is applied to arguments.

## Syntax

Here is the new syntax of implied instance definitions, seen as a delta from the [standard context free syntax of Scala 3](http://dotty.epfl.ch/docs/internals/syntax.html).
```
TmplDef          ::=  ...
                  |  ‘implied’ InstanceDef
InstanceDef      ::=  [id] [DefTypeParamClause] InstanceBody
InstanceBody     ::=  [‘for’ ConstrApp {‘,’ ConstrApp }] {GivenParamClause} [TemplateBody]
                   |  ‘for’ Type {GivenParamClause} ‘=’ Expr
ConstrApp        ::=  SimpleConstrApp
                   |  ‘(’ SimpleConstrApp {‘given’ (PrefixExpr | ParArgumentExprs)} ‘)’
SimpleConstrApp  ::=  AnnotType {ArgumentExprs}
GivenParamClause ::=  ‘given’ (‘(’ [DefParams] ‘)’ | GivenTypes)
GivenTypes       ::=  AnnotType {‘,’ AnnotType}
ConstrApp        ::=  SimpleConstrApp
                   |  ‘(’ SimpleConstrApp {‘given’ (PrefixExpr | ParArgumentExprs)} ‘)’
```
The identifier `id` can be omitted only if either the `for` part or the template body is present.
If the `for` part is missing, the template body must define at least one extension method.
