---
layout: doc-page
title: "Implicit Instances"
---

Implicit instances define "canonical" values of given types
that can be synthesized by the compiler as arguments for
[given clauses](./inferable-params.html). Example:
```scala
trait Ord[T] {
  def compare(x: T, y: T): Int
  def (x: T) < (y: T) = compare(x, y) < 0
  def (x: T) > (y: T) = compare(x, y) > 0
}

implicit IntOrd for Ord[Int] {
  def compare(x: Int, y: Int) =
    if (x < y) -1 else if (x > y) +1 else 0
}

implicit ListOrd[T] for Ord[List[T]] given (ord: Ord[T]) {
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
This code defines a trait `Ord` and two implicit definitions. `IntOrd` defines
an implicit instance of the type `Ord[Int]` whereas `ListOrd[T]` defines implicit instances of type `Ord[List[T]]`
for all types `T` that come with an implicit instance for `Ord[T]` themselves.
The `given` clause in `ListOrd` defines an implicit parameter.
Given clauses are further explained in the [next section](./inferable-params.html).

## Anonymous Implicit Instances

The name of an implicit instance can be left out. So the implicit instance definitions
of the last section can also be expressed like this:
```scala
implicit for Ord[Int] { ... }
implicit [T] for Ord[List[T]] given (ord: Ord[T]) { ... }
```
If the name of an implicit instance is missing, the compiler will synthesize a name from
the type(s) in the `for` clause.

## Alias Implicits

An alias can be used to define an implicit instance that is equal to some expression. E.g.:
```scala
implicit global for ExecutionContext = new ForkJoinPool()
```
This creates an implicit `global` of type `ExecutionContext` that resolves to the right hand side `new ForkJoinPool()`.
The first time `global` is accessed, a new `ForkJoinPool` is created, which is then
returned for this and all subsequent accesses to `global`.

Alias implicits may be anonymous, e.g.
```scala
implicit for Position = enclosingTree.position
```
An alias implicit can have type and context parameters just like any other implicit definition, but it can only implement a single type.

## Implicit Instance Creation

An implicit instance without type parameters or given clause is created on-demand, the first time it is accessed. It is not required to ensure safe publication, which means that different threads might create different instances for the same `implicit` definition. If an `implicit` definition has type parameters or a given clause, a fresh instance is created for each reference.

## Syntax

Here is the new syntax of implicit instances, seen as a delta from the [standard context free syntax of Scala 3](http://dotty.epfl.ch/docs/internals/syntax.html).
```
TmplDef          ::=  ...
                  |  ‘implicit’ InstanceDef
InstanceDef      ::=  [id] [DefTypeParamClause] InstanceBody
InstanceBody     ::=  [‘of’ ConstrApp {‘,’ ConstrApp }] {GivenParamClause} [TemplateBody]
                   |  ‘of’ Type {GivenParamClause} ‘=’ Expr
ConstrApp        ::=  AnnotType {ArgumentExprs}
                   |  ‘(’ ConstrApp {‘given’ (InfixExpr | ParArgumentExprs)} ‘)’
GivenParamClause ::=  ‘given’ (‘(’ [DefParams] ‘)’ | GivenTypes)
GivenTypes       ::=  AnnotType {‘,’ AnnotType}
```
The identifier `id` can be omitted only if either the `for` part or the template body is present.
If the `for` part is missing, the template body must define at least one extension method.
