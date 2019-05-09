---
layout: doc-page
title: "Representatives"
---

Representatives define "canonical" values of given types
that can be synthesized by the compiler as arguments for
[given clauses](./inferable-params.html). Example:
```scala
trait Ord[T] {
  def compare(x: T, y: T): Int
  def (x: T) < (y: T) = compare(x, y) < 0
  def (x: T) > (y: T) = compare(x, y) > 0
}

repr IntOrd of Ord[Int] {
  def compare(x: Int, y: Int) =
    if (x < y) -1 else if (x > y) +1 else 0
}

repr ListOrd[T] of Ord[List[T]] given (ord: Ord[T]) {
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
This code defines a trait `Ord` with two representative clauses. `IntOrd` defines
a representative of the type `Ord[Int]` whereas `ListOrd[T]` defines representatives
of `Ord[List[T]]` for all types `T` that come with a representative of `Ord[T]` themselves.
The `given` clause in `ListOrd` defines an implicit parameter.
Given clauses are further explained in the [next section](./inferable-params.html).

## Anonymous Representatives

The name of a representative can be left out. So the representatives
of the last section can also be expressed like this:
```scala
repr of Ord[Int] { ... }
repr [T] of Ord[List[T]] given (ord: Ord[T]) { ... }
```
If the name of a representative is missing, the compiler will synthesize a name from
the type(s) in the `of` clause.

## Alias Representatives

An alias can be used to define a representative that is equal to some expression. E.g.:
```scala
repr ctx of ExecutionContext = new ForkJoinPool()
```
This creates a repreentative `global` of type `ExecutionContext` that resolves to the right hand side `new ForkJoinPool()`.
The first time `global` is accessed, a new `ForkJoinPool` is created, which is then
returned for this and all subsequent accesses to `global`.

Alias representatives can be anonymous, e.g.
```scala
repr of Position = enclosingTree.position
```
An alias representative can have type and context parameters just like any other representative, but it can only implement a single type.

## Creating Representatives

A representative without type parameters or given clause is created on-demand, the first time it is accessed. It is not required to ensure safe publication, which means that different threads might create different representatives for the same `repr` clause. If a `repr` clause has type parameters or a given clause, a fresh representative is created for each reference.

## Syntax

Here is the new syntax of representative clauses, seen as a delta from the [standard context free syntax of Scala 3](http://dotty.epfl.ch/docs/internals/syntax.html).
```
TmplDef          ::=  ...
                  |   ‘repr’ ReprDef
ReprDef          ::=  [id] [DefTypeParamClause] ReprBody
ReprBody         ::=  [‘of’ ConstrApp {‘,’ ConstrApp }] {GivenParamClause} [TemplateBody]
                   |  ‘of’ Type {GivenParamClause} ‘=’ Expr
ConstrApp        ::=  AnnotType {ArgumentExprs}
                   |  ‘(’ ConstrApp {‘given’ (InfixExpr | ParArgumentExprs)} ‘)’
GivenParamClause ::=  ‘given’ (‘(’ [DefParams] ‘)’ | GivenTypes)
GivenTypes       ::=  AnnotType {‘,’ AnnotType}
```
The identifier `id` can be omitted only if either the `of` part or the template body is present.
If the `of` part is missing, the template body must define at least one extension method.
