---
layout: doc-page
title: "Delegates"
---

Delegates define "canonical" values of certain types
that serve for synthesizing arguments to [given clauses](./given-clauses.html). Example:

```scala
trait Ord[T] {
  def compare(x: T, y: T): Int
  def (x: T) < (y: T) = compare(x, y) < 0
  def (x: T) > (y: T) = compare(x, y) > 0
}

delegate IntOrd for Ord[Int] {
  def compare(x: Int, y: Int) =
    if (x < y) -1 else if (x > y) +1 else 0
}

delegate ListOrd[T] for Ord[List[T]] given (ord: Ord[T]) {
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
This code defines a trait `Ord` with two delegate definitions. `IntOrd` defines
a delegate for the type `Ord[Int]` whereas `ListOrd[T]` defines delegates
for `Ord[List[T]]` for all types `T` that come with a delegate for `Ord[T]` themselves.
The `given` clause in `ListOrd` defines an implicit parameter.
Given clauses are further explained in the [next section](./given-clauses.html).

## Anonymous Delegates

The name of a delegate can be left out. So the delegate definitions
of the last section can also be expressed like this:
```scala
delegate for Ord[Int] { ... }
delegate [T] for Ord[List[T]] given (ord: Ord[T]) { ... }
```
If the name of a delegate is missing, the compiler will synthesize a name from
the type(s) in the `for` clause.

## Alias Delegates

An alias can be used to define a delegate that is equal to some expression. E.g.:
```scala
delegate global for ExecutionContext = new ForkJoinPool()
```
This creates a delegate `global` of type `ExecutionContext` that resolves to the right hand side `new ForkJoinPool()`.
The first time `global` is accessed, a new `ForkJoinPool` is created, which is then
returned for this and all subsequent accesses to `global`.

Alias delegates can be anonymous, e.g.
```scala
delegate for Position = enclosingTree.position
delegate for Context given (outer: Context) =
  outer.withOwner(currentOwner)
```
An alias delegate can have type parameters and given clauses just like any other delegate, but it can only implement a single type.

## Delegate Instantiation

A delegate without type parameters or given clause is instantiated on-demand, the first
time it is accessed. It is not required to ensure safe publication, which means that
different threads might create different delegates for the same `delegate` clause.
If a `delegate` clause has type parameters or a given clause, a fresh delegate is
created for each reference.

## Syntax

Here is the new syntax of delegate clauses, seen as a delta from the [standard context free syntax of Scala 3](http://dotty.epfl.ch/docs/internals/syntax.html).
```
TmplDef          ::=  ...
                  |   ‘delegate’ DelegateDef
DelegateDef      ::=  [id] [DefTypeParamClause] DelegateBody
DelegateBody     ::=  [‘for’ ConstrApp {‘,’ ConstrApp }] {GivenParamClause} [TemplateBody]
                   |  ‘for’ Type {GivenParamClause} ‘=’ Expr
ConstrApp        ::=  SimpleConstrApp
                   |  ‘(’ SimpleConstrApp {‘given’ (PrefixExpr | ParArgumentExprs)} ‘)’
SimpleConstrApp  ::=  AnnotType {ArgumentExprs}
GivenParamClause ::=  ‘given’ (‘(’ [DefParams] ‘)’ | GivenTypes)
GivenTypes       ::=  AnnotType {‘,’ AnnotType}
```
The identifier `id` can be omitted only if either the `for` part or the template body is present.
If the `for` part is missing, the template body must define at least one extension method.
