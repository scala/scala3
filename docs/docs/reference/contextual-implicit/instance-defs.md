---
layout: doc-page
title: "Implicit Instances"
---

Implicit instances define "canonical" values of given types
that can be synthesized by the compiler. Typically, such values are
used as evidence for constraints in [given clauses](./inferable-params.html). Example:

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

implicit ListOrd[T] given (ord: Ord[T]) for Ord[List[T]] {
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
an implicit value of the type `Ord[Int]` whereas `ListOrd[T]` defines implicit values of types `Ord[List[T]]`
for all types `T` that come with implicit values for `Ord[T]` themselves.
The `given` clause in `ListOrd` defines an [implicit parameter](./inferable-params.html).
Given clauses are further explained in the next section.

## Anonymous Implicit Instances

The name of an implicit instance can be left out. So the definitions
of the last section can also be expressed like this:
```scala
implicit for Ord[Int] { ... }
implicit [T] given (ord: Ord[T]) for Ord[List[T]] { ... }
```
If a  name is not given, the compiler will synthesize one from the type(s) in the `for` clause.

## Implicit Aliases

An implicit alias defines an implicit value that is equal to some expression. E.g., assuming a global method `currentThreadPool` returning a value with a member `context`, one could define:
```scala
implicit ctx for ExecutionContext = currentThreadPool().context
```
This creates an implicit `ctx` of type `ExecutionContext` that resolves to the right hand side `currentThreadPool().context`. Each time an implicit for `ExecutionContext` is demanded, the result of evaluating the right-hand side expression is returned.

Implicit aliases may be anonymous, e.g.
```scala
implicit for Position = enclosingTree.position
```
An implicit alias can have type and context parameters just like any other implicit definition, but it can only implement a single type.

## Syntax

Here is the new syntax of implicit instances, seen as a delta from the [standard context free syntax of Scala 3](http://dotty.epfl.ch/docs/internals/syntax.html).
```
TmplDef          ::=  ...
                  |  ‘implicit’ InstanceDef
InstanceDef      ::=  [id] InstanceParams InstanceBody
InstanceParams   ::=  [DefTypeParamClause] {GivenParamClause}
GivenParamClause ::=  ‘given’ (‘(’ [DefParams] ‘)’ | GivenTypes)
InstanceBody     ::=  [‘for’ ConstrApp {‘,’ ConstrApp }] [TemplateBody]
                   |  ‘for’ Type ‘=’ Expr
GivenTypes       ::=  AnnotType {‘,’ AnnotType}
```
The identifier `id` can be omitted only if either the `for` part or the template body is present.
If the `for` part is missing, the template body must define at least one extension method.
