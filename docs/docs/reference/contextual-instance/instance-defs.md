---
layout: doc-page
title: "Instance Definitions"
---

Instance definitions define "canonical" values of given types
that can be synthesized by the compiler. Typically, such values are
used as implicit arguments for constraints in [given clauses](./inferable-params.html). Example:

```scala
trait Ord[T] {
  def compare(x: T, y: T): Int
  def (x: T) < (y: T) = compare(x, y) < 0
  def (x: T) > (y: T) = compare(x, y) > 0
}

instance IntOrd of Ord[Int] {
  def compare(x: Int, y: Int) =
    if (x < y) -1 else if (x > y) +1 else 0
}

instance ListOrd[T] of Ord[List[T]] given (ord: Ord[T]) {
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
This code defines a trait `Ord` and two instance definitions. `IntOrd` defines
an implicit instance of type `Ord[Int]` whereas `ListOrd[T]` defines implicit instances of type `Ord[List[T]]`
for all types `T` that come with an instance for `Ord[T]` themselves.
The `given` clause in `ListOrd` defines an [implicit parameter](./inferable-params.html).
Given clauses are further explained in the next section.

## Anonymous Instance Definitions

The name of an implicit instance can be left out. So the instance definitions
of the last section can also be expressed like this:
```scala
instance of Ord[Int] { ... }
instance [T] of Ord[List[T]] given Ord[T] { ... }
```
If a  name is not given, the compiler will synthesize one from the type(s) in the `of` clause.

## Alias Instances

An alias instance defines an implicit instance that is equal to some expression. E.g., assuming a global method `currentThreadPool` returning a value with a member `context`, one could define:
```scala
instance ctx of ExecutionContext = currentThreadPool().context
```
This creates an implicit instance `ctx` of type `ExecutionContext` that resolves to the right hand side `currentThreadPool().context`.
Each time an instance for `ExecutionContext` is demanded, the result of evaluating the right-hand side expression is returned.

Alias instances may be anonymous, e.g.
```scala
instance of Position = enclosingTree.position
```
An alias instance can have type and context parameters just like any other instance definition, but it can only implement a single type.

## Syntax

Here is the new syntax of instance definitions, seen as a delta from the [standard context free syntax of Scala 3](http://dotty.epfl.ch/docs/internals/syntax.html).
```
TmplDef          ::=  ...
                  |  ‘instance’ InstanceDef
InstanceDef      ::=  [id] [DefTypeParamClause] InstanceBody
InstanceBody     ::=  [‘of’ ConstrApp {‘,’ ConstrApp }] {GivenParamClause} [TemplateBody]
                   |  ‘of’ Type {GivenParamClause} ‘=’ Expr
ConstrApp        ::=  AnnotType {ArgumentExprs}
                   |  ‘(’ ConstrApp {‘given’ (InfixExpr | ParArgumentExprs)} ‘)’
GivenParamClause ::=  ‘given’ (‘(’ [DefParams] ‘)’ | GivenTypes)
GivenTypes       ::=  AnnotType {‘,’ AnnotType}
```
The identifier `id` can be omitted only if either the `of` part or the template body is present.
If the `of` part is missing, the template body must define at least one extension method.
