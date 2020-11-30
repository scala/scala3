---
layout: doc-page
title: "Given Instances"
---

Given instances (or, simply, "givens") define "canonical" values of certain types
that serve for synthesizing arguments to [context parameters](./using-clauses.html). Example:

```scala
trait Ord[T] {
  def compare(x: T, y: T): Int
  extension (x: T) def < (y: T) = compare(x, y) < 0
  extension (x: T) def > (y: T) = compare(x, y) > 0
}

given intOrd as Ord[Int] {
  def compare(x: Int, y: Int) =
    if (x < y) -1 else if (x > y) +1 else 0
}

given listOrd[T](using ord: Ord[T]) as Ord[List[T]] {

  def compare(xs: List[T], ys: List[T]): Int = (xs, ys) match
    case (Nil, Nil) => 0
    case (Nil, _) => -1
    case (_, Nil) => +1
    case (x :: xs1, y :: ys1) =>
      val fst = ord.compare(x, y)
      if (fst != 0) fst else compare(xs1, ys1)
}
```
This code defines a trait `Ord` with two given instances. `intOrd` defines
a given for the type `Ord[Int]` whereas `listOrd[T]` defines givens
for `Ord[List[T]]` for all types `T` that come with a given instance for `Ord[T]`
themselves. The `using` clause in `listOrd` defines a condition: There must be a
given of type `Ord[T]` for a given of type `List[Ord[T]]` to exist.
Such conditions are expanded by the compiler to [context
parameters](./using-clauses.html).

## Anonymous Givens

The name of a given can be left out. So the definitions
of the last section can also be expressed like this:
```scala
given Ord[Int] { ... }
given [T](using Ord[T]) as Ord[List[T]] { ... }
```
If the name of a given is missing, the compiler will synthesize a name from
the implemented type(s).

**Note** The name synthesized by the compiler is chosen to be readable and reasonably concise. For instance, the two instances above would get the names:
```scala
given_Ord_Int
given_Ord_List_T
```
The precise rules for synthesizing names are found [here](./relationship-implicits.html#anonymous-given-instances). These rules do not guarantee absence of name conflicts between
given instances of types that are "too similar". To avoid conflicts one can
use named instances.

**Note** To ensure robust binary compatibility, publicly available libraries should prefer named instances.

## Alias Givens

An alias can be used to define a given instance that is equal to some expression. E.g.:
```scala
given global as ExecutionContext = new ForkJoinPool()
```
This creates a given `global` of type `ExecutionContext` that resolves to the right
hand side `new ForkJoinPool()`.
The first time `global` is accessed, a new `ForkJoinPool` is created, which is then
returned for this and all subsequent accesses to `global`. This operation is thread-safe.

Alias givens can be anonymous as well, e.g.
```scala
given Position = enclosingTree.position
given (using config: Config) as Factory = MemoizingFactory(config)
```

An alias given can have type parameters and context parameters just like any other given,
but it can only implement a single type.

## Given Macros

Given aliases can have the `inline` and `transparent` modifiers.
Example:
```scala
transparent inline given mkAnnotations[A, T] as Annotations[A, T] = ${
  // code producing a value of a subtype of Annotations
}
```
Since `mkAnnotations` is `transparent`, the type of an application is the type of its right hand side, which can be a proper subtype of the declared result type `Annotations[A, T]`.

## Pattern-Bound Given Instances

Given instances can also appear in patterns. Example:

```scala
for given Context <- applicationContexts do

pair match
  case (ctx @ given Context, y) => ...
```
In the first fragment above, anonymous given instances for class `Context` are established by enumerating over `applicationContexts`. In the second fragment, a given `Context`
instance named `ctx` is established by matching against the first half of the `pair` selector.

In each case, a pattern-bound given instance consists of `given` and a type `T`. The pattern matches exactly the same selectors as the type ascription pattern `_: T`.

## Given Instance Initialization

A given instance without type or context parameters is initialized on-demand, the first
time it is accessed. If a given has type or context parameters, a fresh instance
is created for each reference.

## Syntax

Here is the new syntax for given instances, seen as a delta from the [standard context free syntax of Scala 3](../../internals/syntax.md).

```
TmplDef           ::=  ...
                   |   ‘given’ GivenDef
GivenDef          ::=  [GivenSig] Type ‘=’ Expr
                   |   [GivenSig] ConstrApps [TemplateBody]
GivenSig          ::=  [id] [DefTypeParamClause] {UsingParamClause} ‘as’
```
