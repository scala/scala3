---
layout: doc-page
title: "Given Instances"
nightlyOf: https://docs.scala-lang.org/scala3/reference/contextual/givens.html
---

Given instances (or, simply, "givens") define "canonical" values of certain types
that serve for synthesizing arguments to [context parameters](./using-clauses.md). Example:

```scala
trait Ord[T]:
  def compare(x: T, y: T): Int
  extension (x: T)
    def < (y: T) = compare(x, y) < 0
    def > (y: T) = compare(x, y) > 0

given intOrd: Ord[Int]:
  def compare(x: Int, y: Int) =
    if x < y then -1 else if x > y then +1 else 0

given listOrd: [T: Ord] => Ord[List[T]]:

  def compare(xs: List[T], ys: List[T]): Int = (xs, ys) match
    case (Nil, Nil) => 0
    case (Nil, _) => -1
    case (_, Nil) => +1
    case (x :: xs1, y :: ys1) =>
      val fst = summon[Ord[T]].compare(x, y)
      if fst != 0 then fst else compare(xs1, ys1)

```

This code defines a trait `Ord` with two given instances. `intOrd` defines
a given for the type `Ord[Int]` whereas `listOrd[T]` defines givens
for `Ord[List[T]]` for all types `T` that come with a given instance for `Ord[T]`. The clause `[T: Ord]` is a [context bound](./context-bounds.md) which defines a condition: There must be a given of type `Ord[T]` for a given of type `Ord[List[T]]` to exist. Such conditions are expanded by the compiler to [context parameters](./using-clauses.md).

## Anonymous Givens

The name of a given can be left out. So the definitions
of the last section can also be expressed like this:

```scala
given Ord[Int]:
  ...
given [T: Ord] => Ord[List[T]]:
  ...
```

If the name of a given is missing, the compiler will synthesize a name from
the implemented type(s).

**Note:** The name synthesized by the compiler is chosen to be readable and reasonably concise. For instance, the two instances above would get the names:

```scala
given_Ord_Int
given_Ord_List
```

The precise rules for synthesizing names are found [here](./relationship-implicits.html#anonymous-given-instances). These rules do not guarantee absence of name conflicts between
given instances of types that are "too similar". To avoid conflicts one can use named instances.

**Note:** To ensure robust binary compatibility, publicly available libraries should prefer named instances.

## Alias Givens

An alias can be used to define a given instance that is equal to some expression. Example:

```scala
given global: ExecutionContext = ForkJoinPool()
```

This creates a given `global` of type `ExecutionContext` that resolves to the right
hand side `ForkJoinPool()`.
The first time `global` is accessed, a new `ForkJoinPool` is created, which is then
returned for this and all subsequent accesses to `global`. This operation is thread-safe.

Alias givens can be anonymous as well, e.g.

```scala
given Position = enclosingTree.position
```

## Given Instance Initialization

An unconditional given instance without parameters is initialized on-demand, the first
time it is accessed. If the given is a mere alias to some immutable value, the given is implemented as a simple forwarder, without incurring the cost of a field to hold a cached value. If a given is conditional, a fresh instance is created for each reference.

## Syntax

Here is the full syntax for given instances. Some of these forms of givens are explained in a separate page: [Other Forms of Givens](../more-givens.md).

```ebnf
Here is the complete context-free syntax for all proposed features.
```
TmplDef           ::=  ... | 'given' GivenDef
GivenDef          ::=  [id ':'] GivenSig
GivenSig          ::=  GivenImpl
                    |  '(' ')' '=>' GivenImpl
                    |  GivenConditional '=>' GivenSig
GivenImpl         ::=  GivenType ([‘=’ Expr] | TemplateBody)
                    |  ConstrApps TemplateBody
GivenConditional  ::=  DefTypeParamClause
                    |  DefTermParamClause
                    |  '(' FunArgTypes ')'
                    |  GivenType
GivenType         ::=  AnnotType1 {id [nl] AnnotType1}
```

A given instance starts with the reserved keyword `given`, which is followed by

 - An optional name and a colon
 - An optional list of conditions.
 - The implemented type(s) and their implementation, in two forms: alias givens and structural givens.
    - An _alias given_ implements a single type with a right hand side following `=`.
    - A _structural given_ implements one or more class constructors with a
      list of member definitions in a template body.

**Note** Parts of the given syntax have changed in Scala 3.6. The original syntax from Scala 3.0 on is described in a separate page [Previous Given Syntax](../previous-givens.md). The original syntax is still supported for now but will be deprecated and phased out over time.
