---
layout: doc-page
title: "Context Bounds"
nightlyOf: https://docs.scala-lang.org/scala3/reference/contextual/context-bounds.html
---

A context bound is a shorthand for expressing the common pattern of a context parameter that depends on a type parameter. These patterns are commonplace when modelling type classes in Scala. Using a context bound, the `maximum` function of the [last section](./using-clauses.md) can be written like this:

```scala
def maximum[T: Ord](xs: List[T]): T = xs.reduceLeft(max)
```

A bound like `: Ord` on a type parameter `T` of a method or class indicates a context parameter `using Ord[T]`, which is added to the signature of the enclosing method. The generated parameter is called a _witness_ for the context bound.

For instance the `maximum` method above expands to
```scala
def maximum[T](xs: List[T])(using Ord[T]): T = ...
```
Context bounds can be combined with subtype bounds. If both are present, subtype bounds come first, e.g.

```scala
def f[T <: B : C](x: T): R = ...
```

## Named Context Bounds

A context bound can be given a name with an `as` clause. For example, assume the following trait definitions.
```scala
  trait SemiGroup[A]:
    extension (x: A) def combine(y: A): A

  trait Monoid[A] extends SemiGroup[A]:
    def unit: A
```
We can write `reduce` function over lists of monoid instances like this:
```scala
  def reduce[A: Monoid as m](xs: List[A]): A =
    xs.foldLeft(m.unit)(_ `combine` _)
```
We use `as x` after the type of a context bound to bind the instance to `x`. This is analogous to import renaming, which also introduces a new name for something that comes before.

In a context bound with a naming clause the witness parameter carries the given name. For instance the expanded signature of `reduce` would be
```scala
  def reduce[A](xs: List[A])(using m: Monoid[A]): A
```
Since the context parameter now has a name, it can be referred
to in the body of `reduce`. An example is the `m.unit` reference in the definition above.

If the context bound does not carry an `as` clause, the generated witness parameter gets a compiler-synthesized name. However, a [currently experimental
language extension](../experimental/default-names-context-bounds.md) would in this case give the context parameter the same name as the bound type parameter.

Named context bounds were introduced in Scala 3.6.

## Aggregate Context Bounds

A type parameter can have several context bounds. If there are multiple bounds, they are written inside braces `{...}`. Example:
```scala
  trait:
    def showMax[X : {Ord, Show}](x: X, y: X): String
  class B extends A:
    def showMax[X : {Ord as ordering, Show as show}](x: X, y: X): String =
      show.asString(ordering.max(x, y))
```

This syntax is valid from Scala 3.6. The previous syntax used
chains of `:` clauses, as in `[X : Ord : Show]`. This syntax is still available but will be deprecated and removed over time.

## Placement of Generated Context Parameters

The witness context parameter(s) generated from context bounds are added as follows:

 1. If one of the bounds is referred to by its name in a subsequent parameter clause, the context bounds are mapped to a using clause immediately preceding the first such parameter clause.
 2. Otherwise, if the last parameter clause is a using (or implicit) clause, merge all parameters arising from context bounds in front of that clause, creating a single using clause.
 3. Otherwise, let the parameters arising from context bounds form a new using clause at the end.

Rules (2) and (3) match Scala 2's rules. Rule (1) is new but since context bounds so far could not be referred to, it does not apply to legacy code. Therefore, binary compatibility with Scala 2 and earlier Scala 3 versions is maintained.

**Examples:**

 1. By rule 3,
    ```scala
    def f[T: {C1, C2}](x: T): R
    ```
    expands to
    ```scala
    def f[T](x: T)(using C1, C2): R
    ```
    Equally by rule 3,
    ```scala
    def f[T: {C1 as c1, C2 as c2}](x: T): R
    ```
    expands to
    ```scala
    def f[T](x: T)(using c1: C1, c2: C2): R

 2. By rule 2,
    ```scala
    def f[T: {C1, C2}, U: C3](x: T)(using y: U, z: V): R
    ```
    expands to
    ```scala
    def f[T, U](x: T)(using _: C1[T], _: C2[T], _: C3[U], y: U, z: V): R
    ```
    The same expansion occurs if `y` and `z` are Scala 2 style `implicit` parameters.
 3. Assume the following trait definition:
    ```scala
    trait Parser[P]:
      type Input
      type Result
    ```
    Here is a method `run` that runs a parser on an input of the required type:
    ```scala
    def run[P : Parser as p](in: p.Input): p.Result
    ```
    By rule 1, this method definition is expanded to:
    ```scala
    def run[P](using p: Parser[P]](in: p.Input): p.Result
    ```
    Note that the `using` clause is placed in front of the explicit parameter clause `(in: p.Result)` so that
    the type `p.Result` can legally refer to the context parameter `p`.

### Migration

To ease migration, context bounds map in Scala 3.0 - 3.5 to old-style implicit parameters
for which arguments can be passed either with a `(using ...)` clause or with a normal application. From Scala 3.6 on, they will map to context parameters instead, as is described above.

If the source version is `3.6-migration`, any pairing of an evidence
context parameter stemming from a context bound with a normal argument will give a migration
warning. The warning indicates that a `(using ...)` clause is needed instead. The rewrite can be
done automatically under `-rewrite`.

## Context Bounds for Polymorphic Functions

From Scala 3.6 on, context bounds can also be used in polymorphic function types and polymorphic function literals:

```scala
type Comparer = [X: Ord] => (x: X, y: X) => Boolean
val less: Comparer = [X: Ord as ord] => (x: X, y: X) =>
  ord.compare(x, y) < 0
```

The expansion of such context bounds is analogous to the expansion in method types, except that instead of adding a using clause in a method, we insert a [context function type](./context-functions.md).

For instance, the `type` and `val` definitions above would expand to
```scala
type Comparer = [X] => (x: X, y: X) => Ord[X] ?=> Boolean
val less: Comparer = [X] => (x: X, y: X) => (ord: Ord[X]) ?=>
  ord.compare(x, y) < 0
```

The expansion of using clauses does look inside alias types. For instance,
here is a variation of the previous example that uses a parameterized type alias:
```scala
type Cmp[X] = (x: X, y: X) => Ord[X] ?=> Boolean
type Comparer2 = [X: Ord] => Cmp[X]
```
The expansion of the right hand side of `Comparer2` expands the `Cmp[X]` alias
and then inserts the context function at the same place as what's done for `Comparer`.

### Context Bounds for Type Members

From Scala 3.6 on, context bounds can not only used for type parameters but also for abstract type members.

**Example**:

```scala
  class Collection:
    type Element: Ord
```

These context bounds have to expand differently from context bounds for type parameters since there is no parameter list to accommodate any generated witnesses. Instead, context bounds for abstract types map to
[deferred givens](./deferred-givens.md).

For instance, the `Collection` class above expands to:
```scala
  class Collection:
    type Element
    given Ord[Element] = deferred
```
As is explain in the [section on deferred givens](./deferred-givens.md), `deferred` is a special name defined in the `scala.compiletime` package.


## Syntax

The new syntax of context bounds is as follows:

```ebnf
TypeParamBounds   ::=  TypeAndCtxBounds
TypeAndCtxBounds  ::=  TypeBounds [‘:’ ContextBounds]
ContextBounds     ::=  ContextBound
                    |  '{' ContextBound {',' ContextBound} '}'
ContextBound      ::=  Type ['as' id]
```

The syntax of function types and function literals
is generalized as follows to allow context bounds for generic type parameters.

```ebnf
FunType           ::=  FunTypeArgs (‘=>’ | ‘?=>’) Type
                    |  DefTypeParamClause '=>' Type
FunExpr           ::=  FunParams (‘=>’ | ‘?=>’) Expr
                    |  DefTypeParamClause ‘=>’ Expr
```
The syntax for abstract type members is generalized as follows to allow context bounds:

```scala
TypeDef           ::=  id [TypeParamClause] TypeAndCtxBounds
```