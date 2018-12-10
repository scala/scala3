---
layout: doc-page
title: "Context Parameters and Arguments"
---

This page presents new syntax for defining implicit parameters that aligns definition and call syntax. In both cases, the implicit parameter or argument now follows a `with` connective.
On the definition side, the old syntax
```scala
def f(a: A)(implicit b: B)
```
is now expressed as
```scala
def f(a: A) with (b: B)
```
Implicit parameters defined with the new syntax are also called _context parameters_.
They come with a matching syntax for applications: explicit arguments for context parameters are also given after a `with`.

The following example shows shows three methods that each have a context parameter for `Ord[T]`.
```scala
def maximum[T](xs: List[T]) with (cmp: Ord[T]): T =
  xs.reduceLeft((x, y) => if (x < y) y else x)

def descending[T] with (asc: Ord[T]): Ord[T] = new Ord[T] {
  def (x: T) compareTo (y: T) = asc.compareTo(y)(x)
}

def minimum[T](xs: List[T]) with (cmp: Ord[T]) =
  maximum(xs) with descending
```
The `minimum` method's right hand side defines the explicit argument `descending`.
Explicit arguments for context parameters can be left out. For instance,
given `xs: List[Int]`, the following calls are all possible (and they all normalize to the last one:)
```scala
maximum(xs)
maximum(xs) with descending
maximum(xs) with (descending with IntOrd)
```
Arguments for context parameters must be given using the `with` syntax. So the expression `maximum(xs)(descending)` would give a type error.

The `with` connective is treated like an infix operator with the same precedence as other operators that start with a letter. The expression following a `with` may also be an argument list consisting of several implicit arguments separated by commas. If a tuple should be passed as a single implicit argument (probably an uncommon case), it has to be put in a pair of extra parentheses:
```scala
def f with (x: A, y: B)
f with (a, b)

def g with (xy: (A, B))
g with ((a, b))
```

## Implicit Function Types and Closures

Implicit function types are expressed using the new reserved operator `|=>`. Examples:
```scala
Context |=> T
A |=> B |=> T
(A, B) |=> T
(x: A, y: B) |=> T
```
The `|=>` syntax was chosen for its resemblance with a turnstile symbol `|-` which signifies context dependencies.

The old syntax `implicit A => B` is no longer available.
Implicit function types are applied using `with`:
```scala
val f: A |=> B
val a: A
f with a    // OK
f(a)        // error: `f` does not take parameters
```
Since application of regular function types and implicit function types different, implicit function types are no longer subtypes of regular function types.

The `|=>` syntax can also be used for closures. It turns the parameter bindings into implicit
parameters and makes the closure's type an implicit function type
```scala
case class Context(value: String)
val f1: Context |=> String  =  ctx |=> ctx.value
val f2: Context |=> String  =  (ctx: Context) |=> ctx.value
val f3: (A, B) |=> T        =  (a: A, b: B) |=> t
```
The old syntax `implicit (a: A) => B` now creates a closure of a regular function type `A => B` instead of an implicit function type `A |=> B`. This matches the types of implicit closures in Scala 2.x.

## Example

Implementing postconditions via `ensuring`:
```scala
object PostConditions {
  opaque type WrappedResult[T] = T

  private witness WrappedResult {
    def apply[T](x: T): WrappedResult[T] = x
    def (x: WrappedResult[T]) unwrap[T]: T = x
  }

  def result[T] with (wrapped: WrappedResult[T]): T = wrapped.unwrap

  witness {
    def (x: T) ensuring[T] (condition: WrappedResult[T] |=> Boolean): T = {
      assert(condition with WrappedResult(x))
      x
    }
  }
}

object Test {
  import PostConditions._
  val s = List(1, 2, 3).sum.ensuring(result == 6)
}
```
## Syntax

Here is the new syntax for parameters, arguments, and implicit function types seen as a delta from the [standard context free syntax of Scala 3](http://dotty.epfl.ch/docs/internals/syntax.html).
```
ClsParamClause    ::=  ...
                    |  ‘with’ ‘(’ [ClsParams] ‘)’
DefParamClause    ::=  ...
                    |  ‘with’ ‘(’ [DefParams] ‘)’
Type              ::=  ...
                    |  [‘erased’] FunArgTypes ‘|=>’ Type
Expr              ::=  ...
                    |  [‘erased’] FunParams ‘|=>’ Expr
InfixExpr         ::=  ...
                    |  InfixExpr ‘with’ (InfixExpr | ParArgumentExprs)
```
