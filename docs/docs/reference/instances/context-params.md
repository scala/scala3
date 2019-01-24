---
layout: doc-page
title: "Context Parameters and Arguments"
---

Context parameters are the name of a new syntax for implicit parameters that aligns definition and call syntax. Parameter definitions
and method  arguments both follow a `with` connective. On the definition side, the old syntax
```scala
def f(a: A)(implicit b: B)
```
is now expressed as
```scala
def f(a: A) with (b: B)
```
or, leaving out the parameter name,
```scala
def f(a: A) with B
```
Implicit parameters defined with the new syntax are also called _context parameters_.
They come with a matching syntax for applications: explicit arguments for context parameters are also given after a `with`.

The following example shows shows three methods that each have a context parameter for `Ord[T]`.
```scala
def maximum[T](xs: List[T]) with Ord[T]: T =
  xs.reduceLeft((x, y) => if (x < y) y else x)

def descending[T] with (asc: Ord[T]): Ord[T] = new Ord[T] {
  def (x: T) compareTo (y: T) = asc.compareTo(y)(x)
}

def minimum[T](xs: List[T]) with Ord[T] =
  maximum(xs) with descending
```
The `minimum` method's right hand side passes `descending` as an explicit argument to `maximum(xs)`.
But usually, explicit arguments for context parameters are be left out. For instance,
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
Unlike existing implicit parameters, context parameters can be freely mixed with normal parameter lists.
A context parameter may be followed by a normal parameter and _vice versa_. There can be several context parameter
lists in a definition. Example:
```scala
def f with (u: Universe) (x: u.T) with Context = ...

instance global for Universe { type T = String ... }
instance ctx for Context { ... }
```
Then the following calls are all valid (and normalize to the last one)
```scala
f("abc")
(f with global)("abc")
f("abc") with ctx
(f with global)("abc") with ctx
```

## Syntax

Here is the new syntax of parameters and arguments seen as a delta from the [standard context free syntax of Scala 3](http://dotty.epfl.ch/docs/internals/syntax.html).
```
ClsParamClause    ::=  ...
                    |  ‘with’ (‘(’ [ClsParams] ‘)’ | ParamTypes)
DefParamClause    ::=  ...
                    |  InstParamClause
InfixExpr         ::=  ...
                    |  InfixExpr ‘with’ (InfixExpr | ParArgumentExprs)
```
