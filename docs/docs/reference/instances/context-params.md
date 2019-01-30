---
layout: doc-page
title: "Implicit Parameters and Arguments"
---

A new syntax for implicit parameters aligns definition and call syntax. Parameter definitions and method arguments both follow a `given` keyword. On the definition side, the old syntax
```scala
def f(a: A)(implicit b: B)
```
is now expressed as
```scala
def f(a: A) given (b: B)
```
or, leaving out the parameter name,
```scala
def f(a: A) given B
```
Implicit parameters defined with the new syntax are also called _context parameters_.
They come with a matching syntax for applications: explicit arguments for context parameters are also written after a `given`.

The following example shows shows three methods that each have a context parameter for `Ord[T]`.
```scala
def maximum[T](xs: List[T]) given Ord[T]: T =
  xs.reduceLeft((x, y) => if (x < y) y else x)

def descending[T] given (asc: Ord[T]): Ord[T] = new Ord[T] {
  def (x: T) compareTo (y: T) = asc.compareTo(y)(x)
}

def minimum[T](xs: List[T]) given Ord[T] =
  maximum(xs) given descending
```
The `minimum` method's right hand side passes `descending` as an explicit argument to `maximum(xs)`.
But usually, explicit arguments for context parameters are be left out. For instance,
given `xs: List[Int]`, the following calls are all possible (and they all normalize to the last one:)
```scala
maximum(xs)
maximum(xs) given descending
maximum(xs) given (descending given IntOrd)
```
Arguments for context parameters must use the `given` syntax. So the expression `maximum(xs)(descending)` would produce a type error.

The `given` connective is treated like an infix operator with the same precedence as other operators that start with a letter. The expression following a `given` may also be an argument list consisting of several implicit arguments separated by commas. If a tuple should be passed as a single implicit argument (probably an uncommon case), it has to be put in a pair of extra parentheses:
```scala
def f given (x: A, y: B)
f given (a, b)

def g given (xy: (A, B))
g given ((a, b))
```
Unlike existing implicit parameters, context parameters can be freely mixed with normal parameter lists.
A context parameter may be followed by a normal parameter and _vice versa_. There can be several context parameter
lists in a definition. Example:
```scala
def f given (u: Universe) (x: u.T) given Context = ...

instance global of Universe { type T = String ... }
instance ctx of Context { ... }
```
Then the following calls are all valid (and normalize to the last one)
```scala
f("abc")
(f given global)("abc")
f("abc") given ctx
(f given global)("abc") given ctx
```
Context parameters may be given either as a normal parameter list `(...)`
or as a sequence of types. To distinguish the two, a leading `(` always indicates a parameter list.

## Summoning an Instance

A method `summon` in `Predef` creates an implicit instance value for a given type, analogously to what `implicitly[T]` did. The only difference between the two is that
`summon` takes a context parameter, where `implicitly` took an old-style implicit parameter:
```scala
def summon[T] with (x: T) = x
```

## Syntax

Here is the new syntax of parameters and arguments seen as a delta from the [standard context free syntax of Scala 3](http://dotty.epfl.ch/docs/internals/syntax.html).
```
ClsParamClause    ::=  ...
                    |  ‘given’ (‘(’ [ClsParams] ‘)’ | ContextTypes)
DefParamClause    ::=  ...
                    |  InstParamClause
InfixExpr         ::=  ...
                    |  InfixExpr ‘given’ (InfixExpr | ParArgumentExprs)
```
