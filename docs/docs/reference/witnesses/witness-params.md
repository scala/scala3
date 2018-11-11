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
  def compareTo(this x: T)(y: T) = asc.compareTo(y)(x)
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

## Application: Dependency Injection

Witnesses can be used for dependency injection with constructor parameters. As an example, say we have four components `C1,...,C4` each of which depend on some subset of the other components. Constructor-based dependency injection defines these components as classes with explicitly passed parameters. E.g.,
```scala
class C1(c2: C2, c3: C3) { ... }
class C2(c1: C1, c4: C4) { ... }
class C3(c2: C3, c4: C4) { ... }
class C4(c1: C1, c3: C3, c3: C3) { ... }
```
The components can then be "wired together" by defining a set of local witnesses:
```scala
{ witness c1 with (c1: C1, c2: C2, c3: C3) for C1(c1, c2, c3)
  witness c2 with (c1: C1, c4: C4)         for C2 (c1, c4)
  witness c3 with (c2: C3, c4: C4)         for C3(c2, c4)
  witness c4 with (c1: C1, c3: C3, c4: C4) for C4(c1, c3, c4)
  (c1, c2, c3, c4)
}
```
Note that component dependencies in `C1, ..., C4` are _not_ defined themselves as implicit parameters. This prevents components from spreading into the implicit namespace of other components and keeps the wiring strictly to the interface of these modules.

This scheme is essentially what MacWire does. MacWire was implemented as a macro library. It requires whitebox macros which will no longer be supported in Scala 3.

I considered for a while an alternative design where the two notions of an implicit parameter (argument gets synthesized vs. parameter is itself available as an implicit value) are separated. This would allow a nicer expression of component assembly which would not require that dependencies are repeated in the witnesses. The most significant downside of the alternative design is that it's likely to induce choice fatigue. In most cases, implicit parameters should be available itself as a witness, so asking for an opt-in each time a parameter is defined
became quickly tiresome.

## Implicit Function Types and Closures

Implicit function types are expressed using the new reserved operator `|=>`. Examples:
```scala
Context |=> T
A |=> B |=> T
(A, B) |=> T
(x: A, y: B) |=> T
```
The `|=>` syntax was chosen for its resemblance with a turnstile symbol `|-` which signifies context dependencies.

The `|=>` syntax can also be used for closures. It turns the parameter bindings into implicit
parameters.
```scala
case class Context(value: String)
ctx |=> ctx.value
(ctx: Context) |=> ctx.value
(a: A, b: B) |=> t
```

## Example

Implementing postconditions via `ensuring`:
```scala
object PostConditions {
  opaque type WrappedResult[T] = T

  private witness WrappedResult {
    def apply[T](x: T): WrappedResult[T] = x
    def unwrap[T](this x: WrappedResult[T]): T = x
  }

  def result[T] with (wrapped: WrappedResult[T]): T = wrapped.unwrap

  witness {
    def ensuring[T](this x: T)(condition: WrappedResult[T] |=> Boolean): T = {
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
                    |  FunArgTypes ‘|=>’ Type
Expr              ::=  ...
                    |  FunParams ‘|=>’ Expr
InfixExpr         ::=  ...
                    |  InfixExpr ‘with’ (InfixExpr | ParArgumentExprs)
```
