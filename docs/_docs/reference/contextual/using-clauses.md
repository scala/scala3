---
layout: doc-page
title: "Using Clauses"
nightlyOf: https://docs.scala-lang.org/scala3/reference/contextual/using-clauses.html
---

Functional programming tends to express most dependencies as simple function parameterization.
This is clean and powerful, but it sometimes leads to functions that take many parameters where the same value is passed over and over again in long call chains to many
functions. Context parameters can help here since they enable the compiler to synthesize
repetitive arguments instead of the programmer having to write them explicitly.

For example, with the [given instances](./givens.md) defined previously,
a `max` function that works for any arguments for which an ordering exists can be defined as follows:

```scala
def max[T](x: T, y: T)(using ord: Ord[T]): T =
  if ord.compare(x, y) < 0 then y else x
```

Here, `ord` is a _context parameter_ introduced with a `using` clause.
The `max` function can be applied as follows:

```scala
max(2, 3)(using intOrd)
```

The `(using intOrd)` part passes `intOrd` as an argument for the `ord` parameter. But the point of context parameters is that this argument can also be left out (and it usually is). So the following applications are equally valid:

```scala
max(2, 3)
max(List(1, 2, 3), Nil)
```

## Anonymous Context Parameters

In many situations, the name of a context parameter need not be
mentioned explicitly at all, since it is used only in synthesized arguments for
other context parameters. In that case one can avoid defining a parameter name
and just provide its type. Example:

```scala
def maximum[T](xs: List[T])(using Ord[T]): T =
  xs.reduceLeft(max)
```

`maximum` takes a context parameter of type `Ord[T]` only to pass it on as an
inferred argument to `max`. The name of the parameter is left out.

Generally, context parameters may be defined either as a full parameter list `(p_1: T_1, ..., p_n: T_n)` or just as a sequence of types `T_1, ..., T_n`. Vararg parameters are not supported in `using` clauses.

## Class Context Parameters

To make a class context parameter visible from outside the class body, it can be made into a member by adding a `val` or `var` modifier.
```scala
class GivenIntBox(using val usingParameter: Int):
  def myInt = summon[Int]

val b = GivenIntBox(using 23)
import b.usingParameter
summon[Int]  // 23
```

This is preferable to creating an explicit `given` member, as the latter creates ambiguity inside the class body:
```scala
class GivenIntBox2(using usingParameter: Int):
  given givenMember: Int = usingParameter
  def n = summon[Int]  // ambiguous given instances: both usingParameter and givenMember match type Int
```

From the outside of `GivenIntBox`, `usingParameter` appears as if it were defined in the class as `given usingParameter: Int`, in particular it must be imported as described in the section on [importing `given`s](./given-imports.md).

```scala
val b = GivenIntBox(using 23)
// Works:
import b.given
summon[Int]  // 23
usingParameter  // 23

// Fails:
import b.*
summon[Int]      // No given instance found
usingParameter   // Not found
```

## Inferring Complex Arguments

Here are two other methods that have a context parameter of type `Ord[T]`:

```scala
def descending[T](using asc: Ord[T]): Ord[T] = new Ord[T]:
  def compare(x: T, y: T) = asc.compare(y, x)

def minimum[T](xs: List[T])(using Ord[T]) =
  maximum(xs)(using descending)
```

The `minimum` method's right-hand side passes `descending` as an explicit argument to `maximum(xs)`.
With this setup, the following calls are all well-formed, and they all normalize to the last one:

```scala
minimum(xs)
maximum(xs)(using descending)
maximum(xs)(using descending(using intOrd))
```

## Multiple `using` Clauses

There can be several `using` clauses in a definition and `using` clauses can be freely mixed with normal parameter clauses. Example:

```scala
def f(u: Universe)(using ctx: u.Context)(using s: ctx.Symbol, k: ctx.Kind) = ...
```

Multiple `using` clauses are matched left-to-right in applications. Example:

```scala
object global extends Universe { type Context = ... }
given ctx : global.Context { type Symbol = ...; type Kind = ... }
given sym : ctx.Symbol
given kind: ctx.Kind

```
Then the following calls are all valid (and normalize to the last one)

```scala
f(global)
f(global)(using ctx)
f(global)(using ctx)(using sym, kind)
```

But `f(global)(using sym, kind)` would give a type error.

## Explicit `using` Clauses

Missing arguments to an application can be supplied by either implicit search or default arguments.

For an explicit application, with either explicit parameters or implicit parameters supplied with explicit `using`,
missing arguments are filled in by defaults. Additionally, any remaining implicit parameters are supplied,
if an implicit value is available.

For an implicit application of context parameters, where arguments were not supplied explicitly, implicit values
are supplied if they are available, and default arguments used for any that are missing.

In other words, for a context parameter with a default argument, whether an implicit value is supplied
or the default arg depends on whether the application has explicit `using`.

As an illustration,

```scala

def f(using p: Boolean, i: Int, s: String = "hello, world") = s"p=$p, i=$i, s=$s"

given Boolean = true
given Int = 42
given String = "contextual"

f // all args are supplied implicitly from scope, since implicit values are available

f(using p = false, i = 27, s = "explicit") // all args are explicit

f(using s = "partial") // one arg is explicit, others are implicit

f(using i = 27, s = "partial") // only one arg is supplied implicitly

f(using p = false, i = 27) // for explicit using, the missing arg is supplied from the default arg

f(using p = false) // one default arg and one implicit value

```

For the last two cases, where a default arg shadows an implicit value, the compiler warns:

```
Argument for implicit parameter s was supplied using a default argument.
```

Class constructors are handled the same way, except that they are inherently mixed,
since classes always have an explicit parameter list (which may be empty).

For example,

```scala

class C(using p: Boolean, i: Int, s: String = "hello, world"):
  override def toString = s"p=$p, i=$i, s=$s"

new C // same as new C(summon[Boolean], summon[Int], summon[String])()
```

Observe that the explicit parameter list is appended to the signature.

```scala

C() // same as new C, where the implicit arguments are inserted before the explicit contructor proxy parens
C(using p = false, i = 27, s = "explicit") // explicit using, with empty parens supplied as for new C
C(using s = "partial")() // the trailing parens are required when args are previously inferred

```

(Scala 2 `implicit` parameter lists are always trailing, even for constructors.)

## Summoning Instances

The method `summon` in `Predef` returns the given of a specific type. For example,
the given instance for `Ord[List[Int]]` is produced by

```scala
summon[Ord[List[Int]]]  // reduces to listOrd(using intOrd)
```

The `summon` method is simply defined as the (non-widening) identity function over a context parameter.

```scala
def summon[T](using x: T): x.type = x
```

## Syntax

Here is the new syntax of parameters and arguments seen as a delta from the [standard context free syntax of Scala 3](../syntax.md). `using` is a soft keyword, recognized only at the start of a parameter or argument list. It can be used as a normal identifier everywhere else.

```ebnf
ClsParamClause      ::=  ... | UsingClsParamClause
DefParamClause      ::=  ... | UsingParamClause
UsingClsParamClause ::=  ‘(’ ‘using’ (ClsParams | Types) ‘)’
UsingParamClause    ::=  ‘(’ ‘using’ (DefTermParams | Types) ‘)’
ParArgumentExprs    ::=  ... | ‘(’ ‘using’ ExprsInParens ‘)’
```
