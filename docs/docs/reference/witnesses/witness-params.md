---
layout: doc-page
title: "Witness Parameters and Arguments"
---

This page presents new syntax for defining implicit parameters. Previously, implicit parameters, marked with `implicit`, were decoupled from implicit applications which looked like regular applications. On the other hand, external API and internal use were coupled - the `implicit` modifier specified both that missing arguments would be synthesized and that the parameter was in turn available as a candidate for implicit search. The new syntax reverts both of these decisions. Two different mechanisms control whether an argument to a parameter is synthesized and whether the parameter is available as a witness. Furthermore, explicit applications of methods with implicit parameters have a different syntax which matches the parameter definition syntax.

The principal idea is to mark an implicit parameter or argument list with a preceding ‘.’.
If a parameter can itself be used as a witness in the body of a method or class, it is marked with `witness`. So the old syntax
```scala
def f(a: A)(implicit b: B, c: C)
```
would now be expressed as
```scala
def f(a: A).(witness b: B, witness c: C)
```
But the new syntax also allows for other possibilities:
```scala
def f1(a: A).(b: B, c: C)          // b, c passed implicitly, but neither is available as a witness
def f2(a: A).(witness b: B, c: C)  // b, c passed implicitly, only b available as a witness
def f3(a: A).(b: B, witness c: C)  // b, c passed implicitly, only c available as a witness
def f4(witness a: A)               // a passed explicitly, available as a witness
```
To disambiguate between old and new syntax, we call implicit parameters under the new syntax "implicitly passed parameters", or, if they are marked with `witness`, "witness parameters" (in the rare case where a witness parameter is not implicitly passed, we will state that fact explicitly). The new implicit parameter syntax comes with a matching syntax for applications. If an argument list to a implicitly passed parameter is given, it also starts with a ‘.’.

The following example shows shows three methods that each have a witness parameter list for `Ord[T]`.
```scala
def maximum[T](xs: List[T]).(witness cmp: Ord[T]): T =
  xs.reduceLeft((x, y) => if (x < y) y else x)

def descending[T].(witness asc: Ord[T]): Ord[T] = new Ord[T] {
  def compareTo(this x: Int)(y: Int) = asc.compareTo(y)(x)
}

def minimum[T](xs: List[T]).(witness cmp: Ord[T]) =
  maximum(xs).(descending)
```
The `minimum` method's right hand side contains the explicit argument list `.(descending)`.
Explicit argument lists for implicitly passed parameters can be left out. For instance,
given `xs: List[Int]`, the following calls are all possible (and they all normalize to the last one:)
```scala
maximum(xs)
maximum(xs).(descending)
maximum(xs).(descending.(IntOrd))
```
Unlike for traditional implicit parameters, arguments for implicitly passed parameters must be given using the `.( <args> )` syntax. So the expression `maximum(xs)(descending)` would give a type error.

## Application: Dependency Injection

Witnesses and implicitly passed parameters lend themselves well to dependency injection with constructor parameters. As an example, say we have four components `C1,...,C4` each of which depend on some subset of the other components. We can define these components as classes with implicitly passed parameters. E.g.,
```scala
class C1.(c2: C2, c3: C3) { ... }
class C2.(c1: C1, c4: C4) { ... }
class C3.(c2: C3, c4: C4) { ... }
class C4.(c1: C1, c3: C3, c3: C3) { ... }
```
The components can then be "wired together" by defining a set of local witnesses:
```scala
{ witness c1 for C1
  witness c2 for C2
  witness c3 for C3
  witness c4 for C4
  (c1, c2, c3, c4)
}
```
Note that component dependencies are _not_ defined themselves as witness parameters. This prevents components from spreading into the implicit namespace of other components and keeps the wiring strictly to the interface of these modules.

This scheme is essentially what MacWire does. MacWire was implemented as a macro library. It requires whitebox macros which will no longer be supported in Scala 3.

## Summoning a Witness

The `implicitly` method defined in `Predef` computes an implicit value for a given type. Keeping with the "witness" terminology, it seems apt to introduce the name `summon` for this operation. So `summon[T]` summons a witness for `T`, in the same way as `implicitly[T]` does. The definition of `summon` is straightforward:
```scala
def summon[T].(x: T) = x
```

## Implicit Function Types and Closures

Implicit function types are marked with period ‘.’ in front of a parameter list. Examples:
```scala
.Context => T
.A => .B => T
.(A, B) => T
.(x: A, y: B) => T
```
Like methods, closures can also have parameters marked as `witness`. Examples:
```scala
case class Context(value: String)
witness ctx => ctx.value
(witness ctx: Context) => ctx.value
(a: A, witness b: B) => t
```
Closures can also be marked with a prefix ‘.’. This makes the type of the closure
an implicit function type instead of a regular function type. As is the case for methods, a `witness` modifier for a closure parameter affects its internal use (by making the parameter available as a witness in the body) whereas ‘.’ affects the closure's external API and its type. To summarize:
```scala
         (ctx: Context) => ctx.value  :   Context => String
 (witness ctx: Context) => ctx.value  :   Context => String
        .(ctx: Context) => ctx.value  :  .Context => String
.(witness ctx: Context) => ctx.value  :  .Context => String
```
Implicitly applied closures with prefix ‘.’ will probably be written only rarely. But the concept is needed as a way to explain the translation of implicit function types: If the expected type of a term _t_ is an implicit function type, _t_ will be turned into an implicitly applied closure of the form _.x => t_ unless _t_ is already such a closure.

## Example

Implementing postconditions via `ensuring`:
```scala
object PostConditions {
  opaque type WrappedResult[T] = T

  private witness WrappedResult {
    def apply[T](x: T): WrappedResult[T] = x
    def unwrap[T](this x: WrappedResult[T]): T = x
  }

  def result[T].(wrapped: WrappedResult[T]): T = wrapped.unwrap

  witness {
    def ensuring[T](this x: T)(condition: .WrappedResult[T] => Boolean): T = {
      assert(condition.(WrappedResult(x)))
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
                    |  ‘.’ ‘(’ ClsParams ‘)’
ClsParam          ::=  {Annotation} [{Modifier} (‘val’ | ‘var’) | ParamModifier] Param
DefParamClause    ::=  ...
                    |  ‘.’ ‘(’ DefParams ‘)’
DefParam          ::=  {Annotation} [ParamModifier] Param
ParamModifier     ::=  ‘inline’ | ‘witness’
Type              ::=  ...
                    |  ‘.’ FunArgTypes ‘=>’ Type
Expr              ::=  ...
                    |  ‘.’ FunParams ‘=>’ Expr
SimpleExpr1       ::=  ...
                    |  SimpleExpr1 ‘.’ ParArgumentExprs
```
