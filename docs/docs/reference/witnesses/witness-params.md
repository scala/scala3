---
layout: doc-page
title: "Witness Parameters and Arguments"
---

Witness parameters represent a new syntax for defining implicit parameters. Unlike traditional implicit parameters, witness parameters come with a matching syntax for applications that mirrors the parameter syntax.

A witness parameter list starts with a dot ‘.’ and is followed by a normal parameter list. Analogously, a witness argument list also starts with a ‘.’ and is followed by a normal argument list. Example:
```scala
def maximum[T](xs: List[T])
             .(cmp: Ord[T]): T =
  xs.reduceLeft((x, y) => if (x < y) y else x)

def decending[T].(asc: Ord[T]): Ord[T] = new Ord[T] {
  def compareTo(this x: Int)(y: Int) = asc.compareTo(y)(x)
}

def minimum[T](xs: List[T]).(cmp: Ord[T]) =
  maximum(xs).(descending)
```
The example shows three methods that each have a witness parameter list for `Ord[T]`.
The `minimum` method's right hand side contains witness arguments `.(descending)`.
As is the case for implicit arguments, witness arguments can be left out. For instance,
given `xs: List[Int]`, the following calls are all possible (and they all normalize to the last one:)
```scala
maximum(xs)
maximum(xs).(descending)
maximum(xs).(descending.(IntOrd))
```
Unlike for implicit parameters, witness arguments must be passed using the `.( <args> )` syntax. So the expression `maximum(xs)(descending)` would give a type error.

Witness parameters translate straightforwardly to implicit parameters. Here are the previous three method definitions again, this time formulated using implicit parameters.
```scala
def maximum[T](xs: List[T])
              (implicit cmp: Ord[T]): T =
  xs.reduceLeft((x, y) => if (x < y) y else x)

def descending[T](implicit asc: Ord[T]): Ord[T] = new Ord[T] {
  def compareTo(this x: T)(y: T) = asc.compareTo(y)(x)
}

def minimum[T](xs: List[T])(implicit cmp: Ord[T]) =
  maximum(xs)(descending)
```

## Summoning a Witness

The `implicitly` method defined in `Predef` computes an implicit value for a given type. Keeping with the "witness" terminology, it seems apt to inroduce the name `summon` for this operation. So `summon[T]` summons a witness for `T`, in the same way as `implicitly[T]` does.
The definition of `summon` is straightforward:
```scala
def summon[T].(x: T) = x
```

## Implicit Closures and Function Types

A period ‘.’ in front of a parameter list also marks implicit closures and implicit function types. Examples for types:
```scala
.Context => T
.A => .B => T
.(A, B) => T
.(x: A, y: B) => T
```
Examples for closures:
```scala
.ctx => ctx.value
.(ctx: Context) => ctx.value
.(a: A, b: B) => t
```

## Syntax

Here is the new syntax for witness definitions, parameters and arguments, seen as a delta from the [standard context free syntax of Scala 3](http://dotty.epfl.ch/docs/internals/syntax.html).
```
TmplDef         ::=  ...
                  |  ‘witness’ WitnessDef
WitnessDef      ::=  [id] WitnessClauses [‘for’ [ConstrApps]] TemplateBody
                  |  [id] WitnessClauses [‘for’ Type] ‘=’ Expr
                  |  id WitnessClauses ‘for’ Type
WitnessClauses  ::=  [DefTypeParamClause] [‘with’ DefParams]

ClsParamClause    ::=  ...
                    |  ‘.’ ‘(’ ClsParams ‘)’
DefParamClause    ::=  ...
                    |  ‘.’ ‘(’ DefParams ‘)’
Type              ::=  ...
                    |  ‘.’ FunArgTypes ‘=>’ Type
Expr              ::=  ...
                    |  ‘.’ FunParams ‘=>’ Expr

SimpleExpr1       ::=  ...
                    |  SimpleExpr1 ‘.’ ParArgumentExprs
```

## More Examples

Semigroups and monoids:
```scala
trait SemiGroup[T] {
  def combine(this x: T)(y: T): T
}
trait Monoid[T] extends SemiGroup[T] {
  def unit: T
}

witness for Monoid[String] {
  def combine(this x: String)(y: String): String = x.concat(y)
  def unit: String = ""
}

def sum[T: Monoid](xs: List[T]): T =
    xs.foldLeft(summon[Monoid[T]].unit)(_.combine(_))
```
Functors and monads:
```scala
trait Functor[F[_]] {
  def map[A, B](this x: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def flatMap[A, B](this x: F[A])(f: A => F[B]): F[B]
  def map[A, B](this x: F[A])(f: A => B) = x.flatMap(f `andThen` pure)

  def pure[A](x: A): F[A]
}

witness ListMonad for Monad[List] {
  def flatMap[A, B](this xs: List[A])(f: A => List[B]): List[B] =
    xs.flatMap(f)
  def pure[A](x: A): List[A] =
    List(x)
}

witness ReaderMonad[Ctx] for Monad[[X] => Ctx => X] {
  def flatMap[A, B](this r: Ctx => A)(f: A => Ctx => B): Ctx => B =
    ctx => f(r(ctx))(ctx)
  def pure[A](x: A): Ctx => A =
    ctx => x
}
```
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

## Migration

New and old syntax would co-exist initially. Rewrite rules could rewrite old synrax to new automatically. This is trivial in the case of implicit parameters and implicit function types. It is a bit more involved in the case of implicit definitions, since more extensive pattern matching is required to recognize a definition that can be rewritten to a witness.

## Discussion

Several alternatives to the proposed syntax for witness parameters were considered:

 - Leave `implicit` parameters as they are. This suffers from the problems stated
   in the [motivation section](./motivation.md).
 - Leave the syntax of `implicit` parameters but institute two changes: First, applications
   of implicit patameters must be via the pseudo method `.explicitly(...)`. Second, there can be more than one implicit parameter list and implicit parameters may precede explicit ones. This fixes most of the discussed problems, but at the expense of a bulky explicit application syntax. Bulk can be a problem, for instance when the programmer tries to
   construct an extensive explicit argument tree to figure out what went wrong with a missing
   implicit. Another issue is that migration from old to new scheme would be tricky and
   would likely take multiple language versions.
 - Use a different syntactic marker than ‘.’ for designating implicit parameters. The ‘.’ is   ideal for the application syntax, but might be a bit too inconspicuous for the parameter
   syntax. Other ideas I played with were `?` and infix `with`. A problem with these
   is that they have visually the precedence of an infix operator, but we really want something tighter than that. ‘.’ has the same precedence as normal application, so is ideal on the application side. Note also that the ‘.’ can be made to stand out more
   through choice of formatting, e.g. like in the definition of `maximum` at the beginning of
   this section.




