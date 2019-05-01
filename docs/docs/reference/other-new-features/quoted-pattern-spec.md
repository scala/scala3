---
layout: doc-page
title: "Pattern Matching on Quoted Code"
---


## Overview

Any top-level quote `'{ ... }` in a pattern position will become a quoted pattern. Inside quoted pattern parts of the code can be spliced with `$` which extracts that part of the code.
Splices can be of two forms:
* A splice `${ ... : Expr[T] }` that can be placed in any expression position.
* A splice `${ ... : Bind[T] }` that can be placed on names of `val`s, `var`s or `def`s

```scala
def foo(x: Expr[Int]) given tasty.Reflect: Expr[Int] = x match {
  case '{ val $a: Int = $x; (${Bind(`a`)}: Int) + 1 } => '{ $x + 1 } // TODO needs fix for #6328, `a` is currently not in scope while typing
}
```
In the example above we have `$a` which provides a `Bind[Int]`, `$x` which provides an `Expr[Int]` and `${Bind(`a`)}` which probides an `Expr[Int]` that is pattern matched against `Bind(`a`)` to check that it is a reference to `a`.

Quoted patterns are transformed during typer to a call of `scala.internal.quoted.Matcher.unapply` which splits the quoted code into the patterns and a reifiable quote that will be used as witnesses at runtime.

```scala
def foo(x: Expr[Int]) given tasty.Reflect: Expr[Int] = x match {
  case scala.internal.quoted.Matcher.unapply[Tuple3[Bind[Int], Expr[Int], Expr[Int]]](Tuple3(a, x, Bind(`a`), y))('{ @patternBindHole val a: Int = patternHole[Int]; patternHole[Int] + 1 }) =>
}
```


## Runtime semantics

At runtime to a `quoted.Expr` can be matched to another using `scala.internal.quoted.Matcher.unapply`.

```scala
def unapply[Tup <: Tuple](scrutineeExpr: Expr[_])(implicit patternExpr: Expr[_], reflection: Reflection): Option[Tup]
```

The `scrutineeExpr` is a normal quoted expression while `patternExpr` may contain holes representing splices.
The result of pattern matching is `None` if the expressions are not equivalent, otherwise it returns `Some` (some tuple) containing the contents of the matched holes.

Let's define some abstractions on the possible results of this pattern matching using the alias `Matching`:
```scala
type Matching = Option[Tuple]
type Env

def notMatched = None
def matched = Some(()) // aka Some(Tuple0())
def matched[T](x: T) = Some(Tuple1(x))
def (x: Matching) && (y: Matching) = if (x == None || y == None) None else Some(x.get ++ y.get)
def fold[T](m: Mattching*) given Env: Matching = m.fold(matched)(_ && _)

// `a =#= b` stands for `a` matches `b` 
def (scrutinee: Tree) =#= pattern: Tree) given Env: Matching // described by cases in the tables below

def envWith(equiv: (Symbol, Symbol)*) given Env: Env // Adds to the current environment the fact that s1 from the scrutinee is equivalent to s2 in the pattern

def equivalent(s1: Symbol, s2: Symbol) given Env: Env
```

The implementation of `=#=`

| Tree                      | Pattern                     | Returns     |
| :-----------------------: | :-------------------------: | :---------- |
| Term `a`                  | `patternHole[X]`            | `match(quoted.Expr[X]('a))` if type of `a` is a subtype of `X`
| `val a: A`                | `@patternBindHole val x: X` | `match(quoted.Bind[X](a.sym)) && '{val a: A} =#= '{val x: X}`
| Literal `a`               | Literal `x`                 | `matched` if value of `a` is equal to the value of `x`
| `a`                       | `x`                         | `matched` if `equivalent(a.sym, x.sym)`
| `a.b`                     | `x.y`                       | `'a =#= 'x` if `equivalent(b.sym, y.sym)`
| `a: A`                    | `x: X`                      | `'a =#= 'x && '[A] =#= '[X]`
| `fa(.. ai ..)`            | `fx(.. xi ..)`              | `'fa =#= 'fx && fold(.. 'ai =#= 'xi) ..)`
| `fa[.. Ai ..]`            | `fx[.. Xi ..]`              | `'fa =#= 'fx && fold(.. '[Ai] =#= '[Xi] ..)`
| `{.. ai ..}`              | `{.. xi ..}`                | `fold(.. 'ai =#= 'xi ..)`
| `if (a) b else c`         | `if (x) y else z`           | `'a =#= 'x &&  'b =#= 'y && 'c =#= 'z`
| `while (a) b`             | `while (x) y`               | `'a =#= 'x &&  'b =#= 'y`
| Assignment `a = b`        | Assignment `x = y`          | `'b =#= 'y` if `'a =#= 'x.nonEmpty`
| Named argument<br>`n = a` | Named argument<br>`m = x`   | `'a =#= 'x`
| `Seq(.. ai ..): _*`       | `Seq(.. xi ..): _*`         | `fold(.. 'ai =#= 'xi ..)`
| `new A`                   | `new X`                     | `'[A] =#= '[X]`
| `this`                    | `this`                      | `matched` if both refer to the same symbol
| `a.super[B]`              | `x.super[Y]`                | `'a =#= 'x` if `B` equals `Y`
| `val a: A = b`<br>`lazy val a: A = b`<br>`var a: A = b` | `val x: X = y`<br>`lazy val x: X = y`<br>`var x: X = y`              | `'[A] =#= '[X] && 'b =#= 'y given envWith(a.sym -> b.sym)`
| `def a[..Ai..](.. bij: Bij ..): A = c` | `def x[..Xi..](.. yij: Yij ..): X = z` | `fold(..'[Ai] =#= '[Xi]..) && fold(.. 'bij =#= 'yij && '[Bij] =#= '[Yij] ..) && '[A] =#= '[X] && 'c =#= 'z given envWith(a.sym -> b.sym, .. bij.sym -> yij.sym ..)`
| `(.. ai: Ai ..) => b` | `(.. xi: Xi ..) => y` | `fold(.. 'ai =#= 'xi && '[Ai] =#= '[Xi] ..) && 'b =#= 'y given envWith(.. ai.sym -> xi.sym ..)`
| `a match { .. bi .. }`    | `x match { .. yi .. }`   | `'a =#= 'x && fold(.. 'bi =#= 'yi ..)`
| `try a catch { .. bi .. } finally ci`    | `try x catch { .. yi .. } finally z`   | `'a =#= 'x && fold(.. 'bi =#= 'yi ..) && 'c =#= 'z`
|                           |                          |
| `case a if b => c`        | `case x if y => z`       | `'a =#= 'x && 'b =#= 'y && c =#= z`
|                           |                          |
| Inferred `A`              | Inferred `X`             | `matched` if `A <:< X`
| `A[.. Bi ..]`             | `X[.. Yi ..]`            | `matched` if `('[A] && '[X] && fold(.. '[Bi] =#= '[Yi] ..)).nonEmpty`
| `A @annot`                | `X`                      | `'[A] =#= '[X]`
| `A`                       | `X @annot`               | `'[A] && '[X]`
|                           |                          | `notMatched`


| Pattern inside the quote    | Pattern                     | Returns        |
| :-------------------------: |:--------------------------: | :------------- |
| Value `a`                   | Value `x`                   | `'a =#= 'x`
| `a: A`                      | `x: X`                      | `'[A] && '[X]`
| `a @ b`                     | `x @ y`                     | `'b =#= 'y given envWith(a.sym -> b.sym)`
| Unapply `a(..bi..)(..ci..)` | Unapply `x(..yi..)(..zi..)` | `'a =#= 'x && fold(.. 'bi =#= 'yi ..) && fold(.. 'ci =#= 'zi ..)`
| `.. | ai | ..`              | `.. | xi | ..`              | `fold(.. 'ai =#= 'xi ..)`
| `_`                         | `_`                         | `matched`
|                             |                             | `notMatched`

<!-- TODO spec for the environment from patterns propagated to the result -->


## Quoted Patterns transformation

Coming soon...
