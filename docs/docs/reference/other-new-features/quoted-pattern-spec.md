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

def noMatch = None
def emptyMatch = Some(()) // aka Some(Tuple0())
def match[T](x: T) = Some(Tuple1(x))
def (x: Matching) ++ (y: Matching) = if (x == None || y == None) None else Some(x.get ++ y.get)
def fold[T](m: Mattching*) given Env: Matching = m.fold(emptyMatch)(_ ++ _)

def matches(scrutinee: Tree, pattern: Tree) given Env: Matching // described by cases in the tables below

def envWith(equiv: (Symbol, Symbol)*) given Env: Env // Adds to the current environment the fact that s1 from the scrutinee is equivalent to s2 in the pattern

def equivalent(s1: Symbol, s2: Symbol) given Env: Env
```

The implementation of `matches`

| Tree                      | Pattern                     | Returns     |
| :-----------------------: | :-------------------------: | :---------- |
| Term `a`                  | `patternHole[X]`            | `match(quoted.Expr[X]('a))` if type of `a` is a subtype of `X`
| `val a: A`                | `@patternBindHole val x: X` | `match(quoted.Bind[X](a.sym)) ++ matches('{val a: A}, '{val x: X})`
| Literal `a`               | Literal `x`                 | `emptyMatch` if value of `a` is equal to the value of `x`
| `a`                       | `x`                         | `emptyMatch` if `equivalent(a.sym, x.sym)`
| `a.b`                     | `x.y`                       | `matches('a, 'x)` if `equivalent(b.sym, y.sym)`
| `a: A`                    | `x: X`                      | `matches('a, 'x) ++ matches('[A], '[X])`
| `fa(.. ai ..)`            | `fx(.. xi ..)`              | `matches('fa, 'fx) ++ fold(.. matches('ai, 'xi) ..)`
| `fa[.. Ai ..]`            | `fx[.. Xi ..]`              | `matches('fa, 'fx) ++ fold(.. matches('[Ai], '[Xi]) ..)`
| `{.. ai ..}`              | `{.. xi ..}`                | `fold(.. matches('ai, 'xi) ..)`
| `if (a) b else c`         | `if (x) y else z`           | `matches('a, 'x) ++  matches('b, 'y) ++ matches('c, 'z)`
| `while (a) b`             | `while (x) y`               | `matches('a, 'x) ++  matches('b, 'y)`
| Assignment `a = b`        | Assignment `x = y`          | `matches('b, 'y)` if `matches('a, 'x).nonEmpty`
| Named argument<br>`n = a` | Named argument<br>`m = x`   | `matches('a, 'x)`
| `Seq(.. ai ..): _*`       | `Seq(.. xi ..): _*`         | `fold(.. matches('ai, 'xi) ..)`
| `new A`                   | `new X`                     | `matches('[A], '[X])`
| `this`                    | `this`                      | `emptyMatch` if both refer to the same symbol
| `a.super[B]`              | `x.super[Y]`                | `matches('a, 'x)` if `B` equals `Y`
| `val a: A = b`<br>`lazy val a: A = b`<br>`var a: A = b` | `val x: X = y`<br>`lazy val x: X = y`<br>`var x: X = y`              | `matches('[A], '[X]) ++ matches('b, 'y) given envWith(a.sym -> b.sym)`
| `def a[..Ai..](.. bij: Bij ..): A = c` | `def x[..Xi..](.. yij: Yij ..): X = z` | `fold(..matches('[Ai], '[Xi])..) ++ fold(.. matches('bij, 'yij) ++ matches('[Bij], '[Yij]) ..) ++ matches('[A], '[X]) ++ matches('c, 'z) given envWith(a.sym -> b.sym, .. bij.sym -> yij.sym ..)`
| `(.. ai: Ai ..) => b` | `(.. xi: Xi ..) => y` | `fold(.. matches('ai, 'xi) ++ matches('[Ai], '[Xi]) ..) ++ matches('b, 'y) given envWith(.. ai.sym -> xi.sym ..)`
| `a match { .. bi .. }`    | `x match { .. yi .. }`   | `matches('a, 'x) ++ fold(.. matches('bi, 'yi) ..)`
| `try a catch { .. bi .. } finally ci`    | `try x catch { .. yi .. } finally z`   | `matches('a, 'x) ++ fold(.. matches('bi, 'yi) ..) ++ matches('c, 'z)`
|                           |                          |
| `case a if b => c`        | `case x if y => z`       | `matches('a, 'x) ++ matches('b, 'y) ++ matches(c, z)`
|                           |                          |
| Inferred `A`              | Inferred `X`             | `emptyMatch` if `A <:< X`
| `A[.. Bi ..]`             | `X[.. Yi ..]`            | `emptyMatch` if `(matches('[A], '[X]) ++ fold(.. matches('[Bi], '[Yi]) ..)).nonEmpty`
| `A @annot`                | `X`                      | `matches('[A], '[X])`
| `A`                       | `X @annot`               | `matches('[A], '[X])`
|                           |                          | `noMatch`


| Pattern inside the quote    | Pattern                     | Returns        |
| :-------------------------: |:--------------------------: | :------------- |
| Value `a`                   | Value `x`                   | `matches('a, 'x)`
| `a: A`                      | `x: X`                      | `matches('[A], '[X])`
| `a @ b`                     | `x @ y`                     | `matches('b, 'y) given envWith(a.sym -> b.sym)`
| Unapply `a(..bi..)(..ci..)` | Unapply `x(..yi..)(..zi..)` | `matches('a, 'x) ++ fold(.. matches('bi, 'yi) ..) ++ fold(.. matches('ci, 'zi) ..)`
| `.. | ai | ..`              | `.. | xi | ..`              | `fold(.. matches('ai, 'xi) ..)`
| `_`                         | `_`                         | `emptyMatch`
|                             |                             | `noMatch`

<!-- TODO spec for the environment from patterns propagated to the result -->


## Quoted Patterns transformation

Coming soon...
