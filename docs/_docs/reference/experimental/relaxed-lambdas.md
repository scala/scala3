---
layout: doc-page
title: "Relaxed Lambda Syntax"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/relaxed-lambdas.html
---

# Relaxed Lambda Syntax

This experimental addition combines several small improvements to write function literals in more flexible ways. These improvements are specified in
[SIP 74](https://github.com/scala/improvement-proposals/pull/113) and
[SIP 75](https://github.com/scala/improvement-proposals/pull/118).
They are enabled by the experimental language import
```scala
import language.experimental.relaxedLambdaSyntax
```

## Single Line Lambdas

Lambda expression following a `:` on the same line are now supported. Previously,
we needed a newline and indent after the arrow, e.g.
```scala
xs.map: x =>
  x + 1
```
We now also allow to write the lambda on a single line:
```scala
xs.map: x => x + 1
```
The lambda extends in this case to the end of the line.

The syntax works for all kinds of function literals. They can start with one or more parameters, or with type parameters, or they can be partial functions starting
with `case`.

```scala
Seq((1, 2), (3, 4)).map: (a, b) => a + b

Seq((1, 2), (3, 4)).map: (a: Int, b: Int) => a + b

Seq((1, 2), (3, 4)).collect: case (a, b) if b > 2 = a

(1, true).map: [T] => (x: T) => List(x)
```

The syntax does not work for function values that do not contain a `=>` or `?=>`. For instance the following are illegal.

```scala
Seq((1, 2), (3, 4)).map: _ + _ // error

Seq(1, 2, 3).map: plus1 // error
```

Single-line lambdas can be nested, as in:
```scala
  xs.map: x => x.toString + xs.dropWhile: y => y > 0
```

### Detailed Spec

A `:` means application if its is followed by one of the following:

 1. a line end and an indented block,
 2. a parameter section, followed by `=>` or `?=>`, a line end and an indented block,
 3. a parameter section, followed by `=>` or `?=>` and an expression on a single line,
 4. a case clause, representing a single-case partial function.

(1) and (2) is the status quo, (3) and (4) are new.

**Restriction:** (3) and (4) do not apply in code that is immediately enclosed in parentheses (without being more closely enclosed in braces or indentation). This is to avoid an ambiguity with type ascription. For instance,
```scala
(
  x: Int => Int
)
```
still means type ascription, no interpretation as function application is attempted.

## Curried Multi-Line Lambdas

Previously, we admitted only a single parameter section and an arrow before
an indented block. We now also admit multiple such sections. So the following
is now legal:

```scala
def fun(f: Int => Int => Int): Int = f(1)(2)

fun: (x: Int) => y =>
  x + y
```

In the detailed spec above, point (2) is modified as follows:

2. _one or more_ parameter sections, _each_ followed by `=>` or `?=>`, and finally a line end and an indented block.

## Case Expressions

Previously, case clauses making up a partial function had to be written in
braces or an indented block. If there is only a single case clause, we now allow it to be written also inside parentheses or as a top-level expression.

Examples:

```scala
case class Pair(x: Int, y: Int)

Seq(Pair(1, 2), Pair(3, 4)).collect(case Pair(a, b) if b > 2 => a)

Seq(Pair(1, 2), Pair(3, 4)).collect(
  case (a, b) =>
    println(b)
    a
)

val partial: PartialFunction[(Int, Int), Int] = case (a, b) if b > 2 => a
```

## Syntax Changes

```
Expr          ::= ...
               |  ExprCaseClause

ColonArgument ::= colon {LambdaStart} indent (CaseClauses | Block) outdent
               |  colon LambdaStart {LambdaStart} expr ENDlambda
               |  colon ExprCaseClause
```
Here, ENDlambda is a token synthesized at the next end of line following the
token that starts the production.

`ExprCaseClause` already exists in the grammar. It is defined as follows:
```
ExprCaseClause  ::=  ‘case’ Pattern [Guard] ‘=>’ Expr
```
