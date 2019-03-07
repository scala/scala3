---
layout: doc-page
title: "Principled Meta Programming - Pattern matching"
---

## Overview

Quote pattern matching allows to match a quote against another quote while extracting the contents of defined holes. This will be used in as desugaring of quoted patterns.

```scala
def optimize(expr: Expr[Int]): Expr[Int] = expr match {
  case '{ 0 + $y } => optimize(y)
  case '{ ${Literal(a)} + ${Literal(b)} } => (a + b).toExpr
  case '{ ${x} + ${y @ Literal(_)} } => optimize('{ $y + $x })
  ...
  case expr => expr
}
```

Where `x: Expr[Int]`, `y: Expr[Int]`, `a: Int` and `b: Int`.


### Quote patterns

Quote extractors are patterns of the shape `'{ ... }` that can match an `Expr[T]`.

```scala
(expr: Expr[T]) match {
  case '{ pattern } =>
}
```

They allow to extract expresions, types and identifiers

```scala
(expr: Expr[T]) match {
  case '{ ... val x: T = $rhs ... } =>
  case '{ ... val $y: T = ... } =>
  case '{ ... val z: $t = ... } =>
}
```

Where `x: Expr[T]`, `y: Binging[T]` and `z: Type[T]`. 


#### Desugaring

A pattern with splices such as


```scala
(expr: Expr[T]) match {
  case '{ ... ($x: T) ... val $b: U = ... foo[$t] ... } =>
}
```

is transformed to a call to `Matcher.unapply` with the appropriate types for the splices in a tuple.

```scala
(expr: Expr[T]) match {
  case scala.runtime.quoted.Matcher.unapply
      [... *: Expr[X] *: ... *: Binding[T] *: ... *: Type[V] *: ... *: Unit]
      (... *: x *: ... *: b *: ... *: t *: ... *: ())
      (/*implicits*/ '{ ... hole[T] ... bindHole[U] ... Hole[V] ... }, reflect) =>
}
```


### Other expression patterns

```scala
(expr: Expr[T]) match {
  case Literal(lit) => lit: T
  case Binding(binding) => binding: Binding[T]
  case IsValRef(expr2) => expr2: expr.type
  case IsDefRef(expr) => expr2: expr.type
  
  case '{ ... ${Literal(lit)} ... }
}
```


## Spec

### Quote pattern desugaring

Given a quote pattern `'{ pattern }` the desugaring is defined in terms of a splice types `T<pattern>`, a runtime pattern `P<patern>` and extracted bindings `E<pattern>`.
```scala
Matcher.unapply[T<pattern>](E<pattern>)(/*implicits*/ '{ P<pattern> }, reflect)
```



`<pattern>` is defined recursively in the following way:

| Quote pattern syntax | Matcher pattern | Splice types | Extracted |
| ------------- |:-------------:| ----------- | ----------- |
| `<$x>` where `x: Expr[X]` | `Matcher.hole[X]` | `Tuple1[Expr[T]]` | `Tuple1[Expr[T]](x)` |
| `<$t>` where `x: Type[X]` | `Matcher.HOLE[X]` | `Tuple1[Type[T]]` | `Tuple1[Type[T]](t)` |
| `<$n: X>` where `n` is a name of `val`, `var`, `def` or parameter definition | `n: bindHole[X]` | `Tuple1[Binding[X]]` | `Tuple1[Binding[X]](n)`  |
| `<if (e1) e2 else e3>` | `if (<e1>) <e2> else <e3>` | `Concat[T<e1>, Concat[T<e2>, T<e3>]]` | E<`e1`> ++ E<`e2`> ++ E<`e3`>) |
| `<while (e1) e2>` | `while (<e1>) <e2>` |  | E<`e1`> ++ E<`e2`> |
| `<f[T1, ..., Tn](e1, ..., em)>` | `<f>[<T1>, ..., <Tn>](<e1>, ..., <em>)` |  | E<`f`> ++ E<`T1`> ++ ... ++ E<`Tn`> ++ E<`e1`> ++ ... ++ E<`em`> |
| `<(e1: T1, ..., en: Tn) => body>` | `(<e1: T1>, ..., <en: Tn>) => <body>` |  | E<`e1: T1`> ++ ... ++ E<`en: Tn`> ++ E<`body`> |
| `<lhs = rhs>` | `lhs = <rhs>` |  | E<`rhs`> |
| `<new X>` | `new <X>` |   | E<`X`> |
| `<this>` | `this` |  |  |
| `<qual.super[T]>` | `<qual>.super[<T>]` |  | E<`qual`> ++ E<`T`) |
| `<K[T1, ..., Tn]>` | `<K>[<T1>, ..., <Tn>]` |  | E<`K`> ++ E<`T1`> ++ ... ++ E<`Tn`> |
| `<x>` where `x` is an identifier | `x` |  |  |
| `<T>` where `T` is an identifier | `T` |  |  |


### Quote pattern match runtime

| Match | Condition |
| ------------- |:-------------:|
| matches(`s`, `hole[T]`) for `s` of type `S` | `S <:< T` |
| matches(`T`, `Hole[T]`) | |
| matches(`n: X1`, `n: bindHole[X2]`) | matches(`X1`, `X2`) |
| matches(`if (s1) s2 else s3`, `if (p1) p2 else p3`) | matches(`s1`, `p1`) && matches(`s2`, `p2`) && matches(`s3`,`p3`) |
| matches(`while (s1) s2`, `while (p1) p2`) | matches(`s1`, `p1`) && matches(`s2`, `p2`) |

| Match | Result |
| ------------- |:-------------:|
| result(`s`, `hole[T]`) for `s` of type `S` | `s` as an `Expr[T]` |
| result(`T`, `Hole[T]`) | `T` as a `Type[T]` |
| matches(`n: X1`, `n: bindHole[X2]`) | (`n` as a `Binding[X2]`) ++ result(`X1`, `X2`) |
| result(`if (s1) s2 else s3`, `if (p1) p2 else p3`) | result(`s1`, `p1`) ++ result(`s2`, `p2`) ++ result(`s3`,`p3`) |
| result(`while (s1) s2`, `while (p1) p2`) | result(`s1`, `p1`) ++ result(`s2`, `p2`) |