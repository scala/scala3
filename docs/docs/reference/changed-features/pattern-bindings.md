---
layout: doc-page
title: "Pattern Bindings"
---

In Scala 3 pattern definitions can carry ascriptions such as `: @unchecked`, and `for` expressions can prefix `case` when filter.

## Pattern Definitions Ascriptions
Pattern definitions can have ascriptions at the end of them.
`val first :: rest : @unchecked = elems`

## Pattern Bindings in `for` Expressions

The filtering functionality can be obtained in Scala 3 by prefixing the pattern with `case`:
```scala
  val elems: List[Any] = List((1, 2), "hello", (3, 4))
  for (case (x, y) <- elems) yield (y, x)  // returns List((2, 1), (4, 3))
```

## Syntax Changes

There are two syntax changes relative to Scala 2: First, pattern definitions can carry ascriptions such as `: @unchecked`. Second, generators in for expressions may be prefixed with `case`.
```
PatDef         ::=  ids [‘:’ Type] ‘=’ Expr
                 |  Pattern2 [‘:’ Type | Ascription] ‘=’ Expr
Generator      ::=  [‘case’] Pattern1 ‘<-’ Expr
```
