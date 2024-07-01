---
layout: doc-page
title: "Generalized Method Syntax"
nightlyOf: https://docs.scala-lang.org/scala3/reference/other-new-features/generalized-method-syntax.html
---

The inclusion of using clauses is not the only way in which methods have been updated, type parameter clauses are now allowed in any number and at any position.

## Syntax Changes

### In Scala 2

The old syntax only allowed zero or one type parameter clause, followed by any number of term clauses, optionnally followed by an implicit clause:

```scala
def foo[T, U](x: T)(y: U)(z: Int, s: String)(a: Array[T])(implicit ordInt: Ord[Int], l: List[U])
```

### In Scala 3

The new syntax allows any number of type clauses, as long as they are not adjacent:
(do note however that [implicit clause are discouraged, in favor of using clauses](https://docs.scala-lang.org/scala3/reference/contextual/relationship-implicits.html))

```scala
def foo[T, U](x: T)(y: U)[V](z: V, s: String)(using Ord[Int])[A](a: Array[A])(implicit List[U])
```

### Unchanged

Class definitions and type declarations are unaffected, there can only be up to one type clause, in leading posion.

## Motivation

The new syntax is a powerful but natural extension of the old one, it allows new design patterns while staying intuitive and legible.

### Dependent Type Clauses

As type clauses can come after term clauses, it is now possible to have type parameters that depend on term parameters:

```scala
trait Key { type Value }
trait DB {
  def get(k: Key): Option[k.Value] // dependent result type
  def getOrElse(k: Key)[V >: k.Value](default: V): V // dependent type parameter
}
```

Note that simply replacing `V` by `k.Value` would not be equivalent. For example, if `k.Value` is `Some[Int]`, only the above allows:
`getOrElse(k)[Option[Int]](None)`, which returns a `Number`.

## Details

### Application

Method application is unchanged.
When multiple type clauses are expected but not all are passed, the rightmost ones are inferred.

In particular, the following does not type check, even though the argument `Char` is only valid for `C`:
```scala
def triple[I <: Int](using Ordering[I])[C <: Char](a: I, b: C) = ???
triple[Char](0, 'c') // error: Char does not conform to upperbound Int
```

### Extension Methods

Extension methods follow the same syntax, for example the following is valid:
```scala
extension [T](l1: List[T])
  def zipWith[U](l2: List[U])[V](l3: List[V]): List[(T,U,V)]
```

### When to use

We recommand to always put a unique type clause at the beginning, unless it is not possible to do so.
For example, the extension method `zipWith` above should be written `zipWith[U, V](l2: List[U], l3: List[V]): List[(T,U,V)]` instead.
On the other hand, the `getOrElse` method is recommended as-is, as it cannot be written with a leading type clause.

### Formal syntax

```
DefDcl            ::=  DefSig ‘:’ Type
DefDef            ::=  DefSig [‘:’ Type] ‘=’ Expr
DefSig            ::=  id [DefParamClauses] [DefImplicitClause]
DefParamClauses   ::=  DefParamClause { DefParamClause } -- and two DefTypeParamClause cannot be adjacent
DefParamClause    ::=  DefTypeParamClause
                    |  DefTermParamClause
                    |  UsingParamClause
DefTypeParamClause::=  [nl] ‘[’ DefTypeParam {‘,’ DefTypeParam} ‘]’
DefTypeParam      ::=  {Annotation} id [HkTypeParamClause] TypeParamBounds
DefTermParamClause::=  [nl] ‘(’ [DefTermParams] ‘)’
UsingParamClause  ::=  [nl] ‘(’ ‘using’ (DefTermParams | FunArgTypes) ‘)’
DefImplicitClause ::=  [nl] ‘(’ ‘implicit’ DefTermParams ‘)’
DefTermParams     ::=  DefTermParam {‘,’ DefTermParam}
DefTermParam      ::=  {Annotation} [‘inline’] Param
Param             ::=  id ‘:’ ParamType [‘=’ Expr]
```
