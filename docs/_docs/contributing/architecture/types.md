---
layout: doc-page
title: Compiler Types
---

## Common Types and their Representation

Type representations in `dotc` derive from the class `dotty.tools.dotc.core.Types.Type`,
defined in [Types.scala]. The `toString` method on `Type` will display types in a
format corresponding to the backing data structure, e.g. `ExprType(...)`
corresponds to `class ExprType`, defined in [Types.scala].

> You can inspect the representation of any type using the [dotty.tools.printTypes][DottyTypeStealer]
> script, its usage and integration into your debugging workflow is [described here](../debugging/inspection.md).

### Types of Definitions

The following table describes definitions in Scala 3, followed by the `dotc` representation
of two types - a reference to the definition, and then its underlying type.

**Note**: in the following types, `p` refers to the self-type of the enclosing scope of
the definition, or `NoPrefix` for local definitions and parameters.

Definition              | Reference       | Underlying Type
------------------------|-----------------|-------------------------
`type Z >: A <: B`      | `TypeRef(p, Z)` | `RealTypeBounds(A, B)`
`type Z = A`            | `TypeRef(p, Z)` | `TypeAlias(A)`
`type F[T] = T match …` | `TypeRef(p, F)` | `MatchAlias([T] =>> T match …)`
`class C`               | `TypeRef(p, C)` | `ClassInfo(p, C, …)`
`trait T`               | `TypeRef(p, T)` | `ClassInfo(p, T, …)`
`object o`              | `TermRef(p, o)` | `TypeRef(p, o$)` where `o$` is a class
`def f(x: A): x.type`   | `TermRef(p, f)` | `MethodType(x, A, TermParamRef(x))`
`def f[T <: A]: T`      | `TermRef(p, f)` | `PolyType(T, <: A, TypeParamRef(T))`
`def f: A`              | `TermRef(p, f)` | `ExprType(A)`
`(x: => A)`             | `TermRef(p, x)` | `ExprType(A)` where `x` is a parameter
`val x: A`              | `TermRef(p, x)` | `A`

### Types of Values

The following types may appear in part of the type of an expression:

Type                      | Representation
--------------------------|------------------------------
`x.y.type`                | `TermRef(x, y)`
`X#T`                     | `TypeRef(X, T)`
`x.y.T` and `x.y.type#T`  | `TypeRef(TermRef(x, y), T)`
`this.type`               | `ThisType(C)` where `C` is the enclosing class
`"hello"`                 | `ConstantType(Constant("hello"))`
`A & B`                   | `AndType(A, B)`
`A | B`                   | `OrType(A, B)`
`A @foo`                  | `AnnotatedType(A, @foo)`
`[T <: A] =>> T`          | `HKTypeLambda(T, <: A, TypeParamRef(T))`
`x.C[A, B]`               | `AppliedType(x.C, List(A, B))`
`C { type A = T }`        | `RefinedType(C, A, T)`<br/>when `T` is not a member of `C`
`C { type X = Y }`        | `RecType(RefinedType(C, X, z.Y))`<br/>when `X` and `Y` are members of `C`<br/>and `z` is a `RecThis` over the enclosing `RecType`
`super.x.type`            | `TermRef(SuperType(…), x)`

## Constructing Types

### Method Definition Types

You can see above that method definitions can have an underlying type of
either `PolyType`, `MethodType`, or `ExprType`. `PolyType` and `MethodType`
may be mixed recursively however, and either can appear as the result type of the other.

Take this example as given:

```scala
def f[A, B <: Seq[A]](x: A, y: B): Unit
```
it can be constructed by the following code:

```scala
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*

given Context = … // contains the definitions of the compiler

val f: Symbol = … // def f[A, B <: Seq[A]](x: A, y: B): Unit

f.info = PolyType(
  List("A".toTypeName, "B".toTypeName))(
  pt => List(
    TypeBounds(defn.NothingType, defn.AnyType),
    TypeBounds(defn.NothingType, AppliedType(defn.SeqType, List(pt.newParamRef(0))))
  ),
  pt => MethodType(
    List("x".toTermName, "y".toTermName))(
    mt => List(pt.newParamRef(0), pt.newParamRef(1)),
    mt => defn.UnitType
  )
)
```

Note that `pt.newParamRef(0)` and `pt.newParamRef(1)` refers to the
type parameters `A` and `B` respectively.

## Proxy Types and Ground Types
Types in `dotc` are divided into two semantic kinds:
- Ground Types (inheriting from either `CachedGroundType` or `UncachedGroundType`)
- Proxy Types (inheriting from `TypeProxy` via either `CachedProxyType` or `UncachedProxyType`)

A Proxy Type is anything that can be considered to be an abstraction of another type,
which can be accessed by the `underlying` method of the `TypeProxy` class. It's dual, the
Ground Type has no meaningful underlying type, typically it is the type of method and class
definitions, but also union types and intersection types, along with utility types of the
compiler.

Here's a diagram, serving as the mental model of the most important and distinct types available after the `typer` phase, derived from [Types.scala]:

```
Type -+- proxy_type --+- NamedType --------+- TypeRef
      |               |                     \
      |               +- SingletonType ----+- TermRef
      |               |                    +- ThisType
      |               |                    +- SuperType
      |               |                    +- ConstantType
      |               |                    +- TermParamRef
      |               |                    +- RecThis
      |               |                    +- SkolemType
      |               +- TypeParamRef
      |               +- RefinedOrRecType -+-- RefinedType
      |               |                   -+-- RecType
      |               +- AppliedType
      |               +- TypeBounds
      |               +- ExprType
      |               +- AnnotatedType
      |               +- TypeVar
      |               +- HKTypeLambda
      |               +- MatchType
      |
      +- ground_type -+- AndType
                      +- OrType
                      +- MethodOrPoly -----+-- PolyType
                      |                    +-- MethodType
                      +- ClassInfo
                      +- NoType
                      +- NoPrefix
                      +- ErrorType
                      +- WildcardType

```

[Types.scala]: https://github.com/lampepfl/dotty/blob/master/compiler/src/dotty/tools/dotc/core/Types.scala
[DottyTypeStealer]: https://github.com/lampepfl/dotty/blob/master/compiler/test/dotty/tools/DottyTypeStealer.scala
