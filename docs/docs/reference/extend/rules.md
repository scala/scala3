---
layout: doc-page
title: "Syntax and Type Checking"
---

This section summarizes syntax changes and gives an outline of envisaged type checking rules. The type checking part is very provisional since details will probably depend on the
implemented encoding, which is not decided yet (see next section)

## Syntax Changes

The syntax of extensions is specified below as a delta with respect to the Scala syntax given [here](http://dotty.epfl.ch/docs/internals/syntax.html)

New Keywords:

    extension       common

New Productions:

    TmplDef           ::=  ...
                        |  ‘extension’ ExtensionDef
    ExtensionDef      ::=  id [ExtensionParams] 'for' Type ExtensionClause
    ExtensionParams   ::=  [ClsTypeParamClause] [[nl] ImplicitParamClause]
    ExtensionClause   ::=  [`:` Template]
                        |  [nl] ‘{’ ‘def’ DefDef {semi ‘def’ DefDef} ‘}’

    Modifier          ::=  ...
                        |  ‘common’
    Path              ::=  ...
                        |  Path ‘.’ ‘common’


## Type Checking Outline

Here's an outline of the type checking rules and the auxiliary definitions implicitly added to programs. To keep things simple, this section only talks about the first-order case.
The section on "Parameterized Typeclass Traits" talks a bit how to extend this to higher-kinds, and the[typeclass encoding test case](https://github.com/dotty-staging/dotty/blob/add-common/tests/pos/typeclass-encoding3.scala) provides further examples.

### TypeClass Traits

There is a new marker trait `scala.TypeClass`. Traits and classes inheriting directly or indirectly from `TypeClass` are called _typeclass traits_. They differ as follows from normal traits:

 - They can have `common` definitions
 - They can refer to the type `This`

The companion object of a typeclass trait `TC` contains two compiler-generated members

 - A type `Impl[T]` mapping an implementation type `T` of the trait to the type of the
   corresponding implementation object, which coincides with `T`'s `common` part.

 - A method `impl[T]` returning the implementation object of `T` for `TC`.

### The `This` Type

The `This` type is modeled as an abstract type. It is as if each typeclass trait
was augmented with the following definition:

    common type This

Typeclass traits have an implicit conversion in scope that converts `This` to an
instance of the typeclass trait.

### Rules for Common

Common definitions cannot see instance members of their enclosing trait. Common definitions in classes (but not traits) _can_ see the type parameters of the enclosing class.

A Typeclass trait contains an instance member `common` that refers to the object containing the `common` members of the trait. If `p` refers to a typeclass trait, then `p.common` refers to this object.

### Typeclass Implementation

An implementation of a typeclass trait is a class or object that extends the trait (possibly that implementation is generated from an extension clause).
An implementation `C` fixes the type of `This` to

    type This = C

unless `C` extends another implementation class `B` (in this case `B` has already defined `This`.)

### Injections

There is a new standard type `scala.Injectable[T, U]` that allows to abstract over implicit conversions similar to how `scala.<:<` abstracts over subtyping. The `Impl` types
in typeclass traits inherit this type. An extension clause
```scala
extension X for C : T { ... }
```
creates an instance of `Injectable[C, T]` (which is also an `Impl` in the case where `T` is a typeclass trait). There is a standard summon of `Injectable[T, U]` if `T` is a subtype of `U`.

Finally, there is a global implicit conversion that uses `Injectable`:

```scala
  implicit def $inject[From, To](x: From)(implicit ev: Injectable[From, To]: To
```

### Context Bounds

A context bound `[T: B]` expands to an implicit evidence parameter of one of the following types:

 - If `B` has one parameter more than `T`: `B[T]` (this is the previous meaning of context bounds)
 - Otherwise, if `B` is a typeclass trait: `B.Impl[T]`
 - Otherwise, if `B` is a standard trait: `Injectable[T, B]`

