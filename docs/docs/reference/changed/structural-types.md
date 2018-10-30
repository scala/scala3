---
layout: doc-page
title: "Programmatic Structural Types"
---

## Syntax

```
SimpleType    ::= ... | Refinement
Refinement    ::= ‘{’ RefineStatSeq ‘}’
RefineStatSeq ::=  RefineStat {semi RefineStat}
RefineStat    ::= ‘val’ VarDcl | ‘def’ DefDcl | ‘type’ {nl} TypeDcl
```

## Motivation

Some usecases, such as modelling database access, are more awkward in
statically typed languages than in dynamically typed languages: With
dynamically typed languages, it's quite natural to model a row as a
record or object, and to select entries with simple dot notation (e.g.
`row.columnName`).

Achieving the same experience in statically typed
language requires defining a class for every possible row arising from
database manipulation (including rows arising from joins and
projections) and setting up a scheme to map between a row and the
class representing it.

This requires a large amount of boilerplate, which leads developers to
trade the advantages of static typing for simpler schemes where colum
names are represented as strings and passed to other operators (e.g.
`row.select("columnName")`). This approach forgoes the advantages of
static typing, and is still not as natural as the dynamically typed
version.

Structural types help in situations where we would like to support
simple dot notation in dynamic contexts without losing the advantages
of static typing. They allow developers to use dot notation and
configure how fields and methods should be resolved.

An [example](#example) is available at the end of this document.

## Implementation of structural types

The standard library defines a trait `Selectable` in the package
`scala`, defined as follows:

```scala
trait Selectable extends Any {
  def selectDynamic(name: String): Any
  def selectDynamicMethod(name: String, paramClasses: ClassTag[_]*): Any =
    new UnsupportedOperationException("selectDynamicMethod")
}
```

An implementation of `Selectable` that relies on Java reflection is
available in the standard library: `scala.reflect.Selectable`. Other
implementations can be envisioned for platforms where Java reflection
is not available.

`selectDynamic` takes a field name and returns the value associated
with that name in the `Selectable`. Similarly, `selectDynamicMethod`
takes a method name, `ClassTag`s representing its parameters types and
will return the function that matches this
name and parameter types.

Given a value `v` of type `C { Rs }`, where `C` is a class reference
and `Rs` are refinement declarations, and given `v.a` of type `U`, we
consider three distinct cases:

- If `U` is a value type, we map `v.a` to the equivalent of:
  ```scala
  v.a
     --->
  (v: Selectable).selectDynamic("a").asInstanceOf[U]
  ```

- If `U` is a method type `(T1, ..., Tn) => R` with at most 7
  parameters and it is not a dependent method type, we map `v.a` to
  the  equivalent of:
  ```scala
  v.a
     --->
  (v: Selectable).selectDynamic("a", CT1, ..., CTn).asInstanceOf[(T1, ..., Tn) => R]
  ```

- If `U` is neither a value nor a method type, or a dependent method
  type, or has more than 7 parameters, an error is emitted.

We make sure that `r` conforms to type `Selectable`, potentially by
introducing an implicit conversion, and then call either
`selectDynamic` or `selectMethodDynamic`, passing the name of the
member to access and the class tags of the formal parameters, in the
case of a method call. These parameters could be used to disambiguate
one of several overload variants in the future, but overloads are not
supported in structural types at the moment.

## Extensibility

New instances of `Selectable` can be defined to support means of
access other than Java reflection, which would enable usages such as
the database access example given in the "Motivation" section.

## Limitations of structural types

- Methods with more than 7 formal parameters cannot be called via
  structural call.
- Dependent methods cannot be called via structural call.
- Overloaded methods cannot be called via structural call.
- Refinement do not handle polymorphic methods.

## Differences with Scala 2 structural types

- Scala 2 supports structural types by means of Java reflection. Unlike
Scala 3, structural calls do not rely on a mechanism such as
`Selectable`, and reflection cannot be avoided.
- In Scala 2, structural calls to overloaded methods are possible.
- In Scala 2, mutable `var`s are allowed in refinements. In Scala 3,
  they are no longer allowed.

## Migration

Receivers of structural calls need to be instances of `Selectable`. A
conversion from `Any` to `Selectable` is available in the standard
library, in `scala.reflect.Selectable.reflectiveSelectable`. This is
similar to the implementation of structural types in Scala 2.

## Relation with `scala.Dynamic`

There are clearly some connections with `scala.Dynamic` here, since
both select members programmatically. But there are also some
differences.

- Fully dynamic selection is not typesafe, but structural selection
  is, as long as the correspondence of the structural type with the
  underlying value is as stated.

- `Dynamic` is just a marker trait, which gives more leeway where and
  how to define reflective access operations. By contrast
  `Selectable` is a trait which declares the access operations.

- One access operation, `selectDynamic` is shared between both
  approaches, but the other access operations are
  different. `Selectable` defines a `selectDynamicMethod`, which
  takes class tags indicating the method's formal parameter types as
  additional argument. `Dynamic` comes with `applyDynamic` and
  `updateDynamic` methods, which take actual argument values.

## Example

<script src="https://scastie.scala-lang.org/Duhemm/HOZFKyKLTs294XOSYPU5Fw.js"></script>

## Reference

For more info, see [Rethink Structural
Types](https://github.com/lampepfl/dotty/issues/1886).
