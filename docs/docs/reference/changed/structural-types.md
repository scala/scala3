---
layout: doc-page
title: "Programmatic Structural Types"
---

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

## Example

<script src="https://scastie.scala-lang.org/Duhemm/HOZFKyKLTs294XOSYPU5Fw.js"></script>

## Extensibility

New instances of `Selectable` can be defined to support means of
access other than Java reflection, which would enable usages such as
the database access example given at the beginning of this document.

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

[More details](structural-types-spec.html)
