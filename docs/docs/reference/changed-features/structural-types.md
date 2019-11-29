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

```scala
object StructuralTypeExample {

  case class Record(elems: (String, Any)*) extends Selectable {
    def selectDynamic(name: String): Any = elems.find(_._1 == name).get._2
  }

  type Person = Record {
    val name: String
    val age: Int
  }

  def main(args: Array[String]): Unit = {
    val person = Record("name" -> "Emma", "age" -> 42).asInstanceOf[Person]
    println(s"${person.name} is ${person.age} years old.")
    // Prints: Emma is 42 years old.
  }
}
```

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

- Two access operations, `selectDynamic` and `applyDynamic` are shared
  between both approches. In `Selectable`, `applyDynamic` also takes
  `ClassTag` indicating the method's formal parameter types. `Dynamic`
  comes with `updateDynamic`.

## Type Members in Structural Types

A second change to structural types concerns type definitions that appear in them.
Scala-2 allowed new type members in refinement types where the type members do not
override or refine anything in the parent type. Examples are:

```scala
Seq { type Helper = U}   // `Seq` does not contain a definition of `Helper`
{ type T }
```

These constructs are now available only under language feature `newTypesInRefinements`. As usual the feature
can be enabled by an import
```scala
import language.newTypesInRefinements
```
or by a command-line option `-language:newTypesInRefinements`. If the feature is not enabled the compiler will
issue a feature warning.

### Why Put The Construct Under a Flag?

Idiomatic Scala allows to refine only existing member types. This leads to code that is better documented
and easier to typecheck. The support in the Scala compiler for this feature is less mature than the rest of the type checker.

### Why Not Drop it Entirely?

New type members in refinements are allowed by DOT,
the calculus that underpins Scala. However, it should be noted that DOT
does not provide a decision algorithm that can be used for type checking. So
having only second-class support for the construct is defensible.


[More details](structural-types-spec.md)
