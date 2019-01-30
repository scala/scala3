---
layout: doc-page
title: "Implicit Function Types and Closures"
---

An implicit function type describes functions with implicit (context) parameters. Example:
```scala
type Contextual[T] = Context |=> T
```
A value of implicit function type is applied to context arguments, in
the same way a method with context parameters is applied. For instance:
```scala
  implicit val ctx: Context = ...

  def f(x: Int): Contextual[Int] = ...

  f(2) given ctx   // explicit argument
  f(2)             // argument left implicit
```
Conversely, if the expected type of an expression `E` is an implicit
function type `(T_1, ..., T_n) |=> U` and `E` is not already an
implicit function value, `E` is converted to an implicit function value
by rewriting to
```scala
  (x_1: T1, ..., x_n: Tn) |=> E
```
where the names `x_1`, ..., `x_n` are arbitrary. Implicit closures are written
with a `|=>` connective instead of `=>` for normal closures. They differ from normal closures in two ways:

 1. Their parameters are implicit context parameters
 2. Their types are implicit function types.

For example, continuing with the previous definitions,
```scala
  def g(arg: Contextual[Int]) = ...

  g(22)      // is expanded to g(ctx |=> 22)

  g(f(2))    // is expanded to g(ctx |=> f(2) given ctx)

  g(ctx |=> f(22) given ctx) // is left as it is
```
Implicit function types have considerable expressive power. For
instance, here is how they can support the "builder pattern", where
the aim is to construct tables like this:
```scala
  table {
    row {
      cell("top left")
      cell("top right")
    }
    row {
      cell("bottom left")
      cell("bottom right")
    }
  }
```
The idea is to define classes for `Table` and `Row` that allow
addition of elements via `add`:
```scala
  class Table {
    val rows = new ArrayBuffer[Row]
    def add(r: Row): Unit = rows += r
    override def toString = rows.mkString("Table(", ", ", ")")
  }

  class Row {
    val cells = new ArrayBuffer[Cell]
    def add(c: Cell): Unit = cells += c
    override def toString = cells.mkString("Row(", ", ", ")")
  }

  case class Cell(elem: String)
```
Then, the `table`, `row` and `cell` constructor methods can be defined
in terms of implicit function types to avoid the plumbing boilerplate
that would otherwise be necessary.
```scala
  def table(init: Table |=> Unit) = {
    instance t of Table
    init
    t
  }

  def row(init: Row |=> Unit) given (t: Table) = {
    instance r of Row
    init
    t.add(r)
  }

  def cell(str: String) given (r: Row) =
    r.add(new Cell(str))
```
With that setup, the table construction code above compiles and expands to:
```scala
  table { $t: Table |=>
    row { $r: Row |=>
      cell("top left") given $r
      cell("top right") given $r
    } given $t
    row { $r: Row |=>
      cell("bottom left") given $r
      cell("bottom right") given $r
    } given $t
  }
```
### Reference

For more info, see the [blog article](https://www.scala-lang.org/blog/2016/12/07/implicit-function-types.html),
(which uses a different syntax that has been superseded).

[More details](./implicit-function-types-spec.html)
