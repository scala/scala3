---
layout: doc-page
title: "Implicit Function Types"
---

An implicit funciton type describes functions with implicit parameters. Example:

    type Contextual[T] = implicit Context => T

A value of implicit function type is applied to implicit arguments, in
the same way a method with implicit parameters is applied. For instance:

    implicit ctx: Context = ...

    def f(x: Int): Contextual[Int] = ...

    f(2)    // is expanded to f(2)(ctx)

Conversely, if the expected type of an expression `E` is an implicit
function type `implicit (T_1, ..., T_n) => U` and `E` is not already an
implicit function value, `E` is converted to an implicit function value
by rewriting to

    implicit (x_1: T1, ..., x_n: Tn) => E

where the names `x_1`, ..., `x_n` are arbitrary. For example, continuing
with the previous definitions,

    def g(arg: Contextual[Int]) = ...

    g(22)      // is expanded to g { implicit ctx => 22 }

    g(f(2))    // is expanded to g { implicit ctx => f(2)(ctx) }

    g(implicit ctx => f(22)(ctx)) // is left as it is

Implicit function types have considerable expressive power. For
instance, here is how they can support the "builder pattern", where
the aim is to construct tables like this:

    table {
      row {
        cell("top left")
        cell("top right")
      }
      row {
        cell("botttom left")
        cell("bottom right")
      }
    }

The idea is to define classes for `Table` and `Row` that allow
addition of elements via `add`:

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

Then, the `table`, `row` and `cell` constructor methods can be defined
in terms of implicit function types to avoid the plumbing boilerplate
that would otherwise be necessary.

    def table(init: implicit Table => Unit) = {
      implicit val t = new Table
      init
      t
    }

    def row(init: implicit Row => Unit)(implicit t: Table) = {
      implicit val r = new Row
      init
      t.add(r)
    }

    def cell(str: String)(implicit r: Row) =
      r.add(new Cell(str))

With that setup, the table construction code above compiles and expands to:

    table { implicit $t: Table =>
      row { implicit $r: Row =>
        cell("top left")($r)
        cell("top right")($r)
      }($t)
      row { implicit $r: Row =>
        cell("botttom left")($r)
        cell("bottom right")($r)
      }($t)
    }

### Reference

For more info, see the [blog article](https://www.scala-lang.org/blog/2016/12/07/implicit-function-types.html).