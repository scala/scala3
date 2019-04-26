---
layout: doc-page
title: "Context Queries"
---

_Context queries_ are functions with (only) inferable parameters.
_Context query types_ are the types of first-class context queries.
Here is an example for a context query type:
```scala
type Contextual[T] = given Context => T
```
A value of context query type is applied to inferred arguments, in
the same way a method with inferable parameters is applied. For instance:
```scala
  instance ctx of Context = ...

  def f(x: Int): Contextual[Int] = ...

  f(2) given ctx   // explicit argument
  f(2)             // argument is inferred
```
Conversely, if the expected type of an expression `E` is a context query
type `given (T_1, ..., T_n) => U` and `E` is not already a
context query literal, `E` is converted to a context query literal by rewriting to
```scala
  given (x_1: T1, ..., x_n: Tn) => E
```
where the names `x_1`, ..., `x_n` are arbitrary. This expansion is performed
before the expression `E` is typechecked, which means that `x_1`, ..., `x_n`
are available as implicits in `E`.

Like query types, query literals are written with a `given` prefix. They differ from normal function literals in two ways:

 1. Their parameters are inferable.
 2. Their types are context query types.

For example, continuing with the previous definitions,
```scala
  def g(arg: Contextual[Int]) = ...

  g(22)      // is expanded to g(given ctx => 22)

  g(f(2))    // is expanded to g(given ctx => f(2) given ctx)

  g(given ctx => f(22) given ctx) // is left as it is
```
### Example: Builder Pattern

Context query types have considerable expressive power. For
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
in terms of query types to avoid the plumbing boilerplate
that would otherwise be necessary.
```scala
  def table(init: given Table => Unit) = {
    instance t of Table
    init
    t
  }

  def row(init: given Row => Unit) given (t: Table) = {
    instance r of Row
    init
    t.add(r)
  }

  def cell(str: String) given (r: Row) =
    r.add(new Cell(str))
```
With that setup, the table construction code above compiles and expands to:
```scala
  table { given $t: Table =>
    row { given $r: Row =>
      cell("top left") given $r
      cell("top right") given $r
    } given $t
    row { given $r: Row =>
      cell("bottom left") given $r
      cell("bottom right") given $r
    } given $t
  }
```
### Example: Postconditions

As a larger example, here is a way to define constructs for checking arbitrary postconditions using an extension method `ensuring`so that the checked result can be referred to simply by `result`. The example combines opaque aliases, context query types, and extension methods to provide a zero-overhead abstraction.

```scala
object PostConditions {
  opaque type WrappedResult[T] = T

  private object WrappedResult {
    def wrap[T](x: T): WrappedResult[T] = x
    def unwrap[T](x: WrappedResult[T]): T = x
  }

  def result[T] given (r: WrappedResult[T]): T = WrappedResult.unwrap(r)

  def (x: T) ensuring [T](condition: given WrappedResult[T] => Boolean): T = {
    instance of WrappedResult[T] = WrappedResult.wrap(x)
    assert(condition)
    x
  }
}

object Test {
  import PostConditions.{ensuring, result}
  val s = List(1, 2, 3).sum.ensuring(result == 6)
}
```
**Explanations**: We use a context query type `given WrappedResult[T] => Boolean`
as the type of the condition of `ensuring`. An argument to `ensuring` such as
`(result == 6)` will therefore have an implicit instance of type `WrappedResult[T]` in
scope to pass along to the `result` method. `WrappedResult` is a fresh type, to make sure
that we do not get unwanted implicit instances in scope (this is good practice in all cases
where given clauses are involved). Since `WrappedResult` is an opaque type alias, its
values need not be boxed, and since `ensuring` is added as an extension method, its argument
does not need boxing either. Hence, the implementation of `ensuring` is as about as efficient
as the best possible code one could write by hand:

    { val result = List(1, 2, 3).sum
      assert(result == 6)
      result
    }

### Reference

For more info, see the [blog article](https://www.scala-lang.org/blog/2016/12/07/implicit-function-types.html),
(which uses a different syntax that has been superseded).

[More details](./query-types-spec.html)
