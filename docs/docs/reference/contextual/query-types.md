---
layout: doc-page
title: "Contextual Functions"
---

_Contextual functions_ are functions with (only) context
parameters. Their types are _contextual function types_.  Here is an
example of a contextual function type:
```scala
type Executable[T] = given ExecutionContext => T
```
A contextual function is applied to inferred arguments, in
the same way a method with context parameters is applied. For instance:
```scala
  implied ec for ExecutionContext = ...

  def f(x: Int): Executable[Int] = ...

  f(2) given ec    // explicit argument
  f(2)             // argument is inferred
```
Conversely, if the expected type of an expression `E` is a contextual function
type `given (T_1, ..., T_n) => U` and `E` is not already a
contextual lambda, `E` is converted to a contextual lambda by rewriting to
```scala
  given (x_1: T1, ..., x_n: Tn) => E
```
where the names `x_1`, ..., `x_n` are arbitrary. This expansion is performed
before the expression `E` is typechecked, which means that `x_1`, ..., `x_n`
are available as implied instances in `E`.

Like their types, contextual lamndas are written with a `given` prefix. They differ from normal lambdas in two ways:

 1. Their parameters are defined with a given clause.
 2. Their types are contextual function types.

For example, continuing with the previous definitions,
```scala
  def g(arg: Executable[Int]) = ...

  g(22)      // is expanded to g(given ev => 22)

  g(f(2))    // is expanded to g(given ev => f(2) given ev)

  g(given ctx => f(22) given ctx) // is left as it is
```
### Example: Builder Pattern

Contextual function types have considerable expressive power. For
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
with contextual function types as parameters to avoid the plumbing boilerplate
that would otherwise be necessary.
```scala
  def table(init: given Table => Unit) = {
    implied t for Table
    init
    t
  }

  def row(init: given Row => Unit) given (t: Table) = {
    implied r for Row
    init
    t.add(r)
  }

  def cell(str: String) given (r: Row) =
    r.add(new Cell(str))
```
With that setup, the table construction code above compiles and expands to:
```scala
  table { given ($t: Table) =>
    row { given ($r: Row) =>
      cell("top left") given $r
      cell("top right") given $r
    } given $t
    row { given ($r: Row) =>
      cell("bottom left") given $r
      cell("bottom right") given $r
    } given $t
  }
```
### Example: Postconditions

As a larger example, here is a way to define constructs for checking arbitrary postconditions using an extension method `ensuring`so that the checked result can be referred to simply by `result`. The example combines opaque aliases, contextual function types, and extension methods to provide a zero-overhead abstraction.

```scala
object PostConditions {
  opaque type WrappedResult[T] = T

  def result[T] given (r: WrappedResult[T]): T = f

  def (x: T) ensuring [T](condition: given WrappedResult[T] => Boolean): T = {
    implied for WrappedResult[T] = x
    assert(condition)
    x
  }
}

object Test {
  import PostConditions.{ensuring, result}
  val s = List(1, 2, 3).sum.ensuring(result == 6)
}
```
**Explanations**: We use a contextual function type `given WrappedResult[T] => Boolean`
as the type of the condition of `ensuring`. An argument to `ensuring` such as
`(result == 6)` will therefore have an implied instance of type `WrappedResult[T]` in
scope to pass along to the `result` method. `WrappedResult` is a fresh type, to make sure
that we do not get unwanted implied instances in scope (this is good practice in all cases
where context parameters are involved). Since `WrappedResult` is an opaque type alias, its
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
