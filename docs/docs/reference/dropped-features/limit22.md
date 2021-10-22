---
title: "Dropped: Limit 22"
type: section
num: 81
previous-page: /scala3/reference/dropped-features/class-shadowing
next-page: /scala3/reference/dropped-features/xml
---

The limits of 22 for the maximal number of parameters of function types and the
maximal number of fields in tuple types have been dropped.

* Functions can now have an arbitrary number of parameters. Functions beyond
  [`scala.Function22`](https://www.scala-lang.org/api/current/scala/Function22.html) are erased to a new trait [`scala.runtime.FunctionXXL`](https://scala-lang.org/api/3.x/scala/runtime/FunctionXXL.html).

* Tuples can also have an arbitrary number of fields. Tuples beyond [`scala.Tuple22`](https://www.scala-lang.org/api/current/scala/Tuple22.html)
  are erased to a new class [`scala.runtime.TupleXXL`](https://scala-lang.org/api/3.x/scala/runtime/TupleXXL.html) (which extends the trait [`scala.Product`](https://scala-lang.org/api/3.x/scala/Product.html)). Furthermore, they support generic
  operation such as concatenation and indexing.

Both of these are implemented using arrays.
