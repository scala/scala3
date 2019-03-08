---
layout: doc-page
title: Dropped: Limit 22
---

The limits of 22 for the maximal number of parameters of function types
and the maximal number of fields in tuple types have been dropped.

Functions can now have an arbitrary number of
parameters. Functions beyond Function22 are represented with a new trait
`scala.FunctionXXL`.

Tuples can also have an arbitrary number of fields. Furthermore, they support generic operation such as concatenation and indexing.
