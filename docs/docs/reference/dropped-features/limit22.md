---
layout: doc-page
title: Dropped: Limit 22
---

The limit of 22 for the maximal number of parameters of function types
has been dropped.  Functions can now have an arbitrary number of
parameters. Functions beyond Function22 are represented with a new trait
`scala.FunctionXXL`.

The limit of 22 for the size of tuples is about to be dropped. Tuples
will in the future be represented by an HList-like structure which can
be arbitrarily large.
