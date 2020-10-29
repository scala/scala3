---
layout: doc-page
title: Deprecated: Nonlocal Returns
---

Returning from nested anonymous functions has been deprecated. Nonlocal returns are implemented by throwing and catching `scala.runtime.NonLocalReturnException`-s. This is rarely what is intended by the programmer. It can be problematic because of the hidden performance cost of throwing and catching exceptions. Furthermore, it is a leaky implementation: a catch-all exception handler can intercept a `NonLocalReturnException`.

A drop-in library replacement is provided in `scala.util.control.NonLocalReturns`:

```scala
import scala.util.control.NonLocalReturns._

returning { ... throwReturn(x) ... }
```
