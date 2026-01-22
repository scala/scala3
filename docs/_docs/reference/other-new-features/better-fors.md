---
layout: doc-page
title: "Better fors"
nightlyOf: https://docs.scala-lang.org/scala3/reference/other-new-features/better-fors.html
---

Starting in Scala `3.8` or under `-preview` mode using Scala `3.7`, the usability of `for`-comprehensions has improved.

The biggest user facing change is the new ability to start `for`-comprehensions with aliases. This means that the following previously invalid code is now valid:

```scala
for
  as = List(1, 2, 3)
  bs = List(4, 5, 6)
  a <- as
  b <- bs
yield a + b
```

The desugaring of this code is the same as if the aliases were introduced with `val`:

```scala
val as = List(1, 2, 3)
val bs = List(4, 5, 6)
for
  a <- as
  b <- bs
yield a + b
```

Additionally, this extension changes the way `for`-comprehensions are desugared. The desugaring is now done in a more intuitive way and the desugared code can be more efficient, because it avoids some unnecessary method calls. There are two main changes in the desugaring:

1. **Simpler Desugaring for Pure Aliases**:
    When an alias is not followed by a guard, the desugaring is simplified. The last generator and the aliases don't have to be wrapped in a tuple, and instead the aliases are simply introduced as local variables in a block with the next generator.
    **Previous Desugaring**:
      ```scala
      for {
        a <- doSth(arg)
        b = a
      } yield a + b
      ```
      Desugars to:
      ```scala
      doSth(arg).map { a =>
        val b = a
        (a, b)
      }.map { case (a, b) =>
        a + b
      }
      ```
    **New Desugaring**:
      ```scala
      doSth(arg).map { a =>
        val b = a
        a + b
      }
      ```
    This change makes the desugaring more intuitive and avoids unnecessary `map` calls, when an alias is not followed by a guard.

2. **Avoiding Redundant `map` Calls**:
    When the result of the `for`-comprehension is the same expression as the last generator pattern, the desugaring avoids an unnecessary `map` call. But the equality of the last pattern and the result has to be able to be checked syntactically, so it is either a variable or a tuple of variables. There is also a special case for dropping the `map`, if its body is a constant function, that returns `()` (`Unit` constant).
    **Current Desugaring**:
      ```scala
      for {
        a <- List(1, 2, 3)
      } yield a
      ```
      Desugars to:
      ```scala
      List(1, 2, 3).map(a => a)
      ```
    **New Desugaring**:
      ```scala
      List(1, 2, 3)
      ```

For more details on the desugaring scheme see the comment in [`Desugar.scala#makeFor`](https://github.com/scala/scala3/blob/f7470073902d810911bdfb4b4cf905a4716b9bde/compiler/src/dotty/tools/dotc/ast/Desugar.scala#L2091).
