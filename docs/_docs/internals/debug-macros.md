---
layout: doc-page
title: "Debug Macros"
---

Complex macros may break invariants of the compiler, which leads to compiler crashes.
Here we list common compiler crashes and how to deal with them.

## Enable checks

* Always enable `-Xcheck-macros`
* May also enable `-Ycheck:all`

## position not set

For this problem, here is the log that is usually shown:

```
[error] assertion failed: position not set for org.scalactic.anyvals.PosZInt.+$extension3(SizeParam.this.minSize)(
[error]   org.scalactic.anyvals.PosZInt.widenToInt(SizeParam.this.sizeRange)
[error] ) # 2326942 of class dotty.tools.dotc.ast.Trees$Apply in library/src-bootstrapped/scala/tasty/reflect/utils/TreeUtils.scala
```

To debug why the position is not set, note the tree id `2326942`, and enable
the following compiler option:

```
-Ydebug-tree-with-id 2326942
```

With the option above, the compiler will crash when the tree is created. From
the stack trace, we will be able to figure out where the tree is created.

If the position is in the compiler, then either report a compiler bug or
fix the problem with `.withSpan(tree.span)`. The following fix is an example:

- https://github.com/lampepfl/dotty/pull/6581

## unresolved symbols in pickling

Here is the usually stacktrace for unresolved symbols in pickling:

```
[error] java.lang.AssertionError: assertion failed: unresolved symbols: value pos (line 5565) when pickling scalatest/scalatest-test.dotty/target/scala-0.17/src_managed/test/org/scalatest/AssertionsSpec.scala
[error] 	at dotty.tools.dotc.core.tasty.TreePickler.pickle(TreePickler.scala:699)
[error] 	at dotty.tools.dotc.transform.Pickler.run$$anonfun$10$$anonfun$8(Pickler.scala:60)
[error] 	at dotty.runtime.function.JProcedure1.apply(JProcedure1.java:15)
[error] 	at dotty.runtime.function.JProcedure1.apply(JProcedure1.java:10)
[error] 	at scala.collection.immutable.List.foreach(List.scala:392)
[error] 	at dotty.tools.dotc.transform.Pickler.run$$anonfun$2(Pickler.scala:83)
[error] 	at dotty.runtime.function.JProcedure1.apply(JProcedure1.java:15)
[error] 	at dotty.runtime.function.JProcedure1.apply(JProcedure1.java:10)
[error] 	at scala.collection.immutable.List.foreach(List.scala:392)
[error] 	at dotty.tools.dotc.transform.Pickler.run(Pickler.scala:83)
[error] 	at dotty.tools.dotc.core.Phases$Phase.runOn$$anonfun$1(Phases.scala:316)
[error] 	at scala.collection.immutable.List.map(List.scala:286)
[error] 	at dotty.tools.dotc.core.Phases$Phase.runOn(Phases.scala:318)
[error] 	at dotty.tools.dotc.transform.Pickler.runOn(Pickler.scala:87)
```

From the stack trace, we know `pos` at line 5565 cannot be resolved. For the
compiler, it means that the name `pos` (usually a local name, but could also be
a class member) is used in the code but its definition cannot be found.

A possible cause of the problem is that the macro implementation accidentally
dropped the definition of the referenced name.

If you are confident that the macro implementation is correct, then it might be
a bug of the compiler. Try to minimize the code and report a compiler bug.
