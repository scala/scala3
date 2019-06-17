---
layout: doc-page
title: "Debug Macros"
---

Complex macros may break invariants of the compiler, which leads to compiler crashes.
Here we lists common compiler crashes and how to deal with them.

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
