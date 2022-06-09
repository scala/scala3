---
layout: doc-page
title: Using an IDE
---

You can use either Metals (VS Code, vim) or IntelliJ IDEA as described on the
IDE sections of the [getting-started page](https://docs.scala-lang.org/scala3/getting-started.html) to work
on the Scala 3 codebase. There are however a few additional considerations to take into account.


## Bootstrapping Projects

The sbt build for dotty implements bootstrapping within the same build, so each component has
two projects:

```
sbt:scala3> projects
...
[info] 	   scala3-compiler
[info] 	   scala3-compiler-bootstrapped
...
```

These duplicated projects can be confusing and cause issues in IDEs.

When using Metals, the `-bootstrapped` projects are not exported.

In IntelliJ IDEA, we recommend importing the dotty codebase through BSP, then the `-bootstrapped`
projects are not exported.


## Scala Version warning in Metals

When using VS Code, Metals might show a warning that the Scala version (`3.0.0-[...]-NIGHTLY`)
is not supported. The reason is that the dotty repository sometimes uses a nightly build as
reference compiler. The IDE experience is going to be limited in this case (semantic features will
only within single files).
