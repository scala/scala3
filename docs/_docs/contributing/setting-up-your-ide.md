---
layout: doc-page
title: Setting up your IDE
---

You can use either Metals with your favorite editor (VS Code, Neovim, Sublime)
or [IntelliJ IDEA for
Scala](https://www.jetbrains.com/help/idea/discover-intellij-idea-for-scala.html)
to work on the Scala 3 codebase. There are however a few additional
considerations to take into account.

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

### Metals

When using Metals, the `-bootstrapped` projects are not exported by default.
Normally this is fine, but if you're working on certain modules like `scaladoc`
you'll actually want these modules exported. In order to achieve this you'll
first want to ensure you're using `sbt` as your build server instead of the
default Bloop. You can achieve this with the `Metals: Switch Build Server`
command and then choosing sbt. Once you do this, you'll want to find and change
the following under `commonBootstrappedSettings` which is found in the
[`Build.scala`](https://github.com/lampepfl/dotty/blob/main/project/Build.scala)
file.

```diff

-    bspEnabled := false,
+    bspEnabled := true,
```

### IntelliJ

In IntelliJ IDEA, we recommend importing the dotty codebase through BSP, then
the `-bootstrapped` projects are not exported.
