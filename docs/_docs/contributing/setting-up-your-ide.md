---
layout: doc-page
title: Setting up your IDE
---

You can use either Metals with your favorite editor or
[IntelliJ IDEA for Scala](https://www.jetbrains.com/help/idea/discover-intellij-idea-for-scala.html)
to work on the Scala 3 codebase. There are however a few additional
considerations to take into account.

## Bootstrapping Projects

The sbt build for dotty implements bootstrapping within the same build, so each
component has two projects:

```
sbt:scala3> projects
...
[info] 	   scala3-compiler
[info] 	   scala3-compiler-bootstrapped
...
```

These duplicated projects can be confusing and cause issues in IDEs, so it's
import to import the project in a specific way depending on your editor.

### Metals

When using Metals, the `-bootstrapped` projects are not exported by default.
Normally this is fine, but if you're working on certain modules like `scaladoc`
you'll actually want these modules exported. In order to achieve this you'll
want to make sure you do two things:

1. You'll want to find and change the following under
   `commonBootstrappedSettings` which is found in the
   [`Build.scala`](https://github.com/scala/scala3/blob/main/project/Build.scala)
   file.

```diff

-    bspEnabled := false,
+    bspEnabled := true,
```

2. Run `sbt publishLocal` to get the needed presentation compiler jars.

By default Metals uses Bloop build server, however you can also use sbt
directly. You can achieve this with the `Metals: Switch Build Server` command
and then choosing sbt. In VSCode, this looks like this:

![bsp-switch](https://user-images.githubusercontent.com/777748/241986423-0724ae74-0ebd-42ef-a1b7-4d17678992b4.png)

### IntelliJ

In IntelliJ IDEA, we recommend importing the dotty codebase through BSP, then
the `-bootstrapped` projects are not exported.
