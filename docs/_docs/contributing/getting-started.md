---
layout: doc-page
title: Getting Started
---



Requirements
------------
Make sure that you are using macOS or Linux (or WSL on Windows) with Java 8 or newer. You can determine which version of the JDK is the
default by typing `java -version` in a Terminal window.

Compiling and Running
---------------------
Start by cloning the repository:

```bash
$ git clone https://github.com/lampepfl/dotty.git
$ cd dotty
$ sbt managedSources # Needed for IDE import to succeed
```

Dotty provides a standard sbt build: compiling, running and starting a repl can
all be done from within sbt:

```bash
$ sbt
> scalac tests/pos/HelloWorld.scala
> scala HelloWorld
hello world
```

There are also bash scripts that can be used in the same way. Assuming that you have cloned the Dotty repo locally, append
the following line on your `.bash_profile`:

```shell
$ export PATH=$HOME/dotty/bin:$PATH
```

and you will be able to run the corresponding commands directly from your console:

```shell
# Compile code using Dotty
$ scalac tests/pos/HelloWorld.scala

# Run it with the proper classpath
$ scala HelloWorld
```


Starting a REPL
---------------
```bash
$ sbt
> repl
Welcome to Scala.next (pre-alpha)  (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_101).
Type in expressions to have them evaluated.
Type :help for more information.
scala>
```

or via bash:

```bash
$ scala
```
Publish to local repository
---------------------------------
To test our cloned compiler on local projects:

```bash
$ sbt publishLocal
```
Then in the `build.sbt` file of a test project:

```bash
ThisBuild / scalaVersion := "<dotty-version>-bin-SNAPSHOT"
```
where `dotty-version` can be found in the file `project/Build.scala`, like `3.0.0-M2`


Generating Documentation
-------------------------
To generate this page and other static page docs, run
```bash
$ sbt
> scaladoc/generateScalaDocumentation
```
For more information, see `scaladoc/README.md`.

Before contributing to Dotty, we invite you to consult the
[Dotty Developer Guidelines](https://github.com/lampepfl/dotty/blob/main/CONTRIBUTING.md).


Community
-------------
The main development discussion channels are:
- [github.com/lampepfl/dotty/discussions](https://github.com/lampepfl/dotty/discussions)
- [contributors.scala-lang.org](https://contributors.scala-lang.org)
- [gitter.im/scala/contributors](https://gitter.im/scala/contributors)
