---
layout: doc-page
title: Getting Started: Users
movedTo: https://docs.scala-lang.org/scala3/getting-started.html
---

## Trying out Dotty

### In your web browser
[Scastie](https://scastie.scala-lang.org/?target=dotty), the online Scala playground, supports Dotty.
This is an easy way to try Dotty without installing anything, directly in your browser.

### sbt
The fastest way to create a new project compiled by Dotty is using [sbt](http://www.scala-sbt.org/)

Create a simple Dotty project:
```bash
$ sbt new scala/scala3.g8
```

Or a Dotty project that cross compiles with Scala 2:
```bash
$ sbt new scala/scala3-cross.g8
```

You can then start a Dotty REPL directly from your sbt project:
```bash
$ sbt
> console
scala>
```

For more information, see the [Dotty Example Project](https://github.com/scala/scala3-example-project)

### IDE support
Start using the Dotty IDE in any Dotty project by following the
[IDE guide](./ide-support.md).

### Standalone installation
Releases are available for download on the [Releases Section](https://github.com/lampepfl/dotty/releases)
of the Dotty repository. Releases include three executables: `scalac` the Dotty compiler,
`scaladoc` the [Scaladoc](./scaladoc/index.md) and `scala` the Dotty REPL.

```
.
└── bin
    ├── scalac
    ├── scaladoc
    └── scala
```

Add these executables to your `PATH` and you will be able to run the corresponding commands directly
from your console:
```bash
# Compile code using Dotty
$ scalac HelloWorld.scala

# Run it with the proper classpath
$ scala HelloWorld

# Start a Dotty REPL
$ scala
Starting dotty REPL...
scala>
```

If you're a Mac user, we also provide a [homebrew](https://brew.sh/) package that can be installed by running:

```bash
brew install lampepfl/brew/dotty
```

In case you have already installed Dotty via brew, you should instead update it:

```bash
brew upgrade dotty
```

### Scala 3 for Scripting
If you have followed the steps in "Standalone Installation" section and have the `scala` executable on your `PATH`, you can run `*.scala` files as scripts. Given a source named Test.scala:

```scala
@main def Test(name: String): Unit =
  println(s"Hello ${name}!")
```

You can run: `scala Test.scala World` to get an output `Hello World!`.

A "script" is an ordinary Scala file which contains a main method. The semantics of the `scala Script.scala` command is as follows:

- Compile `Script.scala` with `scalac` into a temporary directory.
- Detect the main method in the `*.class` files produced by the compilation.
- Execute the main method.
