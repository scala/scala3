---
layout: doc-page
title: Getting Started: Users
---



## Trying out Dotty

### In your web browser
[Scastie](https://scastie.scala-lang.org/?target=dotty), the online Scala playground, supports Dotty.
This is an easy way to try Dotty without installing anything, directly in your browser.

### sbt
The fastest way to create a new project compiled by Dotty is using [sbt (1.1.4+)](http://www.scala-sbt.org/)

Create a simple Dotty project:
```bash
$ sbt new lampepfl/dotty.g8
```

Or a Dotty project that cross compiles with Scala 2:
```bash
$ sbt new lampepfl/dotty-cross.g8
```

You can then start a Dotty REPL directly from your sbt project:
```bash
$ sbt
> console
scala>
```

For more information, see the [Dotty Example Project](https://github.com/lampepfl/dotty-example-project)

### IDE support
Start using the Dotty IDE in any Dotty project by following the
[IDE guide](./ide-support.md).

### Standalone installation
Releases are available for download on the [Releases Section](https://github.com/lampepfl/dotty/releases)
of the Dotty repository. Releases include three executables: `dotc` the Dotty compiler,
`dotd` the [Dotty Documentation tool](./dottydoc.md) and `dotr` the Dotty REPL.

```
.
└── bin
    ├── dotc
    ├── dotd
    └── dotr
```

Add these executables to your `PATH` and you will be able to run the corresponding commands directly
from your console:
```bash
# Compile code using Dotty
$ dotc HelloWorld.scala

# Run it with the proper classpath
$ dotr HelloWorld

# Start a Dotty REPL
$ dotr
Starting dotty REPL...
scala>
```

If you're a Mac user, we also provide a [homebrew](https://brew.sh/) package that can be installed by running:

```
brew install lampepfl/brew/dotty
```

In case you have already installed Dotty via brew, you should instead update it:

```
brew upgrade dotty
```
