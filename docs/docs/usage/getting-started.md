---
layout: doc-page
title: Getting Started: Users
---



Basics
------------
Make sure that you are using Java 8 or later. The output of `java -version`
should contain `1.8`.


Install Dotty
-------------
If you're a Mac user, you can install dotty with [brew](https://brew.sh/)

```bash
brew install lampepfl/brew/dotty
```

If you're a Linux or Windows user, download the [latest release](https://github.com/lampepfl/dotty/releases). Optionally add path of the folder `bin/` to the system environment variable `PATH`.

Getting Dotty
-------------
```bash
$ git clone --recursive https://github.com/lampepfl/dotty.git
$ cd dotty
$ sbt managedSources # Needed for IDE import to succeed
```

Dotty provides a standard sbt build: compiling, running and starting a repl can
all be done from within sbt:

```bash
$ sbt
> dotc tests/pos/HelloWorld.scala
> dotr HelloWorld
hello world
```

Try Dotty
----------
Try it in your browser with [Scastie](https://scastie.scala-lang.org/?target=dotty)


Create a Dotty Project
-----------------------
The fastest way to create a new project in dotty is using [sbt (0.13.5+)](http://www.scala-sbt.org/)

Create a dotty project:
```bash
$ sbt new lampepfl/dotty.g8
```

Or a Dotty project that cross compiles with Scala 2:
```bash
$ sbt new lampepfl/dotty-cross.g8
```

For an example project, see the [Dotty Example Proejct](https://github.com/lampepfl/dotty-example-project)


Bash Scripts
-------------
Assuming that you have cloned the Dotty repo locally, append the following line on your `.bash_profile`:

```shell
$ export PATH=$HOME/dotty/bin:$PATH
```

and you will be able to run the corresponding commands directly from your console:

```shell
# Compile code using Dotty
$ dotc tests/pos/HelloWorld.scala

# Run it with the proper classpath
$ dotr HelloWorld
```


Starting a REPL
----------------
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
$ dotr
```
