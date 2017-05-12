---
layout: doc-page
title: Getting Started
---

Requirements
------------
Make sure that you are using Java 8 or later. The output of `java -version`
should contain `1.8`.

Compiling and Running
---------------------
Start by cloning the repository:

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

There are also bash scripts that can be used in the same way. Assuming that you have cloned the Dotty repo locally, append 
the following line on your `.bash_profile`:

```shell
export PATH=$HOME/dotty/bin:$PATH
```

and you will be able to run the corresponding commands directly from your console:

```shell
# Compile code using Dotty
dotc tests/pos/HelloWorld.scala

# Run it with the proper classpath
dotr HelloWorld
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
$ dotr
```
