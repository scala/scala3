---
layout: doc-page
title: Getting Started
---

Talks on Dotty
--------------
- [Scala's Road Ahead](https://www.youtube.com/watch?v=GHzWqJKFCk4) by Martin Odersky [\[slides\]](http://www.slideshare.net/Odersky/scala-days-nyc-2016)
- [Compilers are Databases](https://www.youtube.com/watch?v=WxyyJyB_Ssc) by Martin Odersky [\[slides\]](http://www.slideshare.net/Odersky/compilers-are-databases)
- [Dotty: Exploring the future of Scala](https://www.youtube.com/watch?v=aftdOFuVU1o) by Dmitry Petrashko [\[slides\]](https://d-d.me/scalaworld2015/#/). This talk includes details about the design of mini-phases and denotations.
- [Making your Scala applications smaller and faster with the Dotty linker](https://www.youtube.com/watch?v=xCeI1ArdXM4) by Dmitry Petrashko [\[slides\]](https://d-d.me/scaladays2015/#/)
- [Dotty: what is it and how it works](https://www.youtube.com/watch?v=wCFbYu7xEJA) by Guillaume Martres [\[slides\]](http://guillaume.martres.me/talks/dotty-tutorial/#/)
- [Hacking on Dotty: A live demo](https://www.youtube.com/watch?v=0OOYGeZLHs4) by Guillaume Martres [\[slides\]](http://guillaume.martres.me/talks/dotty-live-demo/)
- [AutoSpecialization in Dotty](https://vimeo.com/165928176) by Dmitry Petrashko [\[slides\]](https://d-d.me/talks/flatmap2016/#/)
- [Dotty and types: the story so far](https://www.youtube.com/watch?v=YIQjfCKDR5A) by Guillaume Martres [\[slides\]](http://guillaume.martres.me/talks/typelevel-summit-oslo/)

Requirements
------------
Make sure that you are using Java 8 or later, the output of `java -version`
should contain `1.8`.

Compiling and Running
---------------------
Start by cloning the repository:

```bash
$ git clone --recursive https://github.com/lampepfl/dotty.git
$ cd dotty
$ sbt managedSources ## Needed for IDE import to succeed
```

Dotty provides a standard sbt build: compiling, running and starting a repl can
all be done from within sbt using

```bash
$ sbt
> dotc tests/pos/HelloWorld.scala
> dotr HelloWorld
hello world
```

there is also a bash script that can be used in the same way:

```bash
# Compile code using Dotty
./bin/dotc tests/pos/HelloWorld.scala
# Run it with the proper classpath
./bin/dotr HelloWorld
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
$ ./bin/dotr
```
