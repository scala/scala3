---
layout: default
title: "Getting Started"
---

Getting Started
===============

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

Compiling and running code
--------------------------
```bash
git clone https://github.com/lampepfl/dotty.git
cd dotty
# Clone dotty-compatible stdlib. Needed for running the test suite.
git clone -b dotty-library https://github.com/DarkDimius/scala.git scala-scala
# Compile code using Dotty
./bin/dotc tests/pos/HelloWorld.scala
# Run it with the proper classpath
./bin/dotr HelloWorld
```

Starting a REPL
---------------
```bash
./bin/dotr
```
