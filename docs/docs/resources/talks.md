---
layout: doc-page
title: Talks
movedTo: https://docs.scala-lang.org/scala3/talks.html
---

Talks on Dotty
--------------
- (ScalaDays 2019, Lausanne) [A Tour of Scala 3](https://www.youtube.com/watch?v=_Rnrx2lo9cw) by [Martin Odersky](http://twitter.com/odersky) [\[slides\]](https://www.slideshare.net/Odersky/a-tour-of-scala-3)

- (ScalaDays 2016, Berlin) [Scala's Road Ahead](https://www.youtube.com/watch?v=GHzWqJKFCk4) by [Martin Odersky](http://twitter.com/odersky) [\[slides\]](http://www.slideshare.net/Odersky/scala-days-nyc-2016)

- (JVMLS 2015) [Compilers are Databases](https://www.youtube.com/watch?v=WxyyJyB_Ssc) by [Martin Odersky](http://twitter.com/odersky) [\[slides\]](http://www.slideshare.net/Odersky/compilers-are-databases)

- (Scala World 2015) [Dotty: Exploring the future of Scala](https://www.youtube.com/watch?v=aftdOFuVU1o) by [Dmitry Petrashko](http://twitter.com/darkdimius) [\[slides\]](https://d-d.me/scalaworld2015/#/). 
Dmitry covers many of the new features that Dotty brings on the table such as Intersection and Union types, improved lazy val initialization and more. 
Dmitry also covers dotty internals and in particular the high-level of contextual abstractions of Dotty. You will get to 
become familiar with many core concepts such as `Denotations`, their evolution through (compilation) time, their 
transformations and more.

Deep Dive with Dotty
--------------------
- (ScalaDays 2019, Lausanne) [Metaprogramming in Dotty](https://www.youtube.com/watch?v=ZfDS_gJyPTc) by [Nicolas Stucki](https://github.com/nicolasstucki).

- (ScalaDays 2019, Lausanne) [Future-proofing Scala: the TASTY intermediate representation](https://www.youtube.com/watch?v=zQFjC3zLYwo) by [Guillaume Martres](http://guillaume.martres.me/).

- (Mar 21, 2017) [Dotty Internals 1: Trees & Symbols](https://www.youtube.com/watch?v=yYd-zuDd3S8) by [Dmitry Petrashko](http://twitter.com/darkdimius) [\[meeting notes\]](../internals/dotty-internals-1-notes.md). 
This is a recorded meeting between EPFL and Waterloo, where we introduce first notions inside Dotty: Trees and Symbols.

- (Mar 21, 2017) [Dotty Internals 2: Types](https://www.youtube.com/watch?v=3gmLIYlGbKc) by [Martin Odersky](http://twitter.com/odersky) and [Dmitry Petrashko](http://twitter.com/darkdimius).
This is a recorded meeting between EPFL and Waterloo, where we introduce how types are represented inside Dotty.

- (Jun 15, 2017) [Dotty Internals 3: Denotations](https://youtu.be/9iPA7zMRGKY) by [Martin Odersky](http://twitter.com/odersky) and [Dmitry Petrashko](http://twitter.com/darkdimius).
This is a recorded meeting between EPFL and Waterloo, where we introduce denotations in Dotty.

- (JVM Language Summit) [How do we make the Dotty compiler fast](https://www.youtube.com/watch?v=9xYoSwnSPz0) by [Dmitry Petrashko](http://twitter.com/darkdimius).
[Dmitry Petrashko](http://twitter.com/darkdimius) gives a high-level introduction on what was done to make Dotty .


- (Typelevel Summit Oslo, May 2016) [Dotty and types: the story so far](https://www.youtube.com/watch?v=YIQjfCKDR5A) by 
Guillaume Martres [\[slides\]](http://guillaume.martres.me/talks/typelevel-summit-oslo/).
Guillaume focused on some of the practical improvements to the type system that Dotty makes, like the new type parameter 
inference algorithm that is able to reason about the type safety of more situations than scalac.

- (flatMap(Oslo) 2016) [AutoSpecialization in Dotty](https://vimeo.com/165928176) by [Dmitry Petrashko](http://twitter.com/darkdimius) [\[slides\]](https://d-d.me/talks/flatmap2016/#/). 
The Dotty Linker analyses your program and its dependencies to 
apply a new specialization scheme. It builds on our experience from Specialization, Miniboxing and the Valhalla Project, 
and drastically reduces the size of the emitted bytecode. And, best of all, it's always enabled, happens behind the 
scenes without annotations,  and results in speedups in excess of 20x. Additionally, it "just works" on Scala collections.

- (ScalaSphere 2016) [Hacking on Dotty: A live demo](https://www.youtube.com/watch?v=0OOYGeZLHs4) by Guillaume Martres [\[slides\]](http://guillaume.martres.me/talks/dotty-live-demo/).
Guillaume hacks on Dotty: a live demo during which he 
creates a simple compiler phase to trace method calls at run-time.

- (Scala By the Bay 2016) [Dotty: what is it and how it works](https://www.youtube.com/watch?v=wCFbYu7xEJA) by Guillaume 
Martres [\[slides\]](http://guillaume.martres.me/talks/dotty-tutorial/#/). Guillaume provides a high-level view of the 
compilation-pipeline of Dotty.

- (ScalaDays 2015, Amsterdam) [Making your Scala applications smaller and faster with the Dotty linker](https://www.youtube.com/watch?v=xCeI1ArdXM4) by Dmitry Petrashko [\[slides\]](https://d-d.me/scaladays2015/#/). 
Dmitry introduces the call-graph analysis algorithm 
that Dotty implements and the performance benefits we can get in terms of number of methods, bytecode size, JVM code size 
and the number of objects allocated in the end.
