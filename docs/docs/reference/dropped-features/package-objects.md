---
layout: doc-page
title: Dropped: Package Objects
---

Package objects
```scala
    package object p {
      val a = ...
      def b = ...
    }
```
will be dropped. They are still available in Scala 3.0, but will be deprecated and removed afterwards.

Package objects are no longer needed since all kinds of definitions can now be written at the top-level. E.g.
```scala
    package p
    type Labelled[T] = (String, T)
    val a: Labelled[Int] = ("count", 1)
    def b = a._2

    case class C()

    implicit object Cops {
      def (x: C) pair (y: C) = (x, y)
    }
```
There may be several source files in a package containing such toplevel definitions, and source files can freely mix toplevel value, method, and type definitions with classes and objects.

The compiler generates synthetic objects that wrap toplevel definitions falling into one of the following categories:

 - all pattern, value, method, and type definitions,
 - implicit classes and objects,
 - companion objects of opaque types.

If a source file `src.scala` contains such toplevel definitions, they will be put in a synthetic object named `src$package`. The wrapping is transparent, however. The definitions in `src` can still be accessed as members of the enclosing package.
