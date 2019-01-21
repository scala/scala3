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

Package objects are no longer needed since all kinds of definitions and statements can now be written at the top-level. E.g.
```scala
    package p
    val a = ...
    def b = ...
```
There may be several source files in a package containing such toplevel definitions, and source files can freely mix toplevel value, method, and tyoe definitions with classes and objects.

The compiler generates synthetic objects that wrap toplevel statements that are not imports, or class or object definitions. If a source file `src.scala` contains such toplevel statements, they will be put in a synthetic object named `src$package`. The wrapping is transparent, however. The definitions in `f` can still be accessed as members of the enclosing package.
