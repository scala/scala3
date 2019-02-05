---
layout: doc-page
title: "Dropped: Package Objects"
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

**Note 1:** This means that the name of a source file containing wrapped toplevel definitions is relevant for binary compatibility. If the name changes, so does the name of the generated object and its class.

**Note 2:** A toplevel main method `def main(args: Array[String]): Unit = ...` is wrapped as any other method. If it appears
in a source file `src.scala`, it could be invoked from the command line using a command like `scala src$package`. Since the
"program name" is mangled it is recommended to always put `main` methods in explicitly named objects.

**Note 3:** The notion of `private` is independent of whether a definition is wrapped or not. A `private` toplevel definition is always visible from everywhere in the enclosing package.
