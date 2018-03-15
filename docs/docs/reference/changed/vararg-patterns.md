---
layout: doc-page
title: "Vararg Patterns"
---

The syntax of vararg patterns has changed. In the new syntax one
writes varargs in patterns exactly like one writes them in
expressions, using a `: _*` type annotation:

    xs match {
      case List(1, 2, xs: _*) => println(xs)    // binds xs
      case List(1, _ : _*) =>                   // wildcard pattern
    }

The old syntax, which is shorter but less regular, is no longer
supported:

    /*!*/ case List(1, 2, xs @ _*)       // syntax error
    /*!*/ case List(1, 2, _*) => ...     // syntax error

Another change relates to extractors producing values with `T*` types, which can then be
matched by vararg patterns. Previously, such extractors used an `unapplySeq` whereas now they use an `unapply` (in the long term, we plan to get rid of `unapplySeq` altogether, replacing all its usages with `unapply`).

Example: Previously, this was correct:

    class Person(val name: String, val children: Person *)
    object Person {
      def unapplySeq(p: Person) = Some((p.name, p.children))
    }

    def childCount(p: Person) = p match {
      case Person(_, ns @ _*) => ns.length
    }

Now, the equivalent program is written as follows:

    class Person(val name: String, val children: Person *)
    object Person {
      def unapply(p: Person) = Some((p.name, p.children))
    }

    def childCount(p: Person) = p match {
      case Person(_, ns : _*) => ns.length
    }



