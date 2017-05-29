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


