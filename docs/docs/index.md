---
layout: doc-page
title: "Dotty Documentation"
---

Dotty is a platform to try out new language concepts and compiler technologies for Scala. 
The focus is mainly on simplification. We remove extraneous syntax (e.g. no XML literals), 
and try to boil down Scala’s types into a smaller set of more fundamental constructors. 
The theory behind these constructors is researched in DOT, a calculus for dependent object types.

In this documentation you will find information on how to use the Dotty compiler on your machine, navigate through
the code, setup Dotty with your favorite IDE and more!

Contents
-------

* Usage
    - [Migrating from Scala 2](usage/migrating.md)
    - [Dotty projects with cbt](usage/cbt-projects.md)
    - [Dotty projects with sbt](usage/sbt-projects.md)
* Contributing
    - [Getting Started](contributing/getting-started.md) details on how to run
      tests, use the cli scripts
    - [Workflow](contributing/workflow.md) common dev patterns and hints
    - [Eclipse](contributing/eclipse.md) setting up dev environment
    - [Intellij-IDEA](contributing/intellij-idea.md) setting up dev environment
* Internals document the compiler internals
    - [Syntax Summary](internals/syntax.md)
    - [Project Structure](internals/overall-structure.md)
      of the project
    - [Backend](internals/backend.md) details on the bytecode backend
    - [Contexts](internals/contexts.md) details the use of `Context` in the
      compiler
    - [Dotty vs Scala2](internals/dotc-scalac.md)
    - [Higher Kinded Type Scheme](internals/higher-kinded-v2.md)
      scheme
    - [Periods](internals/periods.md)
    - [Type System](internals/type-system.md)
* Resources
    - [Talks](resources/talks.md)

