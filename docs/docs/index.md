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

* Dotty Language Reference
    - [Intersection Types](reference/intersection-types.md)
    - [Union Types](reference/union-types.md)
    - [Trait Parameters](reference/trait-parameters.md)
    - [Enumerations](reference/enums.md)
    - [Algebraic Data Types](reference/adts.md)
    - [Enum Translation](reference/desugarEnums.md)
    - [By-Name Implicits](reference/implicit-by-name-parameters.md)
* Usage
    - [Dotty projects with sbt](usage/sbt-projects.md): using sbt
    - [IDE support for Dotty](usage/ide-support.md)
    - [Dotty projects with cbt](usage/cbt-projects.md): using cbt
* Contributing
    - [Getting Started](contributing/getting-started.md): details on how to run tests, use the cli scripts
    - [Workflow](contributing/workflow.md): common dev patterns and hints
    - [Eclipse](contributing/eclipse.md): setting up dev environment
    - [Intellij-IDEA](contributing/intellij-idea.md): setting up dev environment
    - [Working with the Backend](contributing/backend.md): working with the scala backend 
* Internals document the compiler internals
    - [Syntax Summary](internals/syntax.md): brief analysis of the syntax
    - [Project Structure](internals/overall-structure.md): of the project
    - [Backend](internals/backend.md): details on the bytecode backend
    - [Contexts](internals/contexts.md): details the use of `Context` in the compiler
    - [Dotty vs Scala 2](internals/dotc-scalac.md): explaining the differences between Scalac and Dotty
    - [Higher Kinded Type Scheme](internals/higher-kinded-v2.md): scheme
    - [Periods](internals/periods.md): details on the concepts of runs, phases and periods
    - [Type System](internals/type-system.md): details on the type system of Dotty
* Resources
    - [Talks](resources/talks.md): presentations of the various aspects of Dotty

