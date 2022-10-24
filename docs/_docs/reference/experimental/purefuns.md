---
layout: doc-page
title: "Pure Function Syntax"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/purefuns.html
---

Pure functions are an experimental feature that can be enabled by the language import
```scala
import language.experimental.pureFunctions
```
Under that import the syntax `A -> B` is available with the intention that it should denote a pure, side effect-free function from `A` to `B`. Some other variants are also supported:
```scala
   (A1, ..., An) -> B           // a multi-argument pure function
   (x1: A1, ..., xn: An) -> B   // a dependent pure function
   A ?-> B                      // a pure context function
   (A1, ..., An) ?-> B          // a multi-argument pure context function
   (x1: A1, ..., xn: An) ?-> B  // a dependent pure context function
   -> B                         // a pure call-by-name parameter
```
A function's purity can be checked by capture tracking, another experimental language feature which is presently in a very early stage. Until that second feature matures, the pure function syntax should be understood to be for documentation only. A pure function type is a requirement that all its instances should be side effect-free. This requirement currently needs to be checked manually, but checking might be automated in the future.

## Why Enable It Now?

There are at least three reasons why one might want to enable `pureFunctions` today:

 - to get better documentation since it makes the intent clear,
 - to prepare the code base for a time when full effect checking is implemented,
 - to have a common code base that can be compiled with or without capture checking enabled.

## More info:

TBD