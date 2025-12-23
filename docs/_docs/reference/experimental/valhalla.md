---
layout: doc-page
title: "Valhalla Value Classes and Traits"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/valhalla.html
---

## Valhalla Value Classes

Valhalla value classes are the Scala equivalence of Java's Project Valhalla's value classes (see [JEP 401](https://openjdk.org/jeps/401)). When used with the Project Valhalla JVM, Valhalla value classes are optimized.

Valhalla value classes extend AnyVal and have a `valhalla` annotation. Valhalla value classes cannot have non-parameter fields and cannot have auxilliary constructors.

Valhalla value classes do not have object identity -- two valhalla value classes are equal when their fields are the same.

```scala
import scala.annotation.valhalla

@valhalla class ValhallaValueClass(val x: Int, val y: Int) extends AnyVal
```

Valhalla value classes are implicitly final and cannot be extended unless it is abstract. Its fields are immutable and cannot be lazy.

Valhalla value classes can extend `AnyVal`, universal traits, or abstract valhalla value classes.

## Valhalla Traits

Valhalla Traits are Universal Traits (traits that extend Any) with a `valhalla` annotation.

Like Valhalla value classes, any Valhalla trait must have immutable fields only.

Valhalla traits can extend `Any` or universal traits.

```scala
import scala.annotation.valhalla

@valhalla trait ValhallaTrait(val x: Int, val y: Int) extends Any

```

## Using Explicit Self with Valhalla

Valhalla traits can have self-type of any trait without mutable fields.

## CanEqual with Valhalla

Valhalla value classes can be null, so the CanEqual of `null` and a valhalla value class returns `true`.

## Using Value Classes

Use the `--Yvalue-classes` and `-experimental` compiler options when compiling value classes in the scala compiler.

## Getting Started

1. Install [Project Valhalla JVM](https://jdk.java.net/valhalla/) or alternatively clone the [source code](https://github.com/openjdk/valhalla). For the latter, follow the [build instructions](https://github.com/openjdk/jdk/blob/master/doc/building.md).

2. Clone [scala-asm](https://github.com/lidaisy/scala-asm) and run `sbt publishLocal` to use it locally.

3. Run `sbt scala-library-nonbootstrapped/publishLocal` in scala3 to use the local library that includes the value class annotation.