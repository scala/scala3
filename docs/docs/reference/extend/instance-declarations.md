---
layout: doc-page
title: "Instance Declarations"
---

In addition to adding methods, an extension can also implement traits. Extensions implementing traits are also called _instance declarations_. For example,

```scala
trait HasArea {
  def area: Double
}

extension CircleHasArea for Circle : HasArea {
  def area = this.radius * this.radius * math.Pi
}
```

This extension makes `Circle` an instance of the `HasArea` trait. Specifically, it defines an implicit subclass of `HasArea`
which takes a `Circle` as argument and provides the given implementation. Hence, the implementation of the extension above would be like this

```scala
implicit class CircleHasArea($this: Circle) extends HasArea {
  def area = $this.radius * $this.radius * math.Pi
}
```

An instance definition can thus provide a kind of "implements" relationship that can be defined independently of the types it connects.

### Generic Instance Declarations

Just like extension methods, instance declarations can also be generic and their type parameters can have bounds.

For example, assume we have the following two traits, which define binary and unary (infix) equality tests:

```scala
trait Eql[T] {
  def eql (x: T, y: T): Boolean
}

trait HasEql[T] {
  def === (that: T): Boolean
}
```

The following extension makes any type `T` with an implicit `Eql[T]` instance implement `HasEql`:

```scala
extension HasEqlImpl[T : Eql] for T : HasEql[T] {
  def === (that: T): Boolean = implicitly[Eql[T]].eql(this, that)
}
```

### Rules for Instance Declarations

An instance declaration can only implement a single trait, not a combination of traits, nor a class. Furthermore,
the implemented trait must be an interface trait according to the following definition:

**Definition** An _interface trait_ is a trait that
 - has only types and methods as members,
 - does not have initialization statements, and
 - has only interface traits as parents.

