---
layout: doc-page
title: "Extension Implementations"
---

In addition to adding methods, an extension can also implement traits and classes. For instance,

```scala
trait HasArea {
  def area: Double
}

extend Circle implements HasArea {
  def area = this.radius * this.radius * math.Pi
}
```

This extension makes `Circle` implement the `HasArea` trait. Specifically, it defines an implicit subclass of `HasArea`
which takes a `Circle` as argument and provides the given implementation. Hence, the implementation of the extension above would be like this

```scala
implicit class circleOps($this: Circle) extends HasArea {
  def area = $this.radius * $this.radius * math.Pi
}
```

An extension implementation can thus provide a kind of "implements" relationship that can be defined independently of the types it connects.

### Adding Implementations to Generic Traits

Just like extension methods, extension implementations can also be generic and their type parameters can have bounds.

For example, assume we have the following two traits, which define binary and unary (infix) equality tests:

```scala
trait Eql[T] {
  def eql (x: T, y: T): Boolean
}

trait HasEql[T] {
  def === (that: T)
}
```

The following extension makes any type `T` with an implicit `Eql[T]` instance implement `HasEql`:

```scala
extend (type T: Eql) implements HasEql[T] {
  def === (that: T): Boolean = implicitly[Eql[T]].eql(this, that)
}
```

### Syntax of Extensions

The syntax of extensions is specified below as a delta with respect to the Scala syntax given [here](http://dotty.epfl.ch/docs/internals/syntax.html)

    Extension           ::=  ‘extend’ BindingTypePattern
                             [[nl] ImplicitParamClause] ExtensionClause
    ExtensionClause     ::=  ‘implements’ Template
                          |  [nl] ‘{’ ‘def’ DefDef {semi ‘def’ DefDef} ‘}’

    ImplicitParamClause ::=  [nl] ‘(’ ImplicitMods ClsParams ‘)’
    ImplicitMods        ::=  `implicit` [`erased`] | `erased` `implicit`

    BindingTypePattern: :=  AnnotType
    Type              ::=  ...
                        |  ‘type’ TypeParamCore                (if inside a BindingTypePattern)
    TypeParamCore     ::=  id [HkTypeParamClause] TypeParamBounds

In this definition, type patterns and types share the same productions. However, the production

    Type              ::=  ‘type’ TypeParamCore

is applicable only inside a `BindingTypePattern`.


