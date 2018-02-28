---
layout: doc-page
title: "Trait Augmentations"
---

In addition to adding methods, an augmentation can also implement traits and classes. For instance,

```scala
trait HasArea {
  def area: Double
}

augment circleOps @ Circle extends HasArea {
  def area = this.radius * this.radius * math.Pi
}
```

This augemntation makes `Circle` implement the `HasArea` trait. Specifically, it defines an implicit subclass of `HasArea`
which takes a `Circle` as argument and provides the given implementation. Hence, the implementation of the augmentation above would be like this

```scala
implicit class circleOps($this: Circle) extends HasArea {
  def area = $this.radius * $this.radius * math.Pi
}
```

A trait augmentation can thus provide a kind of "extends" relationship that can be defined independently of the types it connects.

### Generic Trait Augmentations

Just like method augmentations, trait augmentations can be generic with and their type parameters can have bounds.

For example, assume we have the following two traits, which define binary and unary (infix) equality tests:

```scala
trait Eql[T] {
  def eql (x: T, y: T): Boolean
}

trait HasEql[T] {
  def === (that: T)
}
```

The following augment makes any type `T` with an implicit `Eql[T]` instance implement `HasEql`:

```scala
augment (type T: Eql) extends HasEql[T] {
  def === (that: T): Boolean = implicitly[Eql[T]].eql(this, that)
}
```

### Syntax of Augmentations

The syntax of augmentations iss pecified below as a delta with respect to the Scala syntax given [here](http://dotty.epfl.ch/docs/internals/syntax.html)

    Augmentation        ::=  ‘augment’ [id ‘@’] BindingTypePattern
                             [[nl] ImplicitParamClause] AugmentClause
    AugmentClause       ::=  ‘extends’ Template
                          |  [nl] ‘{’ ‘def’ DefDef {semi ‘def’ DefDef} ‘}’

    ImplicitParamClause ::=  [nl] ‘(’ ImplicitMods ClsParams ‘)’
    ImplicitMods        ::=  `implicit` [`ghost`] | `ghost` `implicit`

    BindingTypePattern: :=  AnnotType
    Type              ::=  ...
                        |  ‘type’ TypeParamCore                (if inside a BindingTypePattern)
    TypeParamCore     ::=  id [HkTypeParamClause] TypeParamBounds

In this definition, type patterns and types share the same productions. However, the production

    Type              ::=  ‘type’ TypeParamCore

is applicable only inside a `BindingTypePattern`.


