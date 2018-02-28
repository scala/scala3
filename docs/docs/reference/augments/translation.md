---
layout: doc-page
title: "Translation of Augmentations"
---

Augmentations are closely related to implicit classes and can be translated into them. In short,
a method augmentation translates into an implicit value class and a trait implementation translates
into a regular implicit class. The following sections sketch this translation.

Conversely, it is conceivable (and desirable) to replace most usages of implicit classes and value classes by augmentations and [opaque types](../opaques.html). We plan to [drop](../dropped/implicit-value-classes.html)
these constructs in future versions of the language. Once that is achieved, the translations described
below can be simply composed with the existing translations of implicit and value classes into the core language. It is
not necessary to retain implicit and value classes as an intermediate step.


### Decomposing Type Patterns

First, define a function `decompose` to decompose a type pattern `TP` into a list of type binders `tparams` and a type `T`. Going from left to right, every type binder `type U <bounds>` in `TP` is added to `tparams` and is replaced by the reference `U` in `T`. For instance, the type pattern

```scala
Map[type K <: AnyRef, List[type T]]
```
would be decomposed into the binders `type K <: AnyRef` and `type T` and the type `Map[K, List[T]]`.

### Translation of Method Augmentations

Now, a assume a method augmentation

    augment <id> @ <TP> <params> { <defs> }

where `<id> @` or `<params>` can be absent. For simplicity assume that there are no context bounds in
type definitions of `<TP>`. This is no essential restriction as any such context bounds can be rewritten in a prior step to be evidence paramseters in `<params>`. Let `(<tparams>, <T>)` be the decomposition of `<TP>`.
The augment clause can be translated to the following implicit value class:

    implicit class <id> <tparams> ($this: <T>) extends AnyVal { <defs'> }

Here `<defs'>` results from `<defs>` by augmenting any definition in <defs> with the parameters <params> and
replacing any occurrence of `this` with `$this`. If the label `<id> @` is missing, a fresh compiler-generated name is chosen instead as the name of the implicit class.

For example, the augmentation

```scala
augment seqOps @ Seq[type T: math.Ordering] {
  def indexOfLargest  = this.zipWithIndex.maxBy(_._1)._2
  def indexOfSmallest = this.zipWithIndex.minBy(_._1)._2
}
```

would be translated to:

```scala
implicit class seqOps[T]($this: List[T]) extends AnyVal {
  def indexOfLargest (implicit $ev: math.Ordering[T]) = $this.zipWithIndex.maxBy(_._1)._2
  def indexOfSmallest(implicit $ev: math.Ordering[T]) = $this.zipWithIndex.minBy(_._1)._2
}
```

### Translation of Trait Augmentations

Now, assume a trait augmentation

    augment <id> @ <TP> <params> extends <parents> { <body> }

Let again `(<tparams>, <T>)` be the decomposition of `<TP>`. This augmentation is translated to

    implicit class <id> <tparams> ($this: <T>) <params> extends <parents> { <body'> }

As before, `<body'>` results from `<body>` by replacing any occurrence of `this` with `$this`. However, all
parameters in <params> now stay on the class definition, instead of beging distributed to all members in `<body>`. This is necessary in general, since `<body>` might contain value definitions or other statements that cannot be
parameterized.

For example, the trait augmentation

```scala
class Test {
  augment (type T: Eql) extends HasEql[T] {
    def === (that: T): Boolean = implicitly[Eql[T]].eql(this, that)
  }
}
```

would be translated to

```scala
class Test {
  implicit class Test$$_augment_T_to_HasEql_1 [T]
                  ($this: T)(implicit $ev: Eql[T]) extends HasEql[T] {
    def === (that: T): Boolean = implicitly[Eql[T]].eql($this, that)
  }
}
```

where the name `Test$$_augment_T_to_HasEql_1` is compiler generated and implementation dependent.