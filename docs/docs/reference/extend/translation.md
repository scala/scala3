---
layout: doc-page
title: "Translation of Extensions"
---

**Note:** This section gives an explanation how extension methods and instance declarations could be mapped to value classes and implicit classes. It does not consider typeclass traits. Given that value classes and implicit classes are supposed to be replaced by
extension methods and instance declarations, it would be good to present a translation that bypasses value classes and that also extends to typeclass traits. There are several
possibilities for such a translation. We should to experiment further to clarify which
scheme to prefer and flesh it out in detail.

Here are links to two possible encodings:

 - A test in this PR: https://github.com/dotty-staging/dotty/blob/add-common/tests/pos/typeclass-encoding3.scala
 - An alternative encoding:  https://gist.github.com/OlivierBlanvillain/d314ddbcb640e2ce5604d860b5073008.

---

Extensions are closely related to implicit classes and can be translated into them. In short,
an extension that just adds extension methods translates into an implicit value class whereas an instance declaration translates into a regular implicit class. The following sections sketch this translation.

Conversely, it is conceivable (and desirable) to replace most usages of implicit classes and value classes by extensions and [opaque types](../opaques.html). We plan to [drop](../dropped/implicit-value-classes.html)
these constructs in future versions of the language. Once that is achieved, the translations described
below can be simply composed with the existing translations of implicit and value classes into the core language. It is
not necessary to retain implicit and value classes as an intermediate step.


### Translation of Extension Methods

Assume an extension

    extension <id> <type-params> <implicit-params> for <type> { <defs> }

where both `<type-params>` and `<implicit-params>` can be absent.
For simplicity assume that there are no context bounds on any of the type parameters
in `<type-params>`. This is not an essential restriction as any such context bounds can be rewritten in a prior step to be evidence parameters in `<implicit-params>`.

The extension is translated to the following implicit value class:

    implicit class <id> <type-params> (private val $this: <type>) extends AnyVal {
      import $this._
      <defs'>
    }

Here `<defs'>` results from `<defs>` by augmenting any definition in <defs> with the parameters <implicit-params> and replacing any occurrence of `this` with `$this`.

For example, the extension

```scala
extension SeqOps[T : math.Ordering] for Seq[T] {
  def indexOfLargest  = this.zipWithIndex.maxBy(_._1)._2
  def indexOfSmallest = zipWithIndex.minBy(_._1)._2
}
```

would be translated to:

```scala
implicit class SeqOps[T](private val $this: List[T]) extends AnyVal {
  import $this._
  def indexOfLargest (implicit $ev: math.Ordering[T]) = $this.zipWithIndex.maxBy(_._1)._2
  def indexOfSmallest(implicit $ev: math.Ordering[T]) = zipWithIndex.minBy(_._1)._2
}
```

### Translation of Instance Declarations

Now, assume an extension

    extension <id> <type-params> <implicit-params> for <type> : <parents> { <body> }

where `<type-params>`, `<implicit-params>` and `<type>` are as before.
This extension is translated to

    implicit class <id> <type-params> ($this: <type>) <implicit-params> extends <parents> {
      import $this._
      <body'>
    }

As before, `<body'>` is computed from `<body>` by replacing any occurrence of `this` with `$this`. However, all parameters in <implicit-params> now stay on the class definition, instead of being distributed to all members in `<body>`. This is necessary in general, since `<body>` might contain value definitions or other statements that cannot be
parameterized.

For example, the extension

```scala
extension HasEqlImpl[T : Eql) for T : HasEql[T] {
  def === (that: T): Boolean = implicitly[Eql[T]].eql(this, that)
}
```

would be translated to

```scala
implicit class HasEqlForEql[T]($this: T)(implicit $ev: Eql[T]) extends HasEql[T] {
  import $this._
  def === (that: T): Boolean = implicitly[Eql[T]].eql($this, that)
}
```
