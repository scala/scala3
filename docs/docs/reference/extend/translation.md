---
layout: doc-page
title: "Translation of Extensions"
---

Extensons are closely related to implicit classes and can be translated into them. In short,
an extension that just adds extension methods translates into an implicit value class whereas
an extension with an `implements` clause translates
into a regular implicit class. The following sections sketch this translation.

Conversely, it is conceivable (and desirable) to replace most usages of implicit classes and value classes by extensions and [opaque types](../opaques.html). We plan to [drop](../dropped/implicit-value-classes.html)
these constructs in future versions of the language. Once that is achieved, the translations described
below can be simply composed with the existing translations of implicit and value classes into the core language. It is
not necessary to retain implicit and value classes as an intermediate step.


### Decomposing Type Patterns

First, define a function `decompose` to decompose a type pattern `TP` into a list of type binders `tparams` and a type `T`. Going from left to right, every type binder `type U <bounds>` in `TP` is added to `tparams` and is replaced by the reference `U` in `T`. For instance, the type pattern

```scala
Map[type K <: AnyRef, List[type T]]
```
would be decomposed into the binders `type K <: AnyRef` and `type T` and the type `Map[K, List[T]]`.

### Computing an Extension Name

Extensions are anonymous -- the first identifier given in an extension designates the type that is extended, not the name of the extension itself. The name of an extension class is computed instead in a predictable way from the extended type pattern and the implemented traits as follows:

 1. Take the sequence of tokens starting with `extends` up to the opening brace of the templete definitions.
 2. Drop an implicit parameter list if one is given and also drop any value arguments to parent constructors.
 3. Drop any tokens that are not keywords of identifiers.
 4. In each token resulting from the previous step, drop all characters that are not legal parts of alphanumeric identifiers. This leaves all characters that satisfy the `java.lang.Character.isUnicodeIdentifierPart` predicate as well as `$`. Among Ascii characters, the retained characters are all letters, digits, as well as `_` and `$`.
 5. Concatenate all non-empty strings resulting from the previous step with `_` separators.

It is an error if two extensions defined in the same scope have the same computed name. These double definition errors can always be avoided by grouping augments together or, as a last resort, defining suitable type aliases. The scheme gives stable names that do not depend on the order in which definitions are given.

### Translation of Extension Methods

Now, a assume an extension

    extend <TP> <params> { <defs> }

where `<params>` can be absent. For simplicity assume that there are no context bounds in
type definitions of `<TP>`. This is not an essential restriction as any such context bounds can be rewritten in a prior step to be evidence paramseters in `<params>`. Let `(<tparams>, <T>)` be the decomposition of `<TP>`.
The extension can be translated to the following implicit value class:

    implicit class <id> <tparams> ($this: <T>) extends AnyVal { <defs'> }

Here `<id>` is the computed extension name
and `<defs'>` results from `<defs>` by augmenting any definition in <defs> with the parameters <params> and
replacing any occurrence of `this` with `$this`.

For example, the extension

```scala
extend Seq[type T: math.Ordering] {
  def indexOfLargest  = this.zipWithIndex.maxBy(_._1)._2
  def indexOfSmallest = this.zipWithIndex.minBy(_._1)._2
}
```

would be translated to:

```scala
implicit class extend_Seq_type_T_math_Ordering [T]($this: List[T]) extends AnyVal {
  def indexOfLargest (implicit $ev: math.Ordering[T]) = $this.zipWithIndex.maxBy(_._1)._2
  def indexOfSmallest(implicit $ev: math.Ordering[T]) = $this.zipWithIndex.minBy(_._1)._2
}
```

### Translation of Extension Implementations

Now, assume an extension

    extend <TP> <params> implements <parents> { <body> }

Let again `(<tparams>, <T>)` be the decomposition of `<TP>`. This extension is translated to

    implicit class <id> <tparams> ($this: <T>) <params> extends <parents> { <body'> }

Again, `<id>` is the computed extension name.
Also as before, `<body'>` is computed from `<body>` by replacing any occurrence of `this` with `$this`. However, all
parameters in <params> now stay on the class definition, instead of being distributed to all members in `<body>`. This is necessary in general, since `<body>` might contain value definitions or other statements that cannot be
parameterized.

For example, the extension

```scala
extend (type T: Eql) implements HasEql[T] {
  def === (that: T): Boolean = implicitly[Eql[T]].eql(this, that)
}
```

would be translated to

```scala
implicit class extend_type_T_Eql_implements_HasEql_T [T]
                  ($this: T)(implicit $ev: Eql[T]) extends HasEql[T] {
    def === (that: T): Boolean = implicitly[Eql[T]].eql($this, that)
  }
}
```
