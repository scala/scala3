---
layout: doc-page
title: Relationship with Scala 2 Implicits
---

Many, but not all, of the new contextual abstraction features in Scala 3 can be mapped to Scala 2's implicits. This page gives a rundown on the relationships between new and old features.

## Simulating Contextual Abstraction with Implicits

### Implied Instance Definitions

Implied instance definitions can be mapped to combinations of implicit objects and implicit methods together with normal classes.

 1. Instance definitions without parameters are mapped to implicit objects. E.g.,
    ```scala
      implied IntOrd for Ord[Int] { ... }
    ```
    maps to
    ```scala
      implicit object IntOrd extends Ord[Int] { ... }
    ```
 2. Parameterized instance definitions are mapped to combinations of classes and implicit methods. E.g.,
    ```scala
      implied ListOrd[T] for Ord[List[T]] given (ord: Ord[T]) { ... }
    ```
    maps to
    ```scala
      class ListOrd[T](implicit ord: Ord[T]) extends Ord[List[T]] { ... }
      final implicit def ListOrd[T](implicit ord: Ord[T]): ListOrd[T] = new ListOrd[T]
    ```
 3. Implied alias instances map to implicit methods. If an alias has neither type parameters nor a given clause, its right-hand side is cached in a variable. There are two cases that can be optimized:

  - If the right hand side is a simple reference, we can
    use a forwarder to that reference without caching it.
  - If the right hand side is more complex, but still known to be pure, we can
    create a `val` that computes it ahead of time.

 Examples:

    ```scala
      implied global for ExecutionContext = new ForkJoinContext()
      implied config for Config = default.config

      val ctx: Context
      implied for Context = ctx
    ```
    would map to
    ```scala
      private[this] var global$cache: ExecutionContext | Null = null
      final implicit def global: ExecutionContext = {
        if (global$cache == null) global$cache = new ForkJoinContext()
        global$cache
      }

      final implicit val config: Config = default.config

      final implicit def Context_repr = ctx
    ```

### Anonymous Implied Instances

Anonymous instances get compiler synthesized names, which are generated in a reproducible way from the implemented type(s). For example, if the names of the `IntOrd` and `ListOrd` instances above were left out, the following names would be synthesized instead:
```scala
  implied Ord_Int_instance for Ord[Int] { ... }
  implied Ord_List_instance[T] for Ord[List[T]] { ... }
```
The synthesized type names are formed from

 - the simple name(s) of the implemented type(s), leaving out any prefixes,
 - the simple name(s) of the toplevel argument type constructors to these types
 - the suffix `_instance`.

Anonymous instances that define extension methods without also implementing a type
get their name from the name of the first extension method and the toplevel type
constructor of its first parameter. For example, the instance
```scala
  implied {
     def (xs: List[T]) second[T] = ...
  }
```
gets the synthesized name `second_of_List_T_instance`.

### Context Parameters

The new context parameter syntax with `given` corresponds largely to Scala-2's implicit parameters. E.g.
```scala
  def max[T](x: T, y: T) given (ord: Ord[T]): T
```
would be written
```scala
  def max[T](x: T, y: T)(implicit ord: Ord[T]): T
```
in Scala 2. The main difference concerns applications of such parameters.
Explicit arguments to context parameters _must_ be written using `given`,
mirroring the definition syntax. E.g, `max(2, 3) given IntOrd`.
Scala 2 uses normal applications `max(2, 3)(IntOrd)` instead. The Scala 2 syntax has some inherent ambiguities and restrictions which are overcome by the new syntax. For instance, multiple implicit parameter lists are not available in the old syntax, even though they can be simulated using auxiliary objects in the "Aux" pattern.

The `the` method corresponds to `implicitly` in Scala 2.
It is precisely the same as the `the` method in Shapeless.
The difference between `the` (in both versions) and `implicitly` is
that `the` can return a more precise type than the type that was
asked for.

### Context Bounds

Context bounds are the same in both language versions. They expand to the respective forms of implicit parameters.

**Note:** To ease migration, context bounds in Dotty map for a limited time to old-style implicit parameters for which arguments can be passed either with `given` or
with a normal application. Once old-style implicits are deprecated, context bounds
will map to given clauses instead.

### Extension Methods

Extension methods have no direct counterpart in Scala 2, but they can be simulated with implicit classes. For instance, the extension method
```scala
  def (c: Circle) circumference: Double = c.radius * math.Pi * 2
```
could be simulated to some degree by
```scala
  implicit class CircleDeco(c: Circle) extends AnyVal {
    def circumference: Double = c.radius * math.Pi * 2
  }
```
Extension methods in implicit instances have no direct counterpart in Scala-2. The only way to simulate these is to make implicit classes available through imports. The Simulacrum macro library can automate this process in some cases.

### Typeclass Derivation

Typeclass derivation has no direct counterpart in the Scala 2 language. Comparable functionality can be achieved by macro-based libraries such as Shapeless, Magnolia, or scalaz-deriving.

### Contextual Function Types

Contextual function types have no analogue in Scala 2.

### Implicit By-Name Parameters

Implicit by-name parameters are not supported in Scala 2, but can be emulated to some degree by the `Lazy` type in Shapeless.

## Simulating Scala 2 Implicits in Dotty

### Implicit Conversions

Implicit conversion methods in Scala 2 can be expressed as implied instances
of the `scala.Conversion` class in Dotty. E.g. instead of
```scala
  implicit def stringToToken(str: String): Token = new Keyword(str)
```
one can write
```scala
  implied stringToToken for Conversion[String, Token] {
    def apply(str: String): Token = new KeyWord(str)
  }
```

### Implicit Classes

Implicit classes in Scala 2 are often used to define extension methods, which are directly supported in Dotty. Other uses of implicit classes can be simulated by a pair of a regular class and a conversion instance.

### Implicit Values

Implicit `val` definitions in Scala 2 can be expressed in Dotty using a regular `val` definition and an alias instance. E.g., Scala 2's
```scala
  lazy implicit val pos: Position = tree.sourcePos
```
can be expressed in Dotty as
```scala
  lazy val pos: Position = tree.sourcePos
  implied for Position = pos
```

### Abstract Implicits

An abstract implicit `val` or `def` in Scala 2 can be expressed in Dotty using a regular abstract definition and an implied alias instaance. E.g., Scala 2's
```scala
  implicit def symDeco: SymDeco
```
can be expressed in Dotty as
```scala
  def symDeco: SymDeco
  implied for SymDeco = symDeco
```

## Implementation Status and Timeline

The Dotty implementation implements both Scala-2's implicits and the new abstractions. In fact, support for Scala-2's implicits is an essential part of the common language subset between 2.13/2.14 and Dotty.
Migration to the new abstractions will be supported by making automatic rewritings available.

Depending on adoption patterns, old style implicits might start to be deprecated in a version following Scala 3.0.
