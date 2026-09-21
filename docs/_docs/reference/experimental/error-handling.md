---
layout: doc-page
title: "Error Handling"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/error-handling.html
---

The language import
```scala
import scala.language.experimental.errorHandling
```
enables a new style of error handling based on maybe types `T?` and result types `T ? E`. The import is legal only under explicit nulls, i.e. setting `-Yexplicit-nulls` must be on.

## Motivation

When it comes to optionals and error handling, do you prefer safety or convenience? You should not have to choose. After all, that's Scala's motto -- combining safety and convenience in one package.

And yet, in this particular area there are real tradeoffs between the two, and they will only get worse.

Take optional data. You can express the absence of a value with `None` or with `null`. Of course, `null`s are terribly unsafe, so Scala programmers have generally avoided them, with some exceptions: Java interop is one, high-performance code another. But with explicit nulls, the balance shifts a little. Nulls are now safer to use, because the type system knows whether a value can be null or not. That puts them closer to `Option` when it comes to safety. And nulls are both more convenient and more efficient than `Option`.

They are more convenient since we don't have to wrap a value with `Some` to make it an `Option`. Say we have a function `f` that takes a parameter `nickname` of type `String` that could be undefined. If `f` was defined like this
```scala
  def f(nickname: Option[String])
```
we'd have to call it with `f(Some("Pete"))`. The `Some` is annoying, since it is clear that `"Pete"` is not `None`, so no ceremonial wrapping should be needed. By contrast, if we define `f` like this
```scala
  def f(nickname: String | Null)
```
then we _can_ call it with just `f("Pete")`. That's not only more convenient, it is also more efficient, since no wrapper is needed.

On the other hand, even with explicit nulls, `T | Null` is not as safe as `Option[T]`. The problem is that `T | Null` is not parametric, which is to say that it does not always produce a type that's different from `T` -- `(String | Null) | Null` is the same as `String | Null`. This is a problem if, for instance, you want to use `null` to signal a missing entry when looking up a value in a map. If `lookup` is defined like this
```scala
  def lookup[Key, Value](m: Map[Key, Value], k: Key): Value | Null
```
and `Value` is instantiated with `String | Null`, then a returned `null` is indistinguishable from a missing entry. In other words, abstractions using `Null` types are leaky and lead to fragile code.

So, even with explicit nulls arriving, there are still good reasons to stick with the parametric types `Option` or `Either`. It's just a shame that these are less convenient and efficient.

But what if we _don't_ have to choose? What if there was a type constructor that is parametric and at the same time just as efficient as, and even more convenient than, unions with `Null`? Such a type constructor can be designed, if we assume a little bit of support from the compiler. The rest of this note explains how.

## The best type for optional values

Ideally, the construct to express optional values should combine the best aspects of `Option` and union types. Like union types, it should require no ceremonial wrapping in `Some` if the intent is clear, which helps both readability and performance. But like `Option`, it should be parametric. And as an extra bonus, it should provide an easy way to upgrade from legacy code using nulls.

We can achieve this by designing a new type with carefully crafted semantics and subtyping and typing rules. Let's call that new type `T?` (pronounced _maybe T_), acting as a replacement for `Option[T]`.

`T?` is used in C#, Kotlin, and other languages to mean essentially `T | Null`. The type proposed here has a crucial difference that makes it parametric: internally, the maybe type `T?` can be seen as a union of _three_ possible types, `T`, `Null`, and `Valid`.
```scala
  opaque type T? = T | Null | Valid
```
`Valid` is an internal type that can be represented by the following case class:
```scala
  case class Valid(elem: Any)
```
`Valid(x)` represents a "valid value", even if the element `x` happens to be `null`. Compared to `Option`, we have the following analogies:
```scala
  null                  None
  Valid(null)           Some(None)
  Valid(Valid(null))    Some(Some(None))
  ...
```
In fact, `Valid` can only wrap elements that are either `null` or other `Valid` instances. But this is not enforced in its type signature, since `Valid` is hidden from user programs anyway. In place of `Valid`, there is a public-facing `Ok` data constructor that evaluates as follows:
```scala
  Ok(x)     --->     Valid(x)   if x == null or x is a Valid instance
            --->     x          otherwise
```
That is, `Ok(x)` is simply `x`, unless `x` is `null` or some wrapped version of `null`.

To take a maybe type apart, you can use a pattern match, just like for `Option`. `Ok` corresponds to `Some`, and `null` corresponds to `None`.
```scala
  def maybeReverse(s: String?): String? = s match
    case Ok(str) => str.reverse
    case null    => null
```
That pattern match can be compiled to very efficient code.

The representation of maybe types is very similar to [@sjrd](https://github.com/sjrd)'s [unboxed option type](https://github.com/sjrd/scala-unboxed-option). The main differences are that maybe types identify `null` with `None`, and that they allow automatic widening through the following **subtyping rules**:

 - `Null <: T?`, for all types `T`
 - `T <: T?`, for all types `T` that are disjoint from `Null`. The disjointness test is exactly the test used for match type reduction.
 - `T? <: T | Null`, for all types `T` that are disjoint from `Null`.
 - The maybe type constructor is covariant: if `T1 <: T2` then `T1? <: T2?`.

Combining these subtyping rules with the rules for union types, we can also derive that `T | Null` is equivalent to `T?` by mutual subtyping if `T` is known to be disjoint from `Null`, i.e. `T | Null <: T? <: T | Null`.

Under explicit nulls, Java types `J` often get mapped to `J | Null`. The equivalence means that we can treat these types as maybe types `J?`, as long as `J` is disjoint from `Null` (which is the most common case by far).

**Erasure**

The erasure of `T?` is the erasure of `T` if `T` is a reference type that is disjoint from `Null`, and `Object` otherwise. For instance, the following overloads are possible, since `String` and `List[String]` are concrete types that do not contain `null`:
```scala
  def f(x: String?) = ...
  def f(x: List[String]?) = ...
```
If we replace `?` with `Option`, then the erasure of the two arguments would be the same, and we'd need a `@targetName` annotation on one of the methods.

Conversely, the following overloads would clash:
```scala
  def f[T](x: T?) = ...
  def f[T](x: T) = ...
```
Here, both `T` and `T?` erase to `Object`. On the other hand, the same example written with an `Option` argument would pass:
```scala
  def f[T](x: Option[T]) = ...
  def f[T](x: T) = ...
```

**Evaluation**

 `T?` combines the best aspects of both `Option[T]` and `T | Null`.

 - Like `Option[T]`, it is parametric. If type arguments `A` and `B` are different, then so are `A?` and `B?`.
 - If `T` is known to not contain `null` (i.e. in most cases), it can be widened automatically to `T?`, just like `T | Null`.
 - `T?` is practically as efficient as `T | Null`. For injection, most types are widened automatically, which is free. Even when injection goes through `Ok(t)`, a single type test will in most cases establish that no wrapping is needed. An extra object is created only in the case where we do wrap `null` as a normal value, and this case should be rare. For decomposition, the situation is similar. If the type `T` is known to not contain `null`, decomposition of `T?` amounts to a single comparison with `null`, plus a downcast. Otherwise, we need one additional type test.

Since `T?` is also shorter to write than either `T | Null` or `Option[T]`, there should be a natural tendency to make it the preferred solution for all new code.


## The best type for error handling

`T?` generalizes naturally to a type that's ideal for error handling. It can be seen as a special case of a result type `T ? E`, which can also carry additional error information of type `E` for missing values. So `T ? E` (pronounced _result T or E_) would be an alternative to `Either[E, T]`.

To go from values `T` to results `T ? E` and back, we use `Ok` as before. For the error part, which was handled by just `null` for maybe types, we now use a new constructor and extractor `Err`. Example:
```scala
  def testPos(x: Int): Int ? String =
    if x >= 0 then x else Err(s"negative $x")

  def usePos(x: Int): Int = testPos(x) match
    case Ok(y) => y
    case Err(s) =>
      log(s)
      0
```
The maybe type `T?` is now simply an abbreviation for `T ? Unit`, a result type where the error component carries no particular information. One tricky aspect is that there are now two ways to signal an error for a maybe type: `null` and `Err(())`. The two ways must come down to the same representation. So we make sure in the `Err` constructor that `Err(()) = null`, and in the `Err` extractor that a `null` value matches an `Err(())` pattern.

The mechanics of all this are a straightforward extension of the scheme for maybe types.

Internally, the result type `T ? E` can be seen as a union of four possible types:
```scala
  opaque type T ? E = T | Valid | Null | Fail[E]
```
Here, `Fail` is the type of invalid (error) values. Like `Valid`, it is an internal type. It can be represented by the following case class:
```scala
  case class Fail[+E](elem: E)
```

The `Err` constructor produces `null` if it is given a unit argument `()`.
```
Err(x)    --->     null      if x == ()
          --->     Fail(x)   otherwise
```
The `Err` pattern match goes the other way, producing a `()` error value when matching `null`.

The `Ok` constructor is now defined as follows:
```scala
  Ok(x)     --->    Valid(x)   if x == null or x is a `Valid` or `Fail` instance
            --->    x          otherwise
```

The subtyping rules subsume the ones for maybe types. We have additionally:

 - `Fail[E] <: T ? E`, for all types `T` and `E`.
 - `T <: T ? E`, if `T` is disjoint from both `Null` and `Fail[Any]`.
 - The result type constructor is also covariant in its error part: if `E1 <: E2` then `T ? E1 <: T ? E2`.

## One error type to rule them all

The new type `T ? E` can express a panoply of existing types in Scala:

```scala
  Option[T]       ~~    T ? Unit  =  T?
  Either[E, T]    ~~    T ? E
  Try[T]          ~~    T ? Exception
```

Arguably, `T ? E` is more efficient and ergonomic than these types. For instance, compared to `Either[E, T]`, `T ? E` is

 - more ergonomic, because you don't need ceremonial `Right(...)` wrapping,
 - more efficient, because the runtime usually does not wrap either,
 - more intuitive, because result and error parts appear in the natural order.

Another big advantage is that `T ? E` is a single type with a large usability spectrum, covering several existing types. So you have to learn error handling patterns only once, and it becomes easier to build re-usable abstractions for error handling (more on that below).

On the other hand, the existing types won't go away, and current and future code bases will surely continue to use them. This is fine. I foresee that adoption of maybe types and result types will begin in codebases where interop with Java is needed, and in greenfield projects where one can start from scratch. If `T?` manages to convince people not to use the non-parametric `T | Null` form, it's already a win.

## Higher Level Usage Patterns

Optionals and error handling are often used in higher-level abstractions. For instance, both `Option` and `Either` can be used in for expressions, which replace explicit pattern matching and construction with a higher-level monadic abstraction. Result types can do that as well. The standard library defines the appropriate `map`, `flatMap` and `withFilter` functions to make this work.

As an example of monadic error handling, consider the task of parsing a string as a date in the format "`day/month/year`". For parsing integers, we define an extension method `parseInt`:
```scala
  extension (str: String) def parseInt: Int? =
    try str.toInt
    catch case ex: NumberFormatException => null
```
`parseDate` can then be written as follows:
```scala
  case class Date(day: Int, month: Int, year: Int)

  def parseDate(str: String): Date? =
    str.split("/") match
      case Array(d, m, y) =>
        for
          day <- d.parseInt
          month <- m.parseInt
          year <- y.parseInt
        yield
          Date(day, month, year)
      case _ =>
        null
```
In fact, this code would look exactly the same if we had used `Option[T]` instead of `T?`.

## Direct Style

We can also define higher-level direct style abstractions that are more flexible and efficient than the monadic ones.

Since we already spent the postfix `?` syntax on types, we might as well use the same syntax on terms to support direct style ([Ox](https://github.com/softwaremill/ox) and [Steps](https://github.com/lampepfl/steps) use `.ok` instead). So we define a postfix operator `?` for terms as well. How it works is best illustrated by porting the `parseDate` function above to direct style:
```scala
  def parseDate(str: String): Date? =
    str.split("/") match
      case Array(d, m, y) =>
        maybe:
          Date(d.parseInt?, m.parseInt?, y.parseInt?)
      case _ =>
        null
```
Here, each usage of `?` works on a left operand of type `Int?`. It checks that the operand is an `Ok` value and produces the underlying integer. If the operand is `null` instead, it aborts to the enclosing `maybe` scope.

That mechanism can be implemented in the library, using implicit function types and `boundary`. The `maybe` object defines an error capability that allows to abort with an error of a given type `E`:

```scala
@experimental
object maybe:
  type CanErr[E] = boundary.Label[Nothing ? E]

```
It also defines an `apply` method that opens a `CanErr` scope:

```scala
  inline def apply[T, E](inline body: CanErr[E] ?=> T): T ? E =
    boundary(Ok(body))
```
It runs its `body` while providing an abort capability, and wraps the final result in `Ok`.

The `?` postfix operator is defined as follows in the `Maybe` companion object:
```scala
  extension [T, E](x: T ? E)
    inline def ? (using CanErr[E]): T = x match
      case Ok(y) => y
      case Err(e) => break(Err(e))
```
Because everything is inline, the existing implementation of `boundary` will translate `maybe` blocks to tight code that uses jumps instead of exceptions for aborting.

If we look at the implied typing rules for `maybe` and `?`, we notice a pleasing duality:

```
          t: R ? E
    ----------------------
     t?: CanErr[E] ?=> R

      t: CanErr[E] ?=> R
    ----------------------
        maybe(t): R ? E
```
So, in terms of types, `maybe` and `?` are duals of each other. `maybe` maps a body with implicit function type to a result, whereas `?` maps that result to an implicit function type.

## Internals

Maybe types are represented internally as instances of trait `scala.compiletime.Maybe`. `T?` is represented as `Maybe[T, Unit]` and `T ? E` is represented as `Maybe[T, E]`. The `Maybe` trait
is defined as follows:

    ```scala
    package scala.compiletime

    @experimental
    sealed trait Maybe[+T, +E] extends Any, Matchable:
      def isEmpty: Boolean
      private[compiletime] def get: T
    ```
  The trait is a only a compiletime artifact, since the erasure of a maybe type is either the underlying `result` type or `Object`

  The trait has members `isEmpty` and `get`, which makes it eligible as a
  result type of `unapply` methods. Their implementations are special-cased in the pattern matcher.

  The `get` method is not accessible from user programs. Therefore, the only way to decompose a maybe type is via a pattern match.

## Utility Methods

 - The companion object of `Maybe` defines extension methods on maybe and result types:

    ```scala
    object Maybe:
      extension [A, E](x: A ? E])
        transparent inline def ? (using maybe.CanErr[E]): A = x match
          case Ok(y) => y
          case Err(e) => break(Err(e))

        def withErr[E1](e: E1): A ? E1 = x match
          case Ok(y) => Ok(y)
          case Err(_) => Err(e)

        def mapErr[E1](f: E => E1): A ? E1 = x match
          case Ok(y) => Ok(y)
          case Err(e) => Err(f(e))

        def map[B](f: A => B): A ? E = x match
          case Ok(y) => Ok(f(y))
          case Err(e) => Err(e)

        def flatMap[B](f: A => B ? E): B ? E = x match
          case Ok(y) => f(y)
          case Err(e) => Err(e)

        ...
    ```



