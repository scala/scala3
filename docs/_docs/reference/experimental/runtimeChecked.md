---
layout: doc-page
title: "The runtimeChecked method"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/runtimeChecked.html
---

The `runtimeChecked` method is an extension method, defined in `scala.Predef`. It can be called on any expression. An expression ending in `.runtimeChecked` is exempt from certain static checks in the compiler, for example pattern match exhaustivity. The idiom is intended to replace a `: @unchecked` type ascription in these cases.

## Example

A common use case for `runtimeChecked` is to assert that a pattern will always match, either for convenience, or because there is a known invariant that the types can not express.

E.g. looking up an expected entry in a dynamically loaded dictionary-like structure:
```scala
// example 1
trait AppConfig:
  def get(key: String): Option[String]

val config: AppConfig = ???

val Some(appVersion) = config.get("appVersion").runtimeChecked
```

or to assert that a value can only match some specific patterns:
```scala
// example 2
enum Day:
  case Mon, Tue, Wed, Thu, Fri, Sat, Sun

val weekDay: Option[Day] = ???

weekDay.runtimeChecked match
  case Some(Mon | Tue | Wed | Thu | Fri) => println("got weekday")
// case Some(Sat | Sun) => // weekend should not appear
  case None =>
```

In both of these cases, without `runtimeChecked` there would either be an error (example 1), or a warning (example 2), because statically, the compiler knows that there could be other cases at runtime - so is right to caution the programmer.

```scala
// warning in example 2 when we don't add `.runtimeChecked`.
-- [E029] Pattern Match Exhaustivity Warning: ----------------------------------
6 |weekDay match
  |^^^^^^^
  |match may not be exhaustive.
  |
  |It would fail on pattern case: Some(Sat), Some(Sun)
```

## Safety

The `runtimeChecked` method only turns off static checks that can be soundly performed at runtime. This means that patterns with unchecked type-tests will still generate warnings. For example:
```scala
scala> val xs = List(1: Any)
     | xs.runtimeChecked match {
     |   case is: ::[Int] => is.head
     | }
1 warning found
-- Unchecked Warning: ---------------------------------------
3 |  case is: ::[Int] => is.head
  |       ^
  |the type test for ::[Int] cannot be checked at runtime
  |because its type arguments can't be determined from List[Any]
val res0: Int = 1
```
As the warning hints, the type `::[Int]` can not be tested at runtime on a value of type `List[Any]`, so using `runtimeChecked` still protects the user against assertions that can not be validated.

To fully avoid warnings, as with previous Scala versions, `@unchecked` should be put on the type argument:
```scala
scala> xs.runtimeChecked match {
     |   case is: ::[Int @unchecked] => is.head
     | }
val res1: Int = 1
```


## Specification

We add a new annotation `scala.internal.RuntimeChecked` as a part of the standard Scala 3 library. A programmer is not expected to use this annotation directly.

```scala
package scala.annotation.internal

final class RuntimeChecked extends Annotation
```

Any term that is the scrutinee of a pattern match, and that has a type annotated with `RuntimeChecked`, is exempt from pattern match exhaustivity checking.


The user facing API is augmented with a new extension method `scala.Predef.runtimeChecked`, qualified for any value:
```scala
package scala

import scala.annotation.internal.RuntimeChecked

object Predef:
  ...
  extension [T](x: T)
    inline def runtimeChecked: x.type @RuntimeChecked =
      x: @RuntimeChecked
```

The `runtimeChecked` method returns its argument, refining its type with the `RuntimeChecked` annotation.

## Motivation

As described in [Pattern Bindings](../changed-features/pattern-bindings.md), under `-source:future` it is an error for a pattern definition to be refutable. For instance, consider:
```scala
def xs: List[Any] = ???
val y :: ys = xs
```

This compiled without warning in 3.0, became a warning in 3.2, and we would like to make it an error by default in a future 3.x version.
As an escape hatch in 3.2 we recommended to use a type ascription of `: @unchecked`:
```
-- Warning: ../../new/test.scala:6:16 -----------------------
6 |  val y :: ys = xs
  |                ^^
  |pattern's type ::[Any] is more specialized than the right
  |hand side expression's type List[Any]
  |
  |If the narrowing is intentional, this can be communicated
  |by adding `: @unchecked` after the expression,
  |which may result in a MatchError at runtime.
```

However, `: @unchecked` is syntactically awkward, and is also a misnomer - in fact in this case the the pattern _is_ fully checked, but the necessary checks occur at runtime. The `runtimeChecked` method is intended to replace `@unchecked` for this purpose. 

The `@unchecked` annotation is still retained for silencing warnings on unsound type tests.

### Restoring Scala 2.13 semantics with runtimeChecked

In Scala 3, the `: @unchecked` type ascription has the effect of turning off all pattern-match warnings on the match scrutinee - this differs from 2.13 in which it strictly turns off only pattern exhaustivity checking. `runtimeChecked` restores the semantics of Scala 2.13.
