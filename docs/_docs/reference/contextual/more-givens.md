---
layout: doc-page
title: "Other Forms Of Givens"
nightlyOf: https://docs.scala-lang.org/scala3/reference/contextual/givens.html
---

The concept of given instances is quite general. This page covers forms of givens that were not treated before.

## Simple Structural Givens

Some givens simply instantiate a class without needing an alias or additional member declarations. Example:

```scala
class IntOrd extends Ord[Int]:
  def compare(x: Int, y: Int) =
    if x < y then -1 else if x > y then +1 else 0

given IntOrd()
```
In this case, the given clause consists of just a class creation expression, such as `IntOrd()` above.

## Conditional Givens with Parameters

Conditional givens can also be defined with parameters. Example:
```scala
given (config: Config) => Factory = MemoizingFactory(config)
```
Here, `(config: Config)` describes a context parameter expressing a condition: We can synthesize a given `Factory` _provided_ we can synthesize a given `config` of type `Config`.

Type parameters and context parameters can be combined. For instance the `listOrd` instance above could alternatively be expressed like this:
```scala
given listOrd: [T] => Ord[T] => Ord[List[T]]:
  ...
  def compare(x: List[T], y: List[T]) = ...
```
As the example shows, each parameter section is followed by an `=>`.

It is also possible to name context parameters:
```scala
given listOrd: [T] => (ord: Ord[T]) => Ord[List[T]]:
  ...
```

## By Name Givens

We sometimes find it necessary that a given alias is re-evaluated each time it is called. For instance, say we have a mutable variable `curCtx` and we want to define a given that returns the current value of that variable. A normal given alias will not do since by default given aliases are mapped to lazy vals.

In general, we want to avoid re-evaluation of givens. But there are situations like the one above where we want to specify _by-name_ evaluation instead. This is achieved by writing a conditional given with an empty parameter list:
```scala
  val curCtx: Context
  given context: () => Context = curCtx
```
With this definition, each time a `Context` is summoned we evaluate `context` function, which produces the current value of `curCtx`.

## Given Macros

Given aliases can have the `inline` and `transparent` modifiers.
Example:

```scala
transparent inline given mkAnnotations: [A, T] => Annotations[A, T] = ${
  // code producing a value of a subtype of Annotations
}
```

Since `mkAnnotations` is `transparent`, the type of an application is the type of its right-hand side, which can be a proper subtype of the declared result type `Annotations[A, T]`.

Structural givens can also have the `inline` modifier. But the `transparent` modifier is not allowed for them as their type is already known from the signature.

Example:

```scala
trait Show[T]:
  inline def show(x: T): String

inline given Show[Foo]:
  inline def show(x: Foo): String = ${ ... }

def app =
  // inlines `show` method call and removes the call to `given Show[Foo]`
  summon[Show[Foo]].show(foo)
```
Note that inline methods within given instances may be `transparent`.

<!--
The inlining of given instances will not inline/duplicate the implementation of the given, it will just inline the instantiation of that instance.
This is used to help dead code elimination of the given instances that are not used after inlining.

-->

## Pattern-Bound Given Instances

Given instances can also appear in patterns. Example:

```scala
for given Context <- applicationContexts do

pair match
  case (ctx @ given Context, y) => ...
```

In the first fragment above, anonymous given instances for class `Context` are established by enumerating over `applicationContexts`. In the second fragment, a given `Context`
instance named `ctx` is established by matching against the first half of the `pair` selector.

In each case, a pattern-bound given instance consists of `given` and a type `T`. The pattern matches exactly the same selectors as the type ascription pattern `_: T`.

## Negated Givens


We sometimes want to have an implicit search succeed if a given instance for some other type is _not_ available. There is a special class [`scala.util.NotGiven`](https://scala-lang.org/api/3.x/scala/util/NotGiven.html) that implements this kind of negation.

For any query type `Q`, [`NotGiven[Q]`](https://scala-lang.org/api/3.x/scala/util/NotGiven.html) succeeds if and only if the implicit
search for `Q` fails, for example:

```scala
import scala.util.NotGiven

trait Tagged[A]

case class Foo[A](value: Boolean)
object Foo:
  given fooTagged: [A] => Tagged[A] => Foo[A] = Foo(true)
  given fooNotTagged: [A] => NotGiven[Tagged[A]] => Foo[A] = Foo(false)

@main def test(): Unit =
  given Tagged[Int]()
  assert(summon[Foo[Int]].value) // fooTagged is found
  assert(!summon[Foo[String]].value) // fooNotTagged is found
```

## Summary

Here is a summary of common forms of given clauses:

```scala
  // Simple typeclass
  given Ord[Int]:
    def compare(x: Int, y: Int) = ...

  // Parameterized typeclass with context bound
  given [A: Ord] => Ord[List[A]]:
    def compare(x: List[A], y: List[A]) = ...

  // Parameterized typeclass with context parameter
  given [A] => Ord[A] => Ord[List[A]]:
    def compare(x: List[A], y: List[A]) = ...

  // Parameterized typeclass with named context parameter
  given [A] => (ord: Ord[A]) => Ord[List[A]]:
    def compare(x: List[A], y: List[A]) = ...

  // Simple alias
  given Ord[Int] = IntOrd()

  // Parameterized alias with context bound
  given [A: Ord] => Ord[List[A]] =
    ListOrd[A]

  // Parameterized alias with context parameter
  given [A] => Ord[A] => Ord[List[A]] =
    ListOrd[A]

  // Deferred given
  given Context = deferred

  // By-name given
  given () => Context = curCtx
```

All of these clauses also exist in named form:
```scala
  // Simple typeclass
  given intOrd: Ord[Int]:
    def compare(x: Int, y: Int) = ...

  // Parameterized typeclass with context bound
  given listOrd: [A: Ord] => Ord[List[A]]:
    def compare(x: List[A], y: List[A]) = ...

  // Parameterized typeclass with context parameter
  given listOrd: [A] => Ord[A] => Ord[List[A]]:
    def compare(x: List[A], y: List[A]) = ...

  // Parameterized typeclass with named context parameter
  given listOrd: [A] => (ord: Ord[A]) => Ord[List[A]]:
    def compare(x: List[A], y: List[A]) = ...

  // Simple alias
  given intOrd: Ord[Int] = IntOrd()

  // Parameterized alias with context bound
  given listOrd: [A: Ord] => Ord[List[A]] =
    ListOrd[A]

  // Parameterized alias with context parameter
  given listOrd: [A] => Ord[A] => Ord[List[A]] =
    ListOrd[A]

  // Abstract or deferred given
  given context: Context = deferred

  // By-name given
  given context: () => Context = curCtx
```
