---
layout: doc-page
title: "The `into` Type"
redirectFrom: /docs/reference/other-new-features/into-modifier.html
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/into-modifier.html
---

This feature is not yet part of the Scala 3 language definition. It can be made available by a language import:

```scala
import scala.language.experimental.into
```

Scala 3's implicit conversions of the `scala.Conversion` class require a language import
```
import scala.language.implicitConversions
```
in any code that uses them as implicit conversions (code that calls conversions explicitly is not affected). If the import is missing, a feature warning is currently issued, and this will become an error in future versions of Scala 3. The motivation for this restriction is two-fold:

 - Code with hidden implicit conversions is hard to understand and might have correctness or performance problems that go undetected.
 - If we require explicit user-opt in for implicit conversions, we can significantly improve type inference by propagating expected type information more widely.

There is one broad use case, however, where implicit conversions are very hard to replace. This is the case where an implicit conversion is used to adapt a method argument to its formal parameter type. An example from the standard library:
```scala
scala> val xs = List(0, 1)
scala> val ys = Array(2, 3)
scala> xs ++ ys
val res0: List[Int] = List(0, 1, 2, 3)
```
The last input made use of an implicit conversion from `Array[Int]` to `IterableOnce[Int]` which is defined as a Scala 2 style implicit conversion in the standard library. Once the standard library is rewritten with Scala 3 conversions, this will
require a language import at the use site, which is clearly unacceptable. It is possible to avoid the need for implicit conversions using method overloading or type classes, but this often leads to longer and more complicated code, and neither of these alternatives work for vararg parameters.

This is where the `into` type alias comes in. Here is a signature of a `++` method on `List[A]` that uses it:

```scala
  def ++ (elems: into[IterableOnce[A]]): List[A]
```
The `into` wrapper on the type of `elems` means that implicit conversions can be applied to convert the actual argument to an `IterableOnce` value, and this without needing a language import.

`into` is defined as follows in the companion object of the `scala.Conversion` class:
```scala
opaque type into[T] >: T = T
```
Types of the form `into[T]` are treated specially during type checking. If the expected type of an expression is `into[T]` then an implicit conversion to that type can be inserted without the need for a language import.

Note: Unlike other types, `into` starts with a lower-case letter. This emphasizes the fact that `into` is treated specially by the compiler, by making `into` look more like a keyword than a regular type.

**Example:**

```scala
given Conversion[Array[Int], IterableOnce[Int]] = wrapIntArray
val xs: List[Int] = List(1)
val ys: Array[Int] = Array(2, 3)
xs ++ ys
```
This inserts the given conversion on the `ys` argument in `xs ++ ys`. It typechecks without a feature warning since the formal parameter of `++` is of type `into[IterableOnce]`, which is also the expected type of `ys`.

The

## `into` in Function Results

`into` allows conversions everywhere it appears as expected type, including in the results of function arguments. For instance, consider the new proposed signature of the `flatMap` method on `List[A]`:

```scala
  def flatMap[B](f: A => into[IterableOnce[B]]): List[B]
```
This accepts all actual arguments `f` that, when applied to an `A`, give a result
that is convertible to `IterableOnce[B]`. So the following would work:
```scala
scala> val xs = List(1, 2, 3)
scala> xs.flatMap(x => x.toString * x)
val res2: List[Char] = List(1, 2, 2, 3, 3, 3)
```
Here, the conversion from `String` to `Iterable[Char]` is applied on the results of `flatMap`'s function argument when it is applied to the elements of `xs`.

## Vararg arguments

When applied to a vararg parameter, `into` allows a conversion on each argument value individually. For example, consider a method `concatAll` that concatenates a variable
number of `IterableOnce[Char]` arguments, and also allows implicit conversions into `IterableOnce[Char]`:

```scala
def concatAll(xss: into[IterableOnce[Char]]*): List[Char] =
  xss.foldLeft(List[Char]())(_ ++ _)
```
Here, the call
```scala
concatAll(List('a'), "bc", Array('d', 'e'))
```
would apply two _different_ implicit conversions: the conversion from `String` to `Iterable[Char]` gets applied to the second argument and the conversion from `Array[Char]` to `Iterable[Char]` gets applied to the third argument.


## Unwrapping `into`

Since `into[T]` is an opaque type, its run-time representation is just `T`.
At compile time, the type `into[T]` is a known supertype of the type `T`. So if `t: T`, then
```scala
  val x: into[T] = t
```
typechecks but
```scala
val y: T = x   // error
```
is ill-typed. We can recover the underlying type `T` using the `underlying` extension method which is also defined in object `Conversion`:
```scala
import Conversion.underlying

val y: T = x.underlying    // ok
```
However, the next section shows that unwrapping with `.underlying` is not needed for parameters, which is the most common use case. So explicit unwrapping should be quite rare.



## Dropping `into` for Parameters in Method Bodies

The typical use cases for `into` wrappers are for parameters. Here, they specify that the
corresponding arguments can be converted to the formal parameter types. On the other hand, inside a method, a parameter type can be assumed to be of the underlying type since the conversion already took place when the enclosing method was called. This is reflected in the type system which erases `into` wrappers in the local types of parameters
as they are seen in a method body. Here is an example:
```scala
  def ++ (elems: into[IterableOnce[A]]): List[A] =
    val buf = ListBuffer[A]()
    for elem <- elems.iterator do // no `.underlying` needed here
      buf += elems
    buf.toList
```
Inside the `++` method, `elems` is of type `IterableOnce[A]`, not `into[IterableOne[A]]`. Hence, we can simply write `elems.iterator` to get at the `iterator` method of the `IterableOnce` class.

Specifically, we erase all `into` wrappers in the local types of parameter types, on the top-level of these
types as well as in all top-level co-variant subparts. Here, a part `S` of a type `T` is
top-level covariant, if it is not enclosed in some type that appears in contra-variant or invariant position in `T`.

## Into in Aliases

Since `into` is a regular type constructor, it can be used anywhere, including in type aliases and type parameters. This gives a lot of flexibility to enable implicit conversions for user-visible types. For instance, the Laminar framework
defined a type `Modifier` that is commonly used as a parameter type of user-defined methods and that should support implicit conversions into it. Pattern like this can be supported by defining a type alias such as
```scala
type Modifier = into[ModifierClass]
```
The into-erasure for function parameters also works for aliases. So a function defining parameters of `Modifier` type can use them internally as if they were from the underlying `ModifierClass`.

