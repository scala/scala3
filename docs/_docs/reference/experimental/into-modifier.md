---
layout: doc-page
title: "The `into` Type Modifier"
redirectFrom: /docs/reference/other-new-features/into-modifier.html
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/into-modifier.html
---

Scala 3's implicit conversions of the `scala.Conversion` class require a language import
```
import scala.language.implicitConversions
```
in any code that uses them as implicit conversions (code that calls conversions explicitly is not affected). If the import is missing, a feature warning is currently issued, and this will become an error in a future version of Scala 3. The motivation for this restriction is that code with hidden implicit conversions is hard to understand and might have correctness or performance problems that go undetected.

There is one broad use case, however, where implicit conversions are very hard to replace. This is the case where an implicit conversion is used to adapt a method argument to its formal parameter type. An example from the standard library:
```scala
scala> val xs = List(0, 1)
scala> val ys = Array(2, 3)
scala> xs ++ ys
val res0: List[Int] = List(0, 1, 2, 3)
```
The last input made use of an implicit conversion from `Array[Int]` to `IterableOnce[Int]` which is defined as a Scala 2 style implicit conversion in the standard library. Once the standard library is rewritten with Scala 3 conversions, this will
require a language import at the use site, which is clearly unacceptable. It is possible to avoid the need for implicit conversions using method overloading or type classes, but this often leads to longer and more complicated code, and neither of these alternatives work for vararg parameters.

This is where the `into` modifier on parameter types comes in. Here is a signature of the `++` method on `List[A]` that uses it:
```scala
  def ++ (elems: into IterableOnce[A]): List[A]
```
The `into` modifier on the type of `elems` means that implicit conversions can be applied to convert the actual argument to an `IterableOnce` value, and this without needing a language import.

## Function arguments

`into` also allows conversions on the results of function arguments. For instance, consider the new proposed signature of the `flatMap` method on `List[A]`:

```scala
  def flatMap[B](f: into A => IterableOnce[B]): List[B]
```
This allows a conversion of the actual argument to the function type `A => IterableOnce[B]`. Crucially, it also allows that conversion to be applied to
the function result. So the following would work:
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
def concatAll(xss: into IterableOnce[Char]*): List[Char] =
  xss.foldLeft(List[Char]())(_ ++ _)
```
Here, the call
```scala
concatAll(List('a'), "bc", Array('d', 'e'))
```
would apply two _different_ implicit conversions: the conversion from `String` to `Iterable[Char]` gets applied to the second argument and the conversion from `Array[Char]` to `Iterable[Char]` gets applied to the third argument.

## Retrofitting Scala 2 libraries

A new annotation `allowConversions` has the same effect as an `into` modifier. It is defined as an `@experimental` class in package `scala.annotation`. It is intended to be used for retrofitting Scala 2 library code so that Scala 3 conversions can be applied to arguments without language imports. For instance, the definitions of
`++` and `flatMap` in the Scala 2.13 `List` class could be retrofitted as follows.
```scala
  def ++ (@allowConversions elems: IterableOnce[A]): List[A]
  def flatMap[B](@allowConversions f: A => IterableOnce[B]): List[B]
```
For Scala 3 code, the `into` modifier is preferred. First, because it is shorter,
and second, because it adheres to the principle that annotations should not influence
typing and type inference in Scala.

## Syntax changes

The addition to the grammar is:
```
ParamType        ::=  [‘=>’] ParamValueType
ParamValueType   ::=  [‘into‘] ExactParamType
ExactParamType   ::=  Type [‘*’]
```
As the grammar shows, `into` can only applied to the type of a parameter; it is illegal in other positions.
