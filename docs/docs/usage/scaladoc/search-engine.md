---
layout: multipage-overview
title: "Type-based search"
partof: scala3-scaladoc
num: 7
previous-page: site-versioning
next-page: settings
---

Searching for functions by their symbolic names can be time-consuming.
That is why the new scaladoc allows searching for methods and fields by their types.


Consider the following extension method definition:
```
extension [T](arr: IArray[T]) def span(p: T => Boolean): (IArray[T], IArray[T]) = ...
```
Instead of searching for `span` we can also search for `IArray[A] => (A => Boolean) => (IArray[A], IArray[A])`.

To use this feature, type the signature of the member you are looking for in the scaladoc searchbar. This is how it works:

![]({{ site.baseurl }}/resources/images/scala3/scaladoc/inkuire-1.0.0-M2_js_flatMap.gif)

This feature is provided by the [Inkuire](https://github.com/VirtusLab/Inkuire) search engine, which works for Scala 3 and Kotlin. To be up-to-date with the development of this feature, follow the [Inkuire repository](https://github.com/VirtusLab/Inkuire).

## Examples of queries

Some examples of queries with intended results:
- `List[Int] => (Int => Long) => List[Long]` -> `map`
- `Seq[A] => (A => B) => Seq[B]` -> `map`
- `(A, B) => A` -> `_1`
- `Set[Long] => Long => Boolean` -> `contains`
- `Int => Long => Int` -> `const`
- `String => Int => Char` -> `apply`
- `(Int & Float) => (String | Double)` -> `toDouble`, `toString`
- `F[A] => Int` -> `length`

## Query syntax

In order for a scaladoc searchbar query to be searched using Inkuire instead of the default search engine, the query has to contain the `=>` character sequence.

Accepted input is similar to a curried function signature in Scala 3. With some differences:
- AndTypes, OrTypes and Functions have to be enclosed in parentheses e.g. `(Int & Any) => String`
- fields and parameterless methods can be found by preceding their type with `=>`, e.g., `=> Int`
- A wildcard `_` can be used to indicate that we want to match any type in a given place e.g. `Long => Double => _`
- Types in the form of single letter e.g. `A` or a letter with a digit `X1` are automatically assumed to be type variables
- Other type variables can be declared just like in polymorphic functions e.g. `[AVariable, AlsoAVariable] => AVariable => AlsoAVariable => AVariable`

### Working with type aliases and method receivers

When it comes to how the code is mapped to InkuireDb entries, there are some transformations to make the engine more opinionated (though open to suggestions and changes). Firstly, the receiver (non-module owner) of a function can be treated as a first argument. Automatic currying is also applied, so that the results don't depend on argument lists. When finding matches, `val`s and `def`s are not distinguished.

So the following declarations should be found by query `Num => Int => Int => Int`:
```
class Num():
  def a(i: Int, j: Int): Int
  def b(i: Int)(j: Int): Int
  def c(i: Int): (Int => Int)
  val d: Int => Int => Int
  val e: Int => Int => Int
  val f: (Int, Int) => Int
end Num

def g(i: Num, j: Int, k: Int): Int
extension (i: Num) def h(j: Int, k: Int): Int
def i(i: Num, j: Int)(k: Int): Int
extension (i: Num) def j(j: Int)(k: Int): Int
...
```

When it comes to type aliases, they are desugared on both the declaration and the query signature. This means that for declarations:
```
type Name = String

def fromName(name: Name): String
def fromString(str: String): Name
```
both methods, `fromName` and `fromString`, should be found for queries `Name => Name`, `String => String`, `Name => String` and `String => Name`.

## How it works

Inkuire works as a JavaScript worker in the browser thanks to the power of [ScalaJS](https://www.scala-js.org/).

To enable Inkuire when running scaladoc, add the flag `-Ygenerate-inkuire`. By adding this flag two files are generated:
- `inkuire-db.json` - this is the file containing all the searchable declarations from the currently documented project in a format readable to the Inkuire search engine.
- `inkuire-config.json` - this file contains the locations of the database files that should be searchable from the documentation of the current project. By default, it will be generated with the location of the local db file as well as the default implied locations of database files in [External mappings](/scala3/guides/scaladoc/settings.html#-external-mappings).
