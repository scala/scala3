---
layout: doc-page
title: Syntax Changes in Dotty 0.22
---

In 2019, we experimented with several syntax changes in Dotty, most notably in the area of
implicits replacements. In Dotty 0.22, released on Feb 5, 2020, we have settled on what
we believe will be the definite syntax for Scala 3. Dotty 0.23 will support only this
syntax. Previous variants will no longer be supported.

Here is a rundown of how previous variants need to be rewritten in the
new syntax. This will be useful as a migration and learning help for people who have already
written code in one of the previous versions of Dotty.

## Given Instances

Given instances are always written with `given` and `as`. The previous use of `:` instead of `as` is no longer supported. Examples:
```scala
given intOrd as Ordering[Int] { ... }
given [T] as Ordering[List[T]] { ... }
given ctx as ExecutionContext = ...
given Ordering[String] { ... }
```
`as` can be omitted if the instance is anonymous and does not have parameters, as in the last definition above.

## Context Parameters

Context parameters are the replacement of Scala 2's implicit parameters. Context parameters and arguments both start with `using`. Examples:
```scala
def max[T](x: T, y: T)(using Ordering[T]): T = ...
given [T](using Ordering[T]) as Ordering[List[T]] { ... }

max(a, b)(using intOrd)
```
The previous syntax that uses `given` also for context parameters and arguments is no longer supported.

Context bounds remain supported as a shorthand for one-parameter type class constraints. So the two definitions above could also be written as
```scala
def max[T: Ordering](x: T, y: T): T = ...
given [T: Ordering] as Ordering[List[T]] { ... }
```
Parameters of context function values are also written with `using`. So it is
```scala
(using x: A) => E
```
instead of `(implicit x: A) => E` or `(given x: A) => E`.

## Context Functions Types

Implicit function types `implicit A => B` have been replaced with context function types, which are written `A ?=> B`. The syntax `(given A) => B` that was used in earlier Dotty versions is no longer supported.

## Given Imports

The syntax of wildcard given import selectors is now `given _`. Examples
```scala
import a.{given _}
import b.{_, given _}
```
The previous syntax, which used just `given` without an underscore is no longer supported. The change was made to better align with typed given import selectors such as `given Ordering[T]`,
which are unchanged.

## Collective Extensions

Collective extensions are now a separate construct. Example:
```scala
extension [T] on List[T] {
  def second: T ...
  def takeRightWhile(p: T => Boolean): List[T] = ...
}
```
Collective extensions still _expand_ to given instances with regular extension methods, but the previous syntaxes that expressed them as some syntactic variant of given instances are no longer supported.

## Extension Methods

There have been two syntax changes for regular extension methods. First,
any type parameters are now written in front, following the `def`. Second,
a "`.`" in front of the method name is now allowed (but not required). Example:
```scala
def [T](xs: List[T]).second: T
```
The previous syntax which used type parameters after the method name is no longer supported.

## Optional Braces For Definitions

Braces around the definitions of a class, object or similar construct can now be omitted
if the leading signature of the definition is followed by a `:` at the end a line. Examples:
```scala
trait Text:
  def toString: String

class Str(str: String) extends Text:
  def toString = str

class Append(txt1: Text, txt2: Text) extends Text:
  def toString = txt1 ++ txt2

object Empty extends Text:
  def toString = ""

extension on (t: Text):
  def length = toString.length

given Ordering[Text]:
  def compare(txt1: Text, txt2: Text): Int =
    summon[Ordering[String]].compare(txt1.toString, txt2.toString)
```
Previous variants required a `with` instead of the `:` or inserted braces around indented code after class, object, ... without any leading token. These are no longer supported.

Note that this interpretation of `:` as an alternative to braces only applies to class-level definitions. The use of `:` at the end of a line to imply braces around a following _function argument_ is not affected by this change. It still requires the `Yindent-colons` option.
