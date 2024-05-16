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
  def flatMap[B](f: A => into IterableOnce[B]): List[B]
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
def concatAll(xss: (into IterableOnce[Char])*): List[Char] =
  xss.foldLeft(List[Char]())(_ ++ _)
```
Here, the call
```scala
concatAll(List('a'), "bc", Array('d', 'e'))
```
would apply two _different_ implicit conversions: the conversion from `String` to `Iterable[Char]` gets applied to the second argument and the conversion from `Array[Char]` to `Iterable[Char]` gets applied to the third argument.

Note that a vararg parameter type with into modifiers needs to be put in parentheses, as is shown in the example above. This is to make the precedence clear: each element of the argument sequence is converted by itself.

## Retrofitting Scala 2 libraries

There is also an annotation `@into` in the `scala.annotation` package that has
the same effect as an `into` modifier. It is intended to be used for retrofitting Scala 2 library code so that Scala 3 conversions can be applied to arguments without language imports. For instance, the definitions of
`++` and `flatMap` in the Scala 2.13 `List` class could be retrofitted as follows.
```scala
  def ++ (elems: IterableOnce[A] @into): List[A]
  def flatMap[B](f: A => IterableOnce[B] @into): List[B]
```
For Scala 3 code, the `into` modifier is preferred, because it adheres to the principle that annotations should not influence typing and type inference in Scala.

## Restrictions

The `into` modifier is only allowed in the types of method parameters. It can be given either for the whole type, or some result type of a top-level function type, but not anywhere else. The `into` modifier does not propagate outside the method. In particular, a partially applied method does not propagate `into` modifiers to its result.

**Example:**

Say we have
```scala
def f(x: Int)(y: into Text): Unit
```
then
```scala
f(3) : Text => Unit
```
Note the `into` modifier is not longer present on the type of `f(3)`. Therefore, follow-on arguments to `f(3)` do not allow implicit conversions. Generally it is not possible to
define function types that allow implicit conversions on their arguments, but it is possible to define SAM types that allow conversions. E.g.
```scala
trait ConvArg:
  def apply(x: into Text): Unit

val x: ConvArg = f(3)(_)
```

Note this is similar to the way vararg parameters are handled in Scala. If we have
```scala
def g(x: Int)(y: Int*): Unit
```
then
```scala
g(4) : Seq[Int] => Unit
```
Observe that the vararg annotation also got dropped in the result type of `g(4)`.

## Syntax changes

The addition to the grammar is:
```
ParamType         ::=  [‘=>’] ParamValueType
ParamValueType    ::=  Type [‘*’]
                    |  IntoType
                    |  ‘(’ IntoType ‘)’ ‘*’
IntoType          ::=  [‘into’] IntoTargetType
                    |  ‘(’ IntoType ‘)’
IntoTargetType    ::=  Type
                    |  FunTypeArgs (‘=>’ | ‘?=>’) IntoType
```
As the grammar shows, `into` can only applied in the type of a parameter; it is illegal in other positions. Also, `into` modifiers in vararg types have to be enclosed in parentheses.
