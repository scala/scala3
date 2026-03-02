---
layout: blog-page
title: Announcing Dotty 0.23.0-RC1 - safe initialization checks, type-level bitwise operations and more
author: Anatolii Kmetiuk
authorImg: /images/anatolii.png
date: 2020-03-18
---

Hello! We are excited to announce 0.23.0-RC1 of Dotty. This version brings safe initialization checks, minor syntactic changes related to the context parameters, type-level bitwise operations and improvements of the metaprogramming capabilities.

You can try out this version right now, from the comfort of your SBT, by visiting the [home page](https://nightly.scala-lang.org/) and scrolling down to the "Create a Dotty Project" section.

Alternatively, you can try this version of Scala online via [Scastie](https://scastie.scala-lang.org/). Once you're there, click "Build Settings" and set "Target" to "Dotty".

Enjoy the ride🚀!

<!--more-->
# Cool new features
## Safe initialization checks
When a class is instantiated, the fields in the class body are initialized by field initializers, which could be any Scala code. Such a versatile language feature gives the programmer flexibility in defining how objects are initialized. However, such flexibility also brings complexity to ensure that we never accidentally use a field before it's initialized. Initialization errors can be difficult to spot in the presence of complex language features, such as inheritance, traits, inner classes, and aliasing. Such errors, sometimes simple sometimes subtle, require programmer efforts to debug and fix, which has been a [pain point for Scala programmers](https://contributors.scala-lang.org/t/improve-forward-reference-handling/3616) for a long time.

Most programming languages do not statically check initialization safety, such as C++, Java, Kotlin, etc.
Or, they check initialization safety but overly restrict how objects are initialized, like Swift.
Now, Scala 3 has the best of two worlds: flexibility of initialization patterns and static check for safety.

Consider the following program:

```scala
abstract class AbstractFile {
   def name: String
   val extension: String = name.reverse.dropWhile(_ != '.').reverse
}

class RemoteFile(url: String) extends AbstractFile {
   val localFile: String = url.hashCode + ".tmp"
   def name: String = localFile
}
```

Above, `extension` value is initialized prior to `localFile` because the fields of the parents of a class are initialized prior to the fields of the class. However, `extension` uses `localFile` during its initialization since it accesses this field from the `name` method. This scenario will lead to a `NullPointerException` on runtime when the access to uninitialized `localFile` happens.


In this release, we have added an aid for the programmer to detect such mistakes automatically. If you compile the above program with the `-Ysafe-init` flag, you will get the following compile-time error:

```scala
-- Error: /Users/kmetiuk/Projects/scala3/pg/release/snip_4.scala:8:7 -----------
8 |   val localFile: String = url.hashCode + ".tmp"
  |       ^
  |Access non-initialized field localFile. Calling trace:
  | -> val extension: String = name.reverse.dropWhile(_ != '.').reverse [ snip_4.scala:4 ]
  |  -> def name: String = localFile  [ snip_4.scala:9 ]
1 error found
```

You can learn more about the feature from the [documentation](https://nightly.scala-lang.org/0.23.0-RC1/docs/reference/other-new-features/safe-initialization.html). For the discussion, see PR [#7789](https://github.com/scala/scala3/pull/7789).

## Bitwise Int compiletime operations
In the previous release, Dotty has [received](https://nightly.scala-lang.org/blog/2020/02/05/22nd-dotty-milestone-release.html#primitive-compiletime-operations-on-singleton-types) a support for type-level arithmetic operations on integers. In this release, we are extending this support by adding bitwise operations. For example:

```scala
import scala.compiletime.ops.int._

@main def Test =
  val t1: 1 << 1 = 2
  val t2: 1 << 2 = 4
  val t3: 1 << 3 = 8
  val t4: 1 << 4 = 0 // error
```

Above `t4` will fail to compile with the following error:

  -- [E007] Type Mismatch Error: /Users/kmetiuk/Projects/scala3/pg/release/snip_3.scala:7:20
7 |  val t67: 1 << 4 = 0 // error
  |                    ^
  |                    Found:    (0 : Int)
  |                    Required: (16 : Int)

You can find the list of all the supported operations in the `scala.compiletime.ops` [package](https://github.com/bishabosha/dotty/blob/e2b0de0bf70bbde5a9a92dc7fa91b36537b02a87/library/src/scala/compiletime/ops/package.scala)

# Syntactic Changes
## Context functions syntax improved
In this release, we have done some work to improve the syntax of context functions. Now, their syntax is closer to the syntax for context parameters of methods.

Previously, a context function was written as follows:

```scala
// OLD SYNTAX
val ctxFunOld = (x: String) ?=> x.toInt
```

Now, it is written as follows:

```scala
val ctxFunNew = (using x: String) => x.toInt
```

We hope that this change will improve the readability of context functions for a person who already knows the syntax for context parameters of ordinary methods.

## Drop `given` parameter syntax
As part of our experimentation with the syntax of the language, we are now dropping the old syntax for context parameters.

The old syntax for context parameters was as follows:

```scala
// OLD SYNTAX, NO LONGER SUPPORTED
def f(given x: Int) = x * x
```

In the previous release, it was [replaced](https://nightly.scala-lang.org/blog/2020/02/05/22nd-dotty-milestone-release.html#further-improvements-to-the-context-parameters-syntax) by the new `using` syntax:

```scala
def f(using x: Int) = x * x
```

However, both syntaxes were supported for that release for experimental purposes. Now, we are dropping the support of the old syntax in favor of the new one as we see it as a clear win over the old one.

# Metaprogramming
## Inline version of `summon`
Inside an inline method, we often want to summon a value without declaring it as a context parameter of the method:

```scala
inline def lookup[X] =
  val x = summon[X]  // error
  // -- Error: /Users/kmetiuk/Projects/scala3/pg/release/snip_5.scala:6:19 ----------
  // 6 |  val x = summon[X]
  //   |                   ^
  //   |no implicit argument of type X was found for parameter x of method summon in object DottyPredef
  // 1 error found
  println(s"x = $x")
```

The above program will give us a compile time error because it cannot find a context parameter of type `X`. This is because the `summon` function is not inline and hence the compiler needs to know that a context parameter of type `X` exists on call site of `summon` which happens to be in the body of `lookup`. Since `X` is unknown in that body, the compiler can't find the context parameter and shows an error.

We have now added an inline version of `summon`:

```scala
import scala.compiletime.summonInline

inline def lookup[X] =
  val x = summonInline[X]
  println(s"x = $x")

@main def Test =
  given Int = 10
  lookup[Int]
```

`summonInline` is an inline version of `summon`. It is defined as follows:

```scala
inline def summonInline[T] <: T = summonFrom {
  case t: T => t
}
```

Since it is inline, the context parameter is resolved on expansion site, not the call site. The expansion site happens to be wherever `lookup` function is expanded, and there the type `X` is bound to a concrete type.

## `ValueOfExpr` renamed to `Unlifted`
This feature allows you to obtain the value captured in an expression using pattern matching:

Macro.scala

```scala
import scala.quoted._

inline def square(inline x: Int): Int = ${ squareImpl('x) }
def squareImpl(x: Expr[Int])(using QuoteContext): Expr[Int] =
  x match
    case Unlifted(value: Int) => Expr(value * value)
```

Test.scala

```scala
@main def Test =
  println(square(10))  // println(100)
```

## Extractors for quotes moved under `scala.quoted` package
The metaprogramming capabilities are undergoing simplifications in this release. In particular, fewer imports are now needed.

Previously, to access the extractors for expressions you had to do the `scala.quoted.matching._` import. Now, the extractors from there have been moved to `scala.quoted`. For example, you can write the following program:

Macro.scala:

```scala
import scala.quoted._

inline def square(inline x: Int): Int = ${ squareImpl('x) }
def squareImpl(xExpr: Expr[Int])(using QuoteContext): Expr[Int] =
  xExpr match
    case Const(x) => Expr(x * x)
```

Test.scala:

```scala
@main def Test =
  println(square(2))  // println(4)
```

Above, `Const` is an extractor that matches constants. Notice how we do not need to import anything else but `scala.quoted._` to use it.

## TASTy Reflect imports simplified
Previously, to access TASTy Reflect features of Dotty, you had to include an import as follows:

```scala
// OLD CODE
import qctx.tasty.{ _, given }
```

Above, `qctx` is a `QuoteContext` which is available in all the macro implementations. The `given` keyword imports all the context instances. In this particular case, it was needed to bring extension methods for ASTs in scope.

With this release, the `given` part is no longer needed and the extension methods are in scope after merely importing `import qctx.tasty._`.

Consider the following example:

Macro.scala

```scala
import scala.quoted._

inline def showTree(inline x: Any): String = ${ showTreeImpl('x) }
def showTreeImpl(x: Expr[Any])(using qctx: QuoteContext): Expr[String] =
  import qctx.tasty._
  x.unseal match
    case Inlined(_, _, app: Apply) =>
      val fun: Term = app.fun
      val args: List[Term] = app.args
      val res = s"Function: $fun\nApplied to: $args"
      Expr(res)
```

Test.scala

```scala
@main def Test =
  def f(x: Int) = x * x
  val x = 10
  println(showTree(f(x)))
```

Notice how above, we are calling `app.fun` and `app.args`. `fun` and `args` are extension methods on `Apply` tree node. Previously they would not have been available unless we did `import qctx.tasty.given`. However as of this release, the above program compiles without errors.

# Let us know what you think!

If you have questions or any sort of feedback, feel free to send us a message on our
[Gitter channel](https://gitter.im/lampepfl/dotty). If you encounter a bug, please
[open an issue on GitHub](https://github.com/scala/scala3/issues/new).

## Contributing

Thank you to all the contributors who made this release possible!

According to `git shortlog -sn --no-merges 0.22.0-RC1..0.23.0-RC1` these are:

```
   165  Martin Odersky
   124  Nicolas Stucki
   121  Liu Fengyun
    45  Robert Stoll
    15  Guillaume Martres
    15  Anatolii
    10  gzoller
     8  Som Snytt
     8  Stéphane Micheloud
     5  Ausmarton Zarino Fernandes
     5  Oron Port
     3  Adam Fraser
     3  Gabriele Petronella
     3  Uko
     3  Anatolii Kmetiuk
     2  ybasket
     2  Dale Wijnand
     2  Dani Rey
     2  Jamie Thompson
     2  Olivier Blanvillain
     2  Tomasz Godzik
     2  Travis Brown
     2  Vlastimil Dort
     1  tanaka takaya
     1  Miles Sabin
     1  Andrew Valencik
     1  bishabosha
     1  fhackett
     1  Lionel Parreaux
     1  kenji yoshida
     1  manojo
     1  odersky
     1  Raj Parekh
     1  Sébastien Doeraene
     1  xuwei-k
```

If you want to get your hands dirty and contribute to Dotty, now is a good time to get involved!
Head to our [Getting Started page for new contributors](https://nightly.scala-lang.org/docs/contributing/getting-started.html),
and have a look at some of the [good first issues](https://github.com/scala/scala3/issues?q=is%3Aissue+is%3Aopen+label%3Aexp%3Anovice).
They make perfect entry points into hacking on the compiler.

We are looking forward to having you join the team of contributors.

## Library authors: Join our community build

Dotty now has a set of widely-used community libraries that are built against every nightly Dotty
snapshot. Currently, this includes ScalaPB, algebra, scalatest, scopt and squants.
Join our [community build](https://github.com/lampepfl/dotty-community-build)
to make sure that our regression suite includes your library.

[Scastie]: https://scastie.scala-lang.org/?target=dotty

[@odersky]: https://github.com/odersky
[@DarkDimius]: https://github.com/DarkDimius
[@smarter]: https://github.com/smarter
[@felixmulder]: https://github.com/felixmulder
[@nicolasstucki]: https://github.com/nicolasstucki
[@liufengyun]: https://github.com/liufengyun
[@OlivierBlanvillain]: https://github.com/OlivierBlanvillain
[@biboudis]: https://github.com/biboudis
[@allanrenucci]: https://github.com/allanrenucci
[@Blaisorblade]: https://github.com/Blaisorblade
[@Duhemm]: https://github.com/Duhemm
[@AleksanderBG]: https://github.com/AleksanderBG
[@milessabin]: https://github.com/milessabin
[@anatoliykmetyuk]: https://github.com/anatoliykmetyuk
