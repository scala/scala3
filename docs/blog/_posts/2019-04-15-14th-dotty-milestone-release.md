---
layout: blog-page
title: Announcing Dotty 0.14.0-RC1 with export, immutable arrays, creator applications and more
author: Anatolii Kmetiuk
authorImg: /images/anatolii.png
date: 2019-04-15
---

Hello! This is the 14th release of Dotty. Some of the most interesting changes in this release include the new `export`, the dual of `import`, feature, an immutable array type and the creator applications syntax.

This release serves as a technology preview that demonstrates new
language features and the compiler supporting them.

Dotty is the project name for technologies that are being considered for
inclusion in Scala 3. Scala has pioneered the fusion of object-oriented and
functional programming in a typed setting. Scala 3 will be a big step towards
realising the full potential of these ideas. Its main objectives are to

- become more opinionated by promoting programming idioms we found to work well,
- simplify where possible,
- eliminate inconsistencies and surprising behaviours,
- build on strong foundations to ensure the design hangs together well,
- consolidate language constructs to improve the language’s consistency, safety, ergonomics, and
  performance.

You can learn more about Dotty on our [website](https://dotty.epfl.ch).

<!--more-->

This is our 14th scheduled release according to our
[6-week release schedule](https://dotty.epfl.ch/docs/usage/version-numbers.html).

# What’s new in the 0.14.0-RC1 technology preview?

## Export as a dual of Import

A new `export` keyword is added to the language that defines aliases for selected members of an object. Consider the following example:

```scala
class BitMap
class InkJet
class Printer {
  type PrinterType
  def print(bits: BitMap): Unit = ???
  def status: List[String] = ???
}
class Scanner {
  def scan(): BitMap = ???
  def status: List[String] = ???
}
class Copier {
  private val printUnit = new Printer { type PrinterType = InkJet }
  private val scanUnit = new Scanner
  export scanUnit.scan
  export printUnit.{status => _, _}
  def status: List[String] = printUnit.status ++ scanUnit.status
}
```

When defined like this, the `Copier` class defines aliases of the `scanner`'s `scan` method and all the methods of `printUnit` except the `status` method. You can hence call them on the `Copier` as follows:

```scala
val copier = new Copier
copier.print(copier.scan())
```

The motivation for this change is to promote composition over inheritance. In OOP languages it is easy to define inheritance but the above example would be tricky if you follow the composition route. One would need to implement proxy methods in the `Copier` to delegate to those of the `Scanner` and the `Printer`.

With the `export` feature, making the `Copier` behave as the `Printer` and the `Scanner` became much more ergonomic. Also, note the fine-grained control over which methods are exposed in cases of the possible method collision, as shown with the `status` method example.

For more information, please read more in the [documentation](http://dotty.epfl.ch/docs/reference/other-new-features/export.html).


## An immutable array type
A new type, `scala.IArray[T]`, is added, which is an immutable version of the `Array` type. Its implementation deserves a special attention, as it uses the new Dotty features in an elegant way (the below is an abstract from the corresponding [commit](https://github.com/lampepfl/dotty/commit/af2a0e66eb4b1204eac5dcb1d979486b92ef93d7#diff-156dc405d9f228bbc0fe406dfba63f65):

```scala
opaque type IArray[T] = Array[T]

object IArray {

  implied arrayOps {
    inline def (arr: IArray[T]) apply[T] (n: Int): T = (arr: Array[T]).apply(n)
    inline def (arr: IArray[T]) length[T] : Int = (arr: Array[T]).length
  }
  def apply[T: ClassTag](xs: T*): IArray[T] = Array(xs: _*)
  /*...*/
}
```

Essentially, the above defines a wrapper around the ordinary `Array` which exposes only its `apply` (to get an element by index) and `length` methods:

- `opaque type IArray[T]` defines a type which is known to be an `Array`, but this information is known only in its companion object `IArray`. To the rest of the world, this information is not available.
- The `implied arrayOps` implied instance defines the extension methods that expose the operations available on the `IArray` type.
- The extension methods, `apply` and `length`, delegate to these of the `Array` type. These methods are inlined which means the performance footprint is the same as that of the original `Array` methods.
- Because the methods are `inline` and because the `IArray` type is `opaque`, we need the `(arr: Array[T])` casts. `IArray` is known to be equal to `Array` only inside the `IArray` object and the inlining will cause the extension methods' bodies to appear outside the `IArray` object when these methods are called.

## Creator Applications
This new language feature is a generalisation of the ability to construct case classes without the `new` keyword:

```scala
class StringBuilder(s: String) {
  def this() = this("")
}
StringBuilder("abc")  // same as new StringBuilder("abc")
StringBuilder()       // same as new StringBuilder()
```

The motivation for the feature is mainly ergonomic. To make it possible, a new interpretation was added to a function call `f(a)`. Previously, the rules were as follows:

Given a function call `f(args)`,

 - if `f` is a method applicable to `args`, typecheck `f(args)` unchanged,
 - otherwise, if `f` has an `apply` method applicable to `args` as a member, continue with `f.apply(args)`,
 - otherwise, if `f` is of the form `p.m` and there is an implicit conversion `c` applicable to `p` so that `c(p).m` is applicable to `args`, continue with  `c(p).m(args)`

 There's now a fourth rule following these rules:

 - otherwise, if `f` is syntactically a stable identifier, and `new f` where `f` is interpreted as a type identifier is applicable to `args`, continue with `new f(args)`.

For more information, please see the [documentation](http://dotty.epfl.ch/docs/reference/other-new-features/creator-applications.html).

## Other changes

Some of the other changes include:

- `infer` method renamed to `the`, the semantics of which is now the same as that of the `the` method of Shapeless. Namely, the implicits are resolved more precisely – see this [gist](https://gist.github.com/milessabin/8833a1dbf7e8245b30f8) for an example in Shapeless, and the Dotty [documentation](http://dotty.epfl.ch/docs/reference/contextual/inferable-params.html#querying-implied-instances) for more details.
- The syntax of quoting and splicing was changed. Now the quoting is expressed via `'{ ... }` and `'[...]` and splicing – via `${...}` and `$id`. Please see the [documentation](http://dotty.epfl.ch/docs/reference/other-new-features/principled-meta-programming.html) for more details on these features.

# Let us know what you think!

If you have questions or any sort of feedback, feel free to send us a message on our
[Gitter channel](https://gitter.im/lampepfl/dotty). If you encounter a bug, please
[open an issue on GitHub](https://github.com/lampepfl/dotty/issues/new).

## Contributing

Thank you to all the contributors who made this release possible!

According to `git shortlog -sn --no-merges 0.13.0-RC1..0.14.0-RC1` these are:

```
   214  Martin Odersky
   151  Nicolas Stucki
    71  Liu Fengyun
    53  Guillaume Martres
    26  Olivier Blanvillain
    10  Aleksander Boruch-Gruszecki
     9  Aggelos Biboudis
     6  Miles Sabin
     4  Allan Renucci
     4  Dale Wijnand
     3  Anatolii Kmetiuk
     2  Fengyun Liu
     2  Alex Zolotko
     1  gnp
     1  tim-zh
     1  Dmitry Petrashko
     1  Dotty CI
     1  Jasper Moeys
     1  Jentsch
     1  Jim Van Horn
     1  Lionel Parreaux
     1  Master-Killer
     1  Olivier ROLAND
     1  Robert Stoll
     1  Seth Tisue
     1  Tomasz Godzik
     1  Victor
```

If you want to get your hands dirty and contribute to Dotty, now is a good time to get involved!
Head to our [Getting Started page for new contributors](https://dotty.epfl.ch/docs/contributing/getting-started.html),
and have a look at some of the [good first issues](https://github.com/lampepfl/dotty/issues?q=is%3Aissue+is%3Aopen+label%3Aexp%3Anovice).
They make perfect entry points into hacking on the compiler.

We are looking forward to having you join the team of contributors.

## Library authors: Join our community build

Dotty now has a set of widely-used community libraries that are built against every nightly Dotty
snapshot. Currently this includes ScalaPB, algebra, scalatest, scopt and squants.
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
