---
layout: doc-page
title: "Restrictions to Implicit Conversions"
---

Previously, an implicit value of type `Function1`, or any of its subtypes
could be used as an implicit conversion. That is, the following code would compile
even though it probably masks a type error:

    implicit val m: Map[String, Int] = Map(1 -> "abc")

    val x: String = 1  // scalac: assigns "abc" to x
                       // Dotty: type error

By contrast, Dotty only considers _methods_ as implicit conversions, so the
`Map` value `m` above would not qualify as a conversion from `String` to `Int`.

To be able to express implicit conversions passed as parameters, `Dotty`
introduces a new type

    abstract class ImplicitConverter[-T, +U] extends Function1[T, U]

Implicit values of type `ImplicitConverter[A, B]` do qualify as implicit
conversions. It is as if there was a global implicit conversion method

    def convert[A, B](x: A)(implicit converter: ImplicitConverter[A, B]): B =
      converter(x)

(In reality the Dotty compiler simulates the behavior of this method directly in
its type checking because this turns out to be more efficient).

In summary, previous code using implicit conversion parameters such as

    def useConversion(implicit f: A => B) = {
      val y: A = ...
      val x: B = a    // error under Dotty
    }

is no longer legal and has to be rewritten to

    def useConversion(implicit f: ImplicitConverter[A, B]) = {
      val y: A = ...
      val x: B = y    // OK
    }

### Reference

For more info, see [PR #2065](https://github.com/lampepfl/dotty/pull/2065).

