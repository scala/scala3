class Opt[T]

class Buf[A](elts: Any, sz: Int):
  def this(n: Int) = this(???, n)

object Buf:
  def apply[A](elts: Any, sz: Int): Buf[A] = new Buf[A](elts, sz)
  def apply[A](n: Int): Buf[A] = apply[A](???, n)

inline def foo(testFun: Any): Unit = {}

val x = foo {
  type Rec[A] = A match
    case String => Opt[Rec[String]]

  val arr = new Buf[Rec[String]](8)
  val arr2 = Buf[Rec[String]](8)
  val arr3 = Buf.apply[Rec[String]](8)
}

import scala.collection.mutable

// https://github.com/plokhotnyuk/jsoniter-scala/blob/74d6d557bf81e904d07d4b8fbead4e4cab700bea/jsoniter-scala-macros/shared/src/test/scala-3/com/github/plokhotnyuk/jsoniter_scala/macros/JsonCodeMakerNewTypeSpec.scala#L40-L148
class Spec {
  inline def in(testFun: => Any): Unit = {
    val _ = testFun
  }

  in {
    type JsonPrimitive = String | Int
    type Rec[JA[_], A] = A match {
      case JsonPrimitive => JsonPrimitive | JA[Rec[JA, JsonPrimitive]]
      case _ => A | JA[Rec[JA, A]]
    }

    type Json = Rec[
      [A] =>> Seq[A],
      JsonPrimitive
    ]

    val arr = new mutable.ArrayBuffer[Json](8)
  }
}
