import scala.language.unsafeJavaReturn

import scala.annotation.CanEqualNull
import java.{util => ju}

class S {

  def test[T <: AnyRef](jc: JC) = {
    val a: Int = jc.a

    val b = jc.b // it returns String @CanEqualNull
    val b2: String = b
    val b3: String @CanEqualNull = jc.b
    val b4: String = jc.b
    val bb = jc.b == null // it's ok to compare String @CanEqualNull with Null
    val btl = jc.b.trim().length() // String @CanEqualNull is just String, unsafe selecting

    val c = jc.c
    val cl = c.length
    val c2: Array[String] = c
    val c3: Array[String @CanEqualNull] @CanEqualNull = jc.c
    val c4: Array[String] = jc.c
    val cml: Array[Int] = c.map(_.length())

    val f1: Int = jc.f1()

    val f21: Array[Int] @CanEqualNull = jc.f2()
    val f22: Array[Int] = jc.f2()
    val f2n = jc.f2() == null

    val g11: String @CanEqualNull = jc.g1()
    val g12: String = jc.g1()
    val g1n = jc.g1() == null
    val g1tl = jc.g1().trim().length()

    val g21: ju.List[String] @CanEqualNull = jc.g2()
    val g22: ju.List[String] = jc.g2()

    val g31: Array[String @CanEqualNull] @CanEqualNull = jc.g3()
    val g32: Array[String] = jc.g3()
    val g3n = jc.g3() == null
    val g3m: Array[Boolean] = jc.g3().map(_ == null)

    val h11: T @CanEqualNull = jc.h1[T]()
    val h12: T = jc.h1[T]()
    val h1n = jc.h1[T]() == null

    val h21: ju.List[T] @CanEqualNull = jc.h2[T]()
    val h22: ju.List[T] = jc.h2[T]()

    val h31: Array[T @CanEqualNull] @CanEqualNull = jc.h3[T]()
    val h32: Array[T] = jc.h3[T]()
    val h3m = jc.h3[T]().map(_ == null)
  }
}