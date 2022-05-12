import scala.language.unsafeJavaReturn
import java.{util => ju}

class S {

  def test[T <: AnyRef](jc: JC) = {
    // val a: Int = jc.a

    // val b = jc.b // it returns String @CanEqualNull
    // val b2: String = b
    // val b3: String = jc.b
    // val bb = jc.b == null // it's ok to compare String @CanEqualNull with Null
    // val btl = jc.b.trim().length() // String @CanEqualNull is just String, unsafe selecting

    // val c = jc.c
    // val cl = c.length
    // val c2: Array[String] = c
    val c3: Array[String] = jc.c
    // val c4: Array[Int] = c.map(_.length())

    // val f1: Int = jc.f1()
    // val f2: Array[Int] = jc.f2()
    // val f2n = jc.f2() == null

    // val g1: String = jc.g1()
    // val g1n = jc.g1() == null
    // val g1tl = jc.g1().trim().length()

    // val g2h: ju.List[String] = jc.g2()

    // val g3: Array[String] = jc.g3()
    // val g3n = jc.g3() == null
    // val g3m: Array[Boolean] = jc.g3().map(_ == null)

    // val h1: T = jc.h1[T]()

    // val h2: ju.List[T] = jc.h2()

    // val h3: Array[T] = jc.h3()
  }
}