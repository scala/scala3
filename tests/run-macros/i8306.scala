import scala.compiletime._

case class A(i: Int)
case class B(a: A)
case class C[T](t: T)

trait Test8 {
  inline def test8: Int =
    inline A(3) match {
      case A(i) => i
    }
}

object Test extends Test8 {

  inline def test1: Int =
    inline A(3) match {
      case A(i) => i
    }

  inline def test2: Int =
    inline (A(3) : A) match {
      case A(i) => i
    }

  inline def test3: Int =
    inline B(A(3)) match {
      case B(A(i)) => i
    }

  inline def test4: Int =
    A(3).i

  val a = A(3)
  inline def test5: Int =
    inline new B(a) match {
      case B(A(i)) => i
    }

  inline def test6: Int =
    inline B(A(3)).a match {
      case A(i) => i
    }

  inline def test7: Int =
    inline new A(3) match {
      case A(i) => i
    }

  inline def test9: Int =
    B(A(3)).a.i

  inline def test10: Int =
    inline C(3) match {
      case C(t) => t
    }

  def main(argv: Array[String]): Unit = {
    println(code"compile-time: ${test1}")
    println(s"run-time: ${test1}")

    println(code"compile-time: ${test2}")
    println(s"run-time: ${test2}")

    println(code"compile-time: ${test3}")
    println(s"run-time: ${test3}")

    println(code"compile-time: ${test4}")
    println(s"run-time: ${test4}")

    // this is the only test that should not be possible to fully inline,
    // because it references a non-inline value
    println(code"compile-time: ${test5}")
    println(s"run-time: ${test5}")

    println(code"compile-time: ${test6}")
    println(s"run-time: ${test6}")

    println(code"compile-time: ${test7}")
    println(s"run-time: ${test7}")

    println(code"compile-time: ${test8}")
    println(s"run-time: ${test8}")

    println(code"compile-time: ${test9}")
    println(s"run-time: ${test9}")

    // with type parameter
    println(code"compile-time: ${test10}")
    println(s"run-time: ${test10}")
  }
}

