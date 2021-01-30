import scala.compiletime.*

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
    println(s"compile-time: ${codeOf(test1)}")
    println(s"run-time: ${codeOf(test1)}")

    println(s"compile-time: ${codeOf(test2)}")
    println(s"run-time: ${codeOf(test2)}")

    println(s"compile-time: ${codeOf(test3)}")
    println(s"run-time: ${codeOf(test3)}")

    println(s"compile-time: ${codeOf(test4)}")
    println(s"run-time: ${codeOf(test4)}")

    // this is the only test that should not be possible to fully inline,
    // because it references a non-inline value
    println(s"compile-time: ${codeOf(test5)}")
    println(s"run-time: ${codeOf(test5)}")

    println(s"compile-time: ${codeOf(test6)}")
    println(s"run-time: ${codeOf(test6)}")

    println(s"compile-time: ${codeOf(test7)}")
    println(s"run-time: ${codeOf(test7)}")

    println(s"compile-time: ${codeOf(test8)}")
    println(s"run-time: ${codeOf(test8)}")

    println(s"compile-time: ${codeOf(test9)}")
    println(s"run-time: ${codeOf(test9)}")

    // with type parameter
    println(s"compile-time: ${codeOf(test10)}")
    println(s"run-time: ${codeOf(test10)}")
  }
}

