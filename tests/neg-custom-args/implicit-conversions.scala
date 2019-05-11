class A
class B

object A {

  implicit for Conversion[A, B] {
    def apply(x: A): B = ???
  }

  implicit for Conversion[B, A] {
    def apply(x: B): A = ???
  }
}

class C

object D {
  implicit for Conversion[A, C] {
    def apply(x: A): C = ???
  }
}

object Test {
  import implicit D._

  val x1: A = new B
  val x2: B = new A  // error under -Xfatal-warnings -feature
  val x3: C = new A  // error under -Xfatal-warnings -feature
}