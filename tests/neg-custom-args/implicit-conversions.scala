class A
class B

object A {

  delegate for Conversion[A, B] {
    def apply(x: A): B = ???
  }

  delegate for Conversion[B, A] {
    def apply(x: B): A = ???
  }
}

class C

object D {
  delegate for Conversion[A, C] {
    def apply(x: A): C = ???
  }
}

object Test {
  import delegate D._

  val x1: A = new B
  val x2: B = new A  // error under -Xfatal-warnings -feature
  val x3: C = new A  // error under -Xfatal-warnings -feature
}