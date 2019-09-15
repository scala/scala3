class A
class B

object A {

  given Conversion[A, B] {
    def apply(x: A): B = ???
  }

  given Conversion[B, A] {
    def apply(x: B): A = ???
  }
}

class C

object D {
  given Conversion[A, C] {
    def apply(x: A): C = ???
  }
}

object Test {
  import given D._

  val x1: A = new B
  val x2: B = new A  // error under -Xfatal-warnings -feature
  val x3: C = new A  // error under -Xfatal-warnings -feature
}