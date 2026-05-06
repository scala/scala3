//> using options  -feature

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
  import D.given

  val x1: A = new B  // warn under -Werror -feature
  val x2: B = new A  // warn under -Werror -feature
  val x3: C = new A  // warn under -Werror -feature
}
