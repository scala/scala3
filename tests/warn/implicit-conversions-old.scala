//> using options  -feature

class A
class B

object A {

  implicit def a2b(x: A): B = ??? // warn under -Werror -feature

  implicit def b2a(x: B): A = ??? // warn under -Werror -feature
}

class C

object D {
  implicit def a2c(x: A): C = ??? // warn under -Werror -feature
}

object Test {
  import D.*

  val x1: A = new B
  val x2: B = new A  // ok, since it's an old-style comversion
  val x3: C = new A  // ok, since it's an old-style comversion
}
