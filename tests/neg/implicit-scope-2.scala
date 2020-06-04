class A {
  class B
}

trait T
object T {
  implicit def b: AA.a.B = ???
}

object AA {
  val a = new A with T {}
}

object test {
  implicitly[AA.a.B] // error
}