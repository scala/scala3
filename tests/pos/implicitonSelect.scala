object test {
  class A
  class B
  implicit def a2b(x: A): B = new B
  class ARef { val a: A = new A }
  val x = new ARef
  val b: B = x.a
}
