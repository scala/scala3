abstract class A {
  def a: Long
}

object O {
  case class B() extends A {
    def a = 5L
  }
  case class C(a2: A) extends A {
    var c: Long = a2.a
    def a = c
  }
  def f(a: A): A = C(f(a))
  def g(): A = f(B())

  val x = g()
}