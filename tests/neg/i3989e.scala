object Test extends App {
  trait A[+X](val x: X)
  class B extends A(5) with A("hello") // error: A is extended twice

  def f(a: A[Int]): Int = a match {
    case b: B => b.x
    case _ => 0
  }

  f(new B)
}