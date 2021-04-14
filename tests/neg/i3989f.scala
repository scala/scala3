object Test extends App {
  trait A[+X](val x: X)
  class B[+X](val y: X) extends A[X](y)
  class C extends B(5) with A[String] // error: illegal inheritance

  class D extends B(5) with A[Any]

  def f(a: A[Int]): String = a match {
    case c: C => c.x
    case _ => "hello"
  }
  f(new C)
}