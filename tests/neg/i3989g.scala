object Test extends App {
  class A[+X](val x: X)
  class B[+X](val y: X) extends A[X](y)
  trait T[+X] extends A[X]
  class C extends B(5) with T[String] // error: illegal inheritance

  def f(a: A[Int]): String = a match {
    case c: C => c.x
    case _ => "hello"
  }
  f(new C)
}