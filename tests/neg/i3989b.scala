object Test extends App {
  trait A[+X]
  case class B[+X](val x: X) extends A[X]
  class C[+X](x: Any) extends B[Any](x) with A[X] // error
  def f(a: A[Int]): Int = a match {
    case B(i) => i
    case _ => 0
  }
  f(new C[Int]("foo"))
}
