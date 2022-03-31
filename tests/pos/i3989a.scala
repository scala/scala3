object Test extends App {
  trait A[+X]
  class B[+X](val x: X) extends A[X]
  def f(a: A[Int]): Int = a match {
    case a: B[_] => a.x
    case _ => 0
  }
}
