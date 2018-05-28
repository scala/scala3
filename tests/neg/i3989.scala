object Test extends App {
  trait A[+X] { def get: X }
  case class B[X](x: X) extends A[X] { def get: X = x }
  class C[X](x: Any) extends B[Any](x) with A[X] // error: not a legal implementation of `get'
  def g(a: A[Int]): Int = a.get
  g(new C[Int]("foo"))
}