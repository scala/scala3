import scala.Option
object Test extends App {
  trait A[+X]
  class B[+X](val x: X) extends A[X]
  object B {
    def unapply[X](b: B[X]): Option[X] = Some(b.x)
  }

  class C[+X](x: Any) extends B[Any](x) with A[X]
  def f(a: A[Int]): Int = a match {
    case B(i) => i   // error
    case _ => 0
  }
  f(new C[Int]("foo"))
}
